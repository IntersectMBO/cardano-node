{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Faucet where

import           Cardano.Api
import           Prelude (show, error, id)
import qualified Data.ByteString.Lazy     as LBS
import           Cardano.Prelude
import           Text.Parsec
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import           Cardano.Api.Byron
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import           Control.Concurrent.STM
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import           Servant
import           Network.Wai.Handler.Warp
import           Data.String
import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Types
import Cardano.CLI.Shelley.Key
import qualified Data.Text as T
import           Control.Monad.Trans.Except.Exit (orDie)
import Cardano.CLI.Shelley.Commands
import Cardano.CLI.Shelley.Run.Transaction
import Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, hoistEither)
import Data.List ((\\))
import qualified Data.List as L
import Cardano.CLI.Environment (readEnvSocketPath)
import Cardano.Api.Shelley (ProtocolParameters(..))
import Cardano.CLI.Shelley.Output (renderScriptCosts)
import Data.Aeson.Encode.Pretty (encodePretty)
import Cardano.Api.Shelley (refInsScriptsAndInlineDatsSupportedInEra)
import Cardano.CLI.Shelley.Script (readFileScriptInAnyLang)
import qualified System.IO as IO

testnet :: NetworkId
testnet = Testnet $ NetworkMagic 1097911063

newtype ShelleyQueryCmdError
  = ShelleyQueryCmdAcquireFailure AcquireFailure
  deriving Show

-- https://faucet.cardano-testnet.iohkdev.io/send-money/addr_test1vr3g684kyarnug89c7p7gqxr5y8t45g7q4ge4u8hghndvugn6yn5s?apiKey=&g-recaptcha-response=03AGdBq24qppnXuY6fIcCG2Hrpqxfp0V9Xd3oDqElSikr38sAuPMmpO4dKke9O0NzhtFnv_-cXVSs8h4loNDBDeM3rIb5UDHmoCsIylCHXmOovfDIOWM7417-9nW_8XegF7murR2CpVGDp8js7L33ygKqbPUus8AQncJ26AikCDiDNOe7_u6pHb20pR_a8a2cjfcRu6Ptrq8uTWxk2QiinvSctAZbnyTRscupJNDVvoJ1l52LNXOFNTFowRuyaRu1K9mLAJvbwy5n1il_05UGWRNvK3raCUA1DKhf0l9yOCfEvoNJNp10rTG5JFWeYaIiI3-ismQITIsR3u4akYy1PPjmNyF12vfcjlgbvXdGOcodyiZvKulnp2XNSQVIu-OHiwERumU5IISD9VRzY804Z1tKkRB7_PxpUvE7SOAKdOqmkvZLMn8ob1Fz8I562qiV8oezkVkSqTfqQbK2Vsqn3dYDd-IY0pjUhnw
-- http[s]://$FQDN:$PORT/send-money/$ADDRESS

type SendMoney = "send-money" :> Capture "destination_address" String :> Post '[OctetStream] LBS.ByteString
type RootDir = SendMoney

takeOneUtxo :: TMVar (Map TxIn Lovelace) -> Lovelace -> STM (Maybe TxIn)
takeOneUtxo utxoTMVar lovelace = do
  utxo <- takeTMVar utxoTMVar
  let
    utxoOfRightSize = Map.filter (== lovelace) utxo
    mTxin = head $ Map.toList $ Map.take 1 utxoOfRightSize
  case mTxin of
    Just (txin, _lovelace) -> do
      let
        trimmedUtxo = Map.delete txin utxo
      putTMVar utxoTMVar trimmedUtxo
      pure $ Just txin
    Nothing -> do
      putTMVar utxoTMVar utxo
      pure Nothing

handleSendMoney :: -- IsShelleyBasedEra era =>
     TQueue (TxInMode CardanoMode)
  -> TMVar (Map TxIn Lovelace)
  -> ShelleyWitnessSigningKey
  -> CardanoEra era
  -> String
  -> Servant.Handler LBS.ByteString
handleSendMoney txQueue utxoTMVar key _era addr = do
  print addr
  let
    addressAny :: AddressAny
    addressAny = case parse (parseAddressAny <* eof) "" addr of
      Right a -> a
      Left _e -> error "not implemented"
  print addressAny
  mTxin <- liftIO $ atomically $ takeOneUtxo utxoTMVar 1000170000
  case mTxin of
    Just txin -> do
      print txin
      let
        --outvalue :: ExceptT ShelleyTxCmdError IO (TxOutValue ShelleyEra)
        --outvalue = toTxOutValueInAnyEra ShelleyEra 1000000000
        out :: IsShelleyBasedEra era => TxOut a era
        out = TxOut (anyAddressInShelleyBasedEra addressAny)
          (TxOutAdaOnly (error "OnlyAdaSupportedInEra era") 1000000000)
          (error "TxOutDatum a era")
          (error "Cardano.Api.Script.ReferenceScript era")
        signedTx :: Tx ShelleyEra
        signedTx = makeSigned key txin out ShelleyEra
        inmode :: TxInMode CardanoMode
        inmode = TxInMode signedTx ShelleyEraInCardanoMode
      liftIO $ atomically $ writeTQueue txQueue inmode
      return $ fromString $ Prelude.show txin
    Nothing -> do
      return "outa funds"

makeTxBody :: IsCardanoEra era => TxIn -> TxOut CtxTx era -> CardanoEra era -> TxBody era
makeTxBody txin txout era = either (error . Prelude.show) id $ makeTransactionBody $ makeTxContents txin txout era

buildOwnAddress :: NetworkId -> PaymentCredential -> Text
buildOwnAddress nw payCred = serialiseAddress $ makeShelleyAddress nw payCred NoStakeAddress

makeTxContents :: TxIn -> TxOut CtxTx era  -> CardanoEra era -> TxBodyContent BuildTx era
makeTxContents txin txout era = TxBodyContent
  { txIns = map (, BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin]
  , txOuts = [txout]
  , txTotalCollateral = TxTotalCollateralNone
  , txReturnCollateral = TxReturnCollateralNone
  , txAuxScripts = TxAuxScriptsNone
  , txWithdrawals = TxWithdrawalsNone
  , txCertificates = TxCertificatesNone
  , txUpdateProposal = TxUpdateProposalNone
  , txMintValue = TxMintNone
  , txInsCollateral = TxInsCollateralNone
  , txInsReference = TxInsReferenceNone
  , txMetadata = TxMetadataNone
  , txProtocolParams = BuildTxWith Nothing
  , txScriptValidity = TxScriptValidityNone
  , txExtraKeyWits = TxExtraKeyWitnessesNone
  , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound supported)
  , txFee = error "no handled"
  }
    where
      supported = case validityNoUpperBoundSupportedInEra era of
        Just a -> a
        Nothing -> error "not implemented"

makeSigned :: IsShelleyBasedEra era => ShelleyWitnessSigningKey -> TxIn -> TxOut CtxTx era -> CardanoEra era -> Tx era
makeSigned keys' txin txout era = makeSignedTransaction [witness] body
  where
    body = makeTxBody txin txout era
    witness = makeShelleyKeyWitness body keys'

server :: -- IsShelleyBasedEra era =>
     TQueue (TxInMode CardanoMode)
  -> TMVar (Map TxIn Lovelace)
  -> ShelleyWitnessSigningKey
  -> CardanoEra era
  -> Server RootDir
server txQueue utxoTMVar key era = handleSendMoney txQueue utxoTMVar key era

userAPI :: Proxy RootDir
userAPI = Proxy

loadvkey :: String -> IO SomeAddressVerificationKey
loadvkey filepath = do
  orDie (T.pack . Prelude.show) $ (readAddressVerificationKeyTextOrFile . VktofVerificationKeyFile . VerificationKeyFile) filepath

vkeyToAddr :: NetworkId -> SomeAddressVerificationKey -> ExceptT ShelleyAddressCmdError IO AddressAny
vkeyToAddr nw (AByronVerificationKey vk) = return (AddressByron (makeByronAddress nw vk))
vkeyToAddr nw (APaymentVerificationKey vk) = AddressShelley <$> buildShelleyAddress vk Nothing nw
vkeyToAddr nw (APaymentExtendedVerificationKey vk) = AddressShelley <$> buildShelleyAddress (castVerificationKey vk) Nothing nw
vkeyToAddr nw (AGenesisUTxOVerificationKey vk) = AddressShelley <$> buildShelleyAddress (castVerificationKey vk) Nothing nw

app :: -- IsShelleyBasedEra era =>
     TQueue (TxInMode CardanoMode)
  -> TMVar (Map TxIn Lovelace)
  -> ShelleyWitnessSigningKey
  -> CardanoEra era
  -> Application
app txQueue utxoTMVar key era = serve userAPI $ server txQueue utxoTMVar key era

startApiServer :: TQueue (TxInMode CardanoMode) -> TMVar (Map TxIn Lovelace) -> ShelleyWitnessSigningKey -> CardanoEra ShelleyEra -> IO ()
startApiServer txQueue utxoTMVar key era = do
  let
    settings = setTimeout 600 $ setPort 1234 $ defaultSettings
  runSettings settings (app txQueue utxoTMVar key era)

foo :: WitnessSigningData
foo = KeyWitnessSigningData (SigningKeyFile "/home/clever/iohk/cardano-node/pay.skey") Nothing

bar :: IO SomeWitness
bar = orDie (T.pack . Prelude.show) $ readWitnessSigningData foo

baz :: SomeWitness -> ShelleyWitnessSigningKey
baz (APaymentSigningKey sk) = WitnessPaymentKey sk
baz (Cardano.CLI.Shelley.Run.Transaction.AByronSigningKey _ _) = error "not implemented"
baz (APaymentExtendedSigningKey _) = error "not implemented"
baz (AStakeSigningKey _) = error "not implemented"
baz (AStakeExtendedSigningKey _) = error "not implemented"
baz (AStakePoolSigningKey _) = error "not implemented"
baz (AGenesisSigningKey _) = error "not implemented"
baz (AGenesisExtendedSigningKey _) = error "not implemented"
baz (AGenesisDelegateSigningKey _) = error "not implemented"
baz (AGenesisDelegateExtendedSigningKey _) = error "not implemented"
baz (AGenesisUTxOSigningKey _) = error "not implemented"

qux :: IO ShelleyWitnessSigningKey
qux = do
  y <- bar
  return $ baz y

main :: IO ()
main = do
  let
    network = testnet
  txQueue <- newTQueueIO
  utxoTMVar <- newEmptyTMVarIO
  key <- qux
  pay_vkey <- loadvkey "/home/clever/iohk/cardano-node/pay.vkey"
  addressAny <- orDie (T.pack . Prelude.show) $ vkeyToAddr network pay_vkey
  let
    waitForTx :: IO (TxInMode CardanoMode)
    waitForTx = atomically $ readTQueue txQueue
    --address = "addr_test1vr3g684kyarnug89c7p7gqxr5y8t45g7q4ge4u8hghndvugn6yn5s"
    --addressAny :: AddressAny
    --(Right addressAny) = parse (parseAddressAny <* eof) "" address
    sockPath = "/tmp/testnet-socket"
    cModeParams = CardanoModeParams $ EpochSlots 21600
    localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
    getUtxoQuery :: AddressAny -> ShelleyBasedEra era2 -> Maybe (EraInMode era2 mode) ->  QueryInMode mode (Either EraMismatch (UTxO era2))
    getUtxoQuery _address _sbe Nothing = error "not handled"
    getUtxoQuery address sbe (Just eInMode) = QueryInEra eInMode query
      where
        qfilter :: QueryUTxOFilter
        qfilter = QueryUTxOByAddress $ Set.singleton address
        query   = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)
  print addressAny
  let
    aquireConnection aquireComplete = do
      pure $ Net.Query.SendMsgAcquire Nothing $ Net.Query.ClientStAcquiring
        { Net.Query.recvMsgAcquired = aquireComplete
        , Net.Query.recvMsgFailure = error "not implemented"
        }
    runQueryThen :: query t -> (t -> IO (Net.Query.ClientStAcquired block point query IO a)) -> IO (Net.Query.ClientStAcquired block point query IO a)
    runQueryThen query queryDone = do
      pure $ Net.Query.SendMsgQuery query $
        Net.Query.ClientStQuerying {
          Net.Query.recvMsgResult = \result -> do
            queryDone result
        }
    queryClient :: Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
    queryClient = LocalStateQueryClient $ do
      aquireConnection $ do
        runQueryThen (QueryCurrentEra CardanoModeIsMultiEra) $ \(AnyCardanoEra era3) -> do
          putStrLn @Text "new era"
          print era3
          _child <- forkIO $ startApiServer txQueue utxoTMVar key ShelleyEra
          case cardanoEraStyle era3 of
            ShelleyBasedEra sbe ->
              runQueryThen (getUtxoQuery addressAny sbe (toEraInMode era3 CardanoMode)) $ \case
                Right result -> do
                  let
                    getValue :: TxOutValue era -> Lovelace
                    getValue (TxOutAdaOnly _ ll) = ll
                    getValue (TxOutValue _ val) = fromMaybe 0 (valueToLovelace val)

                    reduceTxo :: TxOut ctx era -> Lovelace
                    reduceTxo (TxOut _ value _ _) = getValue value
                    reducedUtxo = Map.map reduceTxo $ unUTxO result
                    _hasRightValue :: Lovelace -> Bool
                    _hasRightValue value = value == Lovelace 1000170000
                    _filteredUtxo = Map.filter _hasRightValue reducedUtxo
                  atomically $ putTMVar utxoTMVar reducedUtxo
                  putStrLn @Text "utxo set initialized"
                  --print result
                  void . forever $ threadDelay 43200 {- day in seconds -}
                  pure $ Net.Query.SendMsgRelease $
                    pure $ Net.Query.SendMsgDone ()
                Left _e -> error "not handled"
            _ -> error "not handled"
    waitForTxAndLoop :: IO (Net.Tx.LocalTxClientStIdle (TxInMode CardanoMode) reject IO a)
    waitForTxAndLoop = do
      tx <- waitForTx
      pure $ Net.Tx.SendMsgSubmitTx tx $ \_result -> do
        --print result
        waitForTxAndLoop
    submissionClient =
      LocalTxSubmissionClient $
        waitForTxAndLoop

  connectToLocalNode
    localNodeConnInfo
    LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just queryClient
      , localTxSubmissionClient = Just submissionClient
      , localTxMonitoringClient = Nothing
    }

-- copied from cardano-cli, simplify out
determineEra
  :: LocalNodeConnectInfo CardanoMode
  -> IO (Either ShelleyQueryCmdError AnyCardanoEra)
determineEra localNodeConnInfo = do
    eraQ <- queryNodeLocalState localNodeConnInfo Nothing
                   $ QueryCurrentEra CardanoModeIsMultiEra
    case eraQ of
      Left acqFail -> return $ Left $ ShelleyQueryCmdAcquireFailure acqFail
      Right anyCarEra -> return $ Right anyCarEra



runTxBuild
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe TxOutAnyEra
  -- ^ Return collateral
  -> Maybe Lovelace
  -- ^ Total collateral
  -> [TxIn]
  -- ^ Reference TxIns
  -> [TxOutAnyEra]
  -- ^ Normal outputs
  -> TxOutChangeAddress
  -- ^ A change output
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [RequiredSigner]
  -- ^ Required signers
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsSourceSpec
  -> Maybe UpdateProposalFile
  -> OutputSerialisation
  -> Maybe Word
  -> TxBuildOutputOptions
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuild (AnyCardanoEra era) (AnyConsensusModeParams cModeParams) networkId mScriptValidity
           txins txinsc mReturnCollateral mtotcoll txinsref txouts (TxOutChangeAddress changeAddr) mValue mLowerBound mUpperBound
           certFiles withdrawals reqSigners metadataSchema scriptFiles metadataFiles mpparams
           mUpdatePropFile outputFormat mOverrideWits outputOptions = do
  SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
      consensusMode = consensusModeOnly cModeParams
      dummyFee = Just $ Lovelace 0
      onlyInputs = [input | (input,_) <- txins]

  case (consensusMode, cardanoEraStyle era) of
    (CardanoMode, ShelleyBasedEra sbe) -> do
      --  TxBodyContent {
      --    txIns              :: TxIns build era,
      --    txInsCollateral    :: TxInsCollateral era,
      --    txInsReference     :: TxInsReference era,
      --    txOuts             :: [TxOut CtxTx era],
      --    txTotalCollateral  :: TxTotalCollateral era,
      --    txReturnCollateral :: TxReturnCollateral CtxTx era,
      --    txFee              :: TxFee era,
      --    txValidityRange    :: (TxValidityLowerBound era, TxValidityUpperBound era),
      --    txMetadata         :: TxMetadataInEra era,
      --    txAuxScripts       :: TxAuxScripts era,
      --    txExtraKeyWits     :: TxExtraKeyWitnesses era,
      --    txProtocolParams   :: BuildTxWith build (Maybe ProtocolParameters),
      --    txWithdrawals      :: TxWithdrawals  build era,
      --    txCertificates     :: TxCertificates build era,
      --    txUpdateProposal   :: TxUpdateProposal era,
      --    txMintValue        :: TxMintValue    build era,
      --    txScriptValidity   :: TxScriptValidity era
      --  }

      txBodyContent <-
        TxBodyContent
          <$> validateTxIns               era txins
          <*> validateTxInsCollateral     era txinsc
          <*> validateTxInsReference      era txinsref
          <*> validateTxOuts              era txouts
          <*> validateTxTotalCollateral   era mtotcoll
          <*> validateTxReturnCollateral  era mReturnCollateral
          <*> validateTxFee               era dummyFee
          <*> ((,) <$> validateTxValidityLowerBound era mLowerBound
                   <*> validateTxValidityUpperBound era mUpperBound)
          <*> validateTxMetadataInEra     era metadataSchema metadataFiles
          <*> validateTxAuxScripts        era scriptFiles
          <*> validateRequiredSigners     era reqSigners
          <*> validateProtocolParameters  era mpparams
          <*> validateTxWithdrawals       era withdrawals
          <*> validateTxCertificates      era certFiles
          <*> validateTxUpdateProposal    era mUpdatePropFile
          <*> validateTxMintValue         era mValue
          <*> validateTxScriptValidity    era mScriptValidity

      eInMode <- case toEraInMode era CardanoMode of
                   Just result -> return result
                   Nothing ->
                     left (ShelleyTxCmdEraConsensusModeMismatchTxBalance outputOptions
                            (AnyConsensusMode CardanoMode) (AnyCardanoEra era))

      (utxo, pparams, eraHistory, systemStart, stakePools) <-
        newExceptT . fmap (join . first ShelleyTxCmdAcquireFailure) $
          executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
            unless (null txinsc) $ do
              collateralUtxo <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
                $ QueryInEra eInMode
                $ QueryInShelleyBasedEra sbe (QueryUTxO . QueryUTxOByTxIn $ Set.fromList txinsc)
              txinsExist txinsc collateralUtxo
              notScriptLockedTxIns collateralUtxo

            utxo <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
              $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe
              $ QueryUTxO (QueryUTxOByTxIn (Set.fromList onlyInputs))

            txinsExist onlyInputs utxo

            pparams <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
              $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

            eraHistory <- lift . queryExpr $ QueryEraHistory CardanoModeIsMultiEra

            systemStart <- lift $ queryExpr QuerySystemStart


            stakePools <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . ExceptT $
              queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

            return (utxo, pparams, eraHistory, systemStart, stakePools)

      let cAddr = case anyAddressInEra era changeAddr of
                    Just addr -> addr
                    Nothing -> error "runTxBuild: Byron address used: "

      (BalancedTxBody balancedTxBody _ _fee) <-
        firstExceptT ShelleyTxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance eInMode systemStart eraHistory
                                           pparams stakePools utxo txBodyContent
                                           cAddr mOverrideWits

      liftIO $ IO.putStrLn "Estimated transaction fee"
      case outputOptions of
        OutputScriptCostOnly fp -> do
          case protocolParamPrices pparams of
            Just executionUnitPrices -> do
              scriptExecUnitsMap <- firstExceptT ShelleyTxCmdTxExecUnitsErr $ hoistEither
                                      $ evaluateTransactionExecutionUnits
                                          eInMode systemStart eraHistory
                                          pparams utxo balancedTxBody
              scriptCostOutput <- firstExceptT ShelleyTxCmdPlutusScriptCostErr $ hoistEither
                                    $ renderScriptCosts
                                        executionUnitPrices
                                        (collectTxBodyScriptWitnesses txBodyContent)
                                        scriptExecUnitsMap
              liftIO $ LBS.writeFile fp $ encodePretty scriptCostOutput

            Nothing -> left ShelleyTxCmdPParamExecutionUnitsNotAvailable
        OutputTxBodyOnly (TxBodyFile fpath)  ->
          case outputFormat of
            OutputCliSerialisation ->
              firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
                writeFileTextEnvelope fpath Nothing balancedTxBody
            OutputLedgerCDDLSerialisation ->
              let noWitTx = makeSignedTransaction [] balancedTxBody
              in firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
                   writeTxFileTextEnvelopeCddl fpath noWitTx

    (CardanoMode, LegacyByronEra) -> left ShelleyTxCmdByronEra

    (wrongMode, _) -> left (ShelleyTxCmdUnsupportedMode (AnyConsensusMode wrongMode))
  where
    txinsExist :: Monad m => [TxIn] -> UTxO era -> ExceptT ShelleyTxCmdError m ()
    txinsExist ins (UTxO utxo)
      | null utxo = left $ ShelleyTxCmdTxInsDoNotExist ins
      | otherwise = do
          let utxoIns = Map.keys utxo
              occursInUtxo = [ txin | txin <- ins, txin `elem` utxoIns ]
          if length occursInUtxo == length ins
          then return ()
          else left . ShelleyTxCmdTxInsDoNotExist $ ins \\ ins `L.intersect` occursInUtxo

    notScriptLockedTxIns :: Monad m => UTxO era -> ExceptT ShelleyTxCmdError m ()
    notScriptLockedTxIns (UTxO utxo) = do
      let scriptLockedTxIns =
            filter (\(_, TxOut aInEra _ _ _) -> not $ isKeyAddress aInEra ) $ Map.assocs utxo
      if null scriptLockedTxIns
      then return ()
      else left . ShelleyTxCmdExpectedKeyLockedTxIn $ map fst scriptLockedTxIns


validateTxIns
  :: forall era.
     CardanoEra era
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -> ExceptT ShelleyTxCmdError IO
             [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
validateTxIns era = mapM convert
 where
   convert
     :: (TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))
     -> ExceptT ShelleyTxCmdError IO
                (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
   convert (txin, mScriptWitnessFiles) =
     case mScriptWitnessFiles of
       Just scriptWitnessFiles -> do
         sWit <- createScriptWitness era scriptWitnessFiles
         return ( txin
                , BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit
                )
       Nothing -> return (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)


validateTxInsCollateral :: CardanoEra era
                        -> [TxIn]
                        -> ExceptT ShelleyTxCmdError IO (TxInsCollateral era)
validateTxInsCollateral _   []    = return TxInsCollateralNone
validateTxInsCollateral era txins =
    case collateralSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureCollateral
      Just supported -> return (TxInsCollateral supported txins)

validateTxInsReference :: CardanoEra era
                       -> [TxIn]
                       -> ExceptT ShelleyTxCmdError IO (TxInsReference era)
validateTxInsReference _ [] = return TxInsReferenceNone
validateTxInsReference era txins =
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureReferenceInputs
    Just supp -> return $ TxInsReference supp txins


validateTxOuts :: forall era.
                  CardanoEra era
               -> [TxOutAnyEra]
               -> ExceptT ShelleyTxCmdError IO [TxOut CtxTx era]
validateTxOuts era = mapM (toTxOutInAnyEra era)


validateTxTotalCollateral :: CardanoEra era
                          -> Maybe Lovelace
                          -> ExceptT ShelleyTxCmdError IO (TxTotalCollateral era)
validateTxTotalCollateral _ Nothing = return TxTotalCollateralNone
validateTxTotalCollateral era (Just coll) =
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxTotalCollateral supp coll
    Nothing -> txFeatureMismatch era TxFeatureTotalCollateral

validateTxReturnCollateral :: CardanoEra era
                           -> Maybe TxOutAnyEra
                           -> ExceptT ShelleyTxCmdError IO (TxReturnCollateral CtxTx era)
validateTxReturnCollateral _ Nothing = return TxReturnCollateralNone
validateTxReturnCollateral era (Just retColTxOut) = do
  txout <- toTxOutInAnyEra era retColTxOut
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxReturnCollateral supp txout
    Nothing -> txFeatureMismatch era TxFeatureReturnCollateral


validateTxFee :: CardanoEra era
              -> Maybe Lovelace
              -> ExceptT ShelleyTxCmdError IO (TxFee era)
validateTxFee era mfee =
    case (txFeesExplicitInEra era, mfee) of
      (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
      (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)

      (Right _, Nothing) -> txFeatureMismatch era TxFeatureImplicitFees
      (Left  _, Just _)  -> txFeatureMismatch era TxFeatureExplicitFees


validateTxValidityLowerBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> ExceptT ShelleyTxCmdError IO
                                        (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) =
    case validityLowerBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityLowerBound
      Just supported -> return (TxValidityLowerBound supported slot)

validateTxValidityUpperBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> ExceptT ShelleyTxCmdError IO
                                        (TxValidityUpperBound era)
validateTxValidityUpperBound era Nothing =
    case validityNoUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityNoUpperBound
      Just supported -> return (TxValidityNoUpperBound supported)
validateTxValidityUpperBound era (Just slot) =
    case validityUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityUpperBound
      Just supported -> return (TxValidityUpperBound supported slot)

validateTxMetadataInEra :: CardanoEra era
                        -> TxMetadataJsonSchema
                        -> [MetadataFile]
                        -> ExceptT ShelleyTxCmdError IO (TxMetadataInEra era)
validateTxMetadataInEra _ _ [] = return TxMetadataNone
validateTxMetadataInEra era schema files =
    case txMetadataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureTxMetadata
      Just supported -> do
        metadata <- mconcat <$> mapM (readFileTxMetadata schema) files
        return (TxMetadataInEra supported metadata)



validateTxAuxScripts :: CardanoEra era
                     -> [ScriptFile]
                     -> ExceptT ShelleyTxCmdError IO (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era files =
  case auxScriptsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureAuxScripts
    Just supported -> do
      scripts <- sequence
        [ do script <- firstExceptT ShelleyTxCmdScriptFileError $
                         readFileScriptInAnyLang file
             validateScriptSupportedInEra era script
        | ScriptFile file <- files ]
      return $ TxAuxScripts supported scripts

validateRequiredSigners :: CardanoEra era
                        -> [RequiredSigner]
                        -> ExceptT ShelleyTxCmdError IO (TxExtraKeyWitnesses era)
validateRequiredSigners _ [] = return TxExtraKeyWitnessesNone
validateRequiredSigners era reqSigs =
  case extraKeyWitnessesSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureExtraKeyWits
    Just supported -> do
      rSignerHashes <- mapM readRequiredSigner reqSigs
      return $ TxExtraKeyWitnesses supported rSignerHashes

validateProtocolParameters
  :: CardanoEra era
  -> Maybe ProtocolParamsSourceSpec
  -> ExceptT ShelleyTxCmdError IO
            (BuildTxWith BuildTx (Maybe ProtocolParameters))
validateProtocolParameters _ Nothing = return (BuildTxWith Nothing)
validateProtocolParameters era (Just pparamsspec) =
    case scriptDataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureProtocolParameters
      Just _  -> BuildTxWith . Just <$>
                   readProtocolParametersSourceSpec pparamsspec



validateTxWithdrawals
  :: forall era.
     CardanoEra era
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> ExceptT ShelleyTxCmdError IO (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals =
  case withdrawalsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureWithdrawals
    Just supported -> do
      convWithdrawals <- mapM convert withdrawals
      return (TxWithdrawals supported convWithdrawals)
 where
  convert
    :: (StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))
    -> ExceptT ShelleyTxCmdError IO
              (StakeAddress,
               Lovelace,
               BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just scriptWitnessFiles -> do
        sWit <- createScriptWitness era scriptWitnessFiles
        return ( sAddr
               , ll
               , BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit
               )
      Nothing -> return (sAddr,ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)


validateTxCertificates
  :: forall era.
     CardanoEra era
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> ExceptT ShelleyTxCmdError IO (TxCertificates BuildTx era)
validateTxCertificates _ [] = return TxCertificatesNone
validateTxCertificates era certFiles =
  case certificatesSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureCertificates
    Just supported -> do
      certs <- sequence
                 [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                     readFileTextEnvelope AsCertificate certFile
                 | CertificateFile certFile <- map fst certFiles ]
      reqWits <- Map.fromList . catMaybes  <$> mapM convert certFiles
      return $ TxCertificates supported certs $ BuildTxWith reqWits
  where
   -- We get the stake credential witness for a certificate that requires it.
   -- NB: Only stake address deregistration and delegation requires
   -- witnessing (witness can be script or key)
   deriveStakeCredentialWitness
     :: CertificateFile
     -> ExceptT ShelleyTxCmdError IO (Maybe StakeCredential)
   deriveStakeCredentialWitness (CertificateFile certFile) = do
     cert <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT
               $ readFileTextEnvelope AsCertificate certFile
     case cert of
       StakeAddressDeregistrationCertificate sCred -> return $ Just sCred
       StakeAddressDelegationCertificate sCred _ -> return $ Just sCred
       _ -> return Nothing

   convert
     :: (CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))
     -> ExceptT ShelleyTxCmdError IO
                (Maybe (StakeCredential, Witness WitCtxStake era))
   convert (cert, mScriptWitnessFiles) = do
     mStakeCred <- deriveStakeCredentialWitness cert
     case mStakeCred of
       Nothing -> return Nothing
       Just sCred ->
         case mScriptWitnessFiles of
           Just scriptWitnessFiles -> do
            sWit <- createScriptWitness era scriptWitnessFiles
            return $ Just ( sCred
                          , ScriptWitness ScriptWitnessForStakeAddr sWit
                          )

           Nothing -> return $ Just (sCred, KeyWitness KeyWitnessForStakeAddr)


validateTxUpdateProposal :: CardanoEra era
                         -> Maybe UpdateProposalFile
                         -> ExceptT ShelleyTxCmdError IO (TxUpdateProposal era)
validateTxUpdateProposal _ Nothing = return TxUpdateProposalNone
validateTxUpdateProposal era (Just (UpdateProposalFile file)) =
    case updateProposalSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureCertificates
      Just supported -> do
         prop <- firstExceptT ShelleyTxCmdReadTextViewFileError $ newExceptT $
                   readFileTextEnvelope AsUpdateProposal file
         return (TxUpdateProposal supported prop)


validateTxMintValue :: forall era.
                       CardanoEra era
                    -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
                    -> ExceptT ShelleyTxCmdError IO (TxMintValue BuildTx era)
validateTxMintValue _ Nothing = return TxMintNone
validateTxMintValue era (Just (val, scriptWitnessFiles)) =
    case multiAssetSupportedInEra era of
      Left _ -> txFeatureMismatch era TxFeatureMintValue
      Right supported -> do
        -- The set of policy ids for which we need witnesses:
        let witnessesNeededSet :: Set PolicyId
            witnessesNeededSet =
              Set.fromList [ pid | (AssetId pid _, _) <- valueToList val ]

        -- The set (and map) of policy ids for which we have witnesses:
        witnesses <- mapM (createScriptWitness era) scriptWitnessFiles
        let witnessesProvidedMap :: Map PolicyId (ScriptWitness WitCtxMint era)
            witnessesProvidedMap = Map.fromList
                                     [ (scriptWitnessPolicyId witness, witness)
                                     | witness <- witnesses ]
            witnessesProvidedSet = Map.keysSet witnessesProvidedMap

        -- Check not too many, nor too few:
        validateAllWitnessesProvided   witnessesNeededSet witnessesProvidedSet
        validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet

        return (TxMintValue supported val (BuildTxWith witnessesProvidedMap))
 where
    validateAllWitnessesProvided witnessesNeeded witnessesProvided
      | null witnessesMissing = return ()
      | otherwise = left (ShelleyTxCmdPolicyIdsMissing witnessesMissing)
      where
        witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

    validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
      | null witnessesExtra = return ()
      | otherwise = left (ShelleyTxCmdPolicyIdsExcess witnessesExtra)
      where
        witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)


validateTxScriptValidity :: forall era.
     CardanoEra era
  -> Maybe ScriptValidity
  -> ExceptT ShelleyTxCmdError IO (TxScriptValidity era)
validateTxScriptValidity _ Nothing = pure TxScriptValidityNone
validateTxScriptValidity era (Just scriptValidity) =
  case txScriptValiditySupportedInCardanoEra era of
    Nothing -> txFeatureMismatch era TxFeatureScriptValidity
    Just supported -> pure $ TxScriptValidity supported scriptValidity


txFeatureMismatch :: CardanoEra era
                  -> TxFeature
                  -> ExceptT ShelleyTxCmdError IO a
txFeatureMismatch era feature =
    left (ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature)
