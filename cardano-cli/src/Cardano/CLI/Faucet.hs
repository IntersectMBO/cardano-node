{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use let" -}

module Cardano.CLI.Faucet where



import Cardano.Api
import Cardano.Api.Byron (WitnessNetworkIdOrByronAddress(..))
import Cardano.Api.Shelley ( StakePoolKey, ReferenceScript(..))
import Cardano.CLI.Shelley.Commands
import Cardano.CLI.Shelley.Key
import Cardano.CLI.Shelley.Run.Address
import Cardano.CLI.Types
import Cardano.Prelude
import Control.Concurrent.STM
import Control.Monad.Trans.Except.Exit (orDie)
import Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, hoistEither)
import Data.ByteString.Lazy     qualified as LBS
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as T
import Data.Text qualified as Text
import Network.Wai.Handler.Warp
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Net.Tx
import Prelude (show, error, id)
import Servant
import System.Environment (lookupEnv)
import System.IO qualified as IO
import Text.Parsec

newtype EnvSocketError = CliEnvVarLookup Text deriving Show

data SomeWitness
  = AByronSigningKey           (SigningKey ByronKey) (Maybe (Address ByronAddr))
  | APaymentSigningKey         (SigningKey PaymentKey)
  | APaymentExtendedSigningKey (SigningKey PaymentExtendedKey)
  | AStakeSigningKey           (SigningKey StakeKey)
  | AStakeExtendedSigningKey   (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey       (SigningKey StakePoolKey)
  | AGenesisSigningKey         (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (SigningKey GenesisUTxOKey)

-- | Data required for constructing a Shelley bootstrap witness.
data ShelleyBootstrapWitnessSigningKeyData
  = ShelleyBootstrapWitnessSigningKeyData
      !(SigningKey ByronKey)
      -- ^ Byron signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.

-- | Some kind of Byron or Shelley witness.
data ByronOrShelleyWitness
  = AByronWitness !ShelleyBootstrapWitnessSigningKeyData
  | AShelleyKeyWitness !ShelleyWitnessSigningKey

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: ExceptT EnvSocketError IO SocketPath
readEnvSocketPath =
    maybe (left $ CliEnvVarLookup (Text.pack envName)) (pure . SocketPath)
      =<< liftIO (lookupEnv envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"

-- | Error constructing a Shelley bootstrap witness (i.e. a Byron key witness
-- in the Shelley era).
data ShelleyBootstrapWitnessError
  = MissingNetworkIdOrByronAddressError
  -- ^ Neither a network ID nor a Byron address were provided to construct the
  -- Shelley bootstrap witness. One or the other is required.
  deriving Show

data ShelleyTxCmdError
  = ShelleyTxCmdBootstrapWitnessError !ShelleyBootstrapWitnessError
  | ShelleyTxCmdSocketEnvError !EnvSocketError
  | ShelleyTxCmdTxSubmitError !Text
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  | ShelleyTxCmdTxFeatureMismatch !AnyCardanoEra !TxFeature
  | ShelleyTxCmdAcquireFailure !AcquireFailure
  | ShelleyTxCmdEraConsensusModeMismatchTxBalance !TxBuildOutputOptions !AnyConsensusMode !AnyCardanoEra
  | ShelleyTxCmdBalanceTxBody !TxBodyErrorAutoBalance
  | ShelleyTxCmdTxInsDoNotExist ![TxIn]
  | ShelleyTxCmdScriptFileError

data TxFeature = TxFeatureExplicitFees | TxFeatureImplicitFees | TxFeatureValidityNoUpperBound | TxFeatureShelleyAddresses | TxFeatureMultiAssetOutputs
  deriving Show

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

main :: IO ()
main = do
  let
    network = testnet
  txQueue <- newTQueueIO
  utxoTMVar <- newEmptyTMVarIO
  -- key <- qux
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
          _child <- forkIO $ startApiServer txQueue utxoTMVar (error "key") ShelleyEra
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
    _waitForTxAndLoop :: IO (Net.Tx.LocalTxClientStIdle (TxInMode CardanoMode) reject IO a)
    _waitForTxAndLoop = do
      tx <- waitForTx
      pure $ Net.Tx.SendMsgSubmitTx tx $ \_result -> do
        --print result
        _waitForTxAndLoop
    submissionClient = error "LocalTxSubmissionClient $ waitForTxAndLoop"

  connectToLocalNode
    localNodeConnInfo
    LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just queryClient
      , localTxSubmissionClient = Just submissionClient
      , localTxMonitoringClient = Nothing
    }

-- copied from cardano-cli, simplify out
determineEra ::
     LocalNodeConnectInfo CardanoMode
  -> IO (Either ShelleyQueryCmdError AnyCardanoEra)
determineEra localNodeConnInfo = do
  eraQ <- queryNodeLocalState localNodeConnInfo Nothing $ QueryCurrentEra CardanoModeIsMultiEra
  case eraQ of
    Left acqFail -> return $ Left $ ShelleyQueryCmdAcquireFailure acqFail
    Right anyCarEra -> return $ Right anyCarEra

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

defaultCModeParams :: ConsensusModeParams CardanoMode
defaultCModeParams = CardanoModeParams (EpochSlots defaultByronEpochSlots)

txBuild :: IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> ConsensusModeParams CardanoMode
  -> NetworkId
  -> TxIn
  -> [TxOutAnyEra]
  -> TxOutChangeAddress
  -> TxBuildOutputOptions
  -> ExceptT ShelleyTxCmdError IO (TxBody era)
txBuild sbe cModeParams networkId txin txouts (TxOutChangeAddress changeAddr) outputOptions = do
  SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
  let era = shelleyBasedToCardanoEra sbe
  let dummyFee = Just $ Lovelace 0

  txBodyContent <- TxBodyContent
    <$> pure [(txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
    <*> pure TxInsCollateralNone
    <*> pure TxInsReferenceNone
    <*> mapM (toTxOutInAnyEra era) txouts
    <*> pure TxTotalCollateralNone
    <*> pure TxReturnCollateralNone
    <*> validateTxFee era dummyFee
    <*> noBoundsIfSupported era
    <*> pure TxMetadataNone
    <*> pure TxAuxScriptsNone
    <*> pure TxExtraKeyWitnessesNone
    <*> pure (BuildTxWith Nothing)
    <*> pure TxWithdrawalsNone
    <*> pure TxCertificatesNone
    <*> pure TxUpdateProposalNone
    <*> pure TxMintNone
    <*> pure TxScriptValidityNone

  eInMode <- case toEraInMode era CardanoMode of
    Just result -> return result
    Nothing -> left (ShelleyTxCmdEraConsensusModeMismatchTxBalance outputOptions (AnyConsensusMode CardanoMode) (AnyCardanoEra era))

  (utxo, pparams, eraHistory, systemStart, stakePools) <-
    newExceptT . fmap (join . first ShelleyTxCmdAcquireFailure) $
      executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
        UTxO utxo <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
          $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe
          $ QueryUTxO (QueryUTxOByTxIn (Set.singleton txin))

        when (null utxo || not (txin `L.elem` Map.keys utxo)) $ do
          -- txout for txin does not exist
          left $ ShelleyTxCmdTxInsDoNotExist [txin]

        pparams <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
          $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

        eraHistory <- lift . queryExpr $ QueryEraHistory CardanoModeIsMultiEra

        systemStart <- lift $ queryExpr QuerySystemStart

        stakePools <- firstExceptT ShelleyTxCmdTxSubmitErrorEraMismatch . ExceptT $
          queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

        return (UTxO utxo, pparams, eraHistory, systemStart, stakePools)

  cAddr <- pure $ case anyAddressInEra era changeAddr of
    Just addr -> addr
    Nothing -> error "txBuild: Byron address used: "

  (BalancedTxBody balancedTxBody _ _fee) <- firstExceptT ShelleyTxCmdBalanceTxBody . hoistEither $
    makeTransactionBodyAutoBalance eInMode systemStart eraHistory pparams stakePools utxo txBodyContent cAddr Nothing

  liftIO $ IO.putStrLn "Estimated transaction fee"
  return balancedTxBody

noBoundsIfSupported ::
     CardanoEra era
  -> ExceptT ShelleyTxCmdError IO (TxValidityLowerBound era, TxValidityUpperBound era)
noBoundsIfSupported era = (,)
  <$> pure TxValidityNoLowerBound
  <*> noUpperBoundIfSupported era

noUpperBoundIfSupported ::
     CardanoEra era
  -> ExceptT ShelleyTxCmdError IO (TxValidityUpperBound era)
noUpperBoundIfSupported era = case validityNoUpperBoundSupportedInEra era of
  Nothing -> txFeatureMismatch era TxFeatureValidityNoUpperBound
  Just supported -> return (TxValidityNoUpperBound supported)

validateTxFee ::
     CardanoEra era
  -> Maybe Lovelace
  -> ExceptT ShelleyTxCmdError IO (TxFee era)
validateTxFee era mfee = case (txFeesExplicitInEra era, mfee) of
  (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
  (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)
  (Right _, Nothing) -> txFeatureMismatch era TxFeatureImplicitFees
  (Left  _, Just _)  -> txFeatureMismatch era TxFeatureExplicitFees

txFeatureMismatch ::
     CardanoEra era
  -> TxFeature
  -> ExceptT ShelleyTxCmdError IO a
txFeatureMismatch era feature = left (ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature)

txSign :: IsShelleyBasedEra era
  => NetworkId
  -> TxBody era
  -> [SomeWitness]
  -> ExceptT ShelleyTxCmdError IO (Tx era)
txSign networkId txBody sks = do
  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  -- Byron witnesses require the network ID. This can either be provided
  -- directly or derived from a provided Byron address.
  byronWitnesses <- firstExceptT (ShelleyTxCmdBootstrapWitnessError) . hoistEither $
    mkShelleyBootstrapWitnesses (Just networkId) txBody sksByron

  let shelleyKeyWitnesses = map (makeShelleyKeyWitness txBody) sksShelley
  let tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txBody

  return tx

txSubmit ::
     EraInMode era CardanoMode
  -> ConsensusModeParams CardanoMode
  -> NetworkId
  -> Tx era
  -> ExceptT ShelleyTxCmdError IO ()
txSubmit eraInMode cModeParams network tx = do
  SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath

  txInMode <- pure $ TxInMode tx eraInMode
  localNodeConnInfo <- pure $ LocalNodeConnectInfo
    { localConsensusModeParams = cModeParams
    , localNodeNetworkId = network
    , localNodeSocketPath = sockPath
    }

  res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode

  case res of
    Net.Tx.SubmitSuccess -> liftIO $ putTextLn "Transaction successfully submitted."
    Net.Tx.SubmitFail reason -> case reason of
      TxValidationErrorInMode err _eraInMode -> left . ShelleyTxCmdTxSubmitError . Text.pack $ Prelude.show err
      TxValidationEraMismatch mismatchErr -> left $ ShelleyTxCmdTxSubmitErrorEraMismatch mismatchErr

txBuildSignSubmit :: IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> ConsensusModeParams CardanoMode
  -> EraInMode era CardanoMode
  -> NetworkId
  -> TxIn
  -> [TxOutAnyEra]
  -> TxOutChangeAddress
  -> TxBuildOutputOptions
  -> [SomeWitness]
  -> ExceptT ShelleyTxCmdError IO ()
txBuildSignSubmit sbe cModeParams eraInMode networkId txin txouts txChangeAddress outputOptions sks = do
  txBody <- txBuild sbe cModeParams networkId txin txouts txChangeAddress outputOptions
  tx <- txSign networkId txBody sks
  txSubmit eraInMode cModeParams networkId tx


toTxOutInAnyEra :: CardanoEra era
                -> TxOutAnyEra
                -> ExceptT ShelleyTxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' _mDatumHash _refScriptFp) = do
  addr <- toAddressInAnyEra era addr'
  val <- toTxOutValueInAnyEra era val'
  pure $ TxOut addr val TxOutDatumNone ReferenceScriptNone

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either ShelleyBootstrapWitnessError [KeyWitness era]
mkShelleyBootstrapWitnesses mnw txBody =
  mapM (mkShelleyBootstrapWitness mnw txBody)

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either ShelleyBootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness (WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness (WitnessByronAddress addr) txBody skey

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [ShelleyWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . foldl' go mempty
  where
    reversePartitionedWits (bw, skw) =
      (reverse bw, reverse skw)

    go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
      case byronOrShelleyWit of
        AByronWitness byronWit ->
          (byronWit:byronAcc, shelleyKeyAcc)
        AShelleyKeyWitness shelleyKeyWit ->
          (byronAcc, shelleyKeyWit:shelleyKeyAcc)



categoriseSomeWitness :: SomeWitness -> ByronOrShelleyWitness
categoriseSomeWitness swsk =
  case swsk of
    AByronSigningKey           sk addr -> AByronWitness (ShelleyBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningKey         sk      -> AShelleyKeyWitness (WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk      -> AShelleyKeyWitness (WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk      -> AShelleyKeyWitness (WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk      -> AShelleyKeyWitness (WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk      -> AShelleyKeyWitness (WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                       -> AShelleyKeyWitness (WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk      -> AShelleyKeyWitness (WitnessGenesisUTxOKey     sk)


toAddressInAnyEra
  :: CardanoEra era
  -> AddressAny
  -> ExceptT ShelleyTxCmdError IO (AddressInEra era)
toAddressInAnyEra era addrAny =
  case addrAny of
    AddressByron   bAddr -> return (AddressInEra ByronAddressInAnyEra bAddr)
    AddressShelley sAddr ->
      case cardanoEraStyle era of
        LegacyByronEra -> txFeatureMismatch era TxFeatureShelleyAddresses
        ShelleyBasedEra era' ->
          return (AddressInEra (ShelleyAddressInEra era') sAddr)


toTxOutValueInAnyEra
  :: CardanoEra era
  -> Value
  -> ExceptT ShelleyTxCmdError IO (TxOutValue era)
toTxOutValueInAnyEra era val =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra ->
      case valueToLovelace val of
        Just l  -> return (TxOutAdaOnly adaOnlyInEra l)
        Nothing -> txFeatureMismatch era TxFeatureMultiAssetOutputs
    Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)
