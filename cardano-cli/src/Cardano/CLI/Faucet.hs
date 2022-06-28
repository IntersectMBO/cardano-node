{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use let" -}

module Cardano.CLI.Faucet (main) where

import Cardano.Api (TxInMode, CardanoMode, AddressAny(AddressByron, AddressShelley), CardanoEra, EraInMode, IsShelleyBasedEra, ShelleyBasedEra, QueryInMode(QueryInEra, QueryCurrentEra), UTxO(unUTxO), QueryUTxOFilter(QueryUTxOByAddress), BlockInMode, ChainPoint, AnyCardanoEra(AnyCardanoEra), CardanoEraStyle(ShelleyBasedEra), LocalNodeConnectInfo(LocalNodeConnectInfo), LocalNodeClientProtocols(LocalNodeClientProtocols, localChainSyncClient, localStateQueryClient, localTxSubmissionClient, localTxMonitoringClient), NetworkId(Testnet), NetworkMagic(NetworkMagic), toEraInMode, ConsensusMode(CardanoMode), makeByronAddress, castVerificationKey, QueryInEra(QueryInShelleyBasedEra), QueryInShelleyBasedEra(QueryUTxO), LocalStateQueryClient(LocalStateQueryClient), ConsensusModeIsMultiEra(CardanoModeIsMultiEra), cardanoEraStyle, connectToLocalNode, LocalChainSyncClient(NoLocalChainSyncClient), SigningKey, AsType(AsSigningKey, AsPaymentKey), FromSomeType(FromSomeType), PaymentKey, getVerificationKey)
import Cardano.Api.Byron ()
import Cardano.Api.Shelley ()
import Cardano.CLI.Environment (readEnvSocketPath, renderEnvSocketError)
import Cardano.CLI.Shelley.Commands
import Cardano.CLI.Shelley.Key
import Cardano.CLI.Shelley.Run.Address
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.CLI.Types
import Cardano.Prelude
import Cardano.CLI.Faucet.Misc
import Cardano.CLI.Faucet.Types
import Cardano.CLI.Faucet.Web
import Control.Concurrent.STM (newTQueueIO, newEmptyTMVarIO, putTMVar, readTQueue)
import Control.Monad.Trans.Except.Exit (orDie)
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as T
import Network.Wai.Handler.Warp
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type ()
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Net.Tx
import Prelude qualified
import Servant
import System.Environment (lookupEnv)
import Control.Monad.Trans.Except.Extra (left)

testnet :: NetworkId
testnet = Testnet $ NetworkMagic 1097911063


        --value = valueFromList [(AdaAssetId, 1000001815)]
        --outvalue :: ExceptT ShelleyTxCmdError IO (TxOutValue ShelleyEra)
        --outvalue = toTxOutValueInAnyEra ShelleyEra $ valueFromList [(AdaAssetId, 1000000000)]
      --outvalue' <- liftIO $ orDie renderShelleyTxCmdError outvalue
        --out' :: _
        --out' = toTxOutInAnyEra era
        --out :: IsShelleyBasedEra era => TxOut a era
        --out = TxOut (anyAddressInShelleyBasedEra addressAny)
        --  (TxOutAdaOnly (error "OnlyAdaSupportedInEra era") 1000000000)
        --  (error "TxOutDatum a era")
        --  (error "Cardano.Api.Script.ReferenceScript era")
        --signedTx :: Tx ShelleyEra
        --signedTx = makeSigned key txin out ShelleyEra
        --foo :: TxOutAnyEra
        --foo = TxOutAnyEra addressAny value TxOutDatumByNone ReferenceScriptAnyEraNone

--makeTxBody :: IsCardanoEra era => TxIn -> TxOut CtxTx era -> CardanoEra era -> TxBody era
--makeTxBody txin txout era = either (error . Prelude.show) id $ makeTransactionBody $ makeTxContents txin txout era

--buildOwnAddress :: NetworkId -> PaymentCredential -> Text
--buildOwnAddress nw payCred = serialiseAddress $ makeShelleyAddress nw payCred NoStakeAddress

--makeTxContents :: TxIn -> TxOut CtxTx era  -> CardanoEra era -> TxBodyContent BuildTx era
--makeTxContents txin txout era = TxBodyContent
--  { txIns = map (, BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin]
--  , txOuts = [txout]
--  , txTotalCollateral = TxTotalCollateralNone
--  , txReturnCollateral = TxReturnCollateralNone
--  , txAuxScripts = TxAuxScriptsNone
--  , txWithdrawals = TxWithdrawalsNone
--  , txCertificates = TxCertificatesNone
--  , txUpdateProposal = TxUpdateProposalNone
--  , txMintValue = TxMintNone
--  , txInsCollateral = TxInsCollateralNone
--  , txInsReference = TxInsReferenceNone
--  , txMetadata = TxMetadataNone
--  , txProtocolParams = BuildTxWith Nothing
--  , txScriptValidity = TxScriptValidityNone
--  , txExtraKeyWits = TxExtraKeyWitnessesNone
--  , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound supported)
--  , txFee = error "no handled"
--  }
--    where
--      supported = case validityNoUpperBoundSupportedInEra era of
--        Just a -> a
--        Nothing -> error "not implemented"

--makeSigned :: IsShelleyBasedEra era => ShelleyWitnessSigningKey -> TxIn -> TxOut CtxTx era -> CardanoEra era -> Tx era
--makeSigned keys' txin txout era = makeSignedTransaction [witness] body
--  where
--    body = makeTxBody txin txout era
--    witness = makeShelleyKeyWitness body keys'


loadvkey :: String -> IO SomeAddressVerificationKey
loadvkey filepath = do
  orDie (T.pack . Prelude.show) $ (readAddressVerificationKeyTextOrFile . VktofVerificationKeyFile . VerificationKeyFile) filepath

vkeyToAddr :: NetworkId -> SomeAddressVerificationKey -> ExceptT ShelleyAddressCmdError IO AddressAny
vkeyToAddr nw (AByronVerificationKey vk) = return (AddressByron (makeByronAddress nw vk))
vkeyToAddr nw (APaymentVerificationKey vk) = AddressShelley <$> buildShelleyAddress vk Nothing nw
vkeyToAddr nw (APaymentExtendedVerificationKey vk) = AddressShelley <$> buildShelleyAddress (castVerificationKey vk) Nothing nw
vkeyToAddr nw (AGenesisUTxOVerificationKey vk) = AddressShelley <$> buildShelleyAddress (castVerificationKey vk) Nothing nw

app :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> Application
app era sbe faucetState = serve userAPI $ server era sbe faucetState

startApiServer :: IsShelleyBasedEra era =>
  CardanoEra era
  -> ShelleyBasedEra era
  -> FaucetState era
  -> IO ()
startApiServer era sbe faucetState = do
  let
    settings = setTimeout 600 $ setPort 1234 $ defaultSettings
  runSettings settings (app era sbe faucetState)

main :: IO ()
main = do
  txQueue <- newTQueueIO
  --pay_skey <- bar
  --pay_vkey <- loadvkey "/home/clever/iohk/cardano-node/pay.vkey"
  -- note:
  -- getVerificationKey :: Key keyrole => SigningKey keyrole -> VerificationKey keyrole
  let
    net = testnet
  dryRun <- maybe False (== "1") <$> lookupEnv "DRY_RUN"
  eResult <- runExceptT $ do
    pay_skey <- foo "/home/clever/iohk/cardano-node/pay.skey"
    let pay_vkey = getVerificationKey pay_skey
    print "vkeys"
    print pay_vkey
    print "skey"
    print pay_skey
    SocketPath sockPath <- withExceptT FaucetErrorSocketNotFound readEnvSocketPath
    let
      localNodeConnInfo :: LocalNodeConnectInfo CardanoMode
      localNodeConnInfo = LocalNodeConnectInfo defaultCModeParams net sockPath
      aquireConnection aquireComplete = do
        pure $ Net.Query.SendMsgAcquire Nothing $ Net.Query.ClientStAcquiring
          { Net.Query.recvMsgAcquired = aquireComplete
          , Net.Query.recvMsgFailure = Prelude.error "not implemented"
          }
      runQueryThen :: query t -> (t -> IO (Net.Query.ClientStAcquired block point query IO a)) -> IO (Net.Query.ClientStAcquired block point query IO a)
      runQueryThen query queryDone = do
        pure $ Net.Query.SendMsgQuery query $
          Net.Query.ClientStQuerying {
            Net.Query.recvMsgResult = \result -> do
              queryDone result
          }
      getUtxoQuery :: AddressAny -> ShelleyBasedEra era2 -> Maybe (EraInMode era2 mode) ->  QueryInMode mode (Either EraMismatch (UTxO era2))
      getUtxoQuery _address _sbe Nothing = Prelude.error "not handled"
      getUtxoQuery address sbe (Just eInMode) = QueryInEra eInMode query
        where
          qfilter :: QueryUTxOFilter
          qfilter = QueryUTxOByAddress $ Set.singleton address
          query   = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)

      queryClient :: Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
      queryClient = LocalStateQueryClient $ do
        aquireConnection $ do
          runQueryThen (QueryCurrentEra CardanoModeIsMultiEra) $ \(AnyCardanoEra era3) -> do
            tmvar <- newEmptyTMVarIO
            let
              faucetState = FaucetState
                { utxoTMVar = tmvar
                , network = net
                , queue = txQueue
                , skey = APaymentSigningKey pay_skey
                , vkey = APaymentVerificationKey pay_vkey
                }
            addressAny <- orDie (T.pack . Prelude.show) $ vkeyToAddr (network faucetState) (vkey faucetState)
            print addressAny
            putStrLn @Text "new era"
            print era3
            case cardanoEraStyle era3 of
              ShelleyBasedEra sbe -> do
                _child <- forkIO $ startApiServer era3 sbe faucetState
                runQueryThen (getUtxoQuery addressAny sbe (toEraInMode era3 CardanoMode)) $ \case
                  Right result -> do
                    let
                      --reduceTxo :: TxOut ctx era -> (Lovelace, TxOut ctx era)
                      --reduceTxo out@(TxOut _ value _ _) = (getValue value, out)
                      --reducedUtxo :: Map TxIn (Lovelace, TxOut CtxUTxO era)
                      --reducedUtxo = Map.map reduceTxo $ unUTxO result
                    --atomically $ putTMVar utxoTMVar $ unUTxO result
                    atomically $ putTMVar (utxoTMVar faucetState) (unUTxO result)
                    putStrLn @Text "utxo set initialized"
                    void . forever $ threadDelay 43200 {- day in seconds -}
                    pure $ Net.Query.SendMsgRelease $
                      pure $ Net.Query.SendMsgDone ()
                  Left _e -> Prelude.error "not handled"
              _ -> Prelude.error "not handled"
      waitForTxAndLoop :: IO (Net.Tx.LocalTxClientStIdle (TxInMode CardanoMode) reject IO a)
      waitForTxAndLoop = do
        (tx, prettyTx) <- atomically $ readTQueue txQueue
        case dryRun of
          True -> do
            putStrLn @Text "dry-run, not sending the following tx:"
            putStrLn prettyTx
            waitForTxAndLoop
          False -> pure $ Net.Tx.SendMsgSubmitTx tx $ \_result -> do
            --print result
            waitForTxAndLoop
      submissionClient = Net.Tx.LocalTxSubmissionClient waitForTxAndLoop

    liftIO $ connectToLocalNode
      localNodeConnInfo
      LocalNodeClientProtocols
        { localChainSyncClient    = NoLocalChainSyncClient
        , localStateQueryClient   = Just queryClient
        , localTxSubmissionClient = Just submissionClient
        , localTxMonitoringClient = Nothing
      }
  case eResult of
    Right msg -> print msg
    Left err -> putStrLn $ renderFaucetError err


-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
--mkShelleyBootstrapWitness
--  :: IsShelleyBasedEra era
--  => Maybe NetworkId
--  -> TxBody era
--  -> ShelleyBootstrapWitnessSigningKeyData
--  -> Either ShelleyBootstrapWitnessError (KeyWitness era)
--mkShelleyBootstrapWitness Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
--  Left MissingNetworkIdOrByronAddressError
--mkShelleyBootstrapWitness (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
--  Right $ makeShelleyBootstrapWitness (WitnessNetworkId nw) txBody skey
--mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
--  Right $ makeShelleyBootstrapWitness (WitnessByronAddress addr) txBody skey

--toAddressInAnyEra
--  :: CardanoEra era
--  -> AddressAny
--  -> ExceptT ShelleyTxCmdError IO (AddressInEra era)
--toAddressInAnyEra era addrAny =
--  case addrAny of
--    AddressByron   bAddr -> return (AddressInEra ByronAddressInAnyEra bAddr)
--    AddressShelley sAddr ->
--      case cardanoEraStyle era of
--        LegacyByronEra -> txFeatureMismatch era TxFeatureShelleyAddresses
--        ShelleyBasedEra era' ->
--          return (AddressInEra (ShelleyAddressInEra era') sAddr)


bar :: IO SomeWitness
bar = do
  let
    foo :: WitnessSigningData
    foo = KeyWitnessSigningData (SigningKeyFile "/home/clever/iohk/cardano-node/pay.skey") Nothing
  orDie (T.pack . Prelude.show) $ readWitnessSigningData foo

foo :: FilePath -> ExceptT FaucetError IO (SigningKey PaymentKey)
foo skey_path = do
  eResult <- liftIO $ readSigningKeyFileAnyOf [] [ FromSomeType (AsSigningKey AsPaymentKey) Prelude.id ] $ SigningKeyFile skey_path
  case eResult of
    Left err -> left $ FaucetErrorLoadingKey err
    Right key -> pure key
