{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Api
import           Prelude (String, show, error, id)
import qualified Data.ByteString.Lazy     as LBS
import           Cardano.Prelude
import qualified Cardano.Api as Api
import           Text.Parsec
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import           Cardano.Api.Shelley
import           Cardano.Api.Byron
import qualified Data.Set as Set
import           Data.Map.Strict (keys)
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

testnet :: NetworkId
testnet = Testnet $ NetworkMagic 1097911063

data ShelleyQueryCmdError
  = ShelleyQueryCmdAcquireFailure !AcquireFailure
  deriving Show

-- https://faucet.cardano-testnet.iohkdev.io/send-money/addr_test1vr3g684kyarnug89c7p7gqxr5y8t45g7q4ge4u8hghndvugn6yn5s?apiKey=&g-recaptcha-response=03AGdBq24qppnXuY6fIcCG2Hrpqxfp0V9Xd3oDqElSikr38sAuPMmpO4dKke9O0NzhtFnv_-cXVSs8h4loNDBDeM3rIb5UDHmoCsIylCHXmOovfDIOWM7417-9nW_8XegF7murR2CpVGDp8js7L33ygKqbPUus8AQncJ26AikCDiDNOe7_u6pHb20pR_a8a2cjfcRu6Ptrq8uTWxk2QiinvSctAZbnyTRscupJNDVvoJ1l52LNXOFNTFowRuyaRu1K9mLAJvbwy5n1il_05UGWRNvK3raCUA1DKhf0l9yOCfEvoNJNp10rTG5JFWeYaIiI3-ismQITIsR3u4akYy1PPjmNyF12vfcjlgbvXdGOcodyiZvKulnp2XNSQVIu-OHiwERumU5IISD9VRzY804Z1tKkRB7_PxpUvE7SOAKdOqmkvZLMn8ob1Fz8I562qiV8oezkVkSqTfqQbK2Vsqn3dYDd-IY0pjUhnw
-- http[s]://$FQDN:$PORT/send-money/$ADDRESS

type SendMoney = "send-money" :> Capture "destination_address" String :> Post '[OctetStream] LBS.ByteString
type RootDir = SendMoney

takeOneUtxo :: TMVar (Map TxIn Lovelace) -> Lovelace -> STM (Maybe TxIn)
takeOneUtxo utxoTMVar lovelace = do
  utxo <- takeTMVar utxoTMVar
  let
    utxoOfRightSize = Map.filter (\v -> v == lovelace) utxo
    mTxin = head $ Map.toList $ Map.take 1 utxoOfRightSize
  case mTxin of
    Just (txin, lovelace) -> do
      let
        trimmedUtxo = Map.delete txin utxo
      putTMVar utxoTMVar trimmedUtxo
      pure $ Just txin
    Nothing -> do
      putTMVar utxoTMVar utxo
      pure Nothing

handleSendMoney :: IsShelleyBasedEra era => TQueue (TxInMode CardanoMode) -> TMVar (Map TxIn Lovelace) -> ShelleyWitnessSigningKey -> CardanoEra era -> String -> Servant.Handler LBS.ByteString
handleSendMoney txQueue utxoTMVar key era addr = do
  print addr
  let
    addressAny :: AddressAny
    (Right addressAny) = parse (parseAddressAny <* eof) "" addr
  print addressAny
  mTxin <- liftIO $ atomically $ takeOneUtxo utxoTMVar 1000170000
  case mTxin of
    Just txin -> do
      print txin
      let
        --outvalue :: ExceptT ShelleyTxCmdError IO (TxOutValue ShelleyEra)
        --outvalue = toTxOutValueInAnyEra ShelleyEra 1000000000
        out :: IsShelleyBasedEra era => TxOut a era
        out = TxOut (anyAddressInShelleyBasedEra addressAny) (TxOutAdaOnly undefined 1000000000) undefined undefined
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

buildOwnAddress nw payCred = serialiseAddress $ makeShelleyAddress nw payCred NoStakeAddress

makeTxContents :: TxIn -> TxOut CtxTx era  -> CardanoEra era -> TxBodyContent BuildTx era
makeTxContents txin txout era = TxBodyContent
  { txIns = map (\f -> (f, BuildTxWith $ KeyWitness KeyWitnessForSpending)) [txin]
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
  }
    where
      (Just supported) = validityNoUpperBoundSupportedInEra era

makeSigned :: IsShelleyBasedEra era => ShelleyWitnessSigningKey -> TxIn -> TxOut CtxTx era -> CardanoEra era -> Tx era
makeSigned keys txin txout era = makeSignedTransaction [witness] body
  where
    body = makeTxBody txin txout era
    witness = makeShelleyKeyWitness body keys

server :: IsShelleyBasedEra era => TQueue (TxInMode CardanoMode) -> TMVar (Map TxIn Lovelace) -> ShelleyWitnessSigningKey -> CardanoEra era -> Server RootDir
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

app :: IsShelleyBasedEra era => TQueue (TxInMode CardanoMode) -> TMVar (Map TxIn Lovelace) -> ShelleyWitnessSigningKey -> CardanoEra era -> Application
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

baz (APaymentSigningKey sk) = WitnessPaymentKey sk

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
        , Net.Query.recvMsgFailure = undefined
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
          print "new era"
          print era3
          child <- forkIO $ startApiServer txQueue utxoTMVar key ShelleyEra
          let
            (ShelleyBasedEra sbe) = cardanoEraStyle era3
          runQueryThen (getUtxoQuery addressAny sbe (toEraInMode era3 CardanoMode)) $ \(Right result) -> do
            let
              getValue :: TxOutValue era -> Lovelace
              getValue (TxOutAdaOnly _ ll) = ll
              getValue (TxOutValue _ val) = fromMaybe (0) (valueToLovelace val)

              reduceTxo :: TxOut ctx era -> Lovelace
              reduceTxo (TxOut _ value _ _) = getValue value
              reducedUtxo = Map.map reduceTxo $ unUTxO result
              hasRightValue :: Lovelace -> Bool
              hasRightValue value = value == Lovelace 1000170000
              filteredUtxo = Map.filter hasRightValue reducedUtxo
            atomically $ putTMVar utxoTMVar reducedUtxo
            print "utxo set initialized"
            --print result
            forever $ threadDelay 43200 {- day in seconds -}
            pure $ Net.Query.SendMsgRelease $
              pure $ Net.Query.SendMsgDone ()
    waitForTxAndLoop :: IO (Net.Tx.LocalTxClientStIdle (TxInMode CardanoMode) reject IO a)
    waitForTxAndLoop = do
      tx <- waitForTx
      pure $ Net.Tx.SendMsgSubmitTx tx $ \result -> do
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
