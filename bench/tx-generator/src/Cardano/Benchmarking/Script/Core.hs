{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-} --
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Core
where

import           Prelude
import           Data.Ratio ((%))
import qualified Data.Text as Text (unpack)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Concurrent (threadDelay)
import           Control.Tracer (nullTracer)

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters, protocolParamMaxTxExUnits, protocolParamPrices)

import qualified Cardano.Benchmarking.FundSet as FundSet
import           Cardano.Benchmarking.FundSet (FundInEra(..), Validity(..), Variant(..), liftAnyEra )
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx
                   ( waitBenchmark, walletBenchmark , readSigningKey, secureGenesisFund)
import           Cardano.Benchmarking.GeneratorTx as GeneratorTx
                   (AsyncBenchmarkControl, TxGenError)

import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient, benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import           Cardano.Benchmarking.GeneratorTx.Tx as Core (keyAddress, mkFee, txInModeCardano)

import           Cardano.Benchmarking.OuroborosImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, protocolToCodecConfig, makeLocalConnectInfo)
import           Cardano.Benchmarking.PlutusExample as PlutusExample
import           Cardano.Benchmarking.Tracer as Core
                   ( createLoggingLayerTracers, btTxSubmit_, btN2N_, btConnect_, btSubmission2_)
import           Cardano.Benchmarking.Types as Core
                   (NumberOfInputsPerTx(..), NumberOfOutputsPerTx(..),NumberOfTxs(..), SubmissionErrorPolicy(..)
                   , TPSRate, TxAdditionalSize(..))
import           Cardano.Benchmarking.Wallet as Wallet hiding (keyAddress)
import           Cardano.Benchmarking.FundSet as FundSet (getFundTxIn)
import           Cardano.Benchmarking.ListBufferedSelector

import           Cardano.Benchmarking.Script.Aeson (readProtocolParametersFile)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store
import           Cardano.Benchmarking.Script.Types

liftCoreWithEra :: (forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra coreCall = withEra ( liftIO . runExceptT . coreCall)

withEra :: (forall era. IsShelleyBasedEra era => AsType era -> ActionM x) -> ActionM x
withEra action = do
  era <- get $ User TEra
  case era of
    AnyCardanoEra AlonzoEra  -> action AsAlonzoEra
    AnyCardanoEra MaryEra    -> action AsMaryEra
    AnyCardanoEra AllegraEra -> action AsAllegraEra
    AnyCardanoEra ShelleyEra -> action AsShelleyEra
    AnyCardanoEra ByronEra   -> error "byron not supported"

setProtocolParameters :: ProtocolParametersSource -> ActionM ()
setProtocolParameters s = case s of
  QueryLocalNode -> do
    set ProtocolParameterMode ProtocolParameterQuery
  UseLocalProtocolFile file -> do
    protocolParameters <- liftIO $ readProtocolParametersFile file
    set ProtocolParameterMode $ ProtocolParameterLocal protocolParameters

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  liftIO (runExceptT $ Core.startProtocol filePath) >>= \case
    Left err -> throwE $ CliError err
    Right (loggingLayer, protocol) -> do
      set LoggingLayer loggingLayer
      set Protocol protocol
      set BenchTracers $ Core.createLoggingLayerTracers loggingLayer
      set Genesis $ Core.getGenesis protocol
      set (User TNetworkId) $ protocolToNetworkId protocol

readSigningKey :: KeyName -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  liftIO ( runExceptT $ GeneratorTx.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setName name key

defineSigningKey :: KeyName -> TextEnvelope -> ActionM ()
defineSigningKey name descr
  = case deserialiseFromTextEnvelopeAnyOf types descr of
    Right key -> setName name key
    Left err -> throwE $ ApiError $ show err
  where
    types :: [FromSomeType HasTextEnvelope (SigningKey PaymentKey)]
    types =
      [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey) id
      ]

addFund :: WalletName -> TxIn -> Lovelace -> KeyName -> ActionM ()
addFund wallet txIn lovelace keyName = do
  fundKey  <- getName keyName
  let
    mkOutValue :: forall era. IsShelleyBasedEra era => AsType era -> ActionM (InAnyCardanoEra TxOutValue)
    mkOutValue = \_ -> return $ InAnyCardanoEra (cardanoEra @ era) (mkTxOutValueAdaOnly lovelace)
  outValue <- withEra mkOutValue
  addFundToWallet wallet txIn outValue fundKey

addFundToWallet :: WalletName -> TxIn -> InAnyCardanoEra TxOutValue -> SigningKey PaymentKey -> ActionM ()
addFundToWallet wallet txIn outVal skey = do
  walletRef <- getName wallet
  liftIO (walletRefInsertFund walletRef (FundSet.Fund $ mkFund outVal))
  where
    mkFund = liftAnyEra $ \value -> FundInEra {
           _fundTxIn = txIn
         , _fundVal = value
         , _fundSigningKey = Just skey
         , _fundValidity = Confirmed
         , _fundVariant = PlainOldFund
         }

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = submitTxToNodeLocal <$> getLocalConnectInfo

delay :: Double -> ActionM ()
delay t = liftIO $ threadDelay $ floor $ 1000000 * t

waitBenchmarkCore :: AsyncBenchmarkControl ->  ActionM ()
waitBenchmarkCore ctl = do
  tracers  <- get BenchTracers
  _ <- liftIO $ runExceptT $ GeneratorTx.waitBenchmark (btTxSubmit_ tracers) ctl
  return ()

getConnectClient :: ActionM ConnectClient
getConnectClient = do
  tracers  <- get BenchTracers
  (Testnet networkMagic) <- getUser TNetworkId
  protocol <- get Protocol
  void $ return $(btSubmission2_ tracers)
  ioManager <- askIOManager
  return $ benchmarkConnectTxSubmit
                       ioManager
                       (btConnect_ tracers)
                       nullTracer -- (btSubmission2_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic
waitBenchmark :: ThreadName -> ActionM ()
waitBenchmark n = getName n >>= waitBenchmarkCore

cancelBenchmark :: ThreadName -> ActionM ()
cancelBenchmark n = do
  ctl@(_, _ , _ , shutdownAction) <- getName n
  liftIO shutdownAction
  waitBenchmarkCore ctl

getLocalConnectInfo :: ActionM  (LocalNodeConnectInfo CardanoMode)
getLocalConnectInfo = makeLocalConnectInfo <$> getUser TNetworkId <*> getUser TLocalSocket

queryEra :: ActionM AnyCardanoEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) $ QueryCurrentEra CardanoModeIsMultiEra
  case ret of
    Right era -> return era
    Left err -> throwE $ ApiError $ show err

queryRemoteProtocolParameters :: ActionM ProtocolParameters
queryRemoteProtocolParameters = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip)
                    $ QueryInEra AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
  case ret of
    Right (Right pp) -> return pp
    Right (Left err) -> throwE $ ApiError $ show err
    Left err -> throwE $ ApiError $ show err

getProtocolParameters :: ActionM ProtocolParameters
getProtocolParameters = do
  get ProtocolParameterMode  >>= \case
    ProtocolParameterQuery -> queryRemoteProtocolParameters
    ProtocolParameterLocal parameters -> return parameters

waitForEra :: AnyCardanoEra -> ActionM ()
waitForEra era = do
  currentEra <- queryEra
  if currentEra == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentEra ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era

runWalletScriptInMode :: forall era.
     IsShelleyBasedEra era
  => SubmitMode
  -> WalletScript era
  -> ActionM ()
runWalletScriptInMode submitMode s = do
  step <- liftIO $ runWalletScript s
  case step of
    Done -> return ()
    Error err -> throwE $ ApiError $ show err
    NextTx nextScript tx -> do
      case submitMode of
        LocalSocket -> void $ localSubmitTx $ txInModeCardano tx
        NodeToNode -> throwE $ ApiError "NodeToNodeMode not supported in runWalletScriptInMode"
        DumpToFile filePath -> dumpToFile filePath $ txInModeCardano tx
        DiscardTX -> return ()
      runWalletScriptInMode submitMode nextScript

localSubmitTx :: TxInMode CardanoMode -> ActionM (SubmitResult (TxValidationErrorInMode CardanoMode))
localSubmitTx tx = do
  submit <- getLocalSubmitTx
  ret <- liftIO $ submit tx
  case ret of
    SubmitSuccess -> return ()
    SubmitFail e -> traceDebug $ concat
                        [ "local submit failed: " , show e , " (" , show tx , ")"]
  return ret

makeMetadata :: forall era. IsShelleyBasedEra era => ActionM (TxMetadataInEra era)
makeMetadata = do
  payloadSize <- getUser TTxAdditionalSize
  case mkMetadata $ unTxAdditionalSize payloadSize of
    Right m -> return m
    Left err -> throwE $ MetadataError err

runBenchmark :: WalletName -> SubmitMode -> SpendMode -> ThreadName -> NumberOfTxs -> TPSRate -> ActionM ()
runBenchmark sourceWallet submitMode spendMode threadName txCount tps
  = case spendMode of
      SpendOutput -> withEra $ runBenchmarkInEra sourceWallet submitMode threadName txCount tps
      SpendScript scriptFile scriptBudget scriptData scriptRedeemer
        -> runPlutusBenchmark sourceWallet submitMode scriptFile scriptBudget scriptData scriptRedeemer threadName txCount tps
      SpendAutoScript scriptFile -> spendAutoScript sourceWallet submitMode scriptFile threadName txCount tps

runBenchmarkInEra :: forall era. IsShelleyBasedEra era
  => WalletName
  -> SubmitMode
  -> ThreadName
  -> NumberOfTxs
  -> TPSRate
  -> AsType era
  -> ActionM ()
runBenchmarkInEra sourceWallet submitMode (ThreadName threadName) txCount tps era = do
  tracers  <- get BenchTracers
  networkId <- getUser TNetworkId
  fundKey <- getName $ KeyName "pass-partout" -- should be walletkey
  targets  <- getUser TTargets
  (NumberOfInputsPerTx   numInputs) <- getUser TNumberOfInputsPerTx
  (NumberOfOutputsPerTx numOutputs) <- getUser TNumberOfOutputsPerTx
  fee <- getUser TFee
  minValuePerUTxO <- getUser TMinValuePerUTxO
  protocolParameters <- getProtocolParameters
  walletRefSrc <- getName sourceWallet
  let walletRefDst = walletRefSrc 
  metadata <- makeMetadata
  connectClient <- getConnectClient
  let
    (Quantity minValue) = lovelaceToQuantity $ fromIntegral numOutputs * minValuePerUTxO + fee

  -- this is not totally correct:
  -- beware of rounding errors !
    minValuePerInput = quantityToLovelace $ fromIntegral (if m==0 then d else d+1)
      where
        (d, m) = minValue `divMod` fromIntegral numInputs

--    fundSource :: FundSet.Target -> FundSet.FundSource
--    fundSource target = mkWalletFundSource walletRef $ FundSet.selectInputs ConfirmedBeforeReuse numInputs minTxValue PlainOldFund target

  fundSource <- liftIO (mkBufferedSource walletRefSrc
                   (fromIntegral (unNumberOfTxs txCount) * numInputs)
                   minValuePerInput
                   PlainOldFund numInputs) >>= \case
    Right a  -> return a
    Left err -> throwE $ WalletError err

  let
    inToOut :: [Lovelace] -> [Lovelace]
    inToOut = FundSet.inputsToOutputsWithFee fee numOutputs

    txGenerator = genTx protocolParameters (TxInsCollateralNone, []) (mkFee fee) metadata (KeyWitness KeyWitnessForSpending)

    toUTxO :: FundSet.Target -> FundSet.SeqNumber -> ToUTxO era
    toUTxO target seqNumber = Wallet.mkUTxO networkId fundKey (InFlight target seqNumber)

    fundToStore = mkWalletFundStore walletRefDst

    walletScript :: FundSet.Target -> WalletScript era
    walletScript = benchmarkWalletScript walletRefSrc txGenerator txCount (const fundSource) inToOut toUTxO fundToStore

    coreCall :: AsType era -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall eraProxy = GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               threadName targets tps LogErrors eraProxy txCount walletScript
  case submitMode of
    NodeToNode -> do
      ret <- liftIO $ runExceptT $ coreCall era
      case ret of
        Left err -> liftTxGenError err
        Right ctl -> setName (ThreadName threadName) ctl
    _otherwise -> runWalletScriptInMode submitMode $ walletScript $ FundSet.Target "alternate-submit-mode"

runPlutusBenchmark ::
     WalletName
  -> SubmitMode
  -> FilePath
  -> ScriptBudget
  -> ScriptData
  -> ScriptRedeemer
  -> ThreadName
  -> NumberOfTxs
  -> TPSRate
  -> ActionM ()
runPlutusBenchmark sourceWallet submitMode scriptFile scriptBudget scriptData scriptRedeemer (ThreadName threadName) txCount tps = do
  tracers  <- get BenchTracers
  targets  <- getUser TTargets
  (NumberOfInputsPerTx   numInputs) <- getUser TNumberOfInputsPerTx
  (NumberOfOutputsPerTx numOutputs) <- getUser TNumberOfOutputsPerTx
  networkId <- getUser TNetworkId
  minValuePerUTxO <- getUser TMinValuePerUTxO
  protocolParameters <- getProtocolParameters
  executionUnitPrices <- case protocolParamPrices protocolParameters of
    Just x -> return x
    Nothing -> throwE $ WalletError "unexpected protocolParamPrices == Nothing in runPlutusBenchmark"
  walletRefSrc <- getName sourceWallet
  let
    -- runBenchmark reads and write from the single sourceWallet.
    walletRefDst = walletRefSrc
    walletRefCollateral = walletRefSrc
  fundKey <- getName $ KeyName "pass-partout"
  script <- liftIO $ PlutusExample.readScript scriptFile
  -- This does not remove the collateral from the wallet, i.e. same collateral is uses for everything.
  -- This is fine unless a script ever fails.
  collateralFunds <- liftIO ( askWalletRef walletRefCollateral (FundSet.selectCollateral . walletFunds)) >>= \case
    Right c -> return c
    Left err -> throwE $ WalletError err
  baseFee <- getUser TFee
  _minValuePerUTxO <- getUser TMinValuePerUTxO -- TODO:Fix
  metadata <- makeMetadata
  connectClient <- getConnectClient

  let costsPreRun = preExecuteScript protocolParameters script scriptData scriptRedeemer
  executionUnits <- case (scriptBudget, costsPreRun) of
    (_, Left err) -> throwE $ WalletError ("Cannot pre-execute Plutus script." ++ err)
    (StaticScriptBudget exUnits, _)  -> return exUnits
    (PreExecuteScript, Right preRun) -> return preRun
    (CheckScriptBudget want, Right preRun)
      -> if want == preRun then return preRun
                           else throwE $ WalletError $ concat [
        " Stated execution Units do not match result of pre execution. "
      , " Stated value : ", show want
      , " PreExecution result : ", show preRun
      ]

  let msg = mconcat [ "Plutus Benchmark :"
                  , " Script: ", scriptFile
                  , ", Datum: ", show scriptData
                  , ", Redeemer: ", show scriptRedeemer
                  , ", StatedBudget: ", show executionUnits
                  ]
  traceDebug msg

  let
    -- TODO --    Cardano.Ledger.Alonzo.Scripts.txscriptfee :: Prices -> ExUnits -> Coin
    scriptFee = quantityToLovelace $ Quantity $ ceiling f
       where
         f :: Rational
         f = (executionSteps e `times` priceExecutionSteps p) + (executionMemory e `times` priceExecutionMemory p)
         e = executionUnits
         p = executionUnitPrices
         times w c = fromIntegral w % 1 * c

    totalFee = baseFee +  fromIntegral numInputs * scriptFee
    (Quantity minValue) = lovelaceToQuantity $ fromIntegral numOutputs * minValuePerUTxO + totalFee
  -- this is not totally correct:
  -- beware of rounding errors !
    minValuePerInput = quantityToLovelace $ fromIntegral (if m==0 then d else d+1)
      where
        (d, m) = minValue `divMod` fromIntegral numInputs

--    fundSource :: FundSet.Target -> FundSet.FundSource
--    fundSource target = mkWalletFundSource walletRef $ FundSet.selectInputs ConfirmedBeforeReuse numInputs minTxValue PlainOldFund target

  fundSource <- liftIO (mkBufferedSource walletRefSrc
                   (fromIntegral (unNumberOfTxs txCount) * numInputs)
                   minValuePerInput
                   (PlutusScriptFund scriptFile scriptData) numInputs) >>= \case
    Right a  -> return a
    Left err -> throwE $ WalletError err

  let
    inToOut :: [Lovelace] -> [Lovelace]
    inToOut = FundSet.inputsToOutputsWithFee totalFee numOutputs
--    inToOut = FundSet.inputsToOutputsWithFee totalFee 1

    PlutusScript PlutusScriptV1 script' = script
    scriptWitness :: ScriptWitness WitCtxTxIn AlonzoEra
    scriptWitness = PlutusScriptWitness
                          PlutusScriptV1InAlonzo
                          PlutusScriptV1
                          script'
                          (ScriptDatumForTxIn scriptData)
                          scriptRedeemer
                          executionUnits

    collateral = (TxInsCollateral CollateralInAlonzoEra $  map getFundTxIn collateralFunds, collateralFunds)
    txGenerator = genTx protocolParameters collateral (mkFee totalFee) metadata (ScriptWitness ScriptWitnessForSpending scriptWitness)

    fundToStore = mkWalletFundStore walletRefDst

    toUTxO :: FundSet.Target -> FundSet.SeqNumber -> ToUTxO AlonzoEra
    toUTxO target seqNumber = Wallet.mkUTxO networkId fundKey (InFlight target seqNumber)

    walletScript :: FundSet.Target -> WalletScript AlonzoEra
    walletScript = benchmarkWalletScript walletRefSrc txGenerator txCount (const fundSource) inToOut toUTxO fundToStore

  case submitMode of
    NodeToNode -> do
      ret <- liftIO $ runExceptT $ GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                               threadName targets tps LogErrors AsAlonzoEra txCount walletScript
      case ret of
        Left err -> liftTxGenError err
        Right ctl -> setName (ThreadName threadName) ctl
    _otherwise -> runWalletScriptInMode submitMode $ walletScript $ FundSet.Target "alternate-submit-mode"

dumpToFile :: FilePath -> TxInMode CardanoMode -> ActionM ()
dumpToFile filePath tx = liftIO $ dumpToFileIO filePath tx

dumpToFileIO :: FilePath -> TxInMode CardanoMode -> IO ()
dumpToFileIO filePath tx = appendFile filePath ('\n' : show tx)

importGenesisFund
   :: WalletName
   -> SubmitMode
   -> KeyName
   -> KeyName
   -> ActionM ()
importGenesisFund wallet submitMode genesisKeyName destKey = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- case submitMode of
    LocalSocket -> getLocalSubmitTx
    NodeToNode -> throwE $ WalletError "NodeToNode mode not supported in importGenesisFund"
    DumpToFile filePath -> return $ \tx -> dumpToFileIO filePath tx >> return SubmitSuccess
    DiscardTX -> return $ \_ -> return SubmitSuccess
  networkId <- getUser TNetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.secureGenesisFund tracer localSubmit networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  result <- liftCoreWithEra coreCall
  case result of 
    Left err -> liftTxGenError err
    Right ((txIn, outVal), skey) -> addFundToWallet wallet txIn outVal skey
  
initWallet :: WalletName -> ActionM ()
initWallet name = liftIO Wallet.initWallet >>= setName name

createChange :: WalletName -> WalletName -> SubmitMode -> PayMode -> Lovelace -> Int -> ActionM ()
createChange sourceWallet dstWallet submitMode payMode value count = case payMode of
  PayToAddr keyName -> withEra $ createChangeInEra sourceWallet dstWallet submitMode PlainOldFund keyName value count
  -- Problem here: PayToCollateral will create an output marked as collateral
  -- and also return any change to a collateral, which makes the returned change unusable.
  PayToCollateral keyName -> withEra $ createChangeInEra sourceWallet dstWallet submitMode CollateralFund keyName value count
  PayToScript scriptFile scriptData -> createChangeScriptFunds sourceWallet dstWallet submitMode scriptFile scriptData value count

createChangeScriptFunds :: WalletName -> WalletName -> SubmitMode -> FilePath -> ScriptData -> Lovelace -> Int -> ActionM ()
createChangeScriptFunds sourceWallet dstWallet submitMode scriptFile scriptData value count = do
  walletRef <- getName dstWallet
  networkId <- getUser TNetworkId
  protocolParameters <- getProtocolParameters
  _fundKey <- getName $ KeyName "pass-partout"
  fee <- getUser TFee  
  script <- liftIO $ PlutusExample.readScript scriptFile --TODO: this should throw a file-not-found-error !
  let
    createCoins fundSource coins = do
      let
--        selector :: FundSet.FundSource
--        selector = mkWalletFundSource walletRef $ FundSet.selectMinValue $ sum coins + fee
        inOut :: [Lovelace] -> [Lovelace]
        inOut = Wallet.includeChange fee coins
        toUTxO = PlutusExample.mkUtxoScript networkId (scriptFile, script, scriptData) Confirmed
        fundToStore = mkWalletFundStore walletRef

      tx <- liftIO $ sourceToStoreTransaction
                                      (genTx protocolParameters (TxInsCollateralNone, [])
                                       (mkFee fee) TxMetadataNone (KeyWitness KeyWitnessForSpending))
                                      fundSource inOut toUTxO fundToStore
      return $ fmap txInModeCardano tx
    addressMsg =  Text.unpack $ serialiseAddress $ makeShelleyAddress networkId (PaymentCredentialByScript $ hashScript script) NoStakeAddress
  createChangeGeneric sourceWallet submitMode createCoins addressMsg value count

createChangeInEra :: forall era. IsShelleyBasedEra era
  => WalletName
  -> WalletName
  -> SubmitMode
  -> Variant
  -> KeyName
  -> Lovelace
  -> Int
  -> AsType era
  -> ActionM ()
createChangeInEra sourceWallet dstWallet submitMode variant keyName value count _proxy = do
  networkId <- getUser TNetworkId
  walletRef <- getName dstWallet
  fee <- getUser TFee
  protocolParameters <- getProtocolParameters
  fundKey <- getName keyName
  let
    createCoins :: FundSet.FundSource -> [Lovelace] -> ActionM (Either String (TxInMode CardanoMode))
    createCoins fundSource coins = do
      let
--        selector :: FundSet.FundSource
--        selector = mkWalletFundSource walletRef $ FundSet.selectMinValue $ sum coins + fee
        inOut :: [Lovelace] -> [Lovelace]
        inOut = Wallet.includeChange fee coins
        toUTxO = Wallet.mkUTxOVariant variant networkId fundKey Confirmed
        fundToStore = mkWalletFundStore walletRef

      (tx :: Either String (Tx era)) <- liftIO $ sourceToStoreTransaction
                                                  (genTx protocolParameters (TxInsCollateralNone, [])
                                                   (mkFee fee) TxMetadataNone (KeyWitness KeyWitnessForSpending))
                                                  fundSource inOut toUTxO fundToStore
      return $ fmap txInModeCardano tx
    addressMsg = Text.unpack $ serialiseAddress $ keyAddress @ era networkId fundKey
  createChangeGeneric sourceWallet submitMode createCoins addressMsg value count

createChangeGeneric ::
     WalletName
  -> SubmitMode
  -> (FundSet.FundSource -> [Lovelace] -> ActionM (Either String (TxInMode CardanoMode)))
  -> String
  -> Lovelace
  -> Int
  -> ActionM ()
createChangeGeneric sourceWallet submitMode createCoins addressMsg value count = do
  fee <- getUser TFee
  walletRef <- getName sourceWallet
  let
    coinsList = replicate count value
    maxTxSize = 30
    chunks = chunkList maxTxSize coinsList
    txCount = length chunks
    txValue = fromIntegral (min maxTxSize count) * value + fee
    msg = mconcat [ "createChangeGeneric: outputs: ", show count
                  , " value: ", show value
                  , " number of txs: ", show txCount
                  , " address: ", addressMsg
                  ]
  traceDebug msg
  fundSource <- liftIO (mkBufferedSource walletRef txCount txValue PlainOldFund 1) >>= \case
    Right a  -> return a
    Left err -> throwE $ WalletError err

  forM_ chunks $ \coins -> do
    gen <- createCoins fundSource coins
    case gen of
      Left err -> throwE $ WalletError err
      Right tx -> case submitMode of
        LocalSocket -> void $ localSubmitTx tx
        NodeToNode -> throwE $ WalletError "NodeToNode mode not supported in createChangeGeneric"
        DumpToFile filePath -> dumpToFile filePath tx
        DiscardTX -> return ()

  traceDebug "createChangeGeneric: splitting done"
 where
  chunkList :: Int -> [a] -> [[a]]
  chunkList _ [] = []
  chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs

{-
Use a binary search to find a loop counter that maxes out the available per transaction Plutus budget.
It is intended to be used with the the loop script from cardano-node/plutus-examples/...
loopScriptFile is the FilePath to the Plutus script that implements the delay loop. (for example in /nix/store/).
spendAutoScript relies on a particular calling convention of the loop script.
-}
spendAutoScript :: WalletName -> SubmitMode -> FilePath -> ThreadName -> NumberOfTxs -> TPSRate -> ActionM ()
spendAutoScript sourceWallet submitMode loopScriptFile threadName txCount tps = do
  protocolParameters <- getProtocolParameters
  perTxBudget <- case protocolParamMaxTxExUnits protocolParameters of
    Nothing -> throwE $ ApiError "Cannot determine protocolParamMaxTxExUnits"
    Just b -> return b
  traceDebug $ "Plutus auto mode : Available budget per TX: " ++ show perTxBudget

  numInputs <- fromIntegral <$> getUser TNumberOfInputsPerTx
  let budget = ExecutionUnits
                 (executionSteps perTxBudget `div` numInputs)
                 (executionMemory perTxBudget `div` numInputs)
  traceDebug $ "Plutus auto mode : Available budget per script run: " ++ show budget

  script <- liftIO $ readScript loopScriptFile
  let
    isInLimits :: Integer -> Either String Bool
    isInLimits n = case preExecuteScript protocolParameters script (ScriptDataNumber 0) (toLoopArgument n) of
      Left err -> Left err
      Right use -> Right $ (executionSteps use <= executionSteps budget) && (executionMemory use <= executionMemory budget)
    searchUpperBound = 100000 -- The highest loop count that is tried. (This is about 50 times the current mainnet limit.)
  redeemer <- case startSearch isInLimits 0 searchUpperBound of
    Left err -> throwE $ ApiError $ "cannot find fitting redeemer :" ++ err
    Right n -> return $ toLoopArgument n
  runPlutusBenchmark sourceWallet submitMode loopScriptFile PreExecuteScript (ScriptDataNumber 0) redeemer threadName txCount tps
  where
    -- This is the hardcoded calling convention of the loop.plutus script.
    -- To loop n times one has to pass n + 1_000_000 as redeemer.
    toLoopArgument n = ScriptDataNumber $ n + 1000000
    startSearch f a b = do
      l <- f a
      h <- f b
      if l && not h then search f a b
        else Left $ "Binary search: Bad initial bounds : " ++ show (a,l,b,h)
    search f a b
      = if a + 1 == b then Right a
           else do
             let m = (a + b) `div` 2
             test <- f m
             if test then search f m b else search f a m

{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
reserved :: [String] -> ActionM ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"
