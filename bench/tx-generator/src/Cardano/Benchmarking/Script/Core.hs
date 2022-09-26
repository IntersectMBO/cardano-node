{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Core
where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           "contra-tracer" Control.Tracer (nullTracer)
import           Data.Ratio ((%))

import           Streaming
import qualified Streaming.Prelude as Streaming

import qualified Data.Text as Text (unpack)
import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScriptOrReferenceInput (..), ProtocolParameters,
                   protocolParamMaxTxExUnits, protocolParamPrices)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

import           Cardano.TxGenerator.Fund as Fund
import qualified Cardano.TxGenerator.FundQueue as FundQueue
import           Cardano.TxGenerator.Tx
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.UTxO
import qualified Cardano.TxGenerator.Utils as Utils

import           Cardano.Benchmarking.GeneratorTx as GeneratorTx (AsyncBenchmarkControl)
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx (readSigningKey, waitBenchmark,
                   walletBenchmark)
import qualified Cardano.Benchmarking.GeneratorTx.Genesis as Genesis
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient,
                   benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)

import           Cardano.Benchmarking.OuroborosImports as Core (LocalSubmitTx, SigningKeyFile,
                   makeLocalConnectInfo, protocolToCodecConfig)
import           Cardano.Benchmarking.PlutusExample as PlutusExample

import           Cardano.Benchmarking.LogTypes as Core (TraceBenchTxSubmit (..), btConnect_, btN2N_,
                   btSubmission2_, btTxSubmit_)
import           Cardano.Benchmarking.Types as Core (SubmissionErrorPolicy (..))
import           Cardano.Benchmarking.Wallet as Wallet

import           Cardano.Benchmarking.Script.Aeson (readProtocolParametersFile)
import           Cardano.Benchmarking.Script.Env hiding (Error(TxGenError))
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Version as Version

liftCoreWithEra :: AnyCardanoEra -> (forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra era coreCall = withEra era ( liftIO . runExceptT . coreCall)

withEra :: AnyCardanoEra -> (forall era. IsShelleyBasedEra era => AsType era -> ActionM x) -> ActionM x
withEra era action = do
  case era of
    AnyCardanoEra BabbageEra -> action AsBabbageEra
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

readSigningKey :: KeyName -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  liftIO ( runExceptT $ GeneratorTx.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setName name key

parseSigningKey :: TextEnvelope -> Either TextEnvelopeError (SigningKey PaymentKey)
parseSigningKey = deserialiseFromTextEnvelopeAnyOf types
  where
    types :: [FromSomeType HasTextEnvelope (SigningKey PaymentKey)]
    types =
      [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey) id
      ]

defineSigningKey :: KeyName -> TextEnvelope -> ActionM ()
defineSigningKey name descr
  = case parseSigningKey descr of
    Right key -> setName name key
    Left err -> liftTxGenError $ ApiError err

addFund :: AnyCardanoEra -> WalletName -> TxIn -> Lovelace -> KeyName -> ActionM ()
addFund era wallet txIn lovelace keyName = do
  fundKey  <- getName keyName
  let
    mkOutValue :: forall era. IsShelleyBasedEra era => AsType era -> ActionM (InAnyCardanoEra TxOutValue)
    mkOutValue = \_ -> return $ InAnyCardanoEra (cardanoEra @ era) (lovelaceToTxOutValue lovelace)
  outValue <- withEra era mkOutValue
  addFundToWallet wallet txIn outValue fundKey

addFundToWallet :: WalletName -> TxIn -> InAnyCardanoEra TxOutValue -> SigningKey PaymentKey -> ActionM ()
addFundToWallet wallet txIn outVal skey = do
  walletRef <- getName wallet
  liftIO (walletRefInsertFund walletRef (FundQueue.Fund $ mkFund outVal))
  where
    mkFund = Utils.liftAnyEra $ \value -> FundInEra {
           _fundTxIn = txIn
         , _fundWitness = KeyWitness KeyWitnessForSpending
         , _fundVal = value
         , _fundSigningKey = Just skey
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
    Left err -> liftTxGenError $ TxGenError $ show err

queryRemoteProtocolParameters :: ActionM ProtocolParameters
queryRemoteProtocolParameters = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  era <- queryEra
  let
    callQuery :: forall a. Show a => QueryInMode CardanoMode (Either a ProtocolParameters) -> ActionM ProtocolParameters
    callQuery query = do
      res <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) query
      case res of
        Right (Right pp) -> return pp
        Right (Left err) -> liftTxGenError $ TxGenError $ show err
        Left err -> liftTxGenError $ TxGenError $ show err
  case era of
    AnyCardanoEra ByronEra   -> liftTxGenError $ TxGenError "queryRemoteProtocolParameters Byron not supported"
    AnyCardanoEra ShelleyEra -> callQuery $ QueryInEra ShelleyEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraShelley QueryProtocolParameters
    AnyCardanoEra AllegraEra -> callQuery $ QueryInEra AllegraEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAllegra QueryProtocolParameters
    AnyCardanoEra MaryEra    -> callQuery $ QueryInEra    MaryEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraMary    QueryProtocolParameters
    AnyCardanoEra AlonzoEra  -> callQuery $ QueryInEra  AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    AnyCardanoEra BabbageEra -> callQuery $ QueryInEra BabbageEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraBabbage QueryProtocolParameters

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

localSubmitTx :: TxInMode CardanoMode -> ActionM (SubmitResult (TxValidationErrorInMode CardanoMode))
localSubmitTx tx = do
  submit <- getLocalSubmitTx
  ret <- liftIO $ submit tx
  case ret of
    SubmitSuccess -> return ret
    SubmitFail e -> do
      let msg = concat [ "local submit failed: " , show e , " (" , show tx , ")" ]
      traceDebug msg
      return ret
--      throwE $ ApiError msg

-- TODO:
-- It should be possible to exit the tx-generator with an exception and also get the log messages.
-- Problem 1: When doing throwE $ ApiError msg logmessages get lost !
-- Problem 2: Workbench restarts the tx-generator -> this may be the reason for loss of messages

toMetadata :: forall era. IsShelleyBasedEra era => Maybe Int -> TxMetadataInEra era
toMetadata Nothing = TxMetadataNone
toMetadata (Just payloadSize) = case mkMetadata payloadSize of
  Right m -> m
  Left err -> error err

submitAction :: AnyCardanoEra -> SubmitMode -> Generator -> ActionM ()
submitAction era submitMode generator = withEra era $ submitInEra submitMode generator

submitInEra :: forall era. IsShelleyBasedEra era => SubmitMode -> Generator -> AsType era -> ActionM ()
submitInEra submitMode generator era = do
  txStream <- evalGenerator generator era
  case submitMode of
    NodeToNode _ -> error "NodeToNode deprecated: ToDo: remove"
    Benchmark nodes threadName tpsRate txCount -> benchmarkTxStream txStream nodes threadName tpsRate txCount era
    LocalSocket -> submitAll (void . localSubmitTx . Utils.mkTxInModeCardano) txStream
    DumpToFile filePath -> liftIO $ Streaming.writeFile filePath $ Streaming.map showTx txStream
    DiscardTX -> liftIO $ Streaming.mapM_ forceTx txStream
 where
  forceTx (Right _) = return ()
  forceTx (Left err) = error $ show err
  showTx (Left err) = error $ show err
  showTx (Right tx) = '\n' : show tx
   -- todo: use Streaming.run
  submitAll :: (Tx era -> ActionM ()) -> TxStream IO era -> ActionM ()
  submitAll callback stream = do
    step <- liftIO $ Streaming.inspect stream
    case step of
      (Left ()) -> return ()
      (Right (Left err :> _rest)) -> liftTxGenError $ TxGenError $ show err
      (Right (Right tx :> rest)) -> do
        callback tx
        submitAll callback rest

benchmarkTxStream :: forall era. IsShelleyBasedEra era
  => TxStream IO era
  -> TargetNodes
  -> ThreadName
  -> TPSRate
  -> NumberOfTxs
  -> AsType era
  -> ActionM ()
benchmarkTxStream txStream targetNodes (ThreadName threadName) tps txCount era = do
  tracers  <- get BenchTracers
  connectClient <- getConnectClient
  let
    coreCall :: AsType era -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall eraProxy = GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               threadName targetNodes tps LogErrors eraProxy txCount txStream
  ret <- liftIO $ runExceptT $ coreCall era
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> setName (ThreadName threadName) ctl

evalGenerator :: forall era. IsShelleyBasedEra era => Generator -> AsType era -> ActionM (TxStream IO era)
evalGenerator generator era = do
  networkId <- getUser TNetworkId
  protocolParameters <- getProtocolParameters
  case generator of
    SecureGenesis fee wallet genesisKeyName destKeyName -> do
      genesis  <- get Genesis
      ttl      <- getUser TTTL
      destKey  <- getName destKeyName
      destWallet  <- getName wallet
      genesisKey  <- getName genesisKeyName
      let
        outAddr = Utils.keyAddress @ era networkId destKey
        (_inAddr, lovelace) = Genesis.genesisFundForKey @ era networkId genesis genesisKey
        (tx, fund) = Genesis.genesisExpenditure networkId genesisKey outAddr lovelace fee ttl destKey
        gen = do
          walletRefInsertFund destWallet fund
          return $ Right tx
      return $ Streaming.effect (Streaming.yield <$> gen)
    Split fee walletName payMode payModeChange coins -> do
      wallet <- getName walletName
      (toUTxO, addressOut) <- interpretPayMode payMode
      traceDebug $ "split output address : " ++ addressOut
      (toUTxOChange, addressChange) <- interpretPayMode payModeChange
      traceDebug $ "split change address : " ++ addressChange
      let
        fundSource = walletSource wallet 1
        inToOut = Utils.includeChange fee coins
        txGenerator = genTx protocolParameters (TxInsCollateralNone, []) (Utils.mkTxFee fee) TxMetadataNone
        sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut $ mangleWithChange toUTxOChange toUTxO
      return $ Streaming.effect (Streaming.yield <$> sourceToStore)
    SplitN fee walletName payMode count -> do
      wallet <- getName walletName
      (toUTxO, addressOut) <- interpretPayMode payMode
      traceDebug $ "SplitN output address : " ++ addressOut
      let
        fundSource = walletSource wallet 1
        inToOut = Utils.inputsToOutputsWithFee fee count
        txGenerator = genTx protocolParameters (TxInsCollateralNone, []) (Utils.mkTxFee fee) TxMetadataNone
        sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut (mangle $ repeat toUTxO)
      return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    NtoM fee walletName payMode inputs outputs metadataSize collateralWallet -> do
      wallet <- getName walletName
      collaterals <- selectCollateralFunds collateralWallet
      (toUTxO, addressOut) <- interpretPayMode payMode
      traceDebug $ "NtoM output address : " ++ addressOut
      let
        fundSource = walletSource wallet inputs
        inToOut = Utils.inputsToOutputsWithFee fee outputs
        txGenerator = genTx protocolParameters collaterals (Utils.mkTxFee fee) (toMetadata metadataSize)
        sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut (mangle $ repeat toUTxO)
      return $ Streaming.effect (Streaming.yield <$> sourceToStore)
    Sequence l -> do
      gList <- forM l $ \g -> evalGenerator g era
      return $ Streaming.for (Streaming.each gList) id
    Cycle g -> Streaming.cycle <$> evalGenerator g era
    Take count g -> Streaming.take count <$> evalGenerator g era
    RoundRobin l -> do
      _gList <- forM l $ \g -> evalGenerator g era
      error "return $ foldr1 Streaming.interleaves gList"
    OneOf _l -> error "todo: implement Quickcheck style oneOf generator"

selectCollateralFunds :: forall era. IsShelleyBasedEra era
  => Maybe WalletName
  -> ActionM (TxInsCollateral era, [FundQueue.Fund])
selectCollateralFunds Nothing = return (TxInsCollateralNone, [])
selectCollateralFunds (Just walletName) = do
  cw <- getName walletName
  collateralFunds <- liftIO ( askWalletRef cw FundQueue.toList ) >>= \case
    [] -> throwE $ WalletError "selectCollateralFunds: emptylist"
    l -> return l
  case collateralSupportedInEra (cardanoEra @ era) of
      Nothing -> throwE $ WalletError $ "selectCollateralFunds: collateral: era not supported :" ++ show (cardanoEra @ era)
      Just p -> return (TxInsCollateral p $  map getFundTxIn collateralFunds, collateralFunds)

dumpToFile :: FilePath -> TxInMode CardanoMode -> ActionM ()
dumpToFile filePath tx = liftIO $ dumpToFileIO filePath tx

dumpToFileIO :: FilePath -> TxInMode CardanoMode -> IO ()
dumpToFileIO filePath tx = appendFile filePath ('\n' : show tx)

initWallet :: WalletName -> ActionM ()
initWallet name = liftIO Wallet.initWallet >>= setName name

interpretPayMode :: forall era. IsShelleyBasedEra era => PayMode -> ActionM (CreateAndStore IO era, String)
interpretPayMode payMode = do
  networkId <- getUser TNetworkId
  case payMode of
    PayToAddr keyName destWallet -> do
      fundKey <- getName keyName
      walletRef <- getName destWallet
      return ( createAndStore (mkUTxOVariant networkId fundKey) (mkWalletFundStore walletRef)
             , Text.unpack $ serialiseAddress $ Utils.keyAddress @ era networkId fundKey)
    PayToScript scriptSpec destWallet -> do
      walletRef <- getName destWallet
      (witness, script, scriptData, _scriptFee) <- makePlutusContext scriptSpec
      return ( createAndStore (mkUTxOScript networkId (script, scriptData) witness) (mkWalletFundStore walletRef)
               , Text.unpack $ serialiseAddress $ makeShelleyAddress networkId (PaymentCredentialByScript $ hashScript script) NoStakeAddress )

{-
Use a binary search to find a loop counter that maxes out the available per transaction Plutus budget.
It is intended to be used with the the loop script from cardano-node/plutus-examples/...
loopScriptFile is the FilePath to the Plutus script that implements the delay loop. (for example in /nix/store/).
spendAutoScript relies on a particular calling convention of the loop script.
-}

spendAutoScript ::
     ProtocolParameters
  -> Script PlutusScriptV1
  -> ActionM (ScriptData, ScriptRedeemer)
spendAutoScript protocolParameters script = do
  perTxBudget <- case protocolParamMaxTxExUnits protocolParameters of
    Nothing -> liftTxGenError $ TxGenError "Cannot determine protocolParamMaxTxExUnits"
    Just b -> return b
  traceDebug $ "Plutus auto mode : Available budget per TX: " ++ show perTxBudget

  let
    budget = ExecutionUnits
                 (executionSteps perTxBudget `div`  2) -- TODO FIX
                 (executionMemory perTxBudget `div` 2)
  traceDebug $ "Plutus auto mode : Available budget per script run: " ++ show budget

  let
    isInLimits :: Integer -> Either String Bool
    isInLimits n = case preExecuteScript protocolParameters script (ScriptDataNumber 0) (toLoopArgument n) of
      Left err -> Left err
      Right use -> Right $ (executionSteps use <= executionSteps budget) && (executionMemory use <= executionMemory budget)
    searchUpperBound = 100000 -- The highest loop count that is tried. (This is about 50 times the current mainnet limit.)
  redeemer <- case startSearch isInLimits 0 searchUpperBound of
    Left err -> liftTxGenError $ TxGenError $ "cannot find fitting redeemer :" ++ err
    Right n -> return $ toLoopArgument n
  return (ScriptDataNumber 0, redeemer)
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

makePlutusContext :: forall era. IsShelleyBasedEra era
  => ScriptSpec
  -> ActionM (Witness WitCtxTxIn era, Script PlutusScriptV1, ScriptData, Lovelace)
makePlutusContext scriptSpec = do
  protocolParameters <- getProtocolParameters
  script <- liftIO $ PlutusExample.readScript $ scriptSpecFile scriptSpec

  executionUnitPrices <- case protocolParamPrices protocolParameters of
    Just x -> return x
    Nothing -> throwE $ WalletError "unexpected protocolParamPrices == Nothing in runPlutusBenchmark"

  perTxBudget <- case protocolParamMaxTxExUnits protocolParameters of
    Nothing -> liftTxGenError $ TxGenError "Cannot determine protocolParamMaxTxExUnits"
    Just b -> return b
  traceDebug $ "Plutus auto mode : Available budget per TX: " ++ show perTxBudget

  (scriptData, scriptRedeemer, executionUnits) <- case scriptSpecBudget scriptSpec of
    StaticScriptBudget sdata redeemer units -> return (sdata, redeemer, units)
    CheckScriptBudget sdata redeemer unitsWant -> do
      unitsPreRun <- preExecuteScriptAction protocolParameters script sdata redeemer
      if unitsWant == unitsPreRun
         then return (sdata, redeemer, unitsWant )
         else throwE $ WalletError $ concat [
              " Stated execution Units do not match result of pre execution. "
            , " Stated value : ", show unitsWant
            , " PreExecution result : ", show unitsPreRun
            ]
    AutoScript -> do
      (sdata, redeemer) <- spendAutoScript protocolParameters script
      preRun <- preExecuteScriptAction protocolParameters script sdata redeemer
      return (sdata, redeemer, preRun)

  let msg = mconcat [ "Plutus Benchmark :"
                    , " Script: ", scriptSpecFile scriptSpec
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

    PlutusScript PlutusScriptV1 script' = script
    scriptWitness :: ScriptWitness WitCtxTxIn era
    scriptWitness = case scriptLanguageSupportedInEra (cardanoEra @ era) (PlutusScriptLanguage PlutusScriptV1) of
      Nothing -> error $ "runPlutusBenchmark: Plutus V1 scriptlanguage not supported : in era" ++ show (cardanoEra @ era)
      Just scriptLang -> PlutusScriptWitness
                          scriptLang
                          PlutusScriptV1
                          (PScript script')
                          (ScriptDatumForTxIn scriptData)
                          scriptRedeemer
                          executionUnits

  return (ScriptWitness ScriptWitnessForSpending scriptWitness, script, scriptData, scriptFee)

preExecuteScriptAction ::
     ProtocolParameters
  -> Script PlutusScriptV1
  -> ScriptData
  -> ScriptData
  -> ActionM ExecutionUnits
preExecuteScriptAction protocolParameters script scriptData redeemer
  = case preExecuteScript protocolParameters script scriptData redeemer of
      Left err -> throwE $ WalletError ( "makePlutusContext preExecuteScript failed : " ++ show err )
      Right costs -> return costs

traceTxGeneratorVersion :: ActionM ()
traceTxGeneratorVersion = traceBenchTxSubmit TraceTxGeneratorVersion Version.txGeneratorVersion

{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
reserved :: [String] -> ActionM ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"
