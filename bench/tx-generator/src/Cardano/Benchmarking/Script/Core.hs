{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Benchmarking.Script.Core
where

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScriptOrReferenceInput (..), ShelleyLedgerEra (..),
                   fromAlonzoPrices, fromAlonzoExUnits)

import           Cardano.Benchmarking.GeneratorTx as GeneratorTx (AsyncBenchmarkControl)
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx (waitBenchmark, walletBenchmark)
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient,
                   benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import           Cardano.Benchmarking.LogTypes as Core (AsyncBenchmarkControl (..),
                   TraceBenchTxSubmit (..), btConnect_, btN2N_, btSubmission2_, btTxSubmit_)
import           Cardano.Benchmarking.OuroborosImports as Core (LocalSubmitTx, SigningKeyFile,
                   makeLocalConnectInfo, protocolToCodecConfig)
import           Cardano.Benchmarking.Script.Aeson (prettyPrintOrdered, readProtocolParametersFile)
import           Cardano.Benchmarking.Script.Env hiding (Error (TxGenError))
import qualified Cardano.Benchmarking.Script.Env as Env (Error (TxGenError))
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Types as Core (SubmissionErrorPolicy (..))
import           Cardano.Benchmarking.Version as Version
import           Cardano.Benchmarking.Wallet as Wallet
import qualified Cardano.Ledger.Alonzo.Core as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Plutus as LP
import           Cardano.Logging hiding (LocalSocket)
import           Cardano.TxGenerator.Fund as Fund
import qualified Cardano.TxGenerator.FundQueue as FundQueue
import qualified Cardano.TxGenerator.Genesis as Genesis
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.Setup.Plutus as Plutus
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Tx
import           Cardano.TxGenerator.Types
import qualified Cardano.TxGenerator.Utils as Utils
import           Cardano.TxGenerator.UTxO
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.Trans.RWS.Strict (ask)
import           "contra-tracer" Control.Tracer (Tracer (..))
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import           Data.Ratio ((%))
import qualified Data.Text as Text (unpack)
import           Lens.Micro

import           Streaming
import qualified Streaming.Prelude as Streaming

setProtocolParameters :: ProtocolParametersSource -> ActionM ()
setProtocolParameters s = case s of
  QueryLocalNode -> do
    setProtoParamMode ProtocolParameterQuery
  UseLocalProtocolFile file -> do
    protocolParameters <- liftIO $ readProtocolParametersFile file
    setProtoParamMode $ ProtocolParameterLocal protocolParameters

readSigningKey :: String -> SigningKeyFile In -> ActionM ()
readSigningKey name filePath =
  liftIO (readSigningKeyFile filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setEnvKeys name key

defineSigningKey :: String -> SigningKey PaymentKey -> ActionM ()
defineSigningKey = setEnvKeys

addFund :: ShelleyBasedEra era -> String -> TxIn -> L.Coin -> String -> ActionM ()
addFund sbe wallet txIn lovelace keyName = do
  fundKey  <- getEnvKeys keyName
  let era = toCardanoEra sbe
      outValue = inAnyCardanoEra era $ lovelaceToTxOutValue sbe lovelace
  shelleyBasedEraConstraints sbe $ addFundToWallet wallet txIn outValue fundKey

addFundToWallet :: String -> TxIn -> InAnyCardanoEra TxOutValue -> SigningKey PaymentKey -> ActionM ()
addFundToWallet wallet txIn outVal skey = do
  walletRef <- getEnvWallets wallet
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
delay t = liftIO $ threadDelay $ floor $ 1_000_000 * t

waitBenchmarkCore :: AsyncBenchmarkControl ->  ActionM ()
waitBenchmarkCore ctl = do
  tracers  <- getBenchTracers
  _ <- liftIO $ runExceptT $ GeneratorTx.waitBenchmark (btTxSubmit_ tracers) ctl
  return ()

getConnectClient :: ActionM ConnectClient
getConnectClient = do
  tracers  <- getBenchTracers
  (Testnet networkMagic) <- getEnvNetworkId
  protocol <- getEnvProtocol
  void $ return $ btSubmission2_ tracers
  envConsts <- lift ask
  return $ benchmarkConnectTxSubmit
                       envConsts
                       (Tracer $ traceWith (btConnect_ tracers))
                       mempty -- (btSubmission2_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic
waitBenchmark :: ActionM ()
waitBenchmark = do
  abcMaybe <- getEnvThreads
  case abcMaybe of
    Just abc -> waitBenchmarkCore abc
    Nothing  -> do
      throwE . Env.TxGenError . TxGenError $
        ("waitBenchmark: missing AsyncBenchmarkControl" :: String)

cancelBenchmark :: ActionM ()
cancelBenchmark = do
  Just abc@AsyncBenchmarkControl { .. } <- getEnvThreads
  liftIO abcShutdown
  waitBenchmarkCore abc

getLocalConnectInfo :: ActionM LocalNodeConnectInfo
getLocalConnectInfo = makeLocalConnectInfo <$> getEnvNetworkId <*> getEnvSocketPath

queryEra :: ActionM AnyShelleyBasedEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- getLocalChainTip localNodeConnectInfo
  AnyCardanoEra era <- mapExceptT liftIO .
    modifyError (Env.TxGenError . TxGenError . show) $
      queryNodeLocalState localNodeConnectInfo (SpecificPoint $ chainTipToChainPoint chainTip) QueryCurrentEra
  caseByronOrShelleyBasedEra
    (liftTxGenError $ TxGenError "queryEra Byron not supported")
    (return . AnyShelleyBasedEra)
    era

queryRemoteProtocolParameters :: ()
  => L.AlonzoEraPParams era
  => ShelleyBasedEra era
  -> ActionM (Ledger.PParams (ShelleyLedgerEra era))
queryRemoteProtocolParameters sbe = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  let query = QueryInShelleyBasedEra sbe QueryProtocolParameters
  pp <- liftEither . first (Env.TxGenError . TxGenError . show) =<< mapExceptT liftIO (modifyError (Env.TxGenError . TxGenError . show) $
      queryNodeLocalState localNodeConnectInfo (SpecificPoint $ chainTipToChainPoint chainTip) (QueryInEra query))
  let pparamsFile = "protocol-parameters-queried.json"
  liftIO $ BSL.writeFile pparamsFile $ prettyPrintOrdered pp
  traceDebug $ "queryRemoteProtocolParameters : query result saved in: " ++ pparamsFile
  return pp

getProtocolParameters :: ()
  => L.AlonzoEraPParams era
  => ShelleyBasedEra era
  -> ActionM (Ledger.PParams (ShelleyLedgerEra era))
getProtocolParameters sbe = do
  getProtoParamMode  >>= \case
    ProtocolParameterQuery -> queryRemoteProtocolParameters sbe
    ProtocolParameterLocal parameters -> return $ AnyPParams parameters

waitForEra :: AnyShelleyBasedEra -> ActionM ()
waitForEra era = do
  AnyShelleyBasedEra currentSbe <- queryEra
  if AnyCardanoEra (toCardanoEra currentSbe) == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentSbe ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era

localSubmitTx :: TxInMode -> ActionM (SubmitResult TxValidationErrorInCardanoMode)
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

toMetadata :: ShelleyBasedEra era -> Maybe Int -> TxMetadataInEra era
toMetadata _ Nothing = TxMetadataNone
toMetadata sbe (Just payloadSize) = case mkMetadata sbe payloadSize of
  Right m -> m
  Left err -> error err

submitAction :: ShelleyBasedEra sbe -> SubmitMode -> Generator -> TxGenTxParams -> ActionM ()
submitAction sbe submitMode generator txParams = do
  txStream <- evalGenerator sbe generator txParams
  case submitMode of
    NodeToNode _ -> error "NodeToNode deprecated: ToDo: remove"
    Benchmark nodes tpsRate txCount -> benchmarkTxStream sbe txStream nodes tpsRate txCount
    LocalSocket -> shelleyBasedEraConstraints sbe $ submitAll (void . localSubmitTx . TxInMode sbe) txStream
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

benchmarkTxStream :: ()
  => ShelleyBasedEra era
  -> TxStream IO era
  -> TargetNodes
  -> TPSRate
  -> NumberOfTxs
  -> ActionM ()
benchmarkTxStream sbe txStream targetNodes tps txCount = do
  tracers  <- getBenchTracers
  connectClient <- getConnectClient
  ret <- liftIO $ runExceptT $ GeneratorTx.walletBenchmark sbe (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               targetNodes tps LogErrors txCount txStream
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> setEnvThreads ctl

evalGenerator :: ShelleyBasedEra era -> Generator -> TxGenTxParams -> ActionM (TxStream IO era)
evalGenerator sbe generator txParams@TxGenTxParams{txParamFee = fee} = do
  networkId <- getEnvNetworkId
  AnyPParams ledgerParameters <- getProtocolParameters
  case generator of
    SecureGenesis wallet genesisKeyName destKeyName -> do
      genesis  <- getEnvGenesis
      destKey  <- getEnvKeys destKeyName
      destWallet  <- getEnvWallets wallet
      genesisKey  <- getEnvKeys genesisKeyName
      (tx, fund) <- firstExceptT Env.TxGenError $ hoistEither $
        Genesis.genesisSecureInitialFund networkId genesis genesisKey destKey txParams
      let
        gen = do
          walletRefInsertFund destWallet fund
          return $ Right tx
      return $ Streaming.effect (Streaming.yield <$> gen)

    -- 'Split' combines regular payments and payments for change.
    -- There are lists of payments buried in the 'PayWithChange'
    -- type conditionally sent back by 'Utils.includeChange', to
    -- then be used while partially applied as the @valueSplitter@
    -- in 'sourceToStoreTransactionNew'.
    Split walletName payMode payModeChange coins -> do
      wallet <- getEnvWallets walletName
      (toUTxO, addressOut) <- interpretPayMode payMode
      traceDebug $ "split output address : " ++ addressOut
      (toUTxOChange, addressChange) <- interpretPayMode payModeChange
      traceDebug $ "split change address : " ++ addressChange
      let
        fundSource = walletSource wallet 1
        inToOut = Utils.includeChange fee coins
        txGenerator = genTx shelleyBasedEra ledgerParameters (TxInsCollateralNone, []) feeInEra TxMetadataNone
        sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut $ mangleWithChange toUTxOChange toUTxO
      return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    -- The 'SplitN' case's call chain is somewhat elaborate.
    -- The division is done in 'Utils.inputsToOutputsWithFee'
    -- but things are threaded through
    -- 'Cardano.Benchmarking.Wallet.mangle' and packed into
    -- the transaction assembled by 'sourceToStoreTransactionNew'.
    SplitN walletName payMode count -> do
      wallet <- getEnvWallets walletName
      (toUTxO, addressOut) <- interpretPayMode payMode
      traceDebug $ "SplitN output address : " ++ addressOut
      let
        fundSource = walletSource wallet 1
        inToOut = Utils.inputsToOutputsWithFee fee count
        txGenerator = genTx shelleyBasedEra ledgerParameters (TxInsCollateralNone, []) feeInEra TxMetadataNone
        sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut (mangle $ repeat toUTxO)
      return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    NtoM walletName payMode inputs outputs metadataSize collateralWallet -> do
      wallet <- getEnvWallets walletName
      collaterals <- selectCollateralFunds collateralWallet
      (toUTxO, addressOut) <- interpretPayMode payMode
      traceDebug $ "NtoM output address : " ++ addressOut
      let
        fundSource = walletSource wallet inputs
        inToOut = Utils.inputsToOutputsWithFee fee outputs
        txGenerator = genTx shelleyBasedEra ledgerParameters collaterals feeInEra (toMetadata metadataSize)
        sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut (mangle $ repeat toUTxO)

      fundPreview <- liftIO $ walletPreview wallet inputs
      case sourceTransactionPreview txGenerator fundPreview inToOut (mangle $ repeat toUTxO) of
        Left err -> traceDebug $ "Error creating Tx preview: " ++ show err
        Right tx -> do
          let
            txSize = txSizeInBytes tx
            txFeeEstimate = Just $
                evaluateTransactionFee shelleyBasedEra ledgerParameters (getTxBody tx) (fromIntegral $ inputs + 1) 0 0    -- 1 key witness per tx input + 1 collateral
          traceDebug $ "Projected Tx size in bytes: " ++ show txSize
          traceDebug $ "Projected Tx fee in Coin: " ++ show txFeeEstimate
          -- TODO: possibly emit a warning when (Just txFeeEstimate) is lower than specified by config in TxGenTxParams.txFee
          summary_ <- getEnvSummary
          forM_ summary_ $ \summary -> do
            let summary' = summary { projectedTxSize = Just txSize, projectedTxFee = txFeeEstimate }
            setEnvSummary summary'
            traceBenchTxSubmit TraceBenchPlutusBudgetSummary summary'
          dumpBudgetSummaryIfExisting

      return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    Sequence l -> do
      gList <- forM l $ \g -> evalGenerator g txParams era
      return $ Streaming.for (Streaming.each gList) id

    Cycle g -> Streaming.cycle <$> evalGenerator g txParams era

    Take count g -> Streaming.take count <$> evalGenerator g txParams era

    RoundRobin l -> do
      _gList <- forM l $ \g -> evalGenerator g txParams era
      error "return $ foldr1 Streaming.interleaves gList"

    OneOf _l -> error "todo: implement Quickcheck style oneOf generator"

  where
    feeInEra = Utils.mkTxFee sbe fee

selectCollateralFunds :: ()
  => ShelleyBasedEra era
  -> Maybe String
  -> ActionM (TxInsCollateral era, [FundQueue.Fund])
selectCollateralFunds _ Nothing = return (TxInsCollateralNone, [])
selectCollateralFunds sbe (Just walletName) = do
  cw <- getEnvWallets walletName
  collateralFunds <- liftIO ( askWalletRef cw FundQueue.toList ) >>= \case
    [] -> throwE $ WalletError "selectCollateralFunds: emptylist"
    l -> return l
  case forShelleyBasedEraMaybeEon sbe of
      Nothing -> throwE $ WalletError $ "selectCollateralFunds: collateral: era not supported :" ++ show sbe
      Just p -> return (TxInsCollateral p $  map getFundTxIn collateralFunds, collateralFunds)

dumpToFile :: FilePath -> TxInMode -> ActionM ()
dumpToFile filePath tx = liftIO $ dumpToFileIO filePath tx

dumpToFileIO :: FilePath -> TxInMode -> IO ()
dumpToFileIO filePath tx = appendFile filePath ('\n' : show tx)

initWallet :: String -> ActionM ()
initWallet name = liftIO Wallet.initWallet >>= setEnvWallets name

interpretPayMode :: ShelleyBasedEra era -> PayMode -> ActionM (CreateAndStore IO era, String)
interpretPayMode sbe payMode = do
  networkId <- getEnvNetworkId
  shelleyBasedEraConstraints sbe $ case payMode of
    PayToAddr keyName destWallet -> do
      fundKey <- getEnvKeys keyName
      walletRef <- getEnvWallets destWallet
      return ( createAndStore (mkUTxOVariant sbe networkId fundKey) (mkWalletFundStore walletRef)
             , Text.unpack $ serialiseAddress $ Utils.keyAddress sbe networkId fundKey)
    PayToScript scriptSpec destWallet -> do
      walletRef <- getEnvWallets destWallet
      (witness, script, scriptData, _scriptFee) <- makePlutusContext sbe scriptSpec
      case script of
        ScriptInAnyLang _ script' ->
          return ( createAndStore (mkUTxOScript sbe networkId (script, scriptData) witness) (mkWalletFundStore walletRef)
                 , Text.unpack $ serialiseAddress $ makeShelleyAddress networkId (PaymentCredentialByScript $ hashScript script') NoStakeAddress )

makePlutusContext :: ShelleyBasedEra era
  -> ScriptSpec
  -> ActionM (Witness WitCtxTxIn era, ScriptInAnyLang, ScriptData, L.Coin)
makePlutusContext sbe ScriptSpec{..} = do
  AnyPParams protocolParameters <- getProtocolParameters
  script <- liftIOSafe $ Plutus.readPlutusScript scriptSpecFile

  let executionUnitPrices = fromAlonzoPrices $ protocolParameters ^. L.ppPricesL
      perTxBudget = fromAlonzoExUnits $ protocolParameters ^. L.ppMaxTxExUnitsL
  traceDebug $ "Plutus auto mode : Available budget per TX: " ++ show perTxBudget

  (scriptData, scriptRedeemer, executionUnits) <- case scriptSpecBudget of
    StaticScriptBudget sDataFile redeemerFile units withCheck -> do
      sData <- liftIOSafe (readScriptData sDataFile)
      redeemer <- liftIOSafe (readScriptData redeemerFile)
      when withCheck $ do
        unitsPreRun <- preExecuteScriptAction protocolParameters script (getScriptData sData) (getScriptData redeemer)
        unless (units == unitsPreRun) $
          throwE $ WalletError $ concat [
              " Stated execution Units do not match result of pre execution. "
            , " Stated value : ", show units
            , " PreExecution result : ", show unitsPreRun
            ]
      return (sData, redeemer, units)

    AutoScript redeemerFile txInputs -> do
      redeemer <- liftIOSafe $ readScriptData redeemerFile
      let
        strategy = case scriptSpecPlutusType of
          LimitTxPerBlock_8 -> TargetTxsPerBlock 8
          _                 -> TargetTxExpenditure

        -- reflects properties hard-coded into the loop scripts for benchmarking:
        -- 1. script datum is not used
        -- 2. the loop terminates at 1_000_000 when counting down
        -- 3. the loop's initial value is the first numerical value in the redeemer argument structure
        autoBudget = PlutusAutoBudget
          { autoBudgetUnits = perTxBudget
          , autoBudgetDatum = ScriptDataNumber 0
          , autoBudgetRedeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) (getScriptData redeemer)
          }
      traceDebug $ "Plutus auto mode : Available budget per Tx: " ++ show perTxBudget
                   ++ " -- split between inputs per Tx: " ++ show txInputs

      case plutusAutoScaleBlockfit protocolParameters (either ("builtin: "++) ("plutus file: "++) scriptSpecFile) script autoBudget strategy txInputs of
        Left err -> liftTxGenError err
        Right (summary, PlutusAutoBudget{..}, preRun) -> do
          setEnvSummary summary
          dumpBudgetSummaryIfExisting
          return (unsafeHashableScriptData autoBudgetDatum, autoBudgetRedeemer, preRun)

  let msg = mconcat [ "Plutus Benchmark :"
                    , " Script: ", show scriptSpecFile
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

  case script of
    ScriptInAnyLang lang (PlutusScript version script') ->
      let
        scriptWitness = case scriptLanguageSupportedInEra sbe lang of
          Nothing -> error $ "runPlutusBenchmark: " ++ show version ++ " not supported in era: " ++ show sbe
          Just scriptLang -> PlutusScriptWitness
                              scriptLang
                              version
                              (PScript script')               -- TODO: add capability for reference inputs from Babbage era onwards
                              (ScriptDatumForTxIn $ Just scriptData)
                              scriptRedeemer
                              executionUnits
      in return (ScriptWitness ScriptWitnessForSpending scriptWitness, script, getScriptData scriptData, scriptFee)
    _ ->
      liftTxGenError $ TxGenError "runPlutusBenchmark: only Plutus scripts supported"

preExecuteScriptAction :: ()
  => L.AlonzoEraPParams era
  => Ledger.PParams era
  -> ScriptInAnyLang
  -> ScriptData
  -> ScriptData
  -> ActionM ExecutionUnits
preExecuteScriptAction protocolParameters script scriptData redeemer
  = case Plutus.preExecutePlutusScript protocolParameters script scriptData (unsafeHashableScriptData redeemer) of
      Left err -> throwE $ WalletError ( "makePlutusContext preExecuteScript failed: " ++ show err )
      Right costs -> return costs

dumpBudgetSummaryIfExisting :: ActionM ()
dumpBudgetSummaryIfExisting
  = do
    summary_ <- getEnvSummary
    forM_ summary_ $ \summary -> do
      liftIO $ BSL.writeFile summaryFile $ prettyPrintOrdered summary
      traceDebug $ "dumpBudgetSummaryIfExisting : budget summary created/updated in: " ++ summaryFile
  where
    summaryFile = "plutus-budget-summary.json"

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
