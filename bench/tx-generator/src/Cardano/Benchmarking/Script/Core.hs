{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Benchmarking.Script.Core
where

import           Cardano.Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp

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
import qualified Cardano.Benchmarking.Script.Ogmios as OgmiosBackend
import qualified Cardano.Benchmarking.Script.Submission as Submission
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Types as Core (SubmissionErrorPolicy (..))
import           Cardano.Benchmarking.Version as Version
import           Cardano.Benchmarking.Wallet as Wallet
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Api as LedgerApi (referenceInputsTxBodyL)
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Logging hiding (LocalSocket)
import           Cardano.TxGenerator.Fund as Fund
import qualified Cardano.TxGenerator.FundQueue as FundQueue
import qualified Cardano.TxGenerator.Genesis as Genesis
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.ProtocolParameters
import           Cardano.TxGenerator.ScriptVariants (plutusScriptSize, scriptVariants)
import           Cardano.TxGenerator.Setup.Plutus as Plutus
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Tx
import           Cardano.TxGenerator.Types
import qualified Cardano.TxGenerator.Utils as Utils
import           Cardano.TxGenerator.UTxO

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import           Control.Monad
import           Control.Monad.Trans.RWS.Strict (ask)
import qualified Data.ByteString as BS (hPut)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import           Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Ratio ((%))
import qualified Data.Text as Text (unpack)
import           Data.Type.Equality (TestEquality (..), (:~:) (..))
import           Lens.Micro ((^.))
import           System.IO (IOMode(..), withFile)

import           Streaming
import qualified Streaming.Prelude as Streaming

liftCoreWithEra :: AnyCardanoEra -> (forall era. Exp.EraCommonConstraints era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra era coreCall = withEra era ( liftIO . runExceptT . coreCall)

-- | Transactions are built with the experimental API ("Cardano.Api.Experimental"),
-- which supports only the current mainnet era and the next one.
withEra :: AnyCardanoEra -> (forall era. Exp.EraCommonConstraints era => AsType era -> ActionM x) -> ActionM x
withEra era action = do
  case era of
    AnyCardanoEra ConwayEra   -> action AsConwayEra
    AnyCardanoEra DijkstraEra -> action AsDijkstraEra
    AnyCardanoEra otherEra    -> throwE $ UserError $
      "tx-generator: era " ++ show otherEra ++ " is not supported, use Conway or Dijkstra"

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

addFund :: AnyCardanoEra -> String -> TxIn -> L.Coin -> String -> ActionM ()
addFund era wallet txIn lovelace keyName = do
  fundKey   <- getEnvKeys keyName
  walletRef <- getEnvWallets wallet
  let
    mkFund :: forall era. IsShelleyBasedEra era => AsType era -> ActionM FundQueue.Fund
    mkFund _ = return $ FundQueue.Fund $ InAnyCardanoEra (cardanoEra @era) FundInEra {
           _fundTxIn = txIn
         , _fundWitness = Exp.AnyKeyWitnessPlaceholder
         , _fundVal = lovelace
         , _fundSigningKey = Just fundKey
         }
  fund <- withEra era mkFund
  liftIO $ walletRefInsertFund walletRef fund

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
  codecConfig <- liftIO $ protocolToCodecConfig protocol
  return $ benchmarkConnectTxSubmit
                       envConsts
                       (mkTracer $ traceWith (btConnect_ tracers))
                       mempty -- (btSubmission2_ tracers)
                       codecConfig
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
  inEonForEra
    (liftTxGenError $ TxGenError "queryEra Byron not supported")
    (return . AnyShelleyBasedEra)
    era

queryRemoteProtocolParameters :: ActionM ProtocolParameters
queryRemoteProtocolParameters = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  AnyShelleyBasedEra sbe <- queryEra
  let
    callQuery :: forall era.
                 QueryInEra era (Ledger.PParams (ShelleyLedgerEra era))
              -> ActionM ProtocolParameters
    callQuery query@(QueryInShelleyBasedEra shelleyEra _) = do
      pp <- liftEither . first (Env.TxGenError . TxGenError . show) =<< mapExceptT liftIO (modifyError (Env.TxGenError . TxGenError . show) $
          queryNodeLocalState localNodeConnectInfo (SpecificPoint $ chainTipToChainPoint chainTip) (QueryInEra query))
      let pp' = fromLedgerPParams shelleyEra pp
          pparamsFile = "protocol-parameters-queried.json"
      liftIO $ BSL.writeFile pparamsFile $ prettyPrintOrdered pp'
      traceDebug $ "queryRemoteProtocolParameters : query result saved in: " ++ pparamsFile
      return pp'
  callQuery $ QueryInShelleyBasedEra sbe QueryProtocolParameters

getProtocolParameters :: ActionM ProtocolParameters
getProtocolParameters = do
  getProtoParamMode  >>= \case
    ProtocolParameterQuery -> queryRemoteProtocolParameters
    ProtocolParameterLocal parameters -> return parameters

waitForEra :: AnyShelleyBasedEra -> ActionM ()
waitForEra era = do
  currentEra <- queryEra
  if currentEra == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentEra ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era

localSubmitTx :: TxInMode -> ActionM TxSubmitResult
localSubmitTx tx = do
  submit <- getLocalSubmitTx
  ret <- liftIO $ submit tx
  case ret of
    TxSubmitSuccess -> return ret
    TxSubmitFail e -> do
      let msg = concat [ "local submit failed: " , show e , " (" , show tx , ")" ]
      traceDebug msg
      return ret
    TxSubmitError e -> do
      let msg = concat [ "local submit error: " , show e , " (" , show tx , ")" ]
      traceDebug msg
      return ret
--      throwE $ ApiError msg

-- TODO:
-- It should be possible to exit the tx-generator with an exception and also get the log messages.
-- Problem 1: When doing throwE $ ApiError msg logmessages get lost !
-- Problem 2: Workbench restarts the tx-generator -> this may be the reason for loss of messages

toMetadata :: Maybe Int -> TxMetadata
toMetadata Nothing = mempty
toMetadata (Just payloadSize) = case mkMetadata payloadSize of
  Right m -> m
  Left err -> error err

submitAction :: AnyCardanoEra -> SubmitMode -> Generator -> TxGenTxParams -> ActionM ()
submitAction era submitMode generator txParams = withEra era $ submitInEra submitMode generator txParams

submitInEra :: forall era. Exp.EraCommonConstraints era => SubmitMode -> Generator -> TxGenTxParams -> AsType era -> ActionM ()
submitInEra submitMode generator txParams era = do
  txStream <- evalGenerator generator txParams era
  case submitMode of
    Benchmark nodes tpsRate txCount -> benchmarkTxStream txStream nodes tpsRate txCount era
    LocalSocket -> submitAll (void . localSubmitTx . Utils.mkTxInModeCardano) txStream
    SubmitToEndpoint endpoint -> case endpoint of
      OgmiosEndpoint (EndpointUri uri) -> do
        tracers <- getBenchTracers
        Submission.runSubmitTransport
          (Submission.onRejectionFor generator)
          (OgmiosBackend.withOgmiosTransport (Submission.traceProgress tracers) uri)
          txStream
    DumpToFile filePath -> liftIO $
      withFile filePath AppendMode $ \hdl ->
        Streaming.toHandle hdl $ Streaming.map (either (error . show) show) txStream
    WriteCBORList filePath -> liftIO $
      withFile filePath AppendMode $ \hdl ->
        Streaming.mapM_ (either (error . show) (BS.hPut hdl . serialiseToCBOR)) txStream
    DiscardTX -> liftIO $ Streaming.mapM_ forceTx txStream
 where
  forceTx (Right tx) = let !_ = tx in return ()
  forceTx (Left err) = error $ show err

   -- INVESTIGATE: Can Streaming.run be used here?
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
  -> TPSRate
  -> NumberOfTxs
  -> AsType era
  -> ActionM ()
benchmarkTxStream txStream targetNodes tps txCount era = do
  tracers  <- getBenchTracers
  connectClient <- getConnectClient
  let
    coreCall :: AsType era -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall eraProxy = GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               targetNodes tps LogErrors eraProxy txCount txStream
  ret <- liftIO $ runExceptT $ coreCall era
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> setEnvThreads ctl

evalGenerator :: forall era. Exp.EraCommonConstraints era => Generator -> TxGenTxParams -> AsType era -> ActionM (TxStream IO era)
evalGenerator generator txParams@TxGenTxParams{txParamFee = fee} era = do
  networkId <- getEnvNetworkId
  protocolParameters <- getProtocolParameters
  ledgerParameters <- case convertToLedgerProtocolParameters shelleyBasedEra protocolParameters of
    Left err -> throwE (Env.TxGenError (ApiError err))
    Right (LedgerProtocolParameters ledgerParameters) -> return ledgerParameters
  case generator of
    SecureGenesis wallet genesisKeyName destKeyName -> do
      genesis  <- getEnvGenesis
      genesisDir <- getEnvShelleyGenesisDir
      destKey  <- getEnvKeys destKeyName
      destWallet  <- getEnvWallets wallet
      genesisKey  <- getEnvKeys genesisKeyName
      eitherTxFund <- liftIO $ Genesis.genesisWithResolvedInitialFunds genesisDir genesis $ \resolved ->
        Genesis.genesisSecureInitialFund networkId resolved genesisKey destKey txParams
      (tx, fund) <- firstExceptT Env.TxGenError $ hoistEither eitherTxFund
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
      (outputs, addressOut) <- interpretPayMode payMode
      traceDebug $ "split output address : " ++ addressOut
      (changeOutputs, addressChange) <- interpretPayMode payModeChange
      traceDebug $ "split change address : " ++ addressChange
      let
        fundSource = walletSource wallet 1
        inToOut = Utils.includeChange fee coins
        txGenerator = genTx ledgerParameters ([], []) fee mempty
        sourceToStore = do
          toUTxOs <- outputs
          toUTxOChange <- firstOutput changeOutputs
          sourceToStoreTransactionNew txGenerator fundSource inToOut $ mangleWithChange toUTxOChange toUTxOs
      return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    -- The 'SplitN' case's call chain is somewhat elaborate.
    -- The division is done in 'Utils.inputsToOutputsWithFee'
    -- but things are threaded through
    -- 'Cardano.Benchmarking.Wallet.mangle' and packed into
    -- the transaction assembled by 'sourceToStoreTransactionNew'.
    SplitN walletName payMode count -> do
      wallet <- getEnvWallets walletName
      (outputs, addressOut) <- interpretPayMode payMode
      traceDebug $ "SplitN output address : " ++ addressOut
      let
        fundSource = walletSource wallet 1
        inToOut = Utils.inputsToOutputsWithFee fee count
        txGenerator = genTx ledgerParameters ([], []) fee mempty
        sourceToStore = outputs >>= sourceToStoreTransactionNew txGenerator fundSource inToOut . mangle
      return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    NtoM walletName payMode inputs outputs metadataSize collateralWallet -> do
      wallet <- getEnvWallets walletName
      collaterals <- selectCollateralFunds collateralWallet
      (payOutputs, addressOut) <- interpretPayMode payMode
      traceDebug $ "NtoM output address : " ++ addressOut
      let
        fundSource = walletSource wallet inputs
        inToOut = Utils.inputsToOutputsWithFee fee outputs
        txGenerator = genTx ledgerParameters collaterals fee (toMetadata metadataSize)
      tracers <- getBenchTracers
      let
        -- Which reference scripts each transaction uses, for checking that the
        -- transactions in a block use different ones.
        traceReferences = \case
          Right tx@(ShelleyTx _ ledgerTx) | references@(_ : _) <- txReferenceInputs tx ->
            traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubDebug $
              "NtoM: transaction " ++ Text.unpack (serialiseToRawBytesHexText $ fromShelleyTxId $ Ledger.txIdTx ledgerTx)
                ++ " references " ++ unwords (map (Text.unpack . renderTxIn) references)
          _ -> return ()
        sourceToStore = do
          result <- payOutputs >>= sourceToStoreTransactionNew txGenerator fundSource inToOut . mangle
          traceReferences result
          return result

      fundPreview <- liftIO $ walletPreview wallet inputs
      previewOutputs <- liftIO payOutputs
      refScriptsSize <- referenceScriptsSize $ nubOrd $ mapMaybe getFundReferenceInput fundPreview
      case sourceTransactionPreview txGenerator fundPreview inToOut (mangle previewOutputs) of
        Left err -> traceDebug $ "Error creating Tx preview: " ++ show err
        Right tx -> do
          let
            txSize = txSizeInBytes tx
            txFeeEstimate = Just $
                estimateTxFee ledgerParameters tx (fromIntegral $ inputs + 1) refScriptsSize   -- 1 key witness per tx input + 1 collateral
          traceDebug $ "Projected Tx size in bytes: " ++ show txSize
          traceDebug $ "Projected Tx fee in Coin: " ++ show txFeeEstimate
            ++ (if refScriptsSize > 0 then ", with " ++ show refScriptsSize ++ " bytes of reference scripts" else "")
          -- A transaction that references scripts pays for their size: a fee below the
          -- estimate would get every transaction rejected.
          when (refScriptsSize > 0 && txFeeEstimate > Just fee) $ liftTxGenError $ TxGenError $
            "NtoM: a transaction needs a fee of at least " ++ show txFeeEstimate
              ++ ", above the configured " ++ show fee
          -- TODO: possibly emit a warning when (Just txFeeEstimate) is lower than specified by config in TxGenTxParams.txFee
          summary_ <- getEnvSummary
          forM_ summary_ $ \summary -> do
            let summary' = summary { projectedTxSize = Just txSize, projectedTxFee = txFeeEstimate }
            setEnvSummary summary'
            traceBenchTxSubmit TraceBenchPlutusBudgetSummary summary'
          dumpBudgetSummaryIfExisting

      return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    NestedNtoM walletName payMode inputs outputs metadataSize collateralWallet subTxShape@SubTxShape{..} ->
      case testEquality (Exp.useEra @era) Exp.DijkstraEra of
        Nothing -> throwE $ UserError $
          "NestedNtoM: sub-transactions need the Dijkstra era, not " ++ show (cardanoEra @era)
        Just Refl -> do
          wallet <- getEnvWallets walletName
          collaterals <- selectCollateralFunds collateralWallet
          (payOutputs, addressOut) <- interpretPayMode payMode
          traceDebug $ "NestedNtoM output address : " ++ addressOut ++ ", " ++ show subTxShape
          let
            fundCount = inputs + subTxCount * subTxInputs
            fundSource = walletSource wallet fundCount
            topSplitter = Utils.inputsToOutputsWithFee fee outputs
            subTxSplitter = Utils.inputsToOutputsWithFee 0 subTxOutputs
            txGenerator = genNestedTx ledgerParameters collaterals fee (toMetadata metadataSize)
            sourceToStore = payOutputs >>= sourceToStoreNestedTransaction txGenerator fundSource inputs subTxInputs topSplitter subTxSplitter . mangle

          -- A transaction with many sub-transactions easily outgrows maxTxSize or the
          -- configured fee; check a preview, rather than having every transaction rejected.
          fundPreview <- liftIO $ walletPreview wallet fundCount
          toStore <- mangle <$> liftIO payOutputs
          if length fundPreview < fundCount
            then traceDebug $ "NestedNtoM: not enough funds for a Tx preview: " ++ show (length fundPreview)
            else case nestedTransactionPreview txGenerator fundPreview inputs subTxInputs topSplitter subTxSplitter toStore of
              Left err -> liftTxGenError err
              Right tx -> do
                let
                  txSize = txSizeInBytes tx
                  maxTxSize = fromIntegral $ ledgerParameters ^. Ledger.ppMaxTxSizeL
                  txFeeEstimate = estimateTxFee ledgerParameters tx (fromIntegral $ inputs + 1) 0   -- 1 key witness per top-level input + 1 collateral
                traceDebug $ "Projected Tx size in bytes: " ++ show txSize
                traceDebug $ "Projected Tx fee in Coin: " ++ show txFeeEstimate
                when (txSize > maxTxSize) $ liftTxGenError $ TxGenError $
                  "NestedNtoM: a transaction would be " ++ show txSize ++ " bytes, above maxTxSize "
                    ++ show maxTxSize ++ "; use fewer or smaller sub-transactions"
                when (txFeeEstimate > fee) $ liftTxGenError $ TxGenError $
                  "NestedNtoM: a transaction needs a fee of at least " ++ show txFeeEstimate
                    ++ ", above the configured " ++ show fee
                summary_ <- getEnvSummary
                forM_ summary_ $ \summary -> do
                  let summary' = summary { projectedTxSize = Just txSize, projectedTxFee = Just txFeeEstimate }
                  setEnvSummary summary'
                  traceBenchTxSubmit TraceBenchPlutusBudgetSummary summary'
                dumpBudgetSummaryIfExisting

          return $ Streaming.effect (Streaming.yield <$> sourceToStore)

    PublishScripts walletName setName keyName changePayMode -> do
      wallet <- getEnvWallets walletName
      ScriptSet{..} <- getEnvScriptSet setName
      refKey <- getEnvKeys keyName
      (changeOutputs, addressChange) <- interpretPayMode changePayMode
      let
        refAddress = Utils.keyAddress @era networkId refKey
        maxTxSize = fromIntegral $ ledgerParameters ^. Ledger.ppMaxTxSizeL
      traceDebug $ "PublishScripts " ++ setName ++ ": reference scripts at "
        ++ Text.unpack (serialiseAddress refAddress) ++ ", change to " ++ addressChange
      refOutputs <- forM (zip [0 ..] scriptSetVariants) $ \(k, variant) ->
        case mkReferenceScriptTxOut ledgerParameters refAddress variant of
          Left err  -> liftTxGenError err
          Right out -> return (k, out, fromMaybe 0 (plutusScriptSize variant))
      let
        -- As many outputs per transaction as fit, leaving room for an input,
        -- the change, the fee and a key witness.
        batches = packScripts (maxTxSize - 1_000) refOutputs
        publish batch = walletSource wallet 1 >>= \case
          Left err -> return $ Left err
          Right inFunds -> do
            toChange <- firstOutput changeOutputs
            case publishScriptsTx ledgerParameters fee inFunds [ out | (_, out, _) <- batch ] toChange of
              Left err -> return $ Left err
              Right (tx, publishedId, storeChange) -> do
                storeChange publishedId
                modifyMVar_ scriptSetReferences $ \refs -> return $ Map.union refs $
                  Map.fromList [ (k, TxIn publishedId (TxIx ix)) | ((k, _, _), ix) <- zip batch [0 ..] ]
                return $ Right tx
      traceDebug $ "PublishScripts " ++ setName ++ ": " ++ show (length scriptSetVariants)
        ++ " scripts in " ++ show (length batches) ++ " transactions"
      return $ Streaming.for (Streaming.each batches) $ \batch ->
        Streaming.effect (Streaming.yield <$> publish batch)

    Sequence l -> do
      gList <- forM l $ \g -> evalGenerator g txParams era
      return $ Streaming.for (Streaming.each gList) id

    Cycle g -> Streaming.cycle <$> evalGenerator g txParams era

    Take count g -> Streaming.take count <$> evalGenerator g txParams era

    RoundRobin l -> do
      _gList <- forM l $ \g -> evalGenerator g txParams era
      error "return $ foldr1 Streaming.interleaves gList"

    OneOf _l -> error "todo: implement Quickcheck style oneOf generator"

selectCollateralFunds ::
     Maybe String
  -> ActionM ([TxIn], [FundQueue.Fund])
selectCollateralFunds Nothing = return ([], [])
selectCollateralFunds (Just walletName) = do
  cw <- getEnvWallets walletName
  collateralFunds <- liftIO ( askWalletRef cw FundQueue.toList ) >>= \case
    [] -> throwE $ WalletError "selectCollateralFunds: emptylist"
    l -> return l
  return (map getFundTxIn collateralFunds, collateralFunds)

initWallet :: String -> ActionM ()
initWallet name = liftIO Wallet.initWallet >>= setEnvWallets name

-- | The outputs a 'PayMode' creates, for one transaction at a time: an infinite
-- list of creators, of which a transaction uses one per output. A pay mode that
-- cycles through script variants continues where the previous transaction
-- left off.
type PayModeOutputs era = IO [CreateAndStore IO era]

interpretPayMode :: forall era. Exp.EraCommonConstraints era => PayMode -> ActionM (PayModeOutputs era, String)
interpretPayMode payMode = do
  networkId <- getEnvNetworkId
  case payMode of
    PayToAddr keyName destWallet -> do
      fundKey <- getEnvKeys keyName
      walletRef <- getEnvWallets destWallet
      return ( pure $ repeat $ createAndStore (mkUTxOVariant networkId fundKey) (mkWalletFundStore walletRef)
             , Text.unpack $ serialiseAddress $ Utils.keyAddress @era networkId fundKey)
    PayToScript scriptSpec destWallet -> do
      walletRef <- getEnvWallets destWallet
      (witness, script, scriptData, _scriptFee) <- makePlutusContext scriptSpec
      return ( pure $ repeat $ createAndStore (mkUTxOScript networkId (script, scriptData) witness) (mkWalletFundStore walletRef)
             , scriptAddress networkId script )
    PayToScriptSet ScriptSetPayment{..} destWallet -> do
      walletRef <- getEnvWallets destWallet
      ScriptSet{..} <- getEnvScriptSet scriptSetName
      base <- maybe (throwE $ UserError $ "PayToScriptSet: script set " ++ scriptSetName ++ " is empty") return $
        listToMaybe scriptSetVariants
      when (scriptSetGroupSize < 1) $
        throwE $ UserError $ "PayToScriptSet: the group size must be at least 1, not " ++ show scriptSetGroupSize
      -- All variants cost the same; see "Cardano.TxGenerator.ScriptVariants".
      (scriptData, scriptRedeemer, executionUnits, _scriptFee) <-
        resolvePlutusBudget ("script set " ++ scriptSetName) base scriptSetBudget scriptSetPlutusType
      references <- liftIO $ readMVar scriptSetReferences
      variants <- forM (zip [0 ..] scriptSetVariants) $ \(k, variant) -> do
        source <- case scriptSetSource of
          ScriptInWitness -> return Nothing
          ScriptByReference -> case Map.lookup k references of
            Just txIn -> return $ Just txIn
            Nothing -> throwE $ UserError $
              "PayToScriptSet: variant " ++ show k ++ " of " ++ scriptSetName ++ " has not been published"
        forM_ source $ \txIn ->
          traceDebug $ "PayToScriptSet " ++ scriptSetName ++ ": variant " ++ show k ++ " by reference to "
            ++ Text.unpack (renderTxIn txIn)
        witness <- mkPlutusSpendingWitness @era variant source scriptData scriptRedeemer executionUnits
        return (variant, witness)
      outputs <- liftIO $ cyclingOutputs scriptSetGroupSize
        [ mkUTxOScript networkId (variant, getScriptData scriptData) witness | (variant, witness) <- variants ]
        (mkWalletFundStore walletRef)
      return ( outputs
             , scriptAddress networkId base ++ " and the addresses of " ++ show (length variants - 1)
                 ++ " more variants of " ++ scriptSetName )
 where
  scriptAddress networkId (ScriptInAnyLang _ script) =
    Text.unpack $ serialiseAddress $ makeShelleyAddress networkId (PaymentCredentialByScript $ hashScript script) NoStakeAddress

-- | The first output creator of a 'PayMode', for a single output such as change.
firstOutput :: PayModeOutputs era -> IO (CreateAndStore IO era)
firstOutput outputs = outputs >>= \case
  toUTxO : _ -> return toUTxO
  []         -> fail "firstOutput: a pay mode created no outputs"

-- | 'defineScriptSet' builds the variants of a script and keeps them in the
-- 'Env' under a name.
defineScriptSet :: String -> ScriptSetSpec -> ActionM ()
defineScriptSet name ScriptSetSpec{..} = do
  (base, resolvedTo) <- liftIOSafe $ Plutus.readPlutusScript scriptSetBase
  variants <- either liftTxGenError return $ scriptVariants scriptSetSize scriptSetCount base
  references <- liftIO $ newMVar Map.empty
  setEnvScriptSet name $ ScriptSet variants references
  traceDebug $ concat
    [ "DefineScriptSet ", name, ": ", show scriptSetCount, " variants of ", show resolvedTo, ", "
    , maybe "?" show (listToMaybe variants >>= plutusScriptSize), " bytes each" ]

-- | The reference inputs of a transaction.
txReferenceInputs :: Exp.EraCommonConstraints era => Tx era -> [TxIn]
txReferenceInputs (ShelleyTx _ ledgerTx) =
  map fromShelleyTxIn $ Set.toList $ ledgerTx ^. Ledger.bodyTxL . LedgerApi.referenceInputsTxBodyL

-- | The total size of the reference scripts in these reference outputs, as
-- far as script sets know them.
referenceScriptsSize :: [TxIn] -> ActionM Int
referenceScriptsSize referenced = do
  sets <- getEnvScriptSets
  sizes <- fmap Map.unions $ forM (Map.elems sets) $ \ScriptSet{..} -> do
    references <- liftIO $ readMVar scriptSetReferences
    return $ Map.fromList
      [ (txIn, size)
      | (k, txIn) <- Map.toList references
      , Just size <- [plutusScriptSize =<< listToMaybe (drop k scriptSetVariants)] ]
  return $ sum $ mapMaybe (`Map.lookup` sizes) referenced

-- | Packs reference script outputs into transactions, in order, each up to
-- @budget@ bytes of scripts, and at least one per transaction.
packScripts :: Int -> [(Int, out, Int)] -> [[(Int, out, Int)]]
packScripts budget = go
 where
  go [] = []
  go outs = let (batch, rest) = takeBatch 0 outs in batch : go rest
  takeBatch _ [] = ([], [])
  takeBatch used (o@(_, _, size) : os)
    | used > 0 && used + size + 100 > budget = ([], o : os)
    | otherwise = let (batch, rest) = takeBatch (used + size + 100) os in (o : batch, rest)

-- | A transaction that spends @inFunds@ into reference script outputs and
-- change, with the fee it needs, which is estimated on a first build. Returns
-- the transaction, its id and the action that stores the change.
publishScriptsTx :: forall era. Exp.EraCommonConstraints era
  => Ledger.PParams (ShelleyLedgerEra era)
  -> L.Coin
  -> [Fund]
  -> [Exp.TxOut (ShelleyLedgerEra era)]
  -> CreateAndStore IO era
  -> Either TxGenError (Tx era, TxId, TxId -> IO ())
publishScriptsTx ledgerParameters feeGuess inFunds refOutputs toChange = do
  (draft, _, _) <- build feeGuess
  -- The final change differs from the draft's by a few bytes at most.
  build $ estimateTxFee ledgerParameters draft (fromIntegral $ length inFunds) 0 <> L.Coin 1_000
 where
  deposits = mconcat [ out ^. Ledger.coinTxOutL | Exp.TxOut out <- refOutputs ]
  available = mconcat $ map getFundCoin inFunds
  build fee' = do
    let change = L.Coin $ L.unCoin available - L.unCoin deposits - L.unCoin fee'
    when (change < L.Coin 0) $ Left $ TxGenError $
      "PublishScripts: a fund of " ++ show available ++ " does not cover deposits of "
        ++ show deposits ++ " and a fee of " ++ show fee'
    let (changeOut, storeChange) = toChange change
    (tx, publishedId) <- genTx ledgerParameters ([], []) fee' mempty inFunds (refOutputs ++ [changeOut])
    return (tx, publishedId, storeChange (TxIx $ fromIntegral $ length refOutputs))

makePlutusContext :: forall era. Exp.EraCommonConstraints era
  => ScriptSpec
  -> ActionM (Exp.AnyWitness (ShelleyLedgerEra era), ScriptInAnyLang, ScriptData, L.Coin)
makePlutusContext ScriptSpec{..} = do
  (script, _resolvedTo) <- liftIOSafe $ Plutus.readPlutusScript scriptSpecFile
  (scriptData, scriptRedeemer, executionUnits, scriptFee) <-
    resolvePlutusBudget (show scriptSpecFile) script scriptSpecBudget scriptSpecPlutusType
  witness <- mkPlutusSpendingWitness @era script Nothing scriptData scriptRedeemer executionUnits
  return (witness, script, getScriptData scriptData, scriptFee)

-- | 'resolvePlutusBudget' finds the datum, redeemer and execution units for
-- spending outputs locked by @script@, and the fee for those units, either as
-- given or calibrated by pre-executing the script.
resolvePlutusBudget ::
     String
  -> ScriptInAnyLang
  -> ScriptBudget
  -> TxGenPlutusType
  -> ActionM (HashableScriptData, HashableScriptData, ExecutionUnits, L.Coin)
resolvePlutusBudget scriptLabel script scriptBudget plutusType = do
  protocolParameters <- getProtocolParameters

  executionUnitPrices <- case protocolParamPrices protocolParameters of
    Just x -> return x
    Nothing -> throwE $ WalletError "unexpected protocolParamPrices == Nothing in runPlutusBenchmark"

  perTxBudget <- case protocolParamMaxTxExUnits protocolParameters of
    Nothing -> liftTxGenError $ TxGenError "Cannot determine protocolParamMaxTxExUnits"
    Just b -> return b
  traceDebug $ "Plutus auto mode : Available budget per TX: " ++ show perTxBudget

  (scriptData, scriptRedeemer, executionUnits) <- case scriptBudget of
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
        strategy = case plutusType of
          LimitTxPerBlock_8 -> TargetTxsPerBlock 8
          LimitTxPerBlock_4 -> TargetTxsPerBlock 4
          _                 -> TargetTxExpenditure

        -- reflects properties hard-coded into the loop scripts for benchmarking:
        -- 1. script datum is not used
        -- 2. the loop terminates at 1_000_000 when counting down
        -- 3. the loop's initial value is the first numerical value in the redeemer argument structure
        autoBudget = PlutusAutoBudget
          { autoBudgetUnits = perTxBudget
          , autoBudgetDatum = ScriptDataNumber 0
          , autoBudgetRedeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) (getScriptData redeemer)
          , autoBudgetUpperBoundHint = Nothing
          }
        scriptInfo = (scriptLabel, show strategy)
      traceDebug $ "Plutus auto mode : Available budget per Tx: " ++ show perTxBudget
                   ++ " -- split between inputs per Tx: " ++ show txInputs

      case plutusAutoScaleBlockfit protocolParameters scriptInfo script autoBudget strategy txInputs of
        Left err -> liftTxGenError err
        Right (summary, PlutusAutoBudget{..}, preRun) -> do
          setEnvSummary summary
          dumpBudgetSummaryIfExisting
          return (unsafeHashableScriptData autoBudgetDatum, autoBudgetRedeemer, preRun)

  let msg = mconcat [ "Plutus Benchmark :"
                    , " Script: ", scriptLabel
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
  return (scriptData, scriptRedeemer, executionUnits, scriptFee)

-- | 'mkPlutusSpendingWitness' is 'Utils.plutusSpendingWitness' in 'ActionM'.
mkPlutusSpendingWitness :: forall era. Exp.EraCommonConstraints era
  => ScriptInAnyLang
  -> Maybe TxIn
  -> HashableScriptData
  -> HashableScriptData
  -> ExecutionUnits
  -> ActionM (Exp.AnyWitness (ShelleyLedgerEra era))
mkPlutusSpendingWitness script reference scriptData scriptRedeemer executionUnits =
  either liftTxGenError return $ Utils.plutusSpendingWitness script reference scriptData scriptRedeemer executionUnits

preExecuteScriptAction ::
     ProtocolParameters
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
    summary <- maybe "{}" prettyPrintOrdered <$> getEnvSummary
    liftIO $ BSL.writeFile summaryFile summary
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
