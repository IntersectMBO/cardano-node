{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-error=partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-error=overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-error=unused-imports #-}
{-# OPTIONS_GHC -Wno-error=unused-local-binds #-}
{-# OPTIONS_GHC -Wno-error=unused-pattern-binds #-}

module Cardano.Benchmarking.Script.Core
where

import           Cardano.Api
import           Cardano.Api.Shelley (GovernanceAction (..), LedgerProtocolParameters (..),
                   PlutusScriptOrReferenceInput (..), Proposal (..), ProtocolParameters,
                   ShelleyLedgerEra, Vote (..), VotingProcedure (..),
                   VotingProcedures (..), convertToLedgerProtocolParameters,
                   createProposalProcedure, createVotingProcedure,
                   protocolParamMaxTxExUnits, protocolParamPrices, toShelleyNetwork)

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
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Logging hiding (LocalSocket)
import           Cardano.TxGenerator.Fund as Fund
import qualified Cardano.TxGenerator.FundQueue as FundQueue
import qualified Cardano.TxGenerator.Genesis as Genesis
import           Cardano.TxGenerator.PlutusContext
import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Setup.Plutus as Plutus
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Tx
import           Cardano.TxGenerator.Types
import qualified Cardano.TxGenerator.Utils as Utils
import           Cardano.TxGenerator.UTxO
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import           Prelude hiding (maybe)
import qualified Prelude

import           Control.Concurrent (threadDelay)
import           Control.Monad
import qualified Control.Monad.Random as Random (getRandom)
import           Control.Monad.Trans.RWS.Strict (ask)
import           "contra-tracer" Control.Tracer (Tracer (..))
import           Data.Bitraversable (bimapM)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import           Data.Constraint (type (&))
import           Data.Either.Extra (eitherToMaybe)
import           Data.Functor ((<&>))
import           Data.IntervalMap.Interval as IM (Interval (..), upperBound)
import           Data.IntervalMap.Lazy as IM (adjust, containing, delete, insert, null, toList)
import           Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Ratio ((%))
import           Data.Sequence as Seq (ViewL (..), fromList, viewl, (|>))
import qualified Data.Text as Text (Text, unpack)
import           System.FilePath ((</>))

import           Streaming
import qualified Streaming.Prelude as Streaming

type LiftCoreAction x = forall era . ()
  => IsShelleyBasedEra era
  => AsType era
  -> ExceptT TxGenError IO x

liftCoreWithEra
  :: forall x . ()
  => AnyCardanoEra
  -> LiftCoreAction x
  -> ActionM (Either TxGenError x)
liftCoreWithEra era coreCall = withEra era $ liftIO . runExceptT . coreCall

type WithEraAction x = forall era . ()
  => IsShelleyBasedEra era
  => AsType era
  -> ActionM x

withEra :: forall x . ()
  => AnyCardanoEra
  -> WithEraAction x
  -> ActionM x
withEra anyEra act
  | AnyCardanoEra ConwayEra  <- anyEra = act AsConwayEra
  | AnyCardanoEra BabbageEra <- anyEra = act AsBabbageEra
  | AnyCardanoEra AlonzoEra  <- anyEra = act AsAlonzoEra
  | AnyCardanoEra MaryEra    <- anyEra = act AsMaryEra
  | AnyCardanoEra AllegraEra <- anyEra = act AsAllegraEra
  | AnyCardanoEra ShelleyEra <- anyEra = act AsShelleyEra
  | AnyCardanoEra ByronEra   <- anyEra = error "Byron not supported"

type EraCryptoCon :: Type -> Constraint
type EraCryptoCon era =
  Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto

type WithEraCryptoActionCon :: Type -> Constraint
type WithEraCryptoActionCon era =
  IsShelleyBasedEra era & EraCryptoCon era

type WithEraCryptoAction :: Type -> Type
type WithEraCryptoAction x = forall era . ()
  => WithEraCryptoActionCon era
  => AsType era
  -> ActionM x

withEraCryptoCon :: forall x . ()
  => AnyCardanoEra
  -> WithEraCryptoAction x
  -> ActionM x
withEraCryptoCon anyEra act
  | AnyCardanoEra ConwayEra  <- anyEra = act AsConwayEra
  | AnyCardanoEra BabbageEra <- anyEra = act AsBabbageEra
  | AnyCardanoEra AlonzoEra  <- anyEra = act AsAlonzoEra
  | AnyCardanoEra MaryEra    <- anyEra = act AsMaryEra
  | AnyCardanoEra AllegraEra <- anyEra = act AsAllegraEra
  | AnyCardanoEra ShelleyEra <- anyEra = act AsShelleyEra
  | AnyCardanoEra ByronEra   <- anyEra = error "Byron not supported"

setProtocolParameters :: ProtocolParametersSource -> ActionM ()
setProtocolParameters = \case
  QueryLocalNode -> do
    setProtoParamMode ProtocolParameterQuery
  UseLocalProtocolFile file -> do
    protocolParameters <- liftIO $ readProtocolParametersFile file
    setProtoParamMode $ ProtocolParameterLocal protocolParameters

readSigningKey :: String -> SigningKeyFile In -> ActionM ()
readSigningKey name filePath =
  setEnvKeys name =<< liftIOSafe (readSigningKeyFile filePath)

defineSigningKey :: String -> SigningKey PaymentKey -> ActionM ()
defineSigningKey = setEnvKeys

readDRepKeys :: FilePath -> ActionM ()
readDRepKeys ncFile = do
  genesis <- onNothing throwKeyErr $ getGenesisDirectory <$> liftIOSafe (mkNodeConfig ncFile)
  -- "cache-entry" is a link or copy of the actual genesis folder created by "create-testnet-data"
  -- in the workbench's run directory structure, this link or copy is created for each run - by workbench
  ks <- liftIOSafe . Genesis.genesisLoadDRepKeys $ genesis </> "cache-entry"
  setEnvDRepKeys ks
  traceDebug $ "DRep SigningKeys loaded: " ++ show (length ks) ++ " from: " ++ genesis
  where
    throwKeyErr = liftTxGenError . TxGenError $
      "readDRepKeys: no genesisDirectory could "
        <> "be retrieved from the node config"

-- This should be almost entirely analogous to readDRepKeys.
readStakeKeys :: FilePath -> ActionM ()
readStakeKeys ncFile = do
  genesis <- onNothing throwKeyErr $ getGenesisDirectory <$> liftIOSafe (mkNodeConfig ncFile)
  ks <- liftIOSafe . Genesis.genesisLoadStakeKeys $ genesis </> "cache-entry"
  setEnvStakeKeys ks
  traceDebug $ "Stake SigningKeys loaded: " ++ show (length ks) ++ " from: " ++ genesis
  where
    throwKeyErr = liftTxGenError . TxGenError $
      "readStakeKeys: no genesisDirectory could "
        <> "be retrieved from the node config"

addFund :: AnyCardanoEra -> String -> TxIn -> L.Coin -> String -> ActionM ()
addFund era wallet txIn lovelace keyName = do
  fundKey  <- getEnvKeys keyName
  let
    mkOutValue :: forall era. IsShelleyBasedEra era => AsType era -> ActionM (InAnyCardanoEra TxOutValue)
    mkOutValue _ = return $ InAnyCardanoEra (cardanoEra @era) (lovelaceToTxOutValue (shelleyBasedEra @era) lovelace)
  outValue <- withEra era mkOutValue
  addFundToWallet wallet txIn outValue fundKey

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

queryEra :: ActionM AnyCardanoEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- getLocalChainTip localNodeConnectInfo
  mapExceptT liftIO .
    modifyError (Env.TxGenError . TxGenError . show) $
      queryNodeLocalState localNodeConnectInfo (SpecificPoint $ chainTipToChainPoint chainTip) QueryCurrentEra

queryRemoteProtocolParameters :: ActionM ProtocolParameters
queryRemoteProtocolParameters = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  era <- queryEra
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
  case era of
    AnyCardanoEra ByronEra   -> liftTxGenError $ TxGenError "queryRemoteProtocolParameters Byron not supported"
    AnyCardanoEra ShelleyEra -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraShelley QueryProtocolParameters
    AnyCardanoEra AllegraEra -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraAllegra QueryProtocolParameters
    AnyCardanoEra MaryEra    -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraMary    QueryProtocolParameters
    AnyCardanoEra AlonzoEra  -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    AnyCardanoEra BabbageEra -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraBabbage QueryProtocolParameters
    AnyCardanoEra ConwayEra  -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters

getProtocolParameters :: ActionM ProtocolParameters
getProtocolParameters = do
  getProtoParamMode  >>= \case
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

-- The fixity of Prelude.maybe is 9.
infixl 7 `maybe`
maybe :: b -> (a -> b) -> Maybe a -> b
maybe = Prelude.maybe

toMetadata :: forall era. IsShelleyBasedEra era => Maybe Int -> TxMetadataInEra era
toMetadata = TxMetadataNone `maybe` either error id . mkMetadata

submitAction
  :: AnyCardanoEra
  -> SubmitMode
  -> Generator
  -> TxGenTxParams
  -> ActionM ()
submitAction anyEra submitMode generator txParams =
  withEraCryptoCon anyEra $ submitInEra submitMode generator txParams

submitInEra :: forall era. ()
  => IsShelleyBasedEra era
  => Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
  => SubmitMode
  -> Generator
  -> TxGenTxParams
  -> AsType era
  -> ActionM ()
submitInEra submitMode generator txParams era = do
  txStream <- evalGenerator generator txParams era
  case submitMode of
    NodeToNode _ -> error "NodeToNode deprecated: ToDo: remove"
    Benchmark nodes tpsRate txCount -> benchmarkTxStream txStream nodes tpsRate txCount era
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

data GovCaseEnv era = GovCaseEnv
  { gcEnvEra          :: AsType era
  , gcEnvFeeInEra     :: TxFee era
  , gcEnvTxParams     :: TxGenTxParams
  , gcEnvLedgerParams :: LedgerProtocolParameters era }

data ProposeCase = ProposeCase
  { pcWalletName   :: String
  , pcPayMode      :: PayMode
  , pcCoin         :: L.Coin
  , pcGenStakeCred :: GeneratorStakeCredential
  , pcAnchor       :: Ledger.Anchor Ledger.StandardCrypto
  } deriving Eq

proposeCase :: forall era . ()
  => IsShelleyBasedEra era
  => GovCaseEnv era
  -> ProposeCase
  -> ActionM (TxStream IO era)
proposeCase GovCaseEnv {..} ProposeCase {..}
  | GeneratorStakeCredential {..} <- pcGenStakeCred
  , TxGenTxParams {..} <- gcEnvTxParams
  = do network :: Ledger.Network <- toShelleyNetwork <$> getEnvNetworkId
       maybeLedgerPParams :: Maybe (Ledger.PParams (ShelleyLedgerEra era))
         <- eitherToMaybe . toLedgerPParams sbe <$> getProtocolParameters
       wallet <- getEnvWallets pcWalletName
       (toUTxO, _addressOut) <- interpretPayMode pcPayMode
       let txGenerator :: TxGenerator era
           txGenerator = genTxProposal sbe gcEnvLedgerParams noCollateral gcEnvFeeInEra (unProposal, Nothing) TxMetadataNone
           mangledUTxOs :: CreateAndStoreList IO era [L.Coin]
           mangledUTxOs = mangle $ repeat toUTxO
           fundSource :: FundSource IO
           fundSource = walletSource wallet 1
           inToOut :: [L.Coin] -> [L.Coin]
           inToOut = Utils.inputsToOutputsWithFee txParamFee 1
           sourceToStore :: IO (Either TxGenError (Tx era))
           sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut mangledUTxOs
           govAction :: GovernanceAction era
           govAction = TreasuryWithdrawal [(network, unGeneratorStakeCredential, pcCoin)] SNothing
           Proposal {..} = createProposalProcedure sbe network pcCoin unGeneratorStakeCredential govAction pcAnchor
       fundPreview :: [Fund] <- liftIO $ walletPreview wallet 0
       let sourcePreviewAction = sourceTransactionPreview txGenerator fundPreview inToOut mangledUTxOs
       handleE handlePreviewErr do
         txPreview <- hoistActionEither sourcePreviewAction
         let txFeeEstimate = maybeLedgerPParams <&> \ledgerPParams ->
               evaluateTransactionFee sbe ledgerPParams (getTxBody txPreview) 1 0 0
         traceDebug $ "Projected Tx size in bytes: " <> show (txSizeInBytes txPreview)
         traceDebug $ "Projected Tx fee in Coin: " <> show txFeeEstimate
       pure . Streaming.effect $ Streaming.yield <$> sourceToStore
  where
    handlePreviewErr :: Env.Error -> ActionM ()
    handlePreviewErr err = traceDebug $ "Error creating Tx preview: " <> show err
    sbe :: ShelleyBasedEra era
    sbe = shelleyBasedEra
    noCollateral :: (TxInsCollateral era, [Fund])
    noCollateral = (TxInsCollateralNone, [])

data VoteCase = VoteCase
  { vcWalletName  :: String
  , vcPayMode     :: PayMode
  , vcVote        :: Vote -- yesOrNo
  , vcGenDRepCred :: GeneratorDRepCredential
  -- anchor can likely be assumed Nothing at all times
  , vcAnchor      :: Maybe (Ledger.Url, Text.Text) }

voteCase :: forall era ledgerEra . ()
  => IsShelleyBasedEra era
  => IsConwayBasedEra era
  => ledgerEra ~ ShelleyLedgerEra era
  => Ledger.EraCrypto ledgerEra ~ Ledger.StandardCrypto
  => GovCaseEnv era
  -> VoteCase
  -> ActionM (TxStream IO era)
voteCase GovCaseEnv {..} VoteCase {..}
  | GeneratorDRepCredential (drepGenCred :: GeneratorDRepCredentialBody Ledger.StandardCrypto)
      <- vcGenDRepCred
  , TxGenTxParams {..} <- gcEnvTxParams
  = do maybeLedgerPParams :: Maybe (Ledger.PParams ledgerEra)
         <- eitherToMaybe . toLedgerPParams sbe <$> getProtocolParameters
       wallet <- getEnvWallets vcWalletName
       (toUTxO, _addressOut) <- interpretPayMode vcPayMode
       let txGenerator :: TxGenerator era
           txGenerator = genTxVoting sbe gcEnvLedgerParams noCollateral gcEnvFeeInEra (vote, Nothing) TxMetadataNone
           fundSource :: FundSource IO
           fundSource = walletSource wallet 1
           mangledUTxOs :: CreateAndStoreList IO era [L.Coin]
           mangledUTxOs = mangle $ repeat toUTxO
           inToOut :: [L.Coin] -> [L.Coin]
           inToOut = Utils.inputsToOutputsWithFee txParamFee 1
           sourceToStore :: IO (Either TxGenError (Tx era))
           sourceToStore = sourceToStoreTransactionNew txGenerator fundSource inToOut mangledUTxOs
           VotingProcedure {..} = createVotingProcedure cbe vcVote vcAnchor
           govActionId :: Ledger.GovActionId crypto
           govActionId = undefined
           voter :: Ledger.Voter Ledger.StandardCrypto
           voter = Ledger.DRepVoter drepGenCred
           vote :: VotingProcedures era
           vote = VotingProcedures . Ledger.VotingProcedures $
                    Map.fromList [(voter, Map.fromList [(govActionId, unVotingProcedure)])]
       fundPreview :: [Fund] <- liftIO $ walletPreview wallet 0
       let sourcePreviewAction = sourceTransactionPreview txGenerator fundPreview inToOut mangledUTxOs
       handleE handlePreviewErr do
         txPreview <- hoistActionEither sourcePreviewAction
         let txFeeEstimate = maybeLedgerPParams <&> \ledgerPParams ->
               evaluateTransactionFee sbe ledgerPParams (getTxBody txPreview) 1 0 0
         traceDebug $ "Projected Tx size in bytes: " <> show (txSizeInBytes txPreview)
         traceDebug $ "Projected Tx fee in Coin: " <> show txFeeEstimate
       pure . Streaming.effect $ Streaming.yield <$> sourceToStore
  where
    handlePreviewErr :: Env.Error -> ActionM ()
    handlePreviewErr err = traceDebug $ "Error creating Tx preview: " <> show err
    sbe :: ShelleyBasedEra era
    sbe = shelleyBasedEra
    cbe :: ConwayEraOnwards era
    cbe = conwayBasedEra
    noCollateral :: (TxInsCollateral era, [Fund])
    noCollateral = (TxInsCollateralNone, [])

evalGenerator :: forall era . ()
  => IsShelleyBasedEra era
  => Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
  => Generator
  -> TxGenTxParams
  -> AsType era
  -> ActionM (TxStream IO era)
evalGenerator generator txParams@TxGenTxParams{txParamFee = fee} era = do
  networkId <- getEnvNetworkId
  protocolParameters <- getProtocolParameters
  -- Hmm? hoistActionM seems rather apt here.
  case convertToLedgerProtocolParameters shelleyBasedEra protocolParameters of
    Left err -> throwE (Env.TxGenError (ApiError err))
    Right ledgerParameters ->
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
                txFeeEstimate = case toLedgerPParams shelleyBasedEra protocolParameters of
                  Left{}              -> Nothing
                  Right ledgerPParams -> Just $
                    evaluateTransactionFee shelleyBasedEra ledgerPParams (getTxBody tx) (fromIntegral $ inputs + 1) 0 0    -- 1 key witness per tx input + 1 collateral
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

        Propose pcWalletName pcPayMode pcCoin pcGenStakeCred pcAnchor
          | args <- ProposeCase {..}
          , env  <- GovCaseEnv
                      { gcEnvEra = era
                      , gcEnvTxParams = txParams
                      , gcEnvLedgerParams = ledgerParameters
                      , gcEnvFeeInEra = feeInEra }
          , ce   <- cardanoEra :: CardanoEra era
          , toThrow <- TxGenError $ "Proposal governance action unsupported "
                                    <> "in era and/or protocol version."
          -> forEraInEon ce (liftTxGenError toThrow) \case
               ConwayEraOnwardsConway -> proposeCase env args

        Vote vcWalletName vcPayMode vcVote vcGenDRepCred vcAnchor
          | args <- VoteCase {..}
          , env  <- GovCaseEnv
                      { gcEnvEra = era
                      , gcEnvTxParams = txParams
                      , gcEnvLedgerParams = ledgerParameters
                      , gcEnvFeeInEra = feeInEra }
          , ce   <- cardanoEra :: CardanoEra era
          , toThrow <- TxGenError $ "Voting governance action unsupported "
                                    <> "in era and/or protocol version."
          -> forEraInEon ce (liftTxGenError toThrow) \case
               ConwayEraOnwardsConway -> voteCase env args

        Sequence l -> do
          gList <- forM l $ \g -> evalGenerator g txParams era
          return $ Streaming.for (Streaming.each gList) id

        Cycle g -> Streaming.cycle <$> evalGenerator g txParams era

        Take count g -> Streaming.take count <$> evalGenerator g txParams era

        RoundRobin l -> do
          l' <- forM l evalGenerator'
          pure . Streaming.effect . rrHelper $ Seq.fromList l' where
            rrHelper q = case Seq.viewl q of
              EmptyL -> pure mempty
              h :< t -> do
                consMaybe <- Streaming.uncons h
                case consMaybe of
                  Nothing -> rrHelper t
                  Just (x, h') -> (Streaming.yield x >>) <$>
                                           rrHelper (t |> h')

        OneOf []         -> pure mempty
        OneOf [(g, _)]   -> evalGenerator g txParams era
        OneOf ss@(_:_:_) -> do
          ss' <- forM ss $ evalGenerator' `bimapM` (pure . (/ sum ps))
          randHelper . buildTree $ buildList ss'
          where
            randHelper t = do
              r <- liftIO Random.getRandom
              case IM.toList $ t `IM.containing` r of
                [] | IM.null t -> pure mempty
                   | otherwise -> liftTxGenError $
                                    TxGenError "unexpected empty IntervalMap"
                (i, g) : _ -> do
                  gMaybe <- liftIO $ Streaming.uncons g
                  case gMaybe of
                    Nothing -> randHelper $ IM.delete i t
                    Just (x, g') ->
                      (Streaming.yield x >>) <$>
                        randHelper (IM.adjust (const g') i t)
            (_gs, ps) = unzip ss
            buildTree = foldr (uncurry $ flip IM.insert) mempty
            buildList [] = []
            buildList ((o, x) : xs) = scanl scanStep (o, ClosedInterval 0 x) xs
            scanStep (_o, i) (o', x) = (o', IntervalOC a b)
              where
                a = IM.upperBound i
                b = a + x

        EmptyStream -> return mempty
  where
    feeInEra = Utils.mkTxFee fee
    evalGenerator' = uncurry3 evalGenerator . (, txParams, era)

selectCollateralFunds :: forall era. IsShelleyBasedEra era
  => Maybe String
  -> ActionM (TxInsCollateral era, [FundQueue.Fund])
selectCollateralFunds Nothing = return (TxInsCollateralNone, [])
selectCollateralFunds (Just walletName) = do
  cw <- getEnvWallets walletName
  collateralFunds <- liftIO ( askWalletRef cw FundQueue.toList ) >>= \case
    [] -> throwE $ WalletError "selectCollateralFunds: emptylist"
    l -> return l
  case forEraMaybeEon (cardanoEra @era) of
      Nothing -> throwE $ WalletError $ "selectCollateralFunds: collateral: era not supported :" ++ show (cardanoEra @era)
      Just p -> return (TxInsCollateral p $  map getFundTxIn collateralFunds, collateralFunds)

dumpToFile :: FilePath -> TxInMode -> ActionM ()
dumpToFile filePath tx = liftIO $ dumpToFileIO filePath tx

dumpToFileIO :: FilePath -> TxInMode -> IO ()
dumpToFileIO filePath tx = appendFile filePath ('\n' : show tx)

initWallet :: String -> ActionM ()
initWallet name = liftIO Wallet.initWallet >>= setEnvWallets name

interpretPayMode :: forall era. IsShelleyBasedEra era => PayMode -> ActionM (CreateAndStore IO era, String)
interpretPayMode payMode = do
  networkId <- getEnvNetworkId
  case payMode of
    PayToAddr keyName destWallet -> do
      fundKey <- getEnvKeys keyName
      walletRef <- getEnvWallets destWallet
      return ( createAndStore (mkUTxOVariant networkId fundKey) (mkWalletFundStore walletRef)
             , Text.unpack $ serialiseAddress $ Utils.keyAddress @era networkId fundKey)
    PayToScript scriptSpec destWallet -> do
      walletRef <- getEnvWallets destWallet
      (witness, script, scriptData, _scriptFee) <- makePlutusContext scriptSpec
      case script of
        ScriptInAnyLang _ script' ->
          return ( createAndStore (mkUTxOScript networkId (script, scriptData) witness) (mkWalletFundStore walletRef)
                 , Text.unpack $ serialiseAddress $ makeShelleyAddress networkId (PaymentCredentialByScript $ hashScript script') NoStakeAddress )

makePlutusContext :: forall era. IsShelleyBasedEra era
  => ScriptSpec
  -> ActionM (Witness WitCtxTxIn era, ScriptInAnyLang, ScriptData, L.Coin)
makePlutusContext ScriptSpec{..} = do
  protocolParameters <- getProtocolParameters
  script <- liftIOSafe $ Plutus.readPlutusScript scriptSpecFile

  executionUnitPrices <- case protocolParamPrices protocolParameters of
    Just x -> return x
    Nothing -> throwE $ WalletError "unexpected protocolParamPrices == Nothing in runPlutusBenchmark"

  perTxBudget <- case protocolParamMaxTxExUnits protocolParameters of
    Nothing -> liftTxGenError $ TxGenError "Cannot determine protocolParamMaxTxExUnits"
    Just b -> return b
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
        scriptWitness :: ScriptWitness WitCtxTxIn era
        scriptWitness = case scriptLanguageSupportedInEra (shelleyBasedEra @era) lang of
          Nothing -> error $ "runPlutusBenchmark: " ++ show version ++ " not supported in era: " ++ show (cardanoEra @era)
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
