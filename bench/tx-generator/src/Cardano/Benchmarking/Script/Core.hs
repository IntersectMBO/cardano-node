{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Benchmarking.Script.Core
where

import           Cardano.Api
import qualified Cardano.Api.Shelley as Api (Vote (..))
import           Cardano.Api.Shelley (GovernanceAction (..), LedgerProtocolParameters (..),
                   PlutusScriptOrReferenceInput (..), Proposal (..), ProtocolParameters,
                   ShelleyLedgerEra, StakeCredential (..), VotingProcedure (..),
                   VotingProcedures (..), convertToLedgerProtocolParameters,
                   createProposalProcedure, createVotingProcedure, 
                   protocolParamMaxTxExUnits, protocolParamPrices,
                   singletonVotingProcedures, toShelleyNetwork, toShelleyTxId)

import           Cardano.Benchmarking.GeneratorTx as GeneratorTx (AsyncBenchmarkControl)
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx (waitBenchmark, walletBenchmark)
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient,
                   benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import           Cardano.Benchmarking.LogTypes as Core (AsyncBenchmarkControl (..),
                   TraceBenchTxSubmit (..), btConnect_, btN2N_, btSubmission2_, btTxSubmit_)
import           Cardano.Benchmarking.OuroborosImports as Core (LocalSubmitTx,
                   protocolToCodecConfig)
import           Cardano.Benchmarking.Script.Aeson (prettyPrintOrdered, readProtocolParametersFile)
import           Cardano.Benchmarking.Script.Env hiding (Error (TxGenError))
import qualified Cardano.Benchmarking.Script.Env as Env (Error (TxGenError))
import           Cardano.Benchmarking.Script.Queries
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Types as Core (SubmissionErrorPolicy (..))
import           Cardano.Benchmarking.Version as Version
import           Cardano.Benchmarking.Wallet as Wallet
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Coin as L
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

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.Extra (whenJust)
import qualified Control.Monad.Random as Random (getRandom)
import           Control.Monad.Trans.RWS.Strict (ask)
import           "contra-tracer" Control.Tracer (Tracer (..))
import           Data.Bitraversable (bimapM)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import           Data.Either.Extra (eitherToMaybe)
import           Data.IntervalMap.Interval as IM (Interval (..), upperBound)
import           Data.IntervalMap.Lazy as IM (adjust, containing, delete, insert, null, toList)
import           Data.Kind (Type)
import           Data.List.Extra ((!?))
import           Data.Maybe (fromJust)
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Ratio ((%))
import           Data.Sequence as Seq (ViewL (..), fromList, viewl, (|>))
import qualified Data.Text as Text (Text, unpack)
import           Data.Tuple.Extra (dupe)
import           System.FilePath ((</>))
import qualified System.IO as IO (IOMode (..), openFile)

import           Streaming
import qualified Streaming.Prelude as Streaming

liftCoreWithEra :: AnyCardanoEra -> (forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra era coreCall = withEra era ( liftIO . runExceptT . coreCall)

withEra :: AnyCardanoEra -> (forall era. IsShelleyBasedEra era => AsType era -> ActionM x) -> ActionM x
withEra era action = do
  case era of
    AnyCardanoEra ConwayEra  -> action AsConwayEra
    AnyCardanoEra BabbageEra -> action AsBabbageEra
    AnyCardanoEra AlonzoEra  -> action AsAlonzoEra
    AnyCardanoEra MaryEra    -> action AsMaryEra
    AnyCardanoEra AllegraEra -> action AsAllegraEra
    AnyCardanoEra ShelleyEra -> action AsShelleyEra
    AnyCardanoEra ByronEra   -> error "byron not supported"

type WithEraCryptoAction :: Type -> Type
type WithEraCryptoAction x = forall era ledgerEra . ()
  => ledgerEra ~ ShelleyLedgerEra era
  => IsShelleyBasedEra era
  => Ledger.EraCrypto ledgerEra ~ Ledger.StandardCrypto
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
setProtocolParameters s = case s of
  QueryLocalNode -> do
    setProtoParamMode ProtocolParameterQuery
  UseLocalProtocolFile file -> do
    protocolParameters <- liftIO $ readProtocolParametersFile file
    setProtoParamMode $ ProtocolParameterLocal protocolParameters

readSigningKey :: String -> SigningKeyFile In -> ActionM ()
readSigningKey name filePath =
  setEnvKeys name =<< liftIOSafe (readPaymentKeyFile filePath)

defineSigningKey :: String -> SigningKey PaymentKey -> ActionM ()
defineSigningKey = setEnvKeys

defineDRepCredential :: SigningKey DRepKey -> ActionM ()
defineDRepCredential = setEnvDRepKeys . (: [])

defineStakeCredential :: VerificationKey StakeKey -> ActionM ()
defineStakeCredential = setEnvStakeCredentials . (: []) . StakeCredentialByKey . verificationKeyHash

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
      "readDRepKeys: no genesisDirectory could be retrieved from the node config"

readStakeCredentials :: FilePath -> ActionM ()
readStakeCredentials ncFile = do
  genesis <- onNothing throwKeyErr $ getGenesisDirectory <$> liftIOSafe (mkNodeConfig ncFile)
  -- "cache-entry" is a link or copy of the actual genesis folder created by "create-testnet-data"
  -- in the workbench's run directory structure, this link or copy is created for each run - by workbench
  ks <- liftIOSafe $ Genesis.genesisLoadStakeKeys genesis
  setEnvStakeCredentials $ map (StakeCredentialByKey . verificationKeyHash) ks
  traceDebug $ "StakeCredentials loaded: " ++ show (length ks) ++ " from: " ++ genesis
  where
    throwKeyErr = liftTxGenError . TxGenError $
      "readStakeCredentials: no genesisDirectory could be retrieved from the node config"

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

toMetadata :: forall era. IsShelleyBasedEra era => Maybe Int -> TxMetadataInEra era
toMetadata Nothing = TxMetadataNone
toMetadata (Just payloadSize) = case mkMetadata payloadSize of
  Right m -> m
  Left err -> error err

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
    DumpToFile filePath -> liftIO do
      handle <- IO.openFile filePath IO.AppendMode
      Streaming.toHandle handle $ Streaming.map showTx txStream
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

noCollateral :: (TxInsCollateral era, [Fund])
noCollateral = (TxInsCollateralNone, [])

handlePreviewErr :: Env.Error -> ActionM ()
handlePreviewErr err = traceDebug $ "Error creating Tx preview: " <> show err

genTxProposal' :: forall era . ()
  => IsShelleyBasedEra era
  => LedgerProtocolParameters era
  -> TxFee era
  -> Ledger.ProposalProcedure (ShelleyLedgerEra era)
  -> [Fund]
  -> [TxOut CtxTx era]
  -> Either TxGenError (Tx era, TxId)
genTxProposal' lp fie pp =
  genTxProposal sbe lp noCollateral fie (pp, Nothing) TxMetadataNone where
    sbe = shelleyBasedEra

genTxVoting' :: forall era . ()
  => IsShelleyBasedEra era
  => LedgerProtocolParameters era
  -> TxFee era
  -> VotingProcedures era
  -> [Fund]
  -> [TxOut CtxTx era]
  -> Either TxGenError (Tx era, TxId)
genTxVoting' lp fie vote =
  genTxVoting sbe lp noCollateral fie (vote, Nothing) TxMetadataNone where
    sbe = shelleyBasedEra

data PreviewTxData era = PreviewTxData
  { ptxInputs       :: NumberOfInputsPerTx
  , ptxTxGen        :: TxGenerator era
  , ptxFund         :: [Fund]
  , ptxInToOut      :: [L.Coin] -> [L.Coin]
  , ptxMangledUTxOs :: CreateAndStoreList IO era [L.Coin]
  , ptxLedgerPParams :: Maybe (Ledger.PParams (ShelleyLedgerEra era)) }

previewTx :: forall era . ()
  => IsShelleyBasedEra era
  => PreviewTxData era
  -> ActionM (Int, Maybe L.Coin)
previewTx PreviewTxData {..}
  | inputs' <- fromIntegral $ ptxInputs + 1
  = do
      tx <- hoistActionEither $
        sourceTransactionPreview ptxTxGen ptxFund ptxInToOut ptxMangledUTxOs
      let size = txSizeInBytes tx
      -- 1 key witness per tx input + 1 collateral
          maybeFeeEstimate = txFeeHelper inputs' tx <$> ptxLedgerPParams
      traceDebug $ "Projected Tx size in bytes: " <> show size
      whenJust maybeFeeEstimate \feeEstimate ->
        traceDebug $ "Projected Tx fee in Coin: " <> show feeEstimate
      -- TODO: possibly emit a warning when (Just txFeeEstimate) is
      -- lower than specified by config in TxGenTxParams.txFee
      pure (size, maybeFeeEstimate)

txFeeHelper :: forall era ledgerEra . ()
  => IsShelleyBasedEra era
  => ledgerEra ~ ShelleyLedgerEra era
  => NumberOfInputsPerTx
  -> Tx era
  -> Ledger.PParams ledgerEra
  -> L.Coin
txFeeHelper inputs tx pparams
          | sbe <- shelleyBasedEra
          , body <- getTxBody tx
          , inputs' <- fromIntegral $ inputs + 1
          = evaluateTransactionFee sbe pparams body inputs' 0 0

summarizeTx :: Int -> Maybe L.Coin -> ActionM ()
summarizeTx size maybeFeeEstimate = do
  fmap updateSummary <$> getEnvSummary >>=
    mapM_ \summary -> do
      setEnvSummary summary
      traceBenchTxSubmit TraceBenchPlutusBudgetSummary summary
  dumpBudgetSummaryIfExisting
  where
    updateSummary :: PlutusBudgetSummary -> PlutusBudgetSummary
    updateSummary summary =
      summary { projectedTxSize = Just size
              , projectedTxFee = maybeFeeEstimate }

data GovCaseEnv era = GovCaseEnv
  { gcEnvEra          :: AsType era
  , gcEnvFeeInEra     :: TxFee era
  , gcEnvTxParams     :: TxGenTxParams
  , gcEnvLedgerParams :: LedgerProtocolParameters era }

data ProposeCase = ProposeCase
  { pcWalletName   :: String
  , pcPayMode      :: PayMode
  , pcCoin         :: L.Coin
  , pcStakeCredIdx :: Int
  -- ^ index into
  -- `Cardano.Benchmarking.Script.Env.envStakeCredentials`
  -- yielding a `StakeCredential`
  , pcAnchor       :: Ledger.Anchor Ledger.StandardCrypto
  } deriving Eq

infixl 7 <!?>
(<!?>) :: Functor f => f [t] -> Int -> f (Maybe t)
ellm <!?> n = fmap (!? n) ellm

proposeCase :: forall era ledgerEra . ()
  => IsShelleyBasedEra era
  => ledgerEra ~ ShelleyLedgerEra era
  => Ledger.EraCrypto ledgerEra ~ Ledger.StandardCrypto
  => GovCaseEnv era
  -> ProposeCase
  -> ConwayEraOnwards era
  -> ActionM (TxStream IO era)
proposeCase GovCaseEnv {..} ProposeCase {..} eon
  | TxGenTxParams {..} <- gcEnvTxParams
  , sbe <- shelleyBasedEra @era
  = conwayEraOnwardsConstraints eon do
       network :: Ledger.Network <- toShelleyNetwork <$> getEnvNetworkId
       ptxLedgerPParams :: Maybe (Ledger.PParams ledgerEra)
         <- eitherToMaybe . toLedgerPParams sbe <$> getProtocolParameters
       wallet <- getEnvWallets pcWalletName
       (toUTxO, _addressOut) <- interpretPayMode pcPayMode
       let ptxMangledUTxOs :: CreateAndStoreList IO era [L.Coin]
           ptxMangledUTxOs = mangle $ repeat toUTxO
           fundSource :: FundSource IO
           fundSource = walletSource wallet 1
           ptxInToOut :: [L.Coin] -> [L.Coin]
           ptxInToOut = Utils.inputsToOutputsWithFee txParamFee 1
       stakeCred :: StakeCredential
         <- fromJust <$> getEnvStakeCredentials <!?> pcStakeCredIdx
       let Proposal {..} = createProposalProcedure sbe network pcCoin stakeCred govAction pcAnchor
           ptxTxGen :: TxGenerator era
           ptxTxGen = genTxProposal' gcEnvLedgerParams gcEnvFeeInEra unProposal
           govAction :: GovernanceAction era
           govAction = TreasuryWithdrawal [(network, stakeCred, pcCoin)] SNothing
       ptxFund :: [Fund] <- liftIO $ walletPreview wallet 0
       handleE handlePreviewErr . void $ previewTx PreviewTxData
         { ptxInputs = 0, .. }
       eitherTx
         <- liftIO $ sourceToStoreTransactionNew ptxTxGen fundSource ptxInToOut ptxMangledUTxOs
       whenJust (eitherToMaybe eitherTx) \tx -> do
         let body = getTxBody tx
             txId' = getTxId body
         gs@GovStateSummary {..} <- getEnvGovSummary
         let GovernanceActionIds _ gaids = govProposals
             gaId = Ledger.GovActionId
               { gaidTxId = toShelleyTxId txId'
               , gaidGovActionIx = Ledger.GovActionIx 0 }
         setEnvGovSummary gs
           { govProposals = GovernanceActionIds sbe $ gaids ++ [gaId] }
       pure . Streaming.effect $ Streaming.yield <$> pure eitherTx

data VoteCase crypto = VoteCase
  { vcWalletName  :: String
  , vcPayMode     :: PayMode
  , vcGovActId    :: Int
  -- ^ index into envGovStateSummary
  --   It should retrieve something of type
  --   @Ledger.GovActionId Ledger.StandardCrypto@
  , vcVote        :: Api.Vote -- yesOrNo
  , vcDRepCredIdx :: Int
  -- ^ index into
  -- `Cardano.Benchmarking.Script.Env.envDRepCredentials`
  -- yielding a
  -- @Ledger.Credential 'Ledger.DRepRole Ledger.StandardCrypto@
  , vcAnchor      :: Maybe (Ledger.Url, Text.Text)
  -- ^ vcAnchor can likely be assumed Nothing at all times
  }

unsuppErr :: String -> ActionM t
unsuppErr s = liftTxGenError . TxGenError $
  s <> " governance action unsupported "
  <> "in era and/or protocol version."

voteCase :: forall era ledgerEra crypto . ()
  -- These equality constraints are for the sake of abbreviation.
  => ledgerEra ~ ShelleyLedgerEra era
  => crypto ~ Ledger.EraCrypto ledgerEra
  => crypto ~ Ledger.StandardCrypto
  => GovCaseEnv era
  -> VoteCase crypto
  -> ConwayEraOnwards era
  -> ActionM (TxStream IO era)
voteCase GovCaseEnv {..} VoteCase {..} eon
  | TxGenTxParams {..} <- gcEnvTxParams
  , VotingProcedure {..} <- createVotingProcedure eon vcVote vcAnchor
  , sbe <- conwayEraOnwardsToShelleyBasedEra eon
  , insufReadyProps :: Env.Error
      <- Env.TxGenError $ TxGenError "insufficient ready proposals"
  , ptxInToOut :: [L.Coin] -> [L.Coin]
      <- Utils.inputsToOutputsWithFee txParamFee 1
  = conwayEraOnwardsConstraints eon do
       DRepVerificationKey vkey
         <- getVerificationKey . fromJust <$>
              getEnvDRepKeys <!?> vcDRepCredIdx
       let voter :: Ledger.Voter crypto
           voter = Ledger.DRepVoter . Ledger.KeyHashObj $ Ledger.hashKey vkey
       ptxLedgerPParams :: Maybe (Ledger.PParams ledgerEra)
         <- eitherToMaybe . toLedgerPParams sbe <$> getProtocolParameters
       (wallet, fundSource)
         <- second (flip walletSource 1) . dupe <$> getEnvWallets vcWalletName
       (toUTxO, _addressOut) :: (CreateAndStore IO era, String)
         <- interpretPayMode vcPayMode
       ptxFund :: [Fund] <- liftIO $ walletPreview wallet 0
       GovStateSummary { govProposals = GovernanceActionIds _govEra govIds }
         <- getEnvGovSummary
       govActId :: Ledger.GovActionId crypto
         <- insufReadyProps `hoistMaybe` (govIds !? vcGovActId)
       let ptxTxGen :: TxGenerator era
           ptxTxGen = genTxVoting' gcEnvLedgerParams gcEnvFeeInEra vote
           ptxMangledUTxOs :: CreateAndStoreList IO era [L.Coin]
           ptxMangledUTxOs = mangle $ repeat toUTxO
           vote :: VotingProcedures era
           vote = singletonVotingProcedures eon voter govActId unVotingProcedure
       handleE handlePreviewErr . void $
         previewTx PreviewTxData { ptxInputs = 0, .. }
       pure . Streaming.effect $ Streaming.yield <$>
         sourceToStoreTransactionNew ptxTxGen fundSource ptxInToOut ptxMangledUTxOs

evalGenerator :: forall era ledgerEra crypto . ()
  => IsShelleyBasedEra era
  => ledgerEra ~ ShelleyLedgerEra era
  => crypto ~ Ledger.EraCrypto ledgerEra
  => crypto ~ Ledger.StandardCrypto
  => Generator
  -> TxGenTxParams
  -> AsType era
  -> ActionM (TxStream IO era)
evalGenerator generator txParams@TxGenTxParams{..} era = do
  networkId <- getEnvNetworkId
  protocolParameters <- getProtocolParameters
  -- Hmm? hoistActionM seems rather apt here.
  case convertToLedgerProtocolParameters sbe protocolParameters of
    Left err -> throwE (Env.TxGenError (ApiError err))
    Right ledgerParameters
      | feeInEra <- Utils.mkTxFee txParamFee
      , mangleUTxOs <- mangle . repeat
      , genTx' <- \collateralPair -> genTx sbe ledgerParameters collateralPair feeInEra
      , txGenDefault <- genTx' noCollateral TxMetadataNone
      , evalGenerator' <- uncurry3 evalGenerator . (, txParams, era)
      -> case generator of
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
        Split walletName payMode payModeChange coins
          | inToOut <- Utils.includeChange txParamFee coins
          -> do fundSource <- flip walletSource 1 <$> getEnvWallets walletName
                (toUTxO, addressOut) <- interpretPayMode payMode
                traceDebug $ "split output address : " <> addressOut
                (mangledUTxOChange, addressChange)
                  <- first (flip mangleWithChange toUTxO) <$>
                        interpretPayMode payModeChange
                traceDebug $ "split change address : " <> addressChange
                pure . Streaming.effect $ Streaming.yield <$>
                      sourceToStoreTransactionNew txGenDefault fundSource inToOut mangledUTxOChange

        -- The 'SplitN' case's call chain is somewhat elaborate.
        -- The division is done in 'Utils.inputsToOutputsWithFee'
        -- but things are threaded through
        -- 'Cardano.Benchmarking.Wallet.mangle' and packed into
        -- the transaction assembled by 'sourceToStoreTransactionNew'.
        SplitN walletName payMode count
          | inToOut <- Utils.inputsToOutputsWithFee txParamFee count
          -> do fundSource <- flip walletSource 1 <$> getEnvWallets walletName
                (mangledUTxOs, addressOut)
                  <- first mangleUTxOs <$> interpretPayMode payMode
                traceDebug $ "SplitN output address : " <> addressOut
                pure . Streaming.effect $ Streaming.yield <$>
                  sourceToStoreTransactionNew txGenDefault fundSource inToOut mangledUTxOs

        NtoM walletName payMode ptxInputs outputs metadataSize collateralWallet
          | ptxInToOut <- Utils.inputsToOutputsWithFee txParamFee outputs
          , ptxLedgerPParams <- eitherToMaybe $
                toLedgerPParams sbe protocolParameters
          -> do wallet <- getEnvWallets walletName
                collaterals <- selectCollateralFunds collateralWallet
                (ptxMangledUTxOs, addressOut)
                  <- first mangleUTxOs <$> interpretPayMode payMode
                traceDebug $ "NtoM output address : " <> addressOut
                ptxFund <- liftIO $ walletPreview wallet ptxInputs
                let ptxTxGen = genTx' collaterals $ toMetadata metadataSize
                handleE handlePreviewErr do
                  (size, maybeFeeEstimate) <- previewTx PreviewTxData {..}
                  summarizeTx size maybeFeeEstimate
                let fundSource = walletSource wallet ptxInputs
                pure . Streaming.effect $ Streaming.yield <$>
                  sourceToStoreTransactionNew ptxTxGen fundSource ptxInToOut ptxMangledUTxOs

        Propose pcWalletName pcPayMode pcCoin pcStakeCredIdx pcAnchor
          | args <- ProposeCase {..}
          , env  <- GovCaseEnv
                      { gcEnvEra = era
                      , gcEnvTxParams = txParams
                      , gcEnvLedgerParams = ledgerParameters
                      , gcEnvFeeInEra = feeInEra }
         -> forShelleyBasedEraInEon sbe (unsuppErr "Propose") $
              proposeCase env args

        Vote vcWalletName vcPayMode vcGovActId vcVote vcDRepCredIdx vcAnchor
          | args <- VoteCase {..}
          , env  <- GovCaseEnv { gcEnvEra = era
                               , gcEnvTxParams = txParams
                               , gcEnvLedgerParams = ledgerParameters
                               , gcEnvFeeInEra = feeInEra }
          -> forShelleyBasedEraInEon sbe (unsuppErr "Vote") $ voteCase env args

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
    sbe = shelleyBasedEra @era

selectCollateralFunds :: forall era. IsShelleyBasedEra era
  => Maybe String
  -> ActionM (TxInsCollateral era, [FundQueue.Fund])
selectCollateralFunds Nothing = pure noCollateral
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
