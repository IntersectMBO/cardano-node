{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Benchmarking.Script.Queries
    ( getLocalConnectInfo
    , queryEra
    , queryGovernanceState
    , queryRemoteProtocolParameters

    , debugDumpProposalsPeriodically
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters, ShelleyLedgerEra)

import           Cardano.Benchmarking.OuroborosImports
import           Cardano.Benchmarking.Script.Aeson (prettyPrintOrdered)
import           Cardano.Benchmarking.Script.Env hiding (Error (TxGenError))
import qualified Cardano.Benchmarking.Script.Env as Env (Error (TxGenError))
import           Cardano.Benchmarking.Script.Types
import           Cardano.Ledger.BaseTypes (unboundRational)
import qualified Cardano.Ledger.Conway.Governance as LC
import qualified Cardano.Ledger.Conway.PParams as LC
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..))
import           Cardano.TxGenerator.Setup.NodeConfig (mkConsensusProtocol, mkNodeConfig)
import           Cardano.TxGenerator.Types
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (SomeException (..), catch, try)
import           Control.Monad (forever, void)
import           Control.Monad.Extra (whenJust)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import qualified Data.Either.Combinators as Either (whenRight)
import qualified Data.Foldable as Foldable
import           Data.Ratio
import           Data.Time (defaultTimeLocale, formatTime)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Lens.Micro ((^.))


fileNamePParams :: FilePath
fileNamePParams     = "protocol-parameters-queried.json"

fileNameProposals :: String -> FilePath
fileNameProposals tStamp = "govstate-proposals-" ++ tStamp ++ ".json"

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
      liftIO $ BSL.writeFile fileNamePParams $ prettyPrintOrdered pp'
      traceDebug $ "queryRemoteProtocolParameters: query result saved in: " ++ fileNamePParams
      return pp'

  case era of
    AnyCardanoEra ByronEra   -> liftTxGenError $ TxGenError "queryRemoteProtocolParameters Byron not supported"
    AnyCardanoEra ShelleyEra -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraShelley QueryProtocolParameters
    AnyCardanoEra AllegraEra -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraAllegra QueryProtocolParameters
    AnyCardanoEra MaryEra    -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraMary    QueryProtocolParameters
    AnyCardanoEra AlonzoEra  -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo  QueryProtocolParameters
    AnyCardanoEra BabbageEra -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraBabbage QueryProtocolParameters
    AnyCardanoEra ConwayEra  -> callQuery $ QueryInShelleyBasedEra ShelleyBasedEraConway  QueryProtocolParameters

queryGovernanceState :: ActionM GovStateSummary
queryGovernanceState = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip   <- liftIO $ getLocalChainTip localNodeConnectInfo
  currentEra <- queryEra

  let
    callQuery :: forall era ledgerEra.
      ( ShelleyLedgerEra era ~ ledgerEra
      , LC.GovState ledgerEra ~ LC.ConwayGovState ledgerEra
      , LC.ConwayEraPParams ledgerEra
      ) => ShelleyBasedEra era -> ActionM GovStateSummary
    callQuery era = shelleyBasedEraConstraints era $ do
      let
        query = QueryInEra $ QueryInShelleyBasedEra era QueryGovState

      gs <- liftEither . first (Env.TxGenError . TxGenError . show) =<< mapExceptT liftIO (modifyError (Env.TxGenError . TxGenError . show) $
          queryNodeLocalState localNodeConnectInfo (SpecificPoint $ chainTipToChainPoint chainTip) query)
      let
        props     = LC.cgsProposals gs
        govActIds = Foldable.toList $ LC.proposalsIds props

        pparams   = LC.cgsCurPParams gs
        deposit   = pparams ^. LC.ppGovActionDepositL
        threshold = unboundRational $ LC.dvtTreasuryWithdrawal $ pparams ^. LC.ppDRepVotingThresholdsL
        threshInt = fromInteger (numerator threshold) % fromInteger (denominator threshold)

      pure $ GovStateSummary deposit threshInt (GovernanceActionIds era govActIds)

  case currentEra of
    AnyCardanoEra ConwayEra  -> callQuery ShelleyBasedEraConway
    AnyCardanoEra _          -> liftTxGenError $ TxGenError "queryGovState: pre-Conway eras not supported"

-- | This spawns a debug thread to dump the proposals section of the governance state every minute,
--   iff tx-generator voting workload is specified, and we're in a ConwayEraOnwards.
--
--   All failures and exceptions are silent and non-blocking, i.e. there are just no file dumps appearing.
--
-- NB. This must NEVER be used during an actual benchmark, as this query potentially forces the ledger pulser.
--
debugDumpProposalsPeriodically :: NixServiceOptions -> IO ()
debugDumpProposalsPeriodically NixServiceOptions{..} =
  whenJust _nix_govAct \_ ->
    whenRightM (try @SomeException setup) \(connInfo, _) ->
      forkTheThread ConwayEraOnwardsConway connInfo

  where
    whenRightM :: Monad m => m (Either e t) -> (t -> m ()) -> m ()
    whenRightM mx f = do
      x <- mx
      Either.whenRight x f
    setup :: IO (LocalNodeConnectInfo, AnyCardanoEra)
    setup = do
      proto <- startProtocol _nix_nodeConfigFile

      let
        nodeConnInfo :: LocalNodeConnectInfo
        nodeConnInfo = makeLocalConnectInfo (protocolToNetworkId proto) (File _nix_localNodeSocketPath)

        queryEraIO :: ChainTip -> IO AnyCardanoEra
        queryEraIO tip = fromRightOrFail pure =<< runExceptT
          (queryNodeLocalState nodeConnInfo (SpecificPoint $ chainTipToChainPoint tip) QueryCurrentEra)

      chainTip <- getLocalChainTip nodeConnInfo
      (,) nodeConnInfo <$> queryEraIO chainTip

    forkTheThread :: () => ConwayEraOnwards era -> LocalNodeConnectInfo -> IO ()
    forkTheThread era nodeConnInfo = conwayEraOnwardsConstraints era $ do
      let
        sbe     = conwayEraOnwardsToShelleyBasedEra era
        query   = QueryInEra $ QueryInShelleyBasedEra sbe QueryGovState

        threadBody = do
          chainTip <- getLocalChainTip nodeConnInfo
          govState <- fromRightOrFail pure =<< runExceptT
            (queryNodeLocalState nodeConnInfo (SpecificPoint $ chainTipToChainPoint chainTip) query)
          props    <- fromRightOrFail (pure . LC.cgsProposals) govState
          tStamp   <- formatTime defaultTimeLocale timeStampFormat . systemToUTCTime <$> getSystemTime
          BSL.writeFile (fileNameProposals tStamp) (prettyPrintOrdered props)

      void $ forkIO $ forever $ do
        !_ <- threadBody `catch` \SomeException{} -> pure ()
        threadDelay 60_000_000      -- 1 minute

    -- an ExceptT for the masses
    fromRightOrFail :: MonadFail m => (b -> m c) -> Either a b -> m c
    fromRightOrFail cont = \case
      Left{}    -> fail ""
      Right v   -> cont v

    startProtocol Nothing = fail ""
    startProtocol (Just cfgFile) = do
      mkNodeConfig cfgFile >>= fromRightOrFail mkConsensusProtocol >>= fromRightOrFail pure

    timeStampFormat :: String
    timeStampFormat = "%H-%M-%S"
