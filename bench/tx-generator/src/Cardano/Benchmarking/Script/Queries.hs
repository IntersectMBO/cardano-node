{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Cardano.Benchmarking.Script.Queries
    ( getLocalConnectInfo
    , queryEra
    , queryGovernanceState
    , queryRemoteProtocolParameters

    , debugDumpProposalsPeriodically
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley (GovernanceActionId (..), ProtocolParameters, ShelleyLedgerEra)

import           Cardano.Benchmarking.OuroborosImports
import           Cardano.Benchmarking.Script.Aeson (prettyPrintOrdered)
import           Cardano.Benchmarking.Script.Env hiding (Error (TxGenError))
import qualified Cardano.Benchmarking.Script.Env as Env (Error (TxGenError))
import           Cardano.Ledger.Conway.Governance (ConwayGovState (..))
import qualified Cardano.Ledger.Conway.Governance as LC
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..))
import           Cardano.TxGenerator.Setup.NodeConfig (mkConsensusProtocol, mkNodeConfig)
import           Cardano.TxGenerator.Types
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (SomeException (..), catch, try)
import           Control.Monad (forever, void)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)
import qualified Data.Foldable as Foldable
import           Data.Time (defaultTimeLocale, formatTime)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)


data SomeGovernanceActionId where
  SomeGovernanceActionId ::
    forall era. () => LC.GovActionId (Ledger.EraCrypto (ShelleyLedgerEra era))
                   -> SomeGovernanceActionId

fileNamePParams, _fileNameGovState :: FilePath
fileNamePParams     = "protocol-parameters-queried.json"
_fileNameGovState    = "gov-state-queried.json"

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

queryGovernanceState :: ActionM () -- SomeGovernanceActionId
queryGovernanceState = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip   <- liftIO $ getLocalChainTip localNodeConnectInfo
  currentEra <- queryEra

  let
    callQuery :: forall era ledgerEra.
      ( ShelleyLedgerEra era ~ ledgerEra
      -- , Ledger.EraCrypto ledgerEra ~ StandardCrypto
      , LC.GovState ledgerEra ~ LC.ConwayGovState ledgerEra
      ) => ShelleyBasedEra era -> ActionM () -- SomeGovernanceActionId
    callQuery era = shelleyBasedEraConstraints era $ do
      let
        sbe     = era -- conwayEraOnwardsToShelleyBasedEra era
        query   = QueryInEra $ QueryInShelleyBasedEra sbe QueryGovState

      gs <- liftEither . first (Env.TxGenError . TxGenError . show) =<< mapExceptT liftIO (modifyError (Env.TxGenError . TxGenError . show) $
          queryNodeLocalState localNodeConnectInfo (SpecificPoint $ chainTipToChainPoint chainTip) query)
      let
        -- _props :: Proposals (ShelleyLedgerEra era)
        _props = cgsProposals gs

        -- _govActIds :: _ -- SomeGovernanceActionId -- [GovernanceActionId L.Conway]
        -- _govActIds = SomeGovernanceActionId $
        --  map GovernanceActionId $ Foldable.toList $ LC.proposalsIds _props
        _govActIds = head $ Foldable.toList $ LC.proposalsIds _props

        --liftIO $ BSL.writeFile _fileNameGovState $ prettyPrintOrdered gs
      pure ()
      -- pure (SomeGovernanceActionId _govActIds)

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
debugDumpProposalsPeriodically NixServiceOptions{..}
  | not (or _nix_drep_voting) = pure ()
  | otherwise                 = try setup >>= \case
      Left SomeException{}  -> pure ()
      Right (connInfo, era) -> case era of
        AnyCardanoEra ConwayEra  -> forkTheThread ConwayEraOnwardsConway connInfo
        _                        -> pure ()

  where
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
          props    <- fromRightOrFail (pure . cgsProposals) govState
          tStamp   <- formatTime defaultTimeLocale timeStampFormat . systemToUTCTime <$> getSystemTime
          BSL.writeFile (fileNameProposals tStamp) (prettyPrintOrdered props)

      void $ forkIO $ forever $ do
        !_ <- threadBody `catch` \SomeException{} -> pure ()
        threadDelay 60_000_000      -- 1 minute

    fromRightOrFail :: MonadFail m => (b -> m c) -> Either a b -> m c
    fromRightOrFail cont = \case
      Left{}    -> fail ""
      Right v   -> cont v

    startProtocol Nothing = fail ""
    startProtocol (Just cfgFile) = do
      mkNodeConfig cfgFile >>= fromRightOrFail mkConsensusProtocol >>= fromRightOrFail pure

    timeStampFormat :: String
    timeStampFormat = "%H-%M-%S"
