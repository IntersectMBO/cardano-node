{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.Script.Queries
    ( getLocalConnectInfo
    , queryEra
    , queryGovernanceState
    , queryRemoteProtocolParameters
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters, ShelleyLedgerEra)

import           Cardano.Benchmarking.OuroborosImports (makeLocalConnectInfo)
import           Cardano.Benchmarking.Script.Aeson (prettyPrintOrdered)
import           Cardano.Benchmarking.Script.Env hiding (Error (TxGenError))
import qualified Cardano.Benchmarking.Script.Env as Env (Error (TxGenError))
import           Cardano.Ledger.Conway.Governance (ConwayGovState (..))
import qualified Cardano.Ledger.Core as Ledger (PParams)
import           Cardano.TxGenerator.Types
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import           Data.Bifunctor (first)
import           Data.ByteString.Lazy.Char8 as BSL (writeFile)


fileNamePParams, _fileNameGovState :: FilePath
fileNamePParams     = "protocol-parameters-queried.json"
_fileNameGovState    = "gov-state-queried.json"

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

queryGovernanceState :: ActionM ()
queryGovernanceState = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip   <- liftIO $ getLocalChainTip localNodeConnectInfo
  currentEra <- queryEra

  let
    callQuery :: () => ConwayEraOnwards era -> ActionM ()
    callQuery era = conwayEraOnwardsConstraints era $ do
      let
        sbe     = conwayEraOnwardsToShelleyBasedEra era
        query   = QueryInEra $ QueryInShelleyBasedEra sbe QueryGovState
      _gs <- liftEither . first (Env.TxGenError . TxGenError . show) =<< mapExceptT liftIO (modifyError (Env.TxGenError . TxGenError . show) $
          queryNodeLocalState localNodeConnectInfo (SpecificPoint $ chainTipToChainPoint chainTip) query)

      let pp = fromLedgerPParams sbe (cgsCurPParams _gs)
      liftIO $ BSL.writeFile ("gs-" ++ fileNamePParams) $ prettyPrintOrdered pp
      liftIO $ BSL.writeFile _fileNameGovState $ prettyPrintOrdered _gs

  case currentEra of
    AnyCardanoEra ConwayEra  -> callQuery ConwayEraOnwardsConway
    AnyCardanoEra _          -> liftTxGenError $ TxGenError "queryGovState: pre-Conway eras not supported"
