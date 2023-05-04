{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Convenience query functions
--
module Cardano.Api.Convenience.Query (
    QueryConvenienceError(..),
    determineEra,
    -- * Simplest query related
    executeQueryCardanoMode,
    queryStateForBalancedTx,
    renderQueryConvenienceError,
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, onLeft,
                   onNothing)
import           Data.Bifunctor (first)
import           Data.Function ((&))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Convenience.Error
import           Cardano.Api.Environment
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.IPC.AnyQuery
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Query.ShelleyBased
import           Cardano.Api.TxBody
import           Cardano.Api.Utils

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (AcqFailure e) =
  "Acquiring failure: " <> textShow e
renderQueryConvenienceError (SockErr e) =
  renderEnvSocketError e
renderQueryConvenienceError (QueryEraMismatch (EraMismatch ledgerEraName' otherEraName')) =
  "The era of the node and the tx do not match. " <>
  "The node is running in the " <> ledgerEraName' <>
  " era, but the transaction is for the " <> otherEraName' <> " era."
renderQueryConvenienceError ByronEraNotSupported =
  "Byron era not supported"
renderQueryConvenienceError (EraConsensusModeMismatch cMode anyCEra) =
  "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
  " Era: " <> textShow anyCEra
renderQueryConvenienceError (QueryConvenienceError e) = Text.pack $ show e

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: NetworkId
  -> [TxIn]
  -> IO (Either QueryConvenienceError (AnyUTxO, ProtocolParameters, EraHistory CardanoMode, SystemStart, Set PoolId))
queryStateForBalancedTx networkId allTxIns = runExceptT $ do
  SocketPath sockPath <- newExceptT $ first SockErr <$> readEnvSocketPath
  let cModeParams = CardanoModeParams $ EpochSlots 21600
      localNodeConnInfo = LocalNodeConnectInfo
                            cModeParams
                            networkId
                            sockPath
  firstExceptT QueryConvenienceError $ newExceptT
    $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
        AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
        eInMode <- determineEraInModeAnyQuery era cModeParams
        case cardanoEraStyle era of
          LegacyByronEra -> left AllQueryEraExpectedSbe
          ShelleyBasedEra sbe -> do
            let utxoQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe
                              $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
                pparamsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                 $ QueryInShelleyBasedEra sbe QueryProtocolParameters
                eraHistoryQuery = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra
                systemStartQuery = AnyQueryAnyEra QuerySystemStart
                stakePoolsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                    $ QueryInShelleyBasedEra sbe QueryStakePools
            utxo <- AnyUTxO (shelleyBasedToCardanoEra sbe) <$> queryExprAnyQueryE utxoQuery
            pparams <- queryExprAnyQueryE pparamsQuery
            eraHistory <- queryExprAnyQuery eraHistoryQuery
            systemStart <- queryExprAnyQuery systemStartQuery
            stakePools <- queryExprAnyQueryE stakePoolsQuery
            return (utxo, pparams, eraHistory, systemStart, stakePools)

  -- -- Queries
  -- let utxoQuery = QueryShelleyBasedEra qeInMode $ QueryInShelleyBasedEra qSbe
  --                   $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
  --     pparamsQuery = QueryShelleyBasedEra qeInMode
  --                       $ QueryInShelleyBasedEra qSbe QueryProtocolParameters
  --     eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra
  --     systemStartQuery = QuerySystemStart
  --     stakePoolsQuery = QueryShelleyBasedEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools

  -- -- Query execution
  -- utxo <- ExceptT $ executeQueryCardanoMode era networkId utxoQuery
  -- pparams <- ExceptT $ executeQueryCardanoMode era networkId pparamsQuery
  -- eraHistory <- firstExceptT AcqFailure $ ExceptT $ queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery
  -- systemStart <- firstExceptT AcqFailure $ ExceptT $ queryNodeLocalState localNodeConnInfo Nothing systemStartQuery
  -- stakePools <- ExceptT $ executeQueryCardanoMode era networkId stakePoolsQuery

  -- return (utxo, pparams, eraHistory, systemStart, stakePools)

-- | Query the node to determine which era it is in.
determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> IO (Either AcquiringFailure AnyCardanoEra)
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> return . Right $ AnyCardanoEra ByronEra
    ShelleyMode -> return . Right $ AnyCardanoEra ShelleyEra
    CardanoMode ->
      queryNodeLocalState localNodeConnInfo Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra

-- | Execute a query against the local node. The local
-- node must be in CardanoMode.
executeQueryCardanoMode
  :: CardanoEra era
  -> NetworkId
  -> QueryInMode CardanoMode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryCardanoMode era nid q = runExceptT $ do
  SocketPath sockPath <- firstExceptT SockErr . ExceptT $ readEnvSocketPath

  let localNodeConnInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          , localNodeNetworkId = nid
          , localNodeSocketPath = sockPath
          }

  ExceptT $ executeQueryAnyMode era localNodeConnInfo q

-- | Execute a query against the local node in any mode.
executeQueryAnyMode
  :: forall result era mode. CardanoEra era
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryAnyMode era localNodeConnInfo q = runExceptT $ do
  let cMode = consensusModeOnly $ localConsensusModeParams localNodeConnInfo

  eraInMode <- pure (toEraInMode era cMode)
    & onNothing (left $ EraConsensusModeMismatch
        (AnyConsensusMode CardanoMode)
        (getIsCardanoEraConstraint era $ AnyCardanoEra era))

  case eraInMode of
    ByronEraInByronMode -> left ByronEraNotSupported
    _ ->
      lift (queryNodeLocalState localNodeConnInfo Nothing q)
        & onLeft (left . AcqFailure)
        & onLeft (left . QueryEraMismatch)
