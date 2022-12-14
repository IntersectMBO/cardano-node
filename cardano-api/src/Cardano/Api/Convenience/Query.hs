{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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

import           Prelude

import           Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, except)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Bifunctor (first)
import           Data.Either.Plucky (ProjectError)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Environment
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.TxBody
import           Cardano.Api.Utils

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | SockErr EnvSocketError
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | EraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra

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

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: CardanoEra era
  -> NetworkId
  -> [TxIn]
  -> ExceptT QueryConvenienceError IO (UTxO era, ProtocolParameters, EraHistory CardanoMode, SystemStart, Set PoolId)
queryStateForBalancedTx era networkId allTxIns = do
  SocketPath sockPath <- ExceptT $ first SockErr <$> readEnvSocketPath
  let cModeParams = CardanoModeParams $ EpochSlots 21600
      localNodeConnInfo = LocalNodeConnectInfo
                            cModeParams
                            networkId
                            sockPath
  qSbe <- ExceptT . return . getSbe $ cardanoEraStyle era
  case toEraInMode era CardanoMode of
    Just qeInMode -> do
      -- Queries
      let utxoQuery = QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe
                        $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
          pparamsQuery = QueryInEra qeInMode
                            $ QueryInShelleyBasedEra qSbe QueryProtocolParameters
          eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra
          systemStartQuery = QuerySystemStart
          stakePoolsQuery = QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools

      -- Query execution
      utxo <- executeQueryCardanoMode era networkId utxoQuery
      pparams <- executeQueryCardanoMode era networkId pparamsQuery
      eraHistory <- firstExceptT (AcqFailure . toAcquiringFailure) $ queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery
      systemStart <- firstExceptT (AcqFailure . toAcquiringFailure) $ queryNodeLocalState localNodeConnInfo Nothing systemStartQuery
      stakePools <- executeQueryCardanoMode era networkId stakePoolsQuery
      return (utxo, pparams, eraHistory, systemStart, stakePools)
    Nothing -> throwE $ EraConsensusModeMismatch (AnyConsensusMode CardanoMode) (getIsCardanoEraConstraint era $ AnyCardanoEra era)

-- | Query the node to determine which era it is in.
determineEra
  :: forall e mode. ()
  => ProjectError e AcquireFailure
  => ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT e IO AnyCardanoEra
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> pure $ AnyCardanoEra ByronEra
    ShelleyMode -> pure $ AnyCardanoEra ShelleyEra
    CardanoMode ->
      queryNodeLocalState localNodeConnInfo Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra

getSbe :: CardanoEraStyle era -> Either QueryConvenienceError (ShelleyBasedEra era)
getSbe LegacyByronEra = Left ByronEraNotSupported
getSbe (ShelleyBasedEra sbe) = return sbe

-- | Execute a query against the local node. The local
-- node must be in CardanoMode.
executeQueryCardanoMode
  :: CardanoEra era
  -> NetworkId
  -> QueryInMode CardanoMode (Either EraMismatch result)
  -> ExceptT QueryConvenienceError IO result
executeQueryCardanoMode era nid q = do
  SocketPath sockPath <- ExceptT $ first SockErr <$> readEnvSocketPath
  let localConnectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          , localNodeNetworkId = nid
          , localNodeSocketPath = sockPath
          }
  executeQueryAnyMode era localConnectInfo q

-- | Execute a query against the local node in any mode.
executeQueryAnyMode
  :: forall result era mode. CardanoEra era
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> ExceptT QueryConvenienceError IO result
executeQueryAnyMode era localNodeConnInfo q = do
  let cMode = consensusModeOnly $ localConsensusModeParams localNodeConnInfo
  case toEraInMode era cMode of
    Just eraInMode ->
      case eraInMode of
        ByronEraInByronMode -> throwE ByronEraNotSupported
        _ -> execQuery
    Nothing -> throwE $ EraConsensusModeMismatch (AnyConsensusMode CardanoMode) (getIsCardanoEraConstraint era $ AnyCardanoEra era)
 where
  execQuery :: ExceptT QueryConvenienceError IO result
  execQuery = do
    r <- firstExceptT (AcqFailure . toAcquiringFailure) $ queryNodeLocalState localNodeConnInfo Nothing q
    except $ first QueryEraMismatch r
