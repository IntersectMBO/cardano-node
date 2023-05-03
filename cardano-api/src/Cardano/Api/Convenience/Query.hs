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

import           Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistMaybe, left, onLeft,
                   onNothing)
import           Data.Function ((&))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Prettyprinter as PP

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Environment
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Pretty
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.TxBody
import           Control.Monad.Trans (MonadTrans (..))

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | SockErr EnvSocketError
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | EraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra

renderQueryConvenienceError :: QueryConvenienceError -> Doc Ann
renderQueryConvenienceError (AcqFailure e) =
  reflow $ "Acquiring failure: " <> pretty (show e)
renderQueryConvenienceError (SockErr e) =
  renderEnvSocketError e
renderQueryConvenienceError (QueryEraMismatch (EraMismatch ledgerEraName' otherEraName')) =
  reflow $ PP.hsep
    [ "The era of the node and the query do not match."
    , "The node is running in the" <+> pretty ledgerEraName' <+> "era,"
    , "but the query is for the" <+> pretty otherEraName' <+> "era."
    ]
renderQueryConvenienceError ByronEraNotSupported =
  reflow "Byron era not supported"
renderQueryConvenienceError (EraConsensusModeMismatch cMode anyCEra) =
  reflow $ PP.hsep
    [ "Consensus mode and era mismatch."
    , "Consensus mode:" <+> pretty cMode <+> ","
    , "Era:" <+> pretty anyCEra
    ]

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: SocketPath
  -> CardanoEra era
  -> NetworkId
  -> [TxIn]
  -> IO (Either QueryConvenienceError (UTxO era, ProtocolParameters, EraHistory CardanoMode, SystemStart, Set PoolId))
queryStateForBalancedTx socketPath era networkId allTxIns = runExceptT $ do
  let cModeParams = CardanoModeParams $ EpochSlots 21600
      localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId (unSocketPath socketPath)

  qSbe <- except $ getSbe $ cardanoEraStyle era

  qeInMode <- toEraInMode era CardanoMode
    & hoistMaybe (EraConsensusModeMismatch (AnyConsensusMode CardanoMode) (getIsCardanoEraConstraint era $ AnyCardanoEra era))

  -- Queries
  let utxoQuery = QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe
                    $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
      pparamsQuery = QueryInEra qeInMode
                        $ QueryInShelleyBasedEra qSbe QueryProtocolParameters
      eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra
      systemStartQuery = QuerySystemStart
      stakePoolsQuery = QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools

  -- Query execution
  utxo <- ExceptT $ executeQueryCardanoMode socketPath era networkId utxoQuery
  pparams <- ExceptT $ executeQueryCardanoMode socketPath era networkId pparamsQuery
  eraHistory <- firstExceptT AcqFailure $ ExceptT $ queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery
  systemStart <- firstExceptT AcqFailure $ ExceptT $ queryNodeLocalState localNodeConnInfo Nothing systemStartQuery
  stakePools <- ExceptT $ executeQueryCardanoMode socketPath era networkId stakePoolsQuery

  return (utxo, pparams, eraHistory, systemStart, stakePools)

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

getSbe :: CardanoEraStyle era -> Either QueryConvenienceError (ShelleyBasedEra era)
getSbe LegacyByronEra = Left ByronEraNotSupported
getSbe (ShelleyBasedEra sbe) = return sbe

-- | Execute a query against the local node. The local
-- node must be in CardanoMode.
executeQueryCardanoMode
  :: SocketPath
  -> CardanoEra era
  -> NetworkId
  -> QueryInMode CardanoMode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryCardanoMode socketPath era nid q = runExceptT $ do
  let localNodeConnInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          , localNodeNetworkId = nid
          , localNodeSocketPath = unSocketPath socketPath
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
