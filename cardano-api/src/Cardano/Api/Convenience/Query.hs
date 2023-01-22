{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Convenience query functions
--
module Cardano.Api.Convenience.Query (
    QueryConvenienceError(..),
    determineEra,
    determineEra_,
    -- * Simplest query related
    executeQueryCardanoMode,

    queryStateForBalancedTx,
    renderQueryConvenienceError,
  ) where

import           Control.Monad.Oops (CouldBe, Variant, runOopsInEither, runOopsInExceptT)
import qualified Control.Monad.Oops as OO
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (left, onLeft, onNothing)
import           Data.Function ((&))
import           Data.Map (Map)
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Eras
import           Cardano.Api.IO
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad (executeLocalStateQueryExpr_, queryExpr_)
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.TxBody
import           Cardano.Api.Utils
import           Cardano.Api.Value
import           Control.Monad.Trans (MonadTrans (..))

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | EraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | QueryConvenienceUnsupportedNodeToClientVersion !UnsupportedNtcVersionError

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (AcqFailure e) =
  "Acquiring failure: " <> textShow e
renderQueryConvenienceError (QueryEraMismatch (EraMismatch ledgerEraName' otherEraName')) =
  "The era of the node and the tx do not match. " <>
  "The node is running in the " <> ledgerEraName' <>
  " era, but the transaction is for the " <> otherEraName' <> " era."
renderQueryConvenienceError ByronEraNotSupported =
  "Byron era not supported"
renderQueryConvenienceError (EraConsensusModeMismatch cMode anyCEra) =
  "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
  " Era: " <> textShow anyCEra
renderQueryConvenienceError (QueryConvenienceUnsupportedNodeToClientVersion
  (UnsupportedNtcVersionError minNodeToClientVersion nodeToClientVersion)) =
  "Unsupported Node to Client version: " <> textShow minNodeToClientVersion <> " " <> textShow nodeToClientVersion

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: SocketPath
  -> CardanoEra era
  -> NetworkId
  -> [TxIn]
  -> [Certificate]
  -> IO (Either QueryConvenienceError ( UTxO era
                                      , ProtocolParameters
                                      , EraHistory CardanoMode
                                      , SystemStart
                                      , Set PoolId
                                      , Map StakeCredential Lovelace
                                      )
        )
queryStateForBalancedTx socketPath era networkId allTxIns certs = runExceptT $ runOopsInExceptT @QueryConvenienceError $ do
  let cModeParams = CardanoModeParams $ EpochSlots 21600

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId socketPath

  qSbe <- getSbe (cardanoEraStyle era) & OO.hoistEither

  qeInMode <- toEraInMode era CardanoMode
    & OO.hoistMaybe (EraConsensusModeMismatch (AnyConsensusMode CardanoMode) (getIsCardanoEraConstraint era $ AnyCardanoEra era))

  -- Queries
  let utxoQuery = QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe
                    $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
      pparamsQuery = QueryInEra qeInMode
                        $ QueryInShelleyBasedEra qSbe QueryProtocolParameters
      eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra
      systemStartQuery = QuerySystemStart
      stakePoolsQuery = QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools
      stakeCreds = Set.fromList $ flip mapMaybe certs $ \case
        StakeAddressDeregistrationCertificate cred -> Just cred
        _ -> Nothing
      stakeDelegDepositsQuery =
        QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakeDelegDeposits stakeCreds

  -- Query execution
  utxo <- executeLocalStateQueryExpr_ localNodeConnInfo Nothing
    ( queryExpr_ utxoQuery
    ) & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)
      & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
      & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)
  pparams <- executeLocalStateQueryExpr_ localNodeConnInfo Nothing
    ( queryExpr_ pparamsQuery
    ) & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)
      & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
      & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)
  eraHistory <- executeLocalStateQueryExpr_ localNodeConnInfo Nothing
    ( queryExpr_ eraHistoryQuery
    ) & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
      & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)
  systemStart <- executeLocalStateQueryExpr_ localNodeConnInfo Nothing
    ( queryExpr_ systemStartQuery
    ) & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
      & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)
  stakePools <- executeLocalStateQueryExpr_ localNodeConnInfo Nothing
    ( queryExpr_ stakePoolsQuery
    ) & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)
      & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
      & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)
  stakeDelegDeposits <-
    if null stakeCreds
      then pure mempty
      else
        executeLocalStateQueryExpr_ localNodeConnInfo Nothing
          ( queryExpr_ stakeDelegDepositsQuery
          ) & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)
            & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
            & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)

  return (utxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits)

-- | Query the node to determine which era it is in.
determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> IO (Either AcquiringFailure AnyCardanoEra)
determineEra cModeParams localNodeConnInfo =
  runOopsInEither $ determineEra_ cModeParams localNodeConnInfo

-- | Query the node to determine which era it is in.
determineEra_
  :: forall e mode. ()
  => e `CouldBe` AcquiringFailure
  => ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT (Variant e) IO AnyCardanoEra
determineEra_ cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> pure $ AnyCardanoEra ByronEra
    ShelleyMode -> pure $ AnyCardanoEra ShelleyEra
    CardanoMode ->
      queryNodeLocalState_ localNodeConnInfo Nothing
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
          , localNodeSocketPath = socketPath
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
