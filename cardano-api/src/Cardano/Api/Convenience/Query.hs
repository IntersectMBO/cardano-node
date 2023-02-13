{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Move brackets to avoid $" -}

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

import           Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import           Control.Monad.Trans.Except.Extra (hoistMaybe, left, onLeft, onNothing)
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
import           Cardano.Api.IPC.Monad
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
  | ConvenienceUnsupportedNtcVersionError !UnsupportedNtcVersionError

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
renderQueryConvenienceError (ConvenienceUnsupportedNtcVersionError e) =
  renderUnsupportedNtcVersionError e

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
queryStateForBalancedTx socketPath era networkId allTxIns certs = runExceptT $ do
  let cModeParams = CardanoModeParams $ EpochSlots 21600
      localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId socketPath

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
      stakeCreds = Set.fromList $ flip mapMaybe certs $ \case
        StakeAddressDeregistrationCertificate cred -> Just cred
        _ -> Nothing
      stakeDelegDepositsQuery =
        QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakeDelegDeposits stakeCreds

  -- Query execution
  ( lift $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ runExceptT $ do
      utxo <- ExceptT $ ExceptT $ queryExpr utxoQuery
      pparams <- ExceptT $ ExceptT $ queryExpr pparamsQuery
      eraHistory <- ExceptT $ fmap Right $ ExceptT $ queryExpr eraHistoryQuery
      systemStart <- ExceptT $ fmap Right $ ExceptT $ queryExpr systemStartQuery
      stakePools <- ExceptT $ ExceptT $ queryExpr stakePoolsQuery
      stakeDelegDeposits <-
        if null stakeCreds
          then pure mempty
          else ExceptT $ ExceptT $ queryExpr stakeDelegDepositsQuery
      pure (utxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits)
    ) & onLeft (left . AcqFailure)
      & onLeft (left . ConvenienceUnsupportedNtcVersionError)
      & onLeft (left . QueryEraMismatch)

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
