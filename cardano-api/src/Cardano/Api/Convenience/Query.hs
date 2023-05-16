{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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

    queryCurrentEra_,
    queryChainBlockNo_,
    queryChainPoint_,
    queryStateForBalancedTx_,
    queryUtxo_,
    queryPoolState_,
    queryProtocolState_,
    queryGenesisParameters_,
    queryProtocolParams_,
    queryEraHistory_,
    queryStakePools_,
    querySystemStart_,

    handleQueryConvenienceErrors_,

    renderQueryConvenienceError,

    RequireShelleyBasedEra(..),
    requireShelleyBasedEra_,
  ) where

import           Control.Monad.Oops (CouldBe, Variant, runOopsInEither)
import qualified Control.Monad.Oops as OO
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (left, onLeft, onNothing)
import           Data.Function ((&))
import           Data.Map (Map)
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (WithOrigin)

import           Cardano.Api.Address
import           Cardano.Api.Block (ChainPoint)
import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters (GenesisParameters)
import           Cardano.Api.IO
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad (LocalStateQueryExpr, executeLocalStateQueryExpr_,
                   queryExpr_)
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.TxBody
import           Cardano.Api.Utils
import           Cardano.Api.Value

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | EraConsensusModeMismatch !InvalidEraInMode
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
renderQueryConvenienceError (EraConsensusModeMismatch (InvalidEraInMode anyCEra cMode)) =
  "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
  " Era: " <> textShow anyCEra
renderQueryConvenienceError (QueryConvenienceUnsupportedNodeToClientVersion
  (UnsupportedNtcVersionError minNodeToClientVersion nodeToClientVersion)) =
  "Unsupported Node to Client version: " <> textShow minNodeToClientVersion <> " " <> textShow nodeToClientVersion

handleQueryConvenienceErrors_ :: ()
  => Monad m
  => e `CouldBe` QueryConvenienceError
  => ExceptT (Variant (EraMismatch : AcquiringFailure : UnsupportedNtcVersionError : InvalidEraInMode : e)) m a
  -> ExceptT (Variant e) m a
handleQueryConvenienceErrors_ f = f
  & OO.catch @EraMismatch (OO.throw . QueryEraMismatch)
  & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
  & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)
  & OO.catch @InvalidEraInMode (OO.throw . EraConsensusModeMismatch)

queryCurrentEra_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ConsensusModeIsMultiEra mode
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) AnyCardanoEra
queryCurrentEra_ cMode = queryExpr_ $ QueryCurrentEra cMode

queryChainBlockNo_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) (WithOrigin BlockNo)
queryChainBlockNo_ = queryExpr_ QueryChainBlockNo

queryEraHistory_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ConsensusModeIsMultiEra mode
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) (EraHistory mode)
queryEraHistory_ cMode = queryExpr_ $ QueryEraHistory cMode

queryChainPoint_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ConsensusMode mode
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) ChainPoint
queryChainPoint_ mode = queryExpr_ (QueryChainPoint mode)

queryUtxo_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` EraMismatch
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> QueryUTxOFilter
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) (UTxO era)
queryUtxo_ qeInMode qSbe qFilter = do
  let query = QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryUTxO qFilter

  queryExpr_ query & OO.onLeft @EraMismatch OO.throw

queryProtocolState_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` EraMismatch
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) (ProtocolState era)
queryProtocolState_ eInMode sbe = do
  let query = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolState

  queryExpr_ query & OO.onLeft @EraMismatch OO.throw

queryPoolState_ :: ()
  => e `CouldBe` EraMismatch
  => e `CouldBe` UnsupportedNtcVersionError
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) (SerialisedPoolState era)
queryPoolState_ eInMode sbe mPoolIds = do
  let query = QueryInEra eInMode $ QueryInShelleyBasedEra sbe $ QueryPoolState mPoolIds

  queryExpr_ query & OO.onLeft @EraMismatch OO.throw

-- QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryPoolState

queryGenesisParameters_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` EraMismatch
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) GenesisParameters
queryGenesisParameters_ eInMode sbe = do
  let query = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryGenesisParameters

  queryExpr_ query & OO.onLeft @EraMismatch OO.throw

queryProtocolParams_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` EraMismatch
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) ProtocolParameters
queryProtocolParams_ qeInMode qSbe = do
  let query = QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryProtocolParameters

  queryExpr_ query & OO.onLeft @EraMismatch OO.throw

queryStakePools_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` QueryConvenienceError
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) (Set PoolId)
queryStakePools_ qeInMode qSbe = do
  let query = QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools

  queryExpr_ query & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)

querySystemStart_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ExceptT (Variant e) (LocalStateQueryExpr block point (QueryInMode mode) r IO) SystemStart
querySystemStart_ = queryExpr_ QuerySystemStart

queryStakeDelegDeposits_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` QueryConvenienceError
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> ExceptT
      (Variant e)
      (LocalStateQueryExpr block point (QueryInMode mode) r IO)
      (Map StakeCredential Lovelace)
queryStakeDelegDeposits_ qeInMode qSbe stakeCredentials =
  if null stakeCredentials
    then pure mempty
    else do
      let query = QueryInEra qeInMode
            $ QueryInShelleyBasedEra qSbe
            $ QueryStakeDelegDeposits stakeCredentials

      queryExpr_ query & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)

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
queryStateForBalancedTx socketPath era networkId allTxIns certs = runExceptT $ OO.runOopsInExceptT @QueryConvenienceError $ do
  queryStateForBalancedTx_ socketPath era networkId allTxIns certs
    & OO.catch @EraMismatch (OO.throw . QueryEraMismatch)
    & OO.catch @AcquiringFailure (OO.throw . AcqFailure)
    & OO.catch @UnsupportedNtcVersionError (OO.throw . QueryConvenienceUnsupportedNodeToClientVersion)
    & OO.catch @InvalidEraInMode (OO.throw . EraConsensusModeMismatch)

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx_ :: ()
  => e `CouldBe` QueryConvenienceError
  => e `CouldBe` AcquiringFailure
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` EraMismatch
  => e `CouldBe` InvalidEraInMode
  => SocketPath
  -> CardanoEra era
  -> NetworkId
  -> [TxIn]
  -> [Certificate]
  -> ExceptT (Variant e) IO
      ( UTxO era
      , ProtocolParameters
      , EraHistory CardanoMode
      , SystemStart
      , Set PoolId
      , Map StakeCredential Lovelace
      )
queryStateForBalancedTx_ socketPath era networkId allTxIns certs = do
  let cModeParams = CardanoModeParams $ EpochSlots 21600

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId socketPath

  qSbe <- getSbe (cardanoEraStyle era) & OO.hoistEither

  qeInMode <- toEraInMode_ era CardanoMode

  let stakeCreds = Set.fromList $ flip mapMaybe certs $ \case
        StakeAddressDeregistrationCertificate cred -> Just cred
        _ -> Nothing

  -- Query execution
  executeLocalStateQueryExpr_ localNodeConnInfo Nothing $ do
    utxo <- queryUtxo_ qeInMode qSbe $ QueryUTxOByTxIn $ Set.fromList allTxIns
    pparams <- queryProtocolParams_ qeInMode qSbe
    eraHistory <- queryEraHistory_ CardanoModeIsMultiEra
    systemStart <- querySystemStart_
    stakePools <- queryStakePools_ qeInMode qSbe
    stakeDelegDeposits <- queryStakeDelegDeposits_ qeInMode qSbe stakeCreds

    pure (utxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits)

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
    & onNothing (left $ EraConsensusModeMismatch $ InvalidEraInMode
        (getIsCardanoEraConstraint era $ AnyCardanoEra era)
        (AnyConsensusMode CardanoMode))

  case eraInMode of
    ByronEraInByronMode -> left ByronEraNotSupported
    _ ->
      lift (queryNodeLocalState localNodeConnInfo Nothing q)
        & onLeft (left . AcqFailure)
        & onLeft (left . QueryEraMismatch)
