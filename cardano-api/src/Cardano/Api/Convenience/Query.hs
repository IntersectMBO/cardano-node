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

import           Prelude

import           Data.Bifunctor (first)
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
import           Cardano.Api.TxSubmit.Types (textShow)

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
  -> IO (Either QueryConvenienceError (UTxO era, ProtocolParameters, EraHistory CardanoMode, SystemStart, Set PoolId))
queryStateForBalancedTx era networkId allTxIns = do
 eSocketPath <- first SockErr <$> readEnvSocketPath
 case eSocketPath of
  Left e -> return $ Left e
  Right (SocketPath sockPath) -> do
    let cModeParams = CardanoModeParams $ EpochSlots 21600
        localNodeConnInfo = LocalNodeConnectInfo
                              cModeParams
                              networkId
                              sockPath
    eSbe <- return . getSbe $ cardanoEraStyle era
    case eSbe of
      Left e -> return $ Left e
      Right qSbe -> do
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
            eUtxo <- executeQueryCardanoMode era networkId utxoQuery
            ePparams <- executeQueryCardanoMode era networkId pparamsQuery
            eEraHistory <- queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery
            eSystemStart <- queryNodeLocalState localNodeConnInfo Nothing systemStartQuery
            eStakePools <- executeQueryCardanoMode era networkId stakePoolsQuery
            return $ do
              utxo <- eUtxo
              pparams <- ePparams
              eraHistory <- first AcqFailure eEraHistory
              systemStart <- first AcqFailure eSystemStart
              stakePools <- eStakePools
              Right (utxo, pparams, eraHistory, systemStart, stakePools)
          Nothing -> return $ Left $ EraConsensusModeMismatch
                                      (AnyConsensusMode CardanoMode)
                                      (getIsCardanoEraConstraint era $ AnyCardanoEra era)

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
  :: CardanoEra era
  -> NetworkId
  -> QueryInMode CardanoMode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryCardanoMode era nid q = do
  eSocketPath <- first SockErr <$> readEnvSocketPath
  case eSocketPath of
   Left e -> return $ Left e
   Right (SocketPath sockPath) -> do
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
  -> IO (Either QueryConvenienceError result)
executeQueryAnyMode era localNodeConnInfo q = do
  let cMode = consensusModeOnly $ localConsensusModeParams localNodeConnInfo
  case toEraInMode era cMode of
    Just eraInMode ->
      case eraInMode of
        ByronEraInByronMode -> return $ Left ByronEraNotSupported
        _ -> execQuery
    Nothing -> return $ Left $ EraConsensusModeMismatch
                                      (AnyConsensusMode CardanoMode)
                                      (getIsCardanoEraConstraint era $ AnyCardanoEra era)
 where
  execQuery :: IO (Either QueryConvenienceError result)
  execQuery = collapse <$> queryNodeLocalState localNodeConnInfo Nothing q

collapse
  :: Either AcquiringFailure (Either EraMismatch a)
  -> Either QueryConvenienceError a
collapse res = do
 innerRes <- first AcqFailure res
 first QueryEraMismatch innerRes

