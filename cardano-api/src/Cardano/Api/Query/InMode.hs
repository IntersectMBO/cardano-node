{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Query.InMode where

import           Control.Monad.Trans.Except
import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime, SlotLength)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Ouroboros.Consensus.HardFork.History as History
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Consensus.Ledger.Query as Consensus

import           Cardano.Slotting.EpochInfo (hoistEpochInfo)
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Cardano.Slotting.Time (SystemStart (..))

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes
import           Cardano.Api.Query.Error

-- | QueryInMode queries are era independent
data QueryInMode mode result where
  QueryCurrentEra
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode AnyCardanoEra

  QueryEraHistory
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode (EraHistory mode)

  QuerySystemStart
    :: QueryInMode mode SystemStart

  QueryChainBlockNo
    :: QueryInMode mode (WithOrigin BlockNo)

  QueryChainPoint
    :: ConsensusMode mode
    -> QueryInMode mode ChainPoint


instance NodeToClientVersionOf (QueryInMode mode result) where
  nodeToClientVersionOf :: QueryInMode mode result -> NodeToClientVersion
  nodeToClientVersionOf (QueryCurrentEra _) = NodeToClientV_9
  nodeToClientVersionOf (QueryEraHistory _) = NodeToClientV_9
  nodeToClientVersionOf QuerySystemStart = NodeToClientV_9
  nodeToClientVersionOf QueryChainBlockNo = NodeToClientV_10
  nodeToClientVersionOf (QueryChainPoint _) = NodeToClientV_10


deriving instance Show (QueryInMode mode result)


toConsensusQuery :: forall mode block result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode result
                 -> Some (Consensus.Query block)
toConsensusQuery (QueryCurrentEra CardanoModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra

toConsensusQuery (QueryEraHistory CardanoModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter

toConsensusQuery QuerySystemStart = Some Consensus.GetSystemStart

toConsensusQuery QueryChainBlockNo = Some Consensus.GetChainBlockNo

toConsensusQuery (QueryChainPoint _) = Some Consensus.GetChainPoint



fromConsensusQueryResult
  :: forall mode block result result'. ConsensusBlockForMode mode ~ block
  => QueryInMode mode result
  -> Consensus.Query block result'
  -> result'
  -> result
fromConsensusQueryResult (QueryEraHistory CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetInterpreter)
        -> EraHistory CardanoMode r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QuerySystemStart q' r' =
    case q' of
      Consensus.GetSystemStart
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryChainBlockNo q' r' =
    case q' of
      Consensus.GetChainBlockNo
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryChainPoint mode) q' r' =
    case q' of
      Consensus.GetChainPoint
        -> fromConsensusPointInMode mode r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryCurrentEra CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetCurrentEra)
        -> anyEraInModeToAnyEra (fromConsensusEraIndex CardanoMode r')
      _ -> fromConsensusQueryResultMismatch


data EraHistory mode where
  EraHistory
    :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
    => ConsensusMode mode
    -> History.Interpreter xs
    -> EraHistory mode

getProgress :: SlotNo -> EraHistory mode -> Either Qry.PastHorizonException (RelativeTime, SlotLength)
getProgress slotNo (EraHistory _ interpreter) = Qry.interpretQuery interpreter (Qry.slotToWallclock slotNo)

-- | Returns the slot number for provided relative time from 'SystemStart'
getSlotForRelativeTime :: RelativeTime -> EraHistory mode -> Either Qry.PastHorizonException SlotNo
getSlotForRelativeTime relTime (EraHistory _ interpreter) = do
  (slotNo, _, _) <- Qry.interpretQuery interpreter $ Qry.wallclockToSlot relTime
  pure slotNo

newtype LedgerEpochInfo = LedgerEpochInfo { unLedgerEpochInfo :: Consensus.EpochInfo (Either Text) }

toLedgerEpochInfo :: EraHistory mode -> LedgerEpochInfo
toLedgerEpochInfo (EraHistory _ interpreter) =
    LedgerEpochInfo $ hoistEpochInfo (first (Text.pack . show) . runExcept) $
      Consensus.interpreterToEpochInfo interpreter
