{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Answer queries using the ledger state of the running node
--
-- This allows queries from within the running node as opposed to submitting
-- queries via the LocalStateQuery protocol.
module Cardano.Node.Query
  ( answerQuery
  , answerQueryWithLedgerState
  , InterpreterQuery
  , qryEpochStartTimeOfSlot
  , getEpochStartTimeOfSlot
  ) where

import           Cardano.Prelude

import           Data.Time.Clock (UTCTime)

-- Consensus
import           Ouroboros.Consensus.Block (SlotNo)
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WCT
import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock, configLedger)
import           Ouroboros.Consensus.Config.SupportsNode (getSystemStart)
import qualified Ouroboros.Consensus.HardFork.Combinator.Compat as HF
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
                     (HardForkLedgerConfig (DegenLedgerConfig))
import qualified Ouroboros.Consensus.HardFork.History.Qry as HFI
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState)
import qualified Ouroboros.Consensus.Ledger.Query as Consensus (Query, answerQuery)
import           Ouroboros.Consensus.Node (RunNode)
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

import qualified Cardano.Api.Protocol.Types as Protocol

-- Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Conversions as Byron

-- Shelley
import           Ouroboros.Consensus.Shelley.Ledger (shelleyLedgerGenesis)
import qualified Shelley.Spec.Ledger.API as SL

-- Cardano
import           Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC)
import qualified Ouroboros.Consensus.Cardano.CanHardFork as CanHardFork
import           Ouroboros.Consensus.Shelley.ShelleyHFC (ShelleyBlockHFC)

import           Cardano.Tracing.Kernel

-- | Answer a general query about the current ledger state of the running node.
answerQuery
  :: forall blk result
  . ( RunNode blk
    , Protocol.Protocol IO blk
    )
  => Protocol.BlockType blk
  -> Protocol.ProtocolInfoArgs IO blk
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> HF.HardForkCompatQuery blk result
  -> IO result
answerQuery blkType protocol nodeKernel query = do
    extLedgerState <- atomically $ ChainDB.getCurrentLedger chainDB
    return $ answerQueryWithLedgerState blkType protocol extLedgerState query
  where
    chainDB = getChainDB nodeKernel

-- | Answer a general query about the given ledger state.
answerQueryWithLedgerState
  :: forall blk result
  . ( RunNode blk
    , Protocol.Protocol IO blk
    )
  => Protocol.BlockType blk
  -> Protocol.ProtocolInfoArgs IO blk
  -> ExtLedgerState blk
  -> HF.HardForkCompatQuery blk result
  -> result
answerQueryWithLedgerState blkType protocol extLedgerState query = runIdentity $
    case blkType of
      Protocol.ByronBlockType {} ->
        byronQuery
      Protocol.ShelleyBlockType {} ->
        shelleyBasedQuery
      Protocol.CardanoBlockType {} ->
        HF.forwardCompatQuery
          answerQueryHelper
          query
  where
    cfg :: TopLevelConfig blk
    cfg = pInfoConfig $ Protocol.protocolInfo protocol

    answerQueryHelper
      :: forall m result'. Monad m
      => Consensus.Query blk result'
      -> m result'
    answerQueryHelper q = pure $
      Consensus.answerQuery (ExtLedgerCfg cfg) q extLedgerState

    byronQuery :: blk ~ ByronBlockHFC => Identity result
    byronQuery =
        HF.singleEraCompatQuery
          epochSize
          slotLength
          answerQueryHelper
          query
      where
        DegenLedgerConfig ledgerConfig = configLedger cfg
        genesis = CanHardFork.byronLedgerConfig ledgerConfig
        epochSize = Byron.fromByronEpochSlots $ Byron.configEpochSlots genesis
        slotLength = Byron.fromByronSlotLength $ Byron.genesisSlotLength genesis

    shelleyBasedQuery :: blk ~ ShelleyBlockHFC era => Identity result
    shelleyBasedQuery =
        HF.singleEraCompatQuery
          epochSize
          slotLength
          answerQueryHelper
          query
      where
        DegenLedgerConfig ledgerConfig = configLedger cfg
        genesis = shelleyLedgerGenesis $
                    CanHardFork.shelleyLedgerConfig ledgerConfig
        epochSize = SL.sgEpochLength genesis
        slotLength = WCT.mkSlotLength $ SL.sgSlotLength genesis

-- | To avoid confusing 'HFI.Qry' with 'HF.Query' and 'HF.HardForkCompatQuery,
-- we introduce a type synonym for the former which we can use in docstrings
-- (and type signatures, of course).
--
-- * 'HFI.Qry' (and thus 'InterpreterQuery') is a query about slots/epochs/time
--   that a 'HF.Interpreter' can answer.
--
-- * 'Consensus.Query' and 'HF.HardForkCompatQuery' are more general queries
--   that can ask for anything related to the ledger state.
type InterpreterQuery a = HFI.Qry a

-- | Return the start time of the epoch of the given slot.
qryEpochStartTimeOfSlot :: SlotNo -> InterpreterQuery WCT.RelativeTime
qryEpochStartTimeOfSlot slotNo = HFI.qryFromExpr $
    HFI.ELet (HFI.EAbsToRelSlot (HFI.ELit slotNo)) $ \slotInEra ->
    HFI.ELet (HFI.ERelSlotToTime (HFI.EVar slotInEra)) $ \timeInEra ->
    HFI.ERelToAbsTime (HFI.EVar timeInEra)

-- | Execute the 'qryEpochStartTimeOfSlot' 'InterpreterQuery' against the
-- current ledger state.
--
-- Will return a 'HF.PastHorizonException' when the current chain doesn't give
-- us enough information about the given slot. For example, when syncing from
-- scratch, we don't know yet when the transition to Shelley will happen, so we
-- can't do conversions for slots from the Shelley era.
getEpochStartTimeOfSlot
  :: forall blk
  . ( RunNode blk
    , Protocol.Protocol IO blk
    )
  => Protocol.BlockType blk
  -> Protocol.ProtocolInfoArgs IO blk
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> SlotNo
  -> IO (Either HFI.PastHorizonException UTCTime)
getEpochStartTimeOfSlot blkType protocol nodeKernel slotNo = do
    interpreter <- answerQuery blkType protocol nodeKernel HF.compatGetInterpreter
    -- NOTE: ask for an interpreter sparingly, as it is not super cheap. You can
    -- use that same interpreter to answer many queries.
    --
    -- When the interpreter returns 'HFI.PastHorizonException' for a certain
    -- 'InterpreterQuery', try obtaining a new interpreter. The chain might have
    -- advanced and the new ledger state might contain more information so that
    -- the 'InterpreterQuery' /can/ be answered.
    --
    -- For example, when starting up with an empty chain, the interpreter
    -- obtained from the empty ledger state won't be able to answer queries
    -- about the a slot from the Shelley era, as it doesn't know when the
    -- Shelley transition takes place. So while the node is syncing, you can
    -- periodically try to obtain a new interpreter whenever it returns
    -- 'HFI.PastHorizonException'. Note that doing this after every new block is
    -- pointless and wasteful. Doing it when the ledger hasn't changed is even
    -- more pointless.
    return $
      WCT.fromRelativeTime systemStart <$>
        HFI.interpretQuery interpreter (qryEpochStartTimeOfSlot slotNo)
  where
    systemStart =
          getSystemStart
        . configBlock
        . pInfoConfig
        . Protocol.protocolInfo
        $ protocol
