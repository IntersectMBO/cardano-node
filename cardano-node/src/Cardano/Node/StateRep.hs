module Cardano.Node.StateRep where

data NodeState blk
  -- All node states prior to tracing system going online are effectively invisible.
  = NodeTracingOnlineConfiguring    -- <- initTraceDispatcher
  | NodeConfigCompleteLoadingKernel -- just before Node.run
  | NodeChainDBOpening (TraceEvent blk)
  -- TraceOpenEvent              (TraceOpenEvent               blk)
      -- StartedOpeningDB
      -- StartedOpeningImmutableDB
      -- OpenedImmutableDB
      -- StartedOpeningVolatileDB
      -- OpenedVolatileDB
      -- StartedOpeningLgrDB
  -- TraceLedgerReplayEvent      (LedgerReplayEvent            blk)
      -- ReplayFromGenesis
      -- ReplayFromSnapshot
      -- ReplayedBlock
  -- TraceOpenEvent              (TraceOpenEvent               blk)
      -- OpenedLgrDB
  -- TraceInitChainSelEvent      (TraceInitChainSelEvent       blk)
      -- StartedInitChainSelection
      -- InitalChainSelected
  -- TraceOpenEvent              (TraceOpenEvent               blk)
      -- OpenedDB
  | NodeKernelOnlineSyncing         -- just before onKernel in rnNodeKernelHook
  | NodeSyncing (TraceEvent blk)
  -- TraceAddBlockEvent          (TraceAddBlockEvent           blk)
    -- ChainDB.AddedToCurrentChain
  | NodeShutdownComplete            -- <- finally in handleNodeWithTracers
