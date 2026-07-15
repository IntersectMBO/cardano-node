{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.PullFiction.WorkloadRunner
  ( TargetWorker
  , runWorkload
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (myThreadId)
import GHC.Conc (labelThread)
-----------
-- async --
-----------
import Control.Concurrent.Async qualified as Async
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
------------------
-- pull-fiction --
------------------
import Cardano.Benchmarking.PullFiction.Config.Runtime qualified as Runtime
import Cardano.Benchmarking.PullFiction.Internal.Pipe qualified as Pipe

--------------------------------------------------------------------------------
-- Workload runner.
--------------------------------------------------------------------------------

-- | A worker callback that runs inside a labeled 'Async.Async'.
--
-- 'runWorkload' spawns a labeled async per target that calls this callback with
-- the target's pre-built fetch actions ('Runtime.targetFetcher'). The callback
-- receives:
--
-- 1. The fully resolved 'Runtime.Target' (carries addr, port, batch size, and
--    target name for error attribution).
-- 2. @fetchPayload@: blocking fetch that claims one rate-limit slot and returns
--    one @payload@.
-- 3. @tryFetchPayload@: non-blocking variant that returns @Nothing@ when
--    rate-limited or when the queue is empty, and otherwise behaves like
--    @fetchPayload@.
--
-- The fetch actions come from 'Runtime.targetFetcher', which 'Config.Runtime'
-- built by wrapping the pipe's fetcher through the workload's recycler: the rate
-- limit, on-exhaustion policy and recycle-on-dequeue all happen inside it. This
-- module holds no fetch, rate-limit or recycle logic, and knows nothing about
-- the pipe's queues or the recycler. The callback's only responsibilities are
-- delivering the payload and application-level bookkeeping.
--
-- The thread is already labeled @workloadName\/targetName@ by 'runWorkload'.
-- The callback body runs for the lifetime of the generator. It should not
-- create its own async or label its own thread. 'runWorkload' handles both.
type TargetWorker key input payload
  =  Runtime.Target key input payload -- ^ The resolved target.
  -> IO payload                       -- ^ Blocking fetch (rate-limited, recycles inputs).
  -> IO (Maybe payload)               -- ^ Non-blocking fetch (rate-limited, recycles inputs).
  -> IO ()                            -- ^ Worker body (runs inside labeled async).

-- | Run a load-generation workload: for each target, spawn a labeled async and
-- call the worker callback inside it with the target's two fetch actions.
--
-- Rate limiter creation, the shared\/independent decision, and the recycling
-- fetch are all handled by 'Runtime.resolve' (the fetch lives on
-- 'Runtime.targetFetcher'). This function only spawns and labels the workers.
--
-- For each target the function:
--
-- 1. Reads the pre-built 'Pipe.PayloadFetcher' from 'Runtime.targetFetcher'.
-- 2. Computes a thread label: @workloadName ++ \"\/\" ++ targetName@.
-- 3. Creates an 'Async.Async' that labels the thread, then runs the worker
--    callback with the two fetch actions.
--
-- Returns the list of worker asyncs (__unlinked__). Callers decide how to
-- monitor them: 'Main.hs' links them for immediate propagation. The test
-- harness polls synchronously so Tasty's 'withResource' can cache the
-- exception.
runWorkload
  :: Runtime.Workload key input payload
  -> TargetWorker key input payload
  -> IO [Async.Async ()]
runWorkload workload targetWorker =
  mapM
    (\target -> do
      let fetcher = Runtime.targetFetcher target
          -- Always labeled threads.
          threadLabel =
            Runtime.workloadName workload ++ "/" ++ Runtime.targetName target
      -- Return async (unlinked, caller decides monitoring strategy).
      async <- Async.async $ do
        tid <- myThreadId
        labelThread tid threadLabel
        targetWorker target
          (Pipe.fetchPayload fetcher)
          (Pipe.tryFetchPayload fetcher)
      pure async
    )
    (Map.elems (Runtime.targets workload))
