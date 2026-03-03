{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

-- | Resolved runtime configuration.
--
-- "Cardano.Benchmarking.PullFiction.Config.Validated" has already validated
-- every invariant and cascaded top-level defaults into workloads. This module
-- creates the STM resources (rate limiters, pipeline queues) that downstream
-- code needs to run the load generator.
--
-- Rate limiters are created during resolution. Each validated target carries
-- a pre-computed 'Validated.rateLimitKey' that encodes the sharing boundary:
--
-- * @\@global@: one 'RL.RateLimiter' shared by all targets across all
--   workloads.
-- * @workloadName@: all targets in the workload share one
--   'RL.RateLimiter' at the configured TPS.
-- * @workloadName.targetName@: each target gets its own
--   'RL.RateLimiter' at the full configured TPS.
-- * No rate limit: each target gets 'RL.newUnlimited'.
--
-- Pipeline queues are created during resolution:
--
-- Each workload gets its own unbounded input queue ('TQueue') and bounded
-- payload queue ('TBQueue', capacity 8192). The input queue is unbounded so
-- that bulk-loading initial inputs and recycling outputs never block; the
-- payload queue is the sole source of backpressure in the pipeline. Initial
-- inputs are partitioned equally across workloads (contiguous chunks; the last
-- workload absorbs the remainder). All targets within a workload share the
-- same queues. Spawning builders is the caller's responsibility.
module Cardano.Benchmarking.PullFiction.Config.Runtime
  ( -- * Runtime.
    Runtime
  , config, builders, workloads
    -- * Pipe.
  , Pipe
  , pipeInputQueue, pipePayloadQueue, pipeRecycle
    -- * Builder.
  , Builder
  , parsedBuilder, builderName, builderPipe
    -- * OnExhaustion.
  , Raw.OnExhaustion (..)
    -- * Workload.
  , Workload
  , workloadName, targets
    -- * Target.
  , Target
  , targetName, targetPipe
  , rateLimiter, maxBatchSize, onExhaustion
  , targetAddr, targetPort
    -- * Resolution.
  , resolve
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Foldable (foldlM, toList)
import Numeric.Natural (Natural)
----------------
-- containers --
----------------
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
---------------------
-- pull-fiction --
---------------------
import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw
import Cardano.Benchmarking.PullFiction.Config.Validated qualified as Validated
import Cardano.Benchmarking.PullFiction.Internal.RateLimiter qualified as RL

--------------------------------------------------------------------------------

-- | Fully resolved top-level configuration.
--
-- Carries the original 'Validated.Config' (for fields like initial inputs
-- that need no resolution) and the resolved workloads.
data Runtime input payload = Runtime
  { -- | The original validated configuration.
    config :: !(Validated.Config input)
    -- | One builder per workload. The builder sits between the input queue
    -- and the payload queue: it pulls inputs, produces payloads, and enqueues
    -- @(payload, [input])@ pairs for workers to deliver.
    --
    -- The list order matches the alphabetical workload name order (same as
    -- 'Map.elems' on 'workloads').
  , builders :: [Builder input payload]
    -- | Resolved workloads, keyed by workload name.
  , workloads :: !(Map String (Workload input payload))
  }

-- | Pipeline queues for a workload.
--
-- Holds the input queue, payload queue, and recycling action.
-- All targets within the same workload share the same 'Pipe' instance.
data Pipe input payload = Pipe
  { -- | Input queue feeding the builder. The builder reads inputs from here
    -- to produce payloads.
    --
    -- Unbounded ('TQueue'): input queues must never block on write. At
    -- startup, all initial inputs are bulk-loaded before any consumer is
    -- running; a bounded queue would deadlock when the initial input count
    -- exceeds the capacity. During steady-state, 'pipeRecycle' writes recycled
    -- inputs back here after each delivery; if this queue were bounded, a
    -- burst of recycled inputs could stall the worker thread inside STM. The
    -- payload queue is the only bounded queue in the pipeline and provides all
    -- the backpressure the builder needs.
    pipeInputQueue :: !(STM.TQueue input)
    -- | Payload queue: the builder writes @(payload, [input])@ pairs here;
    -- workers read from here via the rate-limited fetcher in
    -- "Cardano.Benchmarking.PullFiction.WorkloadRunner". Bounded ('TBQueue',
    -- capacity 8192); the sole source of backpressure in the pipeline.
  , pipePayloadQueue :: !(STM.TBQueue (payload, [input]))
    -- | Recycle consumed inputs back to 'pipeInputQueue' after delivery.
    -- Returns an STM action so callers can compose it with other transactions.
    --
    -- NOTE: recycling happens on /delivery/, not on downstream /confirmation/.
    -- This is by design: the pipeline operates in closed-loop mode where
    -- consumed inputs are immediately available for the next payload. For
    -- example, in a Cardano deployment the node may later drop a submitted
    -- transaction from its mempool (e.g. due to rollback or mempool overflow),
    -- causing the recycled inputs to reference UTxOs that do not exist
    -- on-chain. This is an accepted trade-off: it enables indefinite-duration
    -- runs without pre-generating all payloads, at the cost of assuming that
    -- delivered payloads will eventually be confirmed.
    --
    -- Because the input queue is unbounded ('TQueue'), 'pipeRecycle' never
    -- blocks regardless of how many inputs are returned in a single batch.
  , pipeRecycle :: [input] -> STM.STM ()
  }

-- | Builder resources for one workload.
--
-- A 'Builder' is a 'Pipe' paired with a 'Raw.Builder' that describes the
-- payload shape for the workload. The builder pulls inputs from
-- 'pipeInputQueue', produces a payload, and enqueues the @(payload, [input])@
-- pair to 'pipePayloadQueue' for workers to deliver. There is exactly one
-- 'Builder' per workload.
data Builder input payload = Builder
  { -- | Resolved builder for this workload.
    parsedBuilder :: !Raw.Builder
    -- | Unique name to be able to label builders.
    -- Now, as it is one 'Builder' per 'Workload', it's the 'Workload' name.
  , builderName :: String
    -- | Pipeline queues for this workload. The builder reads inputs from
    -- 'pipeInputQueue' and writes @(payload, [input])@ pairs to
    -- 'pipePayloadQueue'. All targets within the workload share this same
    -- 'Pipe'.
  , builderPipe :: !(Pipe input payload)
  }

-- | Fully resolved workload.
--
-- All cascading defaults have been applied, rate limiters and pipeline queues
-- have been created for each target.
--
-- The builder resources ('Pipe' and 'Raw.Builder') live in 'Builder' on the
-- 'Runtime', not here. Each workload has exactly one corresponding 'Builder'
-- (same list order as 'Map.elems' on 'workloads').
data Workload input payload = Workload
  { -- | Unique name identifying this workload.
    workloadName :: !String
    -- | Resolved targets, keyed by target name. Each target carries its config,
    -- rate limiter, and pipeline queues.
  , targets :: !(Map String (Target input payload))
  }

-- | A fully resolved target with rate limiter and pipeline.
--
-- All targets within the same workload share the same 'Pipe' (same underlying
-- queues). Targets with the same 'Validated.rateLimitKey' also share the same
-- 'RL.RateLimiter' (same TVars).
data Target input payload = Target
  { -- | Unique name identifying this target.
    targetName :: !String
    -- | Pipeline queues for this target.
    --
    -- All targets within the same workload share the same 'Pipe' instance (same
    -- underlying queues).
  , targetPipe :: !(Pipe input payload)
    -- | Pre-created rate limiter for this target.
    --
    -- Targets with the same 'Validated.rateLimitKey' share the same
    -- 'RL.RateLimiter' (same TVars); targets with unique keys each get their
    -- own.
  , rateLimiter :: !RL.RateLimiter
    -- | Resolved max tokens per request for this target.
  , maxBatchSize :: !Natural
    -- | What to do when the payload queue is exhausted.
  , onExhaustion :: !Raw.OnExhaustion
    -- | IP address or hostname of the target endpoint.
  , targetAddr :: !String
    -- | Port number of the target endpoint.
  , targetPort :: !Int
  }

instance Show (Target input payload) where
  showsPrec _ t = showString (targetName t)

instance Eq (Target input payload) where
  a == b = targetName a == targetName b

--------------------------------------------------------------------------------
-- Resolution.
--------------------------------------------------------------------------------

-- | Limiter cache: maps a sharing key to an already-created rate limiter.
--
-- Threaded across workloads so that top-level Shared limiters are reused.
type LimiterCache = Map String RL.RateLimiter

-- | Resolve a parsed 'Validated.Config' into a 'Runtime' by creating rate
-- limiters and setting up pipeline queues.
--
-- Initial inputs are taken from 'Validated.initialInputs' (provided by the
-- caller to 'Validated.validate') and partitioned equally across workloads
-- chunks; the last workload absorbs the remainder).
--
-- Each workload gets its own unbounded input queue ('TQueue') and bounded
-- payload queue ('TBQueue', capacity 8192). The input queue is unbounded so
-- that bulk-loading initial inputs and recycling outputs never block; the
-- payload queue is the sole source of backpressure in the pipeline. Spawning
-- builders is the caller's responsibility; each 'Builder' exposes 'pipe' for
-- access to the pipeline queues.
--
-- All validation and cascading has been done by
-- "Cardano.Benchmarking.PullFiction.Config.Validated".
resolve :: Validated.Config input -> IO (Runtime input payload)
resolve validatedConfig = do
  let validatedWorkloadsMap = Validated.workloads validatedConfig
  -- Distribute initial inputs equally across workloads, keyed by workload name.
  -- Both Maps share the same ascending key order, so zip + fromAscList is safe.
  let inputsByWorkloadMap =
        let workloadsCount = Map.size validatedWorkloadsMap
            inputChunks    = partitionInputs
                               workloadsCount
                               (toList (Validated.initialInputs validatedConfig))
        in  Map.fromAscList
              (zip (Map.keys validatedWorkloadsMap) inputChunks)
  -- Resolve builders first: each builder creates its own Pipe (input queue,
  -- payload queue, recycle action) and loads initial inputs.
  resolvedBuilders <- mapM
    (\validatedWorkload -> do
      let workloadInputs = inputsByWorkloadMap Map.! Validated.workloadName validatedWorkload
          workloadBuilder = Validated.builder validatedWorkload
      resolveBuilder validatedWorkload workloadBuilder workloadInputs
    )
    validatedWorkloadsMap
  -- Resolve workloads: assign the pipe from each builder to its targets and
  -- resolve each target's rate limiter. The limiter cache is threaded as a
  -- pure accumulator so that top-level Shared limiters are reused.
  (resolvedWorkloads, _) <-
    foldlM
      (\(acc, cache) (wlName, validatedWorkload) -> do
        let resolvedBuilder = resolvedBuilders Map.! wlName
        (resolved, cache') <-
          resolveWorkload validatedWorkload cache (builderPipe resolvedBuilder)
        pure (Map.insert wlName resolved acc, cache')
      )
      (Map.empty, Map.empty)
      (Map.toAscList validatedWorkloadsMap)
  -- Assemble the final runtime.
  pure Runtime
    { -- The previous state for reference.
      config                 = validatedConfig
      -- One builder per workload (alphabetical order, same as Map.elems).
    , builders               = Map.elems resolvedBuilders
      -- Map String Validated -> Map String Runtime.
    , workloads              = resolvedWorkloads
    }

--------------------------------------------------------------------------------
-- Builder resolution.
--------------------------------------------------------------------------------

-- | Create the builder resources for a single workload: input queue, payload
-- queue, recycle action, and initial input loading.
--
-- The input queue is unbounded ('TQueue') so that bulk-loading initial inputs
-- and recycling outputs never block. See 'Pipe' for the full rationale.
--
-- The payload queue is bounded ('TBQueue', capacity 8192); the sole source of
-- backpressure. The builder blocks here when workers cannot consume fast
-- enough.
--
-- The returned 'Pipe' is shared with all targets in the workload; this is how
-- closed-loop recycling and queue sharing work.
resolveBuilder
  :: Validated.Workload
  -> Raw.Builder
  -- | Initial inputs for this workload.
  -> [input]
  -> IO (Builder input payload)
resolveBuilder validatedWorkload validatedBuilder initialInputs = do
  -- Input queue: unbounded (TQueue) so that bulk-loading initial inputs and
  -- recycling outputs never block. See 'Pipe' for the full rationale.
  inputQueue <- STM.newTQueueIO
  STM.atomically $ mapM_ (STM.writeTQueue inputQueue) initialInputs
  -- Payload queue: bounded (TBQueue, capacity 8192); the sole source of
  -- backpressure. The builder blocks here when workers cannot consume fast
  -- enough. The capacity must be large enough to absorb GC pauses at high TPS
  -- (e.g. 100k TPS drains 256 entries in ~2.5 ms).
  payloadQ <- STM.newTBQueueIO 8192
  let thePipe = Pipe
        { pipeInputQueue   = inputQueue
        , pipePayloadQueue = payloadQ
          -- pipeRecycle: write recycled inputs back to the unbounded input
          -- queue. Because TQueue has no capacity limit, this can never stall
          -- the worker thread inside STM.
        , pipeRecycle       = \is -> mapM_ (STM.writeTQueue inputQueue) is
        }
  pure Builder
    { parsedBuilder = validatedBuilder
    , builderName   = Validated.workloadName validatedWorkload
    , builderPipe   = thePipe
    }

--------------------------------------------------------------------------------
-- Workload resolution.
--------------------------------------------------------------------------------

-- | Resolve a single workload: assign the pre-created 'Pipe' to each target
-- and resolve each target's rate limiter.
--
-- The 'Pipe' is created by 'resolveBuilder' and passed in so that the 'Builder'
-- and all targets share the same underlying queues.
--
-- Cascading defaults and conflict checks have already been performed by
-- "Cardano.Benchmarking.PullFiction.Config.Validated"; this function only
-- creates rate limiters.
resolveWorkload
  :: Validated.Workload
  -- | Limiter cache (threaded as a pure accumulator).
  -> LimiterCache
  -- | Pipe for this workload (created by 'resolveBuilder').
  -> Pipe input payload
  -> IO (Workload input payload, LimiterCache)
resolveWorkload validatedWorkload cache0 thePipe = do
  let wlName = Validated.workloadName validatedWorkload
      validatedTargets = Validated.targets validatedWorkload
  (resolvedTargets, cache') <-
    foldlM
      (\(acc, cache) (tName, validatedTarget) -> do
        (resolved, cache'') <-
          resolveTarget cache thePipe validatedTarget
        pure (Map.insert tName resolved acc, cache'')
      )
      (Map.empty, cache0)
      (Map.toAscList validatedTargets)
  pure ( Workload { workloadName = wlName, targets = resolvedTargets }
       , cache'
       )

--------------------------------------------------------------------------------
-- Target resolution.
--------------------------------------------------------------------------------

-- | Resolve a single target: look up or create its rate limiter from the
-- cache, then build the 'Target' record.
resolveTarget
  :: LimiterCache
  -> Pipe input payload
  -> Validated.Target
  -> IO (Target input payload, LimiterCache)
resolveTarget cache thePipe validatedTarget = do
  (limiter, cache') <- getOrCreateLimiter cache validatedTarget
  pure ( Target
           { targetName   = Validated.targetName validatedTarget
           , targetPipe   = thePipe
           , rateLimiter  = limiter
           , maxBatchSize = Validated.targetMaxBatchSize validatedTarget
           , onExhaustion = Validated.onExhaustion validatedTarget
           , targetAddr   = Validated.addr validatedTarget
           , targetPort   = Validated.port validatedTarget
           }
       , cache'
       )

-- | Look up or create a 'RL.RateLimiter' for a target.
--
-- * 'Nothing' rate limit source → 'RL.newUnlimited' (no cache entry).
-- * Otherwise, use the pre-computed 'Validated.rateLimitKey' as the cache
--   key. If the key already exists the existing limiter is reused; otherwise a
--   'RL.newTokenBucket' is created and inserted.
getOrCreateLimiter
  :: LimiterCache -> Validated.Target
  -> IO (RL.RateLimiter, LimiterCache)
getOrCreateLimiter cache target =
  case Validated.rateLimitSource target of
    Nothing  -> pure (RL.newUnlimited, cache)
    Just src -> do
      let key      = Validated.rateLimitKey src
          tpsValue = Raw.tps (Validated.rateLimit src)
      case Map.lookup key cache of
        Just existing -> pure (existing, cache)
        Nothing       -> do
          limiter <- RL.newTokenBucket tpsValue
          pure (limiter, Map.insert key limiter cache)

--------------------------------------------------------------------------------
-- Input partitioning.
--------------------------------------------------------------------------------

-- | Split a list into @n@ contiguous chunks of roughly equal size.
-- The last chunk absorbs any remainder.
partitionInputs :: Int -> [a] -> [[a]]
partitionInputs n xs
  | n <= 1    = [xs]
  | otherwise = go xs n
  where
    chunkSize = length xs `div` n
    go remaining 1 = [remaining]
    go remaining k =
      let (chunk, rest) = splitAt chunkSize remaining
      in  chunk : go rest (k - 1)
