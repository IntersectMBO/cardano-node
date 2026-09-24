{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

-- | Resolved runtime configuration. Creates STM resources (rate limiters,
-- pipeline queues) from a 'Validated.Config'.
--
-- Rate-limiter sharing is keyed by 'Validated.rateLimitKey':
-- * @\@global@: one limiter for all targets across all workloads.
-- * @workloadName@: one per workload.
-- * @workloadName.targetName@: one per target.
-- * No rate limit: 'RL.newUnlimited'.
module Cardano.Benchmarking.PullFiction.Config.Runtime
  ( -- * Runtime.
    Runtime
  , config, observers, builders, workloads, asyncs
  -- User supplied handles needed to create the runtime "object".
  , ObserverHandle (..)
  , BuilderHandle (..)
    -- * Pipe.
  , Pipe
  , pipeInputQueue, pipeInputCount, pipePayloadQueue, pipeRecycle
  , pipeCounters
    -- * Counters and Stats.
  , Counters (..)
  , Stats (..)
    -- * Observer.
  , Observer
  , observerName, observerAsync
    -- * Builder.
  , Builder
  , builderName, builderPipe, builderAsync, recyclerAsync, statsAsync
    -- * Workload.
  , Workload
  , workloadName, targets
    -- * OnExhaustion.
  , Raw.OnExhaustion (..)
    -- * Target.
  , Target
  , targetName
  , targetPipe
  , rateLimiter
  , maxBatchSize, onExhaustion
  , targetAddr, targetPort
    -- * Resolution.
  , resolve
    -- * Helpers.
  , partitionInputs
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (myThreadId, threadDelay)
import Control.Monad (forever, replicateM)
import Data.Foldable (foldlM, toList)
import GHC.Conc (labelThread)
import Numeric.Natural (Natural)
-----------
-- async --
-----------
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
----------------
-- containers --
----------------
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
------------------
-- pull-fiction --
------------------
import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw
import Cardano.Benchmarking.PullFiction.Config.Validated qualified as Validated
import Cardano.Benchmarking.PullFiction.Internal.RateLimiter qualified as RL

--------------------------------------------------------------------------------

-- | Fully resolved top-level configuration.
data Runtime input payload = Runtime
  { -- | The original validated configuration.
    config    :: !(Validated.Config input)
    -- | Resolved observers, keyed by name.
  , observers :: !(Map String Observer)
    -- | One per workload, alphabetical order (as 'Map.elems' on 'workloads').
  , builders  :: [Builder input payload]
    -- | Resolved workloads, keyed by name.
  , workloads :: !(Map String (Workload input payload))
    -- | All asyncs (observers + builders + recyclers), linked.
    -- Caller should append their own worker asyncs for cleanup.
  , asyncs    :: [Async ()]
  }

--------------------------------------------------------------------------------

-- | Caller-provided observer handle. 'resolve' spawns 'ohRun' in a labeled,
-- linked async and uses the subscription for 'Raw.RecycleOnConfirm' recycling.
data ObserverHandle key confirmed = ObserverHandle
  { -- | IO action that runs the observer (e.g. a NodeToNode connection).
    ohRun        :: !(IO ())
    -- | Subscribe to the observer's broadcast channel.
    -- 'Right' = confirmed (recycle output inputs).
    -- 'Left'  = orphaned  (recycle original inputs).
  , ohSubscribe  :: !(IO (STM.TChan (Either confirmed confirmed)))
    -- | Extract the recycling key from a confirmed or orphaned event.
  , ohExtractKey :: !(confirmed -> key)
  }

-- | Caller-provided build handle. 'resolve' spawns a builder async that pulls
-- inputs via 'bhInputsPerBatch' and produces payloads.
data BuilderHandle key input payload = BuilderHandle
  { -- | How many inputs the builder needs per call to 'bhBuildPayload'.
    bhInputsPerBatch :: !Natural
    -- | Returns @Just (confirmation key, payload, recyclable outputs)@ on
    -- success. Returns 'Nothing' to drop the consumed inputs without
    -- enqueueing a payload or recycling — e.g. when an input is dust the
    -- builder cannot turn into a valid output. The runtime loops on
    -- 'Nothing'; the inputs are simply gone from the pipeline.
  , bhBuildPayload   :: [input] -> IO (Maybe (key, payload, [input]))
  }

--------------------------------------------------------------------------------

-- | A resolved observer with its lifecycle managed by 'resolve'.
data Observer = Observer
  { -- | Key from the config's @\"observers\"@ object.
    observerName  :: !String
    -- | Linked async running the observer connection.
  , observerAsync :: !(Async ())
  }

-- | Pipeline queues for a workload. All targets share the same 'Pipe'.
data Pipe input payload = Pipe
  { -- | Unbounded: must never block on write (bulk-load at startup, recycle
    -- bursts at steady-state). Backpressure comes from 'pipePayloadQueue'.
    pipeInputQueue   :: !(STM.TQueue input)
    -- | Cached size of @pipeInputQueue@. 'STM.TQueue' has no length operation,
    -- so we maintain a counter alongside it (updated atomically with every
    -- read/write) for the queue-depth tracer.
  , pipeInputCount   :: !(STM.TVar Int)
    -- | Bounded (capacity 8192); sole source of backpressure.
  , pipePayloadQueue :: !(STM.TBQueue (payload, [input]))
    -- | Recycle consumed inputs back to 'pipeInputQueue'.
  , pipeRecycle      :: [input] -> STM.STM ()
    -- | Cumulative counters (built / recycled / dropped / submitted),
    -- snapshotted by the queue-depth stats async and reported in 'Stats'.
  , pipeCounters     :: !Counters
  }

-- | Per-workload cumulative counters. Written on the hot paths (build loop,
-- recycler async, dust drop, worker submit) and read by the stats async.
data Counters = Counters
  { -- | Increments on every successful @bhBuildPayload@ that produced a
    -- 'Just' result (i.e. a payload was enqueued).
    cBuilt             :: !(STM.TVar Int)
    -- | Increments on every @Right@ event from the chain follower that
    -- matched a 'pendingRecycle' entry (normal confirmation path).
  , cRecycledConfirmed :: !(STM.TVar Int)
    -- | Increments on every @Left@ event from the chain follower that
    -- matched a 'pendingRecycle' entry (orphan path, original inputs
    -- recovered after rollback).
  , cRecycledOrphan    :: !(STM.TVar Int)
    -- | Increments every time @bhBuildPayload@ returns 'Nothing' (dust
    -- drop). These inputs leave the pipeline silently otherwise.
  , cDropped           :: !(STM.TVar Int)
    -- | Increments on every payload pulled from 'pipePayloadQueue' by a
    -- worker (any target). \"Submitted\" here means \"handed to the
    -- TxSubmission2 protocol client\", not \"acknowledged by the remote\".
  , cSubmitted         :: !(STM.TVar Int)
  }

-- | A snapshot of the queue depths and counters for one workload, produced
-- every 'statsIntervalMicros' microseconds by the stats async.
data Stats = Stats
  { statsInputQueue        :: !Int
  , statsPayloadQueue      :: !Int
  , statsPendingRecycle    :: !Int
  , statsBuilt             :: !Int
  , statsRecycledConfirmed :: !Int
  , statsRecycledOrphan    :: !Int
  , statsDropped           :: !Int
  , statsSubmitted         :: !Int
  }

-- | Builder resources for one workload (exactly one per workload).
data Builder input payload = Builder
  { -- | Same as the workload name.
    builderName   :: !String
    -- | Shared with all targets in the workload.
  , builderPipe   :: !(Pipe input payload)
    -- | Linked async running the builder loop.
  , builderAsync  :: !(Async ())
    -- | 'Raw.RecycleOnConfirm' recycler; 'Nothing' for other strategies.
  , recyclerAsync :: !(Maybe (Async ()))
    -- | Periodic queue-depth tracer async.
  , statsAsync    :: !(Async ())
  }

-- | Fully resolved workload. Builder resources live in 'Builder' on the
-- 'Runtime', not here.
data Workload input payload = Workload
  { -- | Unique name identifying this workload.
    workloadName :: !String
    -- | Resolved targets, keyed by name.
  , targets      :: !(Map String (Target input payload))
  }

-- | A fully resolved target. Targets in the same workload share a 'Pipe';
-- targets with the same 'Validated.rateLimitKey' share a 'RL.RateLimiter'.
data Target input payload = Target
  { -- | Unique name identifying this target.
    targetName   :: !String
    -- | Shared with all targets in the same workload.
  , targetPipe   :: !(Pipe input payload)
    -- | Shared when 'Validated.rateLimitKey' matches.
  , rateLimiter  :: !RL.RateLimiter
    -- | Resolved max tokens per request for this target.
  , maxBatchSize :: !Natural
    -- | What to do when the payload queue is exhausted.
  , onExhaustion :: !Raw.OnExhaustion
    -- | IP address or hostname of the target endpoint.
  , targetAddr   :: !String
    -- | Port number of the target endpoint.
  , targetPort   :: !Int
  }

--------------------------------------------------------------------------------
-- Resolution.
--------------------------------------------------------------------------------

-- | Limiter cache: maps a sharing key to an already-created rate limiter.
--
-- Threaded across workloads so that top-level Shared limiters are reused.
type LimiterCache = Map String RL.RateLimiter

-- | Resolve a 'Validated.Config' into a 'Runtime': create observers, rate
-- limiters, pipeline queues, and spawn builder asyncs.
-- Initial inputs are partitioned equally across workloads (last absorbs
-- remainder).
resolve
  :: Ord key
  -- | Builder factory (index, name, config).
  => (Int -> String -> Raw.Builder  -> IO (BuilderHandle  key input payload))
  -- | Observer factory (index, name, config).
  -> (Int -> String -> Raw.Observer -> IO (ObserverHandle key confirmed))
  -- | Recycle callback (e.g. for tracing).
  -- Receives the builder name, the recycled transaction's key, whether
  -- this is an orphan recycle, and the recycled inputs.
  -> (String -> key -> Bool -> [input] -> IO ())
  -- | Stats callback fired every 'statsIntervalMicros' microseconds.
  -- Receives the builder name and a snapshot of the three queue depths
  -- plus the five cumulative counters (see 'Stats'). A no-op suppresses
  -- the trace; the periodic async still runs.
  -> (String -> Stats -> IO ())
  -> Validated.Config input
  -> IO (Runtime input payload)
resolve mkBuilderFn mkObserverFn onRecycle onStats validatedConfig = do
  -- Create all observers via the factory callback.
  handleResults <- Map.fromAscList <$> mapM
    (\(ix, (obsName, rawObs)) -> do
      -- The ObserverHandle's run action is spawned in a labeled, linked async
      -- (same pattern as builders).
      obsHandle <- mkObserverFn ix obsName rawObs
      obsAsync <- Async.async $ do
        tid <- myThreadId
        labelThread tid ("observer/" ++ obsName)
        ohRun obsHandle
      Async.link obsAsync
      -- The subscription pair is kept locally for `RecycleOnConfirm`.
      pure ( obsName
           , ( Observer { observerName  = obsName
                        , observerAsync = obsAsync
                        }
             , obsHandle
             )
           )
    )
    -- Zero-based index and name provided to the observer factory.
    (zip
      [0..]
      (Map.toAscList (Validated.observers validatedConfig))
    )
  let resolvedObservers = Map.map fst handleResults
      observerHandles   = Map.map snd handleResults
  let validatedWorkloadsMap = Validated.workloads validatedConfig
  -- Distribute initial inputs equally across workloads, keyed by workload name.
  -- Both Maps share the same ascending key order, so zip + fromAscList is safe.
  let inputsByWorkloadMap =
        let workloadsCount = Map.size validatedWorkloadsMap
            inputChunks = partitionInputs
                            workloadsCount
                            (toList (Validated.initialInputs validatedConfig))
        in  Map.fromAscList
              (zip (Map.keys validatedWorkloadsMap) inputChunks)
  -- Create all builders via the factory callback.
  -- Resolve builders first: each builder creates its own Pipe (input queue,
  -- payload queue, recycle action) and loads initial inputs.
  resolvedBuilders <- Map.fromAscList <$> mapM
    (\(ix, (wlName, validatedWorkload)) -> do
      let workloadInputs = inputsByWorkloadMap Map.! wlName
          workloadBuilder = Validated.builder validatedWorkload
      builder <- resolveBuilder
                   mkBuilderFn
                   observerHandles
                   onRecycle
                   onStats
                   ix
                   validatedWorkload
                   workloadBuilder
                   workloadInputs
      pure (wlName, builder)
    )
    -- Zero-based index and name provided to the builder factory.
    (zip
      [0..]
      (Map.toAscList validatedWorkloadsMap)
    )
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
  -- Collect all asyncs: observers + builders + recyclers + stats.
  let builderList = Map.elems resolvedBuilders
      observerAsyncs = map observerAsync (Map.elems resolvedObservers)
      builderAsyncs = concatMap
        (\b -> builderAsync b
             : statsAsync b
             : maybe [] pure (recyclerAsync b))
        builderList
  -- Assemble the final runtime.
  pure Runtime
    { config    = validatedConfig
    , observers = resolvedObservers
    , builders  = builderList
    , workloads = resolvedWorkloads
    , asyncs    = observerAsyncs ++ builderAsyncs
    }

--------------------------------------------------------------------------------
-- Builder resolution.
--------------------------------------------------------------------------------

-- | Create builder resources for one workload (queues, recycle action, builder
-- async). The returned 'Pipe' is shared with all targets.
resolveBuilder
  :: Ord key
  -- | Builder factory (index, name, config).
  => (Int -> String -> Raw.Builder -> IO (BuilderHandle key input payload))
  -- | Observer handles keyed by name, for 'RecycleOnConfirm' subscriptions.
  -> Map String (ObserverHandle key confirmed)
  -- | Recycle callback (e.g. for tracing).
  -> (String -> key -> Bool -> [input] -> IO ())
  -- | Stats callback (e.g. for tracing). Receives builder name and a
  -- snapshot of the three queue depths plus five cumulative counters.
  -> (String -> Stats -> IO ())
  -- | Zero-based builder index.
  -> Int
  -- | Validated workload configuration.
  -> Validated.Workload
  -- | Builder configuration (opaque type + params).
  -> Raw.Builder
  -- | Initial inputs for this workload.
  -> [input]
  -> IO (Builder input payload)
resolveBuilder
  mkBuilderFn
  observerHandles
  onRecycle
  onStats
  builderIndex
  validatedWorkload
  validatedBuilder
  initialInputs = do
  -- Input queue: unbounded (TQueue) so that bulk-loading initial inputs and
  -- recycling outputs never block. See 'Pipe' for the full rationale.
  inputQueue <- STM.newTQueueIO
  -- Counter mirroring the input queue size; TQueue has no length op.
  -- Initialised outside STM so we don't recompute 'length initialInputs'
  -- per atomically retry.
  let initialCount = length initialInputs
  inputCount <- STM.newTVarIO initialCount
  STM.atomically $ mapM_ (STM.writeTQueue inputQueue) initialInputs
  -- Payload queue: bounded (TBQueue, capacity 8192); the sole source of
  -- backpressure. The builder blocks here when workers cannot consume fast
  -- enough. The capacity must be large enough to absorb GC pauses at high TPS
  -- (e.g. 100k TPS drains 256 entries in ~2.5 ms).
  payloadQ <- STM.newTBQueueIO 8192
  -- Cumulative counters live alongside the queues so workers (submitted),
  -- the builder loop (built / dropped), and the recycler async (confirmed /
  -- orphan) can all reach them via 'pipeCounters'.
  counters <- Counters
    <$> STM.newTVarIO 0
    <*> STM.newTVarIO 0
    <*> STM.newTVarIO 0
    <*> STM.newTVarIO 0
    <*> STM.newTVarIO 0
  let thePipe = Pipe
        { pipeInputQueue   = inputQueue
        , pipeInputCount   = inputCount
        , pipePayloadQueue = payloadQ
          -- pipeRecycle: write recycled inputs back to the unbounded input
          -- queue. Because TQueue has no capacity limit, this can never stall
          -- the worker thread inside STM. Bumps 'pipeInputCount' in the same
          -- transaction so the cached size stays consistent.
        , pipeRecycle       = \is -> do
            mapM_ (STM.writeTQueue inputQueue) is
            STM.modifyTVar' inputCount (+ length is)
        , pipeCounters     = counters
        }
  let name    = Validated.workloadName validatedWorkload
      recycle = Raw.builderRecycle validatedBuilder
  -- Resolve observer handle for RecycleOnConfirm.
  mObserverHandle <- case recycle of
    Just (Raw.RecycleOnConfirm obsName) ->
      case Map.lookup obsName observerHandles of
        Nothing -> fail $ "resolveBuilder: builder references unknown observer "
                       ++ show obsName
        Just handle -> pure (Just handle)
    _ -> pure Nothing
  -- Set up recycling and get the enqueue action + optional recycler async.
  -- 'pendingRecycle' is also returned so the stats async below can read its
  -- size (the third diagnostic depth, alongside input and payload queues).
  -- 'builderRunner' is also given the confirmed/orphan counters to bump
  -- inside the recycler async's STM transaction.
  (enqueue, mRecycler, pendingRecycle) <-
    builderRunner recycle thePipe name mObserverHandle (onRecycle name)
  -- Create the caller's build handle.
  builderHandle <- mkBuilderFn builderIndex name validatedBuilder
  let batchSize = fromIntegral (bhInputsPerBatch builderHandle) :: Int
  -- Spawn the builder async: forever pull inputs, build, enqueue.
  async <- Async.async $ do
    tid <- myThreadId
    labelThread tid name
    forever $ do
      inputs <- STM.atomically $ do
        is <- replicateM batchSize (STM.readTQueue inputQueue)
        STM.modifyTVar' inputCount (subtract batchSize)
        pure is
      mResult <- bhBuildPayload builderHandle inputs
      case mResult of
        Just (key, payload, outputInputs) -> do
          enqueue inputs key payload outputInputs
          STM.atomically $ STM.modifyTVar' (cBuilt counters) (+ 1)
        Nothing ->
          -- Builder dropped the inputs (e.g. dust). Nothing to enqueue
          -- and nothing to recycle; the inputs are gone from the pipeline.
          STM.atomically $ STM.modifyTVar' (cDropped counters) (+ 1)
  Async.link async
  -- Link the recycler async (consistent with builder async linking above).
  case mRecycler of
    Just r  -> Async.link r
    Nothing -> pure ()
  -- Spawn the stats async: every 'statsIntervalMicros' microseconds,
  -- snapshot the three pipeline depths and the five counters in one STM
  -- transaction and hand them to the caller's callback. Always linked so a
  -- callback exception surfaces immediately.
  stats <- Async.async $ do
    tid <- myThreadId
    labelThread tid (name ++ "/stats")
    forever $ do
      snap <- STM.atomically $ Stats
        <$> STM.readTVar inputCount
        <*> (fromIntegral <$> STM.lengthTBQueue payloadQ)
        <*> (Map.size  <$> STM.readTVar pendingRecycle)
        <*> STM.readTVar (cBuilt             counters)
        <*> STM.readTVar (cRecycledConfirmed counters)
        <*> STM.readTVar (cRecycledOrphan    counters)
        <*> STM.readTVar (cDropped           counters)
        <*> STM.readTVar (cSubmitted         counters)
      onStats name snap
      threadDelay statsIntervalMicros
  Async.link stats
  pure Builder
    { builderName   = name
    , builderPipe   = thePipe
    , builderAsync  = async
    , recyclerAsync = mRecycler
    , statsAsync    = stats
    }

-- | How often the queue-depth tracer fires (5 seconds).
statsIntervalMicros :: Int
statsIntervalMicros = 5 * 1_000_000

--------------------------------------------------------------------------------
-- Workload resolution.
--------------------------------------------------------------------------------

-- | Resolve a single workload: assign the pre-created 'Pipe' to each target and
-- resolve each target's rate limiter.
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
  pure ( Workload { workloadName = wlName
                  , targets      = resolvedTargets
                  }
       , cache'
       )

--------------------------------------------------------------------------------
-- Target resolution.
--------------------------------------------------------------------------------

-- | Resolve a single target: look up or create its rate limiter from the cache,
-- then build the 'Target' record.
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
           , maxBatchSize = Validated.maxBatchSize validatedTarget
           , onExhaustion = Validated.onExhaustion validatedTarget
           , targetAddr   = Validated.addr validatedTarget
           , targetPort   = Validated.port validatedTarget
           }
       , cache'
       )

-- | Look up or create a 'RL.RateLimiter' for a target.
--
-- * 'Nothing' rate limit source → 'RL.newUnlimited' (no cache entry).
-- * Otherwise, use the pre-computed 'Validated.rateLimitKey' as the cache key.
--   If the key already exists the existing limiter is reused; otherwise a
--   'RL.newTokenBucket' is created and inserted.
getOrCreateLimiter
  :: LimiterCache
  -> Validated.Target
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
-- Builder runner.
--------------------------------------------------------------------------------

-- | Set up recycling infrastructure for a builder and return an enqueue action
-- and an optional recycler async.
--
-- For 'RecycleOnConfirm', a background recycler async reads confirmations from
-- the provided channel and recycles the output inputs. The original inputs are
-- retained in the pending map so that orphaned transactions (rolled back by the
-- node) can recover their consumed inputs instead of permanently leaking them
-- from the recycling loop.
builderRunner
  :: Ord key
  -- | Recycling strategy (or 'Nothing' for no recycling).
  => Maybe Raw.RecycleStrategy
  -- | Pipeline queues shared with all targets in the workload.
  -> Pipe input payload
  -- | Builder name (used to label the recycler thread).
  -> String
  -- | For 'RecycleOnConfirm': the observer handle to subscribe to for
  -- transaction confirmations and rollback orphans. Ignored for other
  -- strategies.
  -> Maybe (ObserverHandle key confirmed)
  -- | Callback invoked each time inputs are recycled (e.g. for tracing).
  -> (key -> Bool -> [input] -> IO ())
  -- | Returns the enqueue action (see 'mkEnqueue'), an optional recycler
  -- async (only for 'RecycleOnConfirm'), and the pending-recycle TVar.
  -- The TVar is exposed so callers (e.g. the queue-depth stats async in
  -- 'resolveBuilder') can read its size without owning the strategy logic.
  -> IO ( [input] -> key -> payload -> [input] -> IO ()
        , Maybe (Async ())
        , STM.TVar (Map key ([input], [input]))
        )
builderRunner strategy pipe name mObserverHandle onRecycle = do
  pendingRecycle <- STM.newTVarIO Map.empty
  -- For RecycleOnConfirm, spawn a recycler that reads confirmations and
  -- recycles the matching inputs. The recycler async is returned to the caller
  -- ('resolveBuilder') which links it.
  mRecycler <- case (strategy, mObserverHandle) of
    (Just (Raw.RecycleOnConfirm _), Just handle) -> do
      chan <- ohSubscribe handle
      let extractKey = ohExtractKey handle
      recycler <- Async.async $ do
        tid <- myThreadId
        labelThread tid (name ++ "/recycler")
        forever $ do
          event <- STM.atomically $ STM.readTChan chan
          let k = either extractKey extractKey event
          mEntry <- STM.atomically $ do
            m <- STM.readTVar pendingRecycle
            case Map.lookup k m of
              Nothing    -> pure Nothing
              Just entry -> do
                STM.writeTVar pendingRecycle (Map.delete k m)
                pure (Just entry)
          case mEntry of
            Nothing -> pure ()
            Just (originalInputs, outputInputs) -> do
              let (isOrphan, toRecycle) = case event of
                    Left  _ -> (True,  originalInputs)
                    Right _ -> (False, outputInputs)
                  counter = if isOrphan
                    then cRecycledOrphan    (pipeCounters pipe)
                    else cRecycledConfirmed (pipeCounters pipe)
              STM.atomically $ do
                pipeRecycle pipe toRecycle
                STM.modifyTVar' counter (+ 1)
              onRecycle k isOrphan toRecycle
      pure (Just recycler)
    _ -> pure Nothing
  pure (mkEnqueue strategy pipe pendingRecycle onRecycle, mRecycler, pendingRecycle)

-- | Enqueue a built payload and handle input recycling.
--
-- Called by the builder loop after each payload is built. Parameters follow
-- the pipeline order: consumed inputs → confirmation key → payload → produced
-- outputs.
--
-- * 'Nothing': enqueue @(payload, [])@, no recycling at all.
-- * 'Raw.RecycleOnBuild': enqueue @(payload, [])@, recycle outputs immediately.
-- * 'Raw.RecycleOnPull': enqueue @(payload, outputs)@, recycled on fetch.
-- * 'Raw.RecycleOnConfirm': enqueue @(payload, [])@, track @key →
--   (originalInputs, outputInputs)@ in a pending map.
--
-- The @key@ and @originalInputs@ parameters are only used by 'RecycleOnConfirm'
-- and ignored otherwise.
mkEnqueue
  :: Ord key
  -- | Recycling strategy (or 'Nothing' for no recycling).
  => Maybe Raw.RecycleStrategy
  -- | Pipeline queues shared with all targets in the workload.
  -> Pipe input payload
  -- | Pending recycle map (shared with the recycler async).
  -> STM.TVar (Map key ([input], [input]))
  -- | Callback invoked each time inputs are recycled (e.g. for tracing).
  -> (key -> Bool -> [input] -> IO ())
  -- | Inputs consumed to build this payload. Stored by 'RecycleOnConfirm' so
  -- orphaned transactions can recover their consumed inputs.
  -> [input]
  -- | Confirmation key (only used by 'RecycleOnConfirm').
  -> key
  -- | The built payload to enqueue for workers.
  -> payload
  -- | New inputs produced by this payload (recycled on confirmation or
  -- immediately, depending on the strategy).
  -> [input]
  -> IO ()
mkEnqueue
  strategy
  pipe
  pendingRecycle
  onRecycle
  originalInputs
  key
  payload
  outputInputs =
    case strategy of
      Nothing -> STM.atomically $ do
        ---------- STM START ----------
        STM.writeTBQueue (pipePayloadQueue pipe) (payload, [])
        ---------- STM ENDED ----------
      Just Raw.RecycleOnBuild -> do
        STM.atomically $ do
          ---------- STM START ----------
          STM.writeTBQueue (pipePayloadQueue pipe) (payload, [])
          pipeRecycle pipe outputInputs
          ---------- STM ENDED ----------
        onRecycle key False outputInputs
      Just Raw.RecycleOnPull -> STM.atomically $ do
        ---------- STM START ----------
        STM.writeTBQueue (pipePayloadQueue pipe) (payload, outputInputs)
        ---------- STM ENDED ----------
      Just (Raw.RecycleOnConfirm _) -> STM.atomically $ do
        ---------- STM START ----------
        STM.writeTBQueue (pipePayloadQueue pipe) (payload, [])
        STM.modifyTVar' pendingRecycle
          (Map.insert key (originalInputs, outputInputs))
        ---------- STM ENDED ----------

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

