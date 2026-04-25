{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

-- | Resolved runtime configuration — creates STM resources (rate limiters,
-- pipeline queues) from a 'Validated.Config'.
--
-- Rate-limiter sharing is keyed by 'Validated.rateLimitKey':
--
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
  , pipeInputQueue, pipePayloadQueue, pipeRecycle
    -- * Observer.
  , Observer
  , observerName, observerAsync
    -- * Builder.
  , Builder
  , builderName, builderPipe, builderAsync, recyclerAsync
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
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (myThreadId)
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
    config :: !(Validated.Config input)
    -- | Resolved observers, keyed by name.
  , observers :: !(Map String Observer)
    -- | One per workload, alphabetical order (as 'Map.elems' on 'workloads').
  , builders :: [Builder input payload]
    -- | Resolved workloads, keyed by name.
  , workloads :: !(Map String (Workload input payload))
    -- | All asyncs (observers + builders + recyclers), linked.
    -- Caller should append their own worker asyncs for cleanup.
  , asyncs :: [Async ()]
  }

--------------------------------------------------------------------------------

-- | Caller-provided observer handle. 'resolve' spawns 'ohRun' in a labeled,
-- linked async and uses the subscription for 'Raw.RecycleOnConfirm' recycling.
-- 'ohExtractKey' must produce the same @key@ type used by 'BuilderHandle'.
data ObserverHandle key confirmed = ObserverHandle
  { -- | IO action that runs the observer (e.g. a NodeToNode connection).
    ohRun        :: !(IO ())
    -- | Subscribe to the observer's confirmation broadcast channel.
  , ohSubscribe  :: !(IO (STM.TChan confirmed))
    -- | Extract the recycling key from a confirmation event.
  , ohExtractKey :: !(confirmed -> key)
  }

-- | Caller-provided build handle. 'resolve' spawns a builder async that pulls
-- inputs via 'bhInputsPerBatch' and produces payloads.
data BuilderHandle key input payload = BuilderHandle
  { -- | How many inputs the builder needs per call to 'bhBuildPayload'.
    bhInputsPerBatch :: !Natural
    -- | Returns @(confirmation key, payload, recyclable outputs)@.
  , bhBuildPayload   :: [input] -> IO (key, payload, [input])
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
    pipeInputQueue :: !(STM.TQueue input)
    -- | Bounded (capacity 8192); sole source of backpressure.
  , pipePayloadQueue :: !(STM.TBQueue (payload, [input]))
    -- | Recycle consumed inputs back to 'pipeInputQueue'.
  , pipeRecycle :: [input] -> STM.STM ()
  }

-- | Builder resources for one workload (exactly one per workload).
data Builder input payload = Builder
  { -- | Same as the workload name.
    builderName :: !String
    -- | Shared with all targets in the workload.
  , builderPipe :: !(Pipe input payload)
    -- | Linked async running the builder loop.
  , builderAsync :: !(Async ())
    -- | 'Raw.RecycleOnConfirm' recycler; 'Nothing' for other strategies.
  , recyclerAsync :: !(Maybe (Async ()))
  }

-- | Fully resolved workload. Builder resources live in 'Builder' on the
-- 'Runtime', not here.
data Workload input payload = Workload
  { -- | Unique name identifying this workload.
    workloadName :: !String
    -- | Resolved targets, keyed by name.
  , targets :: !(Map String (Target input payload))
  }

-- | A fully resolved target. Targets in the same workload share a 'Pipe';
-- targets with the same 'Validated.rateLimitKey' share a 'RL.RateLimiter'.
data Target input payload = Target
  { -- | Unique name identifying this target.
    targetName :: !String
    -- | Shared with all targets in the same workload.
  , targetPipe :: !(Pipe input payload)
    -- | Shared when 'Validated.rateLimitKey' matches.
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
  => (Int -> String -> Raw.Builder -> IO (BuilderHandle key input payload))
  -- ^ Builder factory (index, name, config).
  -> (Int -> String -> Raw.Observer -> IO (ObserverHandle key confirmed))
  -- ^ Observer factory (index, name, config).
  -> (String -> [input] -> IO ())
  -- ^ Recycle callback (e.g. for tracing).
  -> Validated.Config input
  -> IO (Runtime input payload)
resolve mkBuilderFn mkObserverFn onRecycle validatedConfig = do
  -- Create all observers via the factory callback. The ObserverHandle's run
  -- action is spawned in a labeled, linked async (same pattern as builders).
  -- The subscription pair is kept locally for RecycleOnConfirm.
  handleResults <- Map.fromAscList <$> mapM
    (\(ix, (name, rawObs)) -> do
      handle <- mkObserverFn ix name rawObs
      obsAsync <- Async.async $ do
        tid <- myThreadId
        labelThread tid ("observer/" ++ name)
        ohRun handle
      Async.link obsAsync
      pure ( name
           , ( Observer { observerName  = name
                        , observerAsync = obsAsync
                        }
             , handle
             )
           )
    )
    (zip
      [0..]
      (Map.toAscList (Validated.observers validatedConfig))
    )
  let resolvedObservers = Map.map fst handleResults
  -- Internal lookup for RecycleOnConfirm: subscribe to named observer.
  let resolveObserver name =
        case Map.lookup name handleResults of
          Nothing -> fail $ "resolve: builder references unknown observer "
                         ++ show name
          Just (_, handle) -> do
            chan <- ohSubscribe handle
            pure (chan, ohExtractKey handle)
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
  resolvedBuilders <- Map.fromAscList <$> mapM
    (\(ix, (wlName, validatedWorkload)) -> do
      let workloadInputs = inputsByWorkloadMap Map.! wlName
          workloadBuilder = Validated.builder validatedWorkload
      builder <- resolveBuilder mkBuilderFn resolveObserver onRecycle
                   ix validatedWorkload workloadBuilder workloadInputs
      pure (wlName, builder)
    )
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
  -- Collect all asyncs: observers + builders + recyclers.
  let builderList = Map.elems resolvedBuilders
      observerAsyncs = map observerAsync (Map.elems resolvedObservers)
      builderAsyncs = concatMap
        (\b -> builderAsync b : maybe [] pure (recyclerAsync b))
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
  => (Int -> String -> Raw.Builder -> IO (BuilderHandle key input payload))
  -> (String -> IO (STM.TChan confirmed, confirmed -> key))
  -> (String -> [input] -> IO ())
  -> Int
  -> Validated.Workload
  -> Raw.Builder
  -- | Initial inputs for this workload.
  -> [input]
  -> IO (Builder input payload)
resolveBuilder mkBuilderFn resolveObserver onRecycle
               builderIndex validatedWorkload validatedBuilder initialInputs = do
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
  let name    = Validated.workloadName validatedWorkload
      recycle = Raw.builderRecycle validatedBuilder
  -- Resolve confirm source for RecycleOnConfirm.
  mConfirmSource <- case recycle of
    Just (Raw.RecycleOnConfirm obsName) ->
      Just <$> resolveObserver obsName
    _ -> pure Nothing
  -- Set up recycling and get the enqueue action + optional recycler async.
  (enqueue, mRecycler) <-
    builderRunner recycle thePipe name mConfirmSource (onRecycle name)
  -- Create the caller's build handle.
  builderHandle <- mkBuilderFn builderIndex name validatedBuilder
  -- Spawn the builder async: forever pull inputs, build, enqueue.
  async <- Async.async $ do
    tid <- myThreadId
    labelThread tid name
    forever $ do
      inputs <- STM.atomically $
        replicateM (fromIntegral (bhInputsPerBatch builderHandle))
          (STM.readTQueue (pipeInputQueue thePipe))
      (key, payload, outputInputs) <- bhBuildPayload builderHandle inputs
      enqueue payload key outputInputs
  Async.link async
  -- Link the recycler async (consistent with builder async linking above).
  case mRecycler of
    Just r  -> Async.link r
    Nothing -> pure ()
  pure Builder
    { builderName   = name
    , builderPipe   = thePipe
    , builderAsync  = async
    , recyclerAsync = mRecycler
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
  pure ( Workload { workloadName = wlName
                  , targets      = resolvedTargets
                  }
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
-- Builder runner.
--------------------------------------------------------------------------------

-- | Set up recycling infrastructure for a builder and return an enqueue action.
--
-- The returned action is called by the builder loop after each payload is built.
-- It handles enqueueing the payload and recycling inputs according to the
-- configured strategy:
--
-- * 'Nothing': enqueue @(payload, [])@ — no recycling at all.
-- * 'Raw.RecycleOnBuild': enqueue @(payload, [])@, recycle inputs immediately.
-- * 'Raw.RecycleOnPull': enqueue @(payload, inputs)@ — recycled on fetch.
-- * 'Raw.RecycleOnConfirm': enqueue @(payload, [])@, track @key → inputs@ in
--   a pending map; a background recycler async reads confirmations from the
--   provided channel and recycles matching inputs.
--
-- The @key@ parameter of the returned action is only used by 'RecycleOnConfirm'
-- and ignored otherwise.
builderRunner
  :: Ord key
  => Maybe Raw.RecycleStrategy
  -> Pipe input payload
  -> String
  -- | For 'RecycleOnConfirm': pre-subscribed broadcast channel and a function
  -- to extract the confirmation key. Ignored for other strategies.
  -> Maybe (STM.TChan confirmed, confirmed -> key)
  -- | Callback invoked each time inputs are recycled (e.g. for tracing).
  -> ([input] -> IO ())
  -> IO (payload -> key -> [input] -> IO (), Maybe (Async ()))
builderRunner strategy pipe name mConfirmSource onRecycle = do
  pendingRecycle <- STM.newTVarIO Map.empty
  -- For RecycleOnConfirm, spawn a recycler that reads confirmations and
  -- recycles the matching inputs. The recycler async is returned to the
  -- caller ('resolveBuilder') which links it.
  mRecycler <- case (strategy, mConfirmSource) of
    (Just (Raw.RecycleOnConfirm _), Just (chan, extractKey)) -> do
      recycler <- Async.async $ do
        tid <- myThreadId
        labelThread tid (name ++ "/recycler")
        forever $ do
          confirmed <- STM.atomically $ STM.readTChan chan
          let k = extractKey confirmed
          mInputs <- STM.atomically $ do
            m <- STM.readTVar pendingRecycle
            case Map.lookup k m of
              Nothing     -> pure Nothing
              Just inputs -> do
                STM.writeTVar pendingRecycle (Map.delete k m)
                pure (Just inputs)
          case mInputs of
            Nothing     -> pure ()
            Just inputs -> do
              STM.atomically $ pipeRecycle pipe inputs
              onRecycle inputs
      pure (Just recycler)
    _ -> pure Nothing
  -- Return the enqueue action and the optional recycler async.
  let enqueue payload key inputs =
        case strategy of
          Nothing ->
            STM.atomically $ STM.writeTBQueue (pipePayloadQueue pipe) (payload, [])
          Just Raw.RecycleOnBuild -> do
            STM.atomically $ STM.writeTBQueue (pipePayloadQueue pipe) (payload, [])
            STM.atomically $ pipeRecycle pipe inputs
            onRecycle inputs
          Just Raw.RecycleOnPull ->
            STM.atomically $ STM.writeTBQueue (pipePayloadQueue pipe) (payload, inputs)
          Just (Raw.RecycleOnConfirm _) -> STM.atomically $ do
            STM.writeTBQueue (pipePayloadQueue pipe) (payload, [])
            STM.modifyTVar' pendingRecycle (Map.insert key inputs)
  pure (enqueue, mRecycler)

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
