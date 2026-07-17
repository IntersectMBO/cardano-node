{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

-- | Resolves a 'Validated.Config' into a 'Runtime': live STM resources
-- (queues, rate limiters) grouped into name-keyed pools, with the threads
-- running them. See 'Runtime' for the pools and 'resolve' for resolution.
module Cardano.Benchmarking.PullFiction.Config.Runtime
  ( -- * Runtime.
    Runtime
  , config, builders, pipes, recyclers, observers, workloads, asyncs
    -- * Handles.
    -- ** Behaviour handles (what the resource does).
  , BuilderApi     (..)
  , BuilderHandle  (..)
  , ObserverHandle (..)
    -- ** Event handlers (fired here, used for tracing).
  , PipeHandle     (..)
  , RecyclerHandle (..)
    -- * Builder.
  , Builder
  , builderName, builderPipe, builderRecycler, builderAsync
    -- * Recycler.
  , Recycler
  , recyclerName, recyclerInternal, recyclerAsync
    -- * Observer.
  , Observer
  , observerName, observerHandle, observerAsync
    -- * Workload.
  , Workload
  , workloadName, targets
    -- * OnExhaustion.
  , Raw.OnExhaustion (..)
    -- * Target.
  , Target
  , targetName
  , targetFetcher
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
import Control.Monad (forever)
import Data.Foldable (foldlM, toList)
import GHC.Conc (labelThread)
import Numeric.Natural (Natural)
-----------
-- async --
-----------
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
import Cardano.Benchmarking.PullFiction.Internal.Pipe qualified as Pipe
import Cardano.Benchmarking.PullFiction.Internal.RateLimiter qualified as RL
import Cardano.Benchmarking.PullFiction.Internal.Recycler qualified as Recycler

--------------------------------------------------------------------------------

-- | Fully resolved top-level configuration.
--
-- Everything is a name-keyed pool. Today the name is always the workload name
-- (one builder/pipe/recycler/observer per workload), but the pools are the
-- natural home once the config allows sharing or interconnection.
--
-- Each pool entry carries its own name and the async(s) running it, so
-- 'asyncs' just gathers all of them. The exception is 'pipes': a pipe is
-- pure structure, a queue pair ('Pipe.Pipe') with no name and no async.
data Runtime key input payload = Runtime
  { -- | The original validated configuration.
    config    :: !(Validated.Config input)
    -- | Resolved builders (build loops), keyed by name.
  , builders  :: !(Map String Builder)
    -- | Resolved pipes (queue pairs), keyed by name. Pure structure: the
    -- only pool whose entry has no name and no async, just the queue pair.
  , pipes     :: !(Map String (Pipe.Pipe key input payload))
    -- | Resolved recyclers, keyed by name.
  , recyclers :: !(Map String (Recycler key input payload))
    -- | Resolved observers, keyed by name.
  , observers :: !(Map String (Observer key))
    -- | Resolved workloads (target groups), keyed by name.
  , workloads :: !(Map String (Workload key input payload))
    -- | All asyncs (builders + recyclers + confirm forwarders + observers),
    -- linked. Caller should append their own worker asyncs for cleanup.
  , asyncs    :: ![Async.Async ()]
  }

--------------------------------------------------------------------------------

-- | The privileged operations a builder loop may perform, each a closure over
-- this builder's pipe and recycler. The raw 'Pipe.Pipe' and 'Recycler' are not
-- exposed, so a builder drives its own loop with full freedom but cannot reach
-- or corrupt the machinery: it may only pull inputs, publish a payload
-- (recording the recycle bookkeeping), or abandon inputs.
--
-- The builder loop owns conservation of inputs: every batch returned by
-- 'baTakeInputs' should be handed to exactly one of 'baAddPayload' (as its
-- consumed set) or 'baDropInputs'. The engine cannot enforce this, so a loop
-- that leaks a batch merely shrinks the recyclable pool.
data BuilderApi key input payload = BuilderApi
  { -- | Pull this many inputs off the input queue. Blocks until that many are
    -- available.
    baTakeInputs :: Natural -> IO [input]
    -- | Publish a payload. Records its consumed inputs and produced outputs
    -- with the recycler (so the outputs return to the input queue when the
    -- recycle strategy fires), then enqueues the payload. Arguments:
    -- confirmation key, payload, consumed inputs, recyclable outputs.
  , baAddPayload :: key -> payload -> [input] -> [input] -> IO ()
    -- | Abandon inputs already pulled by 'baTakeInputs': they are neither
    -- recycled nor re-enqueued. The correct terminus for an unbuildable (e.g.
    -- dust) batch.
  , baDropInputs :: [input] -> IO ()
  }

-- | Caller-provided builder. 'resolve' spawns one async per builder and runs
-- 'bhRunBuilder' in it, handing over a 'BuilderApi' wired to that builder's pipe
-- and recycler. The builder owns its loop (batching, grouping, coin selection);
-- the engine owns the thread and the machinery behind the API.
newtype BuilderHandle key input payload = BuilderHandle
  { bhRunBuilder :: BuilderApi key input payload -> IO ()
  }

-- | Caller-provided pipe queue-event handlers (e.g. for tracing). 'resolve'
-- unpacks these into the workload's 'Pipe', which fires them as items are added
-- to or removed from its two queues. Queue mechanics are pipe events, not
-- builder events, so they live here and not on 'BuilderHandle'.
data PipeHandle key input = PipeHandle
  { -- | Fired after inputs added to the input queue (initial load or recycle).
    phOnInputsEnqueued  :: !(Pipe.OnInputsEvent input)
    -- | Fired after inputs are removed from the input queue (builder take).
  , phOnInputsDequeued  :: !(Pipe.OnInputsEvent input)
    -- | Fired after a payload is added to the payload queue.
  , phOnPayloadEnqueued :: !(Pipe.OnPayloadEvent key)
    -- | Fired after payload removed from the payload queue (worker dequeue).
  , phOnPayloadDequeued :: !(Pipe.OnPayloadEvent key)
  }

-- | Caller-provided recycler event handlers (e.g. for tracing). 'resolve'
-- unpacks them into the workload's 'Recycler', which fires them as payloads
-- enter the pending backlog and as inputs are added to the pipe's input
-- queue. Kept separate from 'PipeHandle' because recycling is a 'Recycler'
-- concern, not a 'Pipe' one.
data RecyclerHandle input = RecyclerHandle
  { -- | Fired by the recycler with the inputs it holds and the resulting
    -- pending count each time a payload enters the backlog.
    rhOnPending   :: !(Recycler.OnPendingEvent input)
    -- | Fired by the recycler with the inputs it adds to the pipe and the
    -- resulting pending count, each recycle.
  , rhOnAddToPipe :: !(Recycler.OnAddToPipeEvent input)
  }

-- | Caller-provided observer handle. 'resolve' spawns 'ohRun' in a labeled,
-- linked async and uses the subscription for 'Raw.RecycleOnConfirm' recycling.
data ObserverHandle key = ObserverHandle
  { -- | IO action that runs the observer (e.g. a NodeToNode connection).
    ohRun       :: !(IO ())
    -- | Subscribe to the observer's event stream: returns an STM action that
    -- reads the next event as its recycle key.
    -- 'Right' = confirmed (recycle output inputs).
    -- 'Left'  = orphaned  (recycle original inputs).
  , ohSubscribe :: !(IO (STM.STM (Either key key)))
  }

--------------------------------------------------------------------------------

-- | A resolved build loop: it takes inputs from a pipe, builds payloads, and
-- signals a recycler. It references its 'pipes' and 'recyclers' entries /by
-- name/ (today both the workload name). Its loop thread is 'builderAsync',
-- also collected into 'asyncs'.
data Builder = Builder
  { -- | This builder's name (today the workload name).
    builderName     :: !String
    -- | Name of the 'pipes' entry it drives (takes from / adds to).
  , builderPipe     :: !String
    -- | Name of the 'recyclers' entry it signals via 'Recycler.onBuild'.
  , builderRecycler :: !String
    -- | Linked async running the build loop.
  , builderAsync    :: !(Async.Async ())
  }

-- | A resolved recycler: the 'Internal.Recycler' logic plus the async running
-- its worker. A thin wrapper so the 'recyclers' pool carries its own thread,
-- like 'Builder' and 'Observer'. The 'Recycler.Recycler' inside deliberately
-- holds no async. The observer bridge for 'RecycleOnConfirm' is a separate
-- thread ('spawnConfirmForwarders'), not held here.
data Recycler key input payload = Recycler
  { -- | This recycler's name (today the workload name).
    recyclerName     :: !String
    -- | The underlying recycler from "Internal.Recycler".
  , recyclerInternal :: !(Recycler.Recycler key input payload)
    -- | Linked async running its worker (the sole writer of recycled inputs
    -- back onto the pipe).
  , recyclerAsync    :: !(Async.Async ())
  }

-- | A resolved observer with its lifecycle managed by 'resolve': the
-- caller-provided 'ObserverHandle' plus the async running its 'ohRun'.
data Observer key = Observer
  { -- | Key from the config's @\"observers\"@ object.
    observerName   :: !String
    -- | The caller-provided handle ('ohRun' + 'ohSubscribe'). The confirm
    -- forwarders subscribe to it for 'RecycleOnConfirm' workloads.
  , observerHandle :: !(ObserverHandle key)
    -- | Linked async running the observer connection ('ohRun').
  , observerAsync  :: !(Async.Async ())
  }

-- | Fully resolved workload. Builder resources live in 'Builder' on the
-- 'Runtime', not here.
data Workload key input payload = Workload
  { -- | Unique name identifying this workload.
    workloadName :: !String
    -- | Resolved targets, keyed by name.
  , targets      :: !(Map String (Target key input payload))
  }

-- | A fully resolved target. Targets in the same workload share a 'Pipe'.
-- Targets with the same 'Validated.rateLimitKey' share a 'RL.RateLimiter'.
data Target key input payload = Target
  { -- | Unique name identifying this target.
    targetName    :: !String
    -- | Rate-limited, recycling payload fetch for this target, pre-built by
    -- 'resolveTarget' (wraps the shared pipe's fetcher through the recycler).
    -- The worker pulls through this and never touches the pipe or the recycler.
  , targetFetcher :: !(Pipe.PayloadFetcher payload)
    -- | Shared when 'Validated.rateLimitKey' matches.
  , rateLimiter   :: !RL.RateLimiter
    -- | Resolved max tokens per request for this target.
  , maxBatchSize  :: !Natural
    -- | What to do when the payload queue is exhausted.
  , onExhaustion  :: !Raw.OnExhaustion
    -- | IP address or hostname of the target endpoint.
  , targetAddr    :: !String
    -- | Port number of the target endpoint.
  , targetPort    :: !Int
  }

--------------------------------------------------------------------------------
-- Resolution.
--------------------------------------------------------------------------------

-- | Limiter cache: maps a sharing key to an already-created rate limiter.
--
-- Threaded across workloads so that top-level Shared limiters are reused.
type LimiterCache = Map String RL.RateLimiter

-- | Resolve a 'Validated.Config' into a 'Runtime'. Everything is built into
-- name-keyed pools in dependency order: observers, then pipes, then recyclers
-- (each references its pipe), then builders and targets (each references a pipe
-- and recycler). A separate top-level pass ('spawnConfirmForwarders') bridges
-- each 'RecycleOnConfirm' observer to its recycler. Today the name is always
-- the workload name (one of each per workload).
--
-- Initial inputs are partitioned equally across workloads (last absorbs the
-- remainder).
resolve
  :: Ord key
  -- | Builder factory (index, name, config).
  => (Int -> String -> Raw.Builder  -> IO (BuilderHandle  key input payload))
  -- | Pipe-events factory (index, name): the tracing handlers the pipe fires at
  -- each queue event.
  -> (Int -> String -> IO (PipeHandle key input))
  -- | Recycler-events factory (index, name): the tracing handlers the recycler
  -- fires as payloads enter the pending backlog and as it recycles inputs.
  -> (Int -> String -> IO (RecyclerHandle input))
  -- | Observer factory (index, name, config).
  -> (Int -> String -> Raw.Observer -> IO (ObserverHandle key))
  -> Validated.Config input
  -> IO (Runtime key input payload)
resolve mkBuilderFn mkPipeHandleFn mkRecyclerHandleFn mkObserverFn validatedConfig = do
  let workloadsMap = Validated.workloads validatedConfig
  -- Observers first, so the confirm forwarders below can subscribe to them.
  resolvedObservers <- resolveObservers
                         mkObserverFn
                         (Validated.observers validatedConfig)
  -- Distribute initial inputs equally across workloads, keyed by workload name.
  -- Both Maps share the same ascending key order, so zip + fromAscList is safe.
  let inputsByWorkload =
        Map.fromAscList $ zip
          (Map.keys workloadsMap)
          (partitionInputs
            (Map.size workloadsMap)
            (toList (Validated.initialInputs validatedConfig))
          )
  -- Resolve the name-keyed pools. The binding order below is the /dependency/
  -- order (each pool references the earlier ones), the reverse of the builder,
  -- pipe, recycler, observer order the fields use. A recycler needs its pipe, a
  -- builder needs its pipe and recycler.
  resolvedPipes     <- resolvePipes     mkPipeHandleFn inputsByWorkload workloadsMap
  resolvedRecyclers <- resolveRecyclers mkRecyclerHandleFn resolvedPipes workloadsMap
  resolvedBuilders  <- resolveBuilders  mkBuilderFn resolvedPipes resolvedRecyclers workloadsMap
  resolvedWorkloads <- resolveWorkloads resolvedPipes resolvedRecyclers workloadsMap
  -- Bridge each RecycleOnConfirm observer to its recycler (see the forwarders).
  forwarderAsyncs   <- spawnConfirmForwarders resolvedRecyclers resolvedObservers workloadsMap
  -- Assemble the final runtime.
  pure Runtime
    { config    = validatedConfig
    , builders  = resolvedBuilders
    , pipes     = resolvedPipes
    , recyclers = resolvedRecyclers
    , observers = resolvedObservers
    , workloads = resolvedWorkloads
    -- Collect all asyncs. Each pool entry carries a single async
    -- ('builderAsync', 'recyclerAsync', 'observerAsync'). The confirm
    -- forwarders are separate, from 'spawnConfirmForwarders'.
    , asyncs    =     map builderAsync  (Map.elems resolvedBuilders)
                   ++ map recyclerAsync (Map.elems resolvedRecyclers)
                   ++ forwarderAsyncs
                   ++ map observerAsync (Map.elems resolvedObservers)
    }

--------------------------------------------------------------------------------
-- Named-pool resolution (builders, pipes, recyclers, observers).
--------------------------------------------------------------------------------

-- Definitions are listed builder, pipe, recycler, observer for consistency
-- with the field order. 'resolve' calls them in the reverse (dependency) order.

-- | Resolve one builder per workload into a name-keyed pool: spawn its build
-- loop over the workload's pipe and recycler (both by workload name). Each loop
-- takes inputs, builds a payload, records the tx's recyclable inputs with the
-- recycler ('Recycler.onBuild'), then enqueues the payload. Each 'Builder'
-- keeps its loop thread as 'builderAsync' (which 'resolve' also collects into
-- 'asyncs'). The pipe fires its own (pure) trace handlers. The recycler owns
-- all recycle timing.
resolveBuilders
  -- | Builder factory (index, name, config).
  :: (Int -> String -> Raw.Builder -> IO (BuilderHandle key input payload))
  -> Map String (Pipe.Pipe key input payload)
  -> Map String (Recycler key input payload)
  -> Map String Validated.Workload
  -> IO (Map String Builder)
resolveBuilders mkBuilderFn resolvedPipes resolvedRecyclers workloadsMap =
  Map.fromAscList <$> mapM
    (\(ix, (wlName, validatedWorkload)) -> do
      let thePipe  = resolvedPipes     Map.! wlName
          recycler = recyclerInternal (resolvedRecyclers Map.! wlName)
      builderHandle <- mkBuilderFn ix wlName (Validated.builder validatedWorkload)
      -- The safe capability API handed to the builder loop: closures over this
      -- builder's pipe and recycler, never the raw resources. 'baAddPayload'
      -- records the recyclable inputs with the recycler, then makes the payload
      -- dequeuable. The recycler handles the build and the later release
      -- ('onDequeue' \/ 'onConfirm') in either order, so this order is just the
      -- natural one, not a correctness requirement. 'baDropInputs' abandons a
      -- batch: 'Pipe.takeInputs' already removed it from the input queue, so
      -- skipping the recycler is all it takes to drop it.
      let api = BuilderApi
            { baTakeInputs = Pipe.takeInputs thePipe
            , baAddPayload = \key payload consumed outputInputs -> do
                Recycler.onBuild recycler key consumed outputInputs
                Pipe.addPayload thePipe key payload
            , baDropInputs = \_inputs -> pure ()
            }
      async <- Async.async $ do
        -- Always labeled threads.
        tid <- myThreadId
        labelThread tid wlName
        bhRunBuilder builderHandle api
      Async.link async
      pure ( wlName
           , Builder { builderName     = wlName
                     , builderPipe     = wlName
                     , builderRecycler = wlName
                     , builderAsync    = async
                     }
           )
    )
    -- Zero-based index and name provided to the builder factory.
    (zip [0..] (Map.toAscList workloadsMap))

-- | Resolve one pipe per workload into a name-keyed pool (name = workload
-- name), loading that workload's initial inputs into it. 'Pipe.mkPipe' owns all
-- queue creation and wires the caller's tracing handlers. The pipe knows
-- nothing about recycling.
resolvePipes
  -- | Pipe-events factory (index, name): the tracing handlers the pipe fires at
  -- each queue event.
  :: (Int -> String -> IO (PipeHandle key input))
  -- | Each workload's initial inputs, keyed by workload name.
  -> Map String [input]
  -> Map String Validated.Workload
  -> IO (Map String (Pipe.Pipe key input payload))
resolvePipes mkPipeHandleFn inputsByWorkload workloadsMap =
  Map.fromAscList <$> mapM
    (\(ix, (wlName, _validatedWorkload)) -> do
      pipeHandle <- mkPipeHandleFn ix wlName
      thePipe <- Pipe.mkPipe
                   (phOnInputsEnqueued  pipeHandle)
                   (phOnInputsDequeued  pipeHandle)
                   (phOnPayloadEnqueued pipeHandle)
                   (phOnPayloadDequeued pipeHandle)
      -- Load the initial inputs one at a time through 'Pipe.addInputs', the
      -- same call the recycler uses so each enters exactly like a recycled one.
      mapM_
        (\initialInput -> Pipe.addInputs thePipe [initialInput])
        (inputsByWorkload Map.! wlName)
      pure (wlName, thePipe)
    )
    -- Zero-based index and name provided to the pipe factory.
    (zip [0..] (Map.toAscList workloadsMap))

-- | Resolve one recycler per workload into a name-keyed pool. For each, build
-- the 'Internal.Recycler' logic and start its worker (the sole writer of
-- recycled inputs back onto the pipe). Each 'Recycler' wraps that logic with
-- its worker 'recyclerAsync', collected into 'asyncs'. The observer bridge for
-- 'RecycleOnConfirm' is spawned separately, by 'spawnConfirmForwarders'.
resolveRecyclers
  :: Ord key
  -- | Recycler-events factory (index, name): the tracing handlers the recycler
  -- fires as payloads enter the pending backlog and as it recycles inputs.
  => (Int -> String -> IO (RecyclerHandle input))
  -> Map String (Pipe.Pipe key input payload)
  -> Map String Validated.Workload
  -> IO (Map String (Recycler key input payload))
resolveRecyclers mkRecyclerHandleFn resolvedPipes workloadsMap =
  Map.fromAscList <$> mapM
    (\(ix, (wlName, validatedWorkload)) -> do
      let recycleStrategy = Raw.builderRecycle (Validated.builder validatedWorkload)
      recyclerHandle <- mkRecyclerHandleFn ix wlName
      internal <- Recycler.mkRecycler
                    recycleStrategy
                    (resolvedPipes Map.! wlName)
                    (rhOnPending recyclerHandle)
                    (rhOnAddToPipe recyclerHandle)
      worker <- Recycler.runRecycler internal wlName
      Async.link worker
      pure ( wlName
           , Recycler { recyclerName     = wlName
                      , recyclerInternal = internal
                      , recyclerAsync    = worker
                      }
           )
    )
    -- Zero-based index and name provided to the recycler factory.
    (zip [0..] (Map.toAscList workloadsMap))

-- | Resolve the observers into a name-keyed pool, spawning each one's 'ohRun'
-- in a labeled, linked async. Each 'Observer' keeps its 'ObserverHandle' (as
-- 'observerHandle'), which the confirm forwarders subscribe to on behalf of
-- each 'RecycleOnConfirm' workload's recycler.
resolveObservers
  -- | Observer factory (index, name, config).
  :: (Int -> String -> Raw.Observer -> IO (ObserverHandle key))
  -> Map String Raw.Observer
  -> IO (Map String (Observer key))
resolveObservers mkObserverFn rawObservers =
  Map.fromAscList <$> mapM
    (\(ix, (obsName, rawObs)) -> do
      obsHandle <- mkObserverFn ix obsName rawObs
      obsAsync  <- Async.async $ do
        -- Always labeled threads.
        tid <- myThreadId
        labelThread tid ("observer/" ++ obsName)
        ohRun obsHandle
      Async.link obsAsync
      pure ( obsName
           , Observer { observerName   = obsName
                      , observerHandle = obsHandle
                      , observerAsync  = obsAsync
                      }
           )
    )
    -- Zero-based index and name provided to the observer factory.
    (zip [0..] (Map.toAscList rawObservers))

--------------------------------------------------------------------------------
-- Confirm forwarders (observer to recycler, for RecycleOnConfirm).
--------------------------------------------------------------------------------

-- | Spawn the confirm forwarders. For every 'RecycleOnConfirm' workload, this
-- starts a thread that reads the named observer's confirm\/orphan stream and
-- forwards each event to the recycler via 'Recycler.onConfirm'. It is the sole
-- bridge from the observer (which knows a transaction's outcome) to the
-- recycler, which is observer-agnostic and drains only its own event queue.
-- Workloads with any other strategy contribute no forwarder. Returns the
-- forwarder asyncs for 'resolve' to collect into 'asyncs'.
spawnConfirmForwarders
  -- | The recyclers to notify, keyed by workload name.
  :: Map String (Recycler key input payload)
  -- | Observers keyed by name (a strategy names the one to subscribe to).
  -> Map String (Observer key)
  -- | Validated workloads (their recycle strategy selects the on-confirm ones).
  -> Map String Validated.Workload
  -> IO [Async.Async ()]
spawnConfirmForwarders resolvedRecyclers resolvedObservers workloadsMap =
  fmap concat $ mapM
    (\(wlName, validatedWorkload) ->
      case Raw.builderRecycle (Validated.builder validatedWorkload) of
        Just (Raw.RecycleOnConfirm obsName) -> do
          -- Both are guaranteed present: recyclers are keyed by workload name,
          -- and 'Validated.validate' rejects an undefined observer reference.
          let recycler = recyclerInternal (resolvedRecyclers Map.! wlName)
          readEvent <- ohSubscribe (observerHandle (resolvedObservers Map.! obsName))
          forwarder <- Async.async $ do
            -- Always labeled threads.
            tid <- myThreadId
            labelThread tid (wlName ++ "/confirm-forwarder")
            forever $ do
              event <- STM.atomically readEvent
              case event of
                -- 'Left' = orphaned (rolled back). 'Right' = confirmed.
                Left  key -> Recycler.onConfirm recycler key True
                Right key -> Recycler.onConfirm recycler key False
          Async.link forwarder
          pure [forwarder]
        _ -> pure []
    )
    (Map.toAscList workloadsMap)

--------------------------------------------------------------------------------
-- Workload resolution.
--------------------------------------------------------------------------------

-- | Resolve every workload's targets into a name-keyed pool, threading the
-- rate-limiter cache across all of them so top-level Shared limiters are
-- reused.
-- Each workload's targets fetch from that workload's pipe and signal its
-- recycler (both looked up by workload name). See 'resolveWorkload'.
resolveWorkloads
  :: Map String (Pipe.Pipe key input payload)
  -> Map String (Recycler key input payload)
  -> Map String Validated.Workload
  -> IO (Map String (Workload key input payload))
resolveWorkloads resolvedPipes resolvedRecyclers workloadsMap = do
  (resolvedWorkloads, _) <- foldlM
    (\(acc, cache) (wlName, validatedWorkload) -> do
      (resolved, cache') <-
        resolveWorkload
          validatedWorkload
          cache
          (resolvedPipes Map.! wlName)
          (recyclerInternal (resolvedRecyclers Map.! wlName))
      pure (Map.insert wlName resolved acc, cache')
    )
    (Map.empty, Map.empty)
    (Map.toAscList workloadsMap)
  pure resolvedWorkloads

-- | Resolve a single workload: build each target's rate-limited recycling fetch
-- and resolve each target's rate limiter.
--
-- The 'Pipe' and its 'Recycler' come from the 'pipes' \/ 'recyclers' pools
-- (created by 'resolvePipes' \/ 'resolveRecyclers') and are passed in so that
-- all of the workload's targets share the same underlying queues and recycle
-- loop.
--
-- Cascading defaults and conflict checks have already been performed by
-- "Cardano.Benchmarking.PullFiction.Config.Validated". This function only
-- creates rate limiters and fetchers.
resolveWorkload
  :: Validated.Workload
  -- | Limiter cache (threaded as a pure accumulator).
  -> LimiterCache
  -- | Pipe shared by all the workload's targets (from the 'pipes' pool).
  -> Pipe.Pipe key input payload
  -- | Recycler for this workload (from the 'recyclers' pool).
  -> Recycler.Recycler key input payload
  -> IO (Workload key input payload, LimiterCache)
resolveWorkload validatedWorkload cache0 thePipe recycler = do
  let wlName = Validated.workloadName validatedWorkload
      validatedTargets = Validated.targets validatedWorkload
  (resolvedTargets, cache') <- foldlM
    (\(acc, cache) (tName, validatedTarget) -> do
      (resolved, cache'') <-
        resolveTarget cache thePipe recycler validatedTarget
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
-- build its rate-limited recycling fetch from the workload's recycler, then
-- build the 'Target' record.
resolveTarget
  :: LimiterCache
  -> Pipe.Pipe key input payload
  -> Recycler.Recycler key input payload
  -> Validated.Target
  -> IO (Target key input payload, LimiterCache)
resolveTarget cache thePipe recycler validatedTarget = do
  (limiter, cache') <- getOrCreateLimiter cache validatedTarget
  let onEx = Validated.onExhaustion validatedTarget
      -- The recycling fetch: fetch from the pipe, tell the recycler a dequeue
      -- happened, deliver the payload. Runtime only calls the pipe's fetcher
      -- and fires the dequeue event. The recycler decides whether that dequeue
      -- matters.
      inner = Pipe.payloadFetcher thePipe limiter onEx
      fetcher = Pipe.PayloadFetcher
        { Pipe.fetchPayload = do
            (key, payload) <- Pipe.fetchPayload inner
            Recycler.onDequeue recycler key
            pure payload
        , Pipe.tryFetchPayload = do
            mKeyPayload <- Pipe.tryFetchPayload inner
            case mKeyPayload of
              Nothing             -> pure Nothing
              Just (key, payload) -> do
                Recycler.onDequeue recycler key
                pure (Just payload)
        }
  pure ( Target
           { targetName    = Validated.targetName validatedTarget
           , targetFetcher = fetcher
           , rateLimiter   = limiter
           , maxBatchSize  = Validated.maxBatchSize validatedTarget
           , onExhaustion  = onEx
           , targetAddr    = Validated.addr validatedTarget
           , targetPort    = Validated.port validatedTarget
           }
       , cache'
       )

-- | Look up or create a 'RL.RateLimiter' for a target. Limiters are shared by
-- the pre-computed 'Validated.rateLimitKey', which encodes the sharing scope:
--
-- * @\@global@: one limiter for all targets across all workloads.
-- * @workloadName@: one per workload.
-- * @workloadName.targetName@: one per target.
-- * no rate-limit source: 'RL.newUnlimited' (uncached).
--
-- A cache hit reuses the existing limiter. A miss creates a
-- 'RL.newTokenBucket', inserts it, and returns it.
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

