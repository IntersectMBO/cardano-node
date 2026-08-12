{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | Resolves a 'Validated.Config' into a 'Runtime': live STM resources (queues,
-- rate limiters) grouped into name-keyed pools, with the threads running them.
-- See 'Runtime' for the pools and 'resolve' for resolution.
module Cardano.Benchmarking.PullFiction.Config.Runtime
  ( -- * Runtime.
    Runtime
  , config, builders, pipes, recyclers, observers, forwarders, workloads, asyncs
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
    -- * Forwarder.
  , Forwarder
  , forwarderName, forwarderObserver, forwarderRecycler, forwarderAsync
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
-- Everything is a name-keyed pool. Today the name is the workload name for
-- 'builders', 'pipes', 'recyclers' and 'workloads' (one of each per workload),
-- the config key for 'observers', and the wiring path for 'forwarders' (see
-- 'Forwarder'), but the pools are the natural home once the config allows
-- sharing or interconnection.
--
-- Each pool entry also carries its own name as a field (repeating its 'Map'
-- key, for labels and traces) and the async(s) running it, so 'asyncs' just
-- gathers all of them. The exception is 'pipes': a pipe is named by its 'Map'
-- key like every entry, but the entry itself is pure structure, a queue pair
-- ('Pipe.Pipe') carrying neither a name field nor an async.
data Runtime key input payload = Runtime
  { -- | The original validated configuration.
    config     :: !(Validated.Config input)
    -- | Resolved builders (build loops), keyed by name.
  , builders   :: !(Map String Builder)
    -- | Resolved pipes (queue pairs), keyed by name. Pure structure: the only
    -- pool whose entries do not repeat their name as a field (it lives only
    -- in the 'Map' key) and run no async, just the queue pair.
  , pipes      :: !(Map String (Pipe.Pipe key input payload))
    -- | Resolved recyclers, keyed by name.
  , recyclers  :: !(Map String (Recycler key input payload))
    -- | Resolved observers, keyed by name.
  , observers  :: !(Map String (Observer key))
    -- | Resolved forwarders (observer to recycler bridges), keyed by wiring
    -- path (see 'Forwarder').
  , forwarders :: !(Map String Forwarder)
    -- | Resolved workloads (target groups), keyed by name.
  , workloads  :: !(Map String (Workload key input payload))
    -- | All asyncs (builders + recyclers + forwarders + observers), linked.
    -- Caller should append their own worker asyncs for cleanup.
  , asyncs     :: ![Async.Async ()]
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
-- that leaks a batch merely shrinks the set of recyclable inputs.
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
-- 'bhRunBuilder' in it, handing over a 'BuilderApi' wired to that builder's
-- pipe and recycler. The builder owns its loop (batching, grouping, coin
-- selection); the engine owns the thread and the machinery behind the API.
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
-- enter the backlog and as inputs are added to the pipe's input queue. Kept
-- separate from 'PipeHandle' because recycling is a 'Recycler' concern, not a
-- 'Pipe' one.
data RecyclerHandle key input payload = RecyclerHandle
  { -- | Fired by the recycler with the added entry (key, consumed inputs,
    -- produced outputs) and the resulting backlog size each time it adds a
    -- payload to the backlog.
    rhOnAddToBacklog :: !(Recycler.OnAddToBacklogEvent key input)
    -- | Fired by the recycler with the inputs it adds to the pipe and the
    -- resulting backlog size, each recycle.
  , rhOnAddToPipe    :: !(Recycler.OnAddToPipeEvent input)
    -- | Fired with the dropped inputs, the dropped payloads, the fresh inputs
    -- and the resulting backlog size each time a reset drops the queued inputs
    -- and payloads and reseeds the input queue.
  , rhOnReset        :: !(Recycler.OnResetEvent key input payload)
    -- | Optional recovery action. The forwarder runs it on an orphan and feeds
    -- its result to 'Recycler.reset' (keyed under on_confirm, see
    -- 'resolveForwarders'), which drops the pipe's queued inputs and payloads
    -- and reseeds the input queue with it.
  , rhRecover        :: !(Maybe (IO [input]))
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
    --
    -- Each call must create an independent subscription (every subscriber sees
    -- every event). Observers are independent entities: one observer can serve
    -- many workloads, and what each does with the events (plain confirm
    -- recycling, rollback recovery) is decided per workload, never here.
  , ohSubscribe :: !(IO (STM.STM (Either key key)))
  }

--------------------------------------------------------------------------------

-- | A resolved build loop: it takes inputs from a pipe, builds payloads, and
-- signals a recycler. It references its 'pipes' and 'recyclers' entries /by
-- name/ (today both the workload name). Its loop thread is 'builderAsync', also
-- collected into 'asyncs'.
data Builder = Builder
  { -- | This builder's name (today the workload name).
    builderName     :: !String
    -- | Name of the 'pipes' entry it drives (takes from / adds to).
  , builderPipe     :: !String
    -- | Name of the 'recyclers' entry it signals via 'Recycler.addToBacklog'.
  , builderRecycler :: !String
    -- | Linked async running the build loop.
  , builderAsync    :: !(Async.Async ())
  }

-- | A resolved recycler: the 'Internal.Recycler' logic plus the async running
-- its worker. A thin wrapper so the 'recyclers' pool carries its own thread,
-- like 'Builder' and 'Observer'. The 'Recycler.Recycler' inside deliberately
-- holds no async. The observer bridge for 'RecycleOnConfirm' is a separate
-- thread ('resolveForwarders'), not held here.
data Recycler key input payload = Recycler
  { -- | This recycler's name (today the workload name).
    recyclerName     :: !String
    -- | The underlying recycler from "Internal.Recycler".
  , recyclerInternal :: !(Recycler.Recycler key input payload)
    -- | Optional recovery action (from 'rhRecover'): the forwarder runs it on
    -- an orphan and feeds its result to 'Recycler.reset'.
  , recyclerRecover  :: !(Maybe (IO [input]))
    -- | Linked async running its worker (the sole writer of recycled inputs
    -- back onto the pipe).
  , recyclerAsync    :: !(Async.Async ())
  }

-- | A resolved observer with its lifecycle managed by 'resolve': the
-- caller-provided 'ObserverHandle' plus the async running its 'ohRun'.
data Observer key = Observer
  { -- | Key from the config's @\"observers\"@ object.
    observerName   :: !String
    -- | The caller-provided handle ('ohRun' + 'ohSubscribe'). The forwarders
    -- subscribe to it for the workloads whose strategy names this observer.
  , observerHandle :: !(ObserverHandle key)
    -- | Linked async running the observer connection ('ohRun').
  , observerAsync  :: !(Async.Async ())
  }

-- | A resolved forwarder: the bridge thread that reads one observer
-- subscription and feeds each event through its workload's strategy wiring into
-- the recycler actions. Unlike the other pools it is keyed by the config
-- reference that wired it, @workload\/site\/observer@, with @site@ being
-- @recycle@ (the on_confirm strategy's observer) or @recovery@ (the recovery's
-- explicit observer), since one workload may subscribe to several observers.
-- Key uniqueness relies on names containing no @\'/\'@, enforced at validation
-- time like the rate-limit key scheme. It references what it bridges by name,
-- like 'Builder' does.
data Forwarder = Forwarder
  { -- | This forwarder's name (the @workload\/site\/observer@ wiring path, also
    -- the thread label).
    forwarderName     :: !String
    -- | Name of the 'observers' entry it subscribes to.
  , forwarderObserver :: !String
    -- | Name of the 'recyclers' entry it feeds.
  , forwarderRecycler :: !String
    -- | Linked async reading the subscription forever.
  , forwarderAsync    :: !(Async.Async ())
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
    -- 'resolveTarget' (wraps the shared pipe's fetcher through the workload's
    -- dequeue wiring).
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
-- (each references its pipe), then builders and targets (each references a
-- pipe and recycler), then forwarders (each bridges an observer to a
-- recycler). See 'Runtime' for each pool's naming.
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
  -- fires as payloads enter the backlog and as it recycles inputs.
  -> (Int -> String -> IO (RecyclerHandle key input payload))
  -- | Observer factory (index, name, config).
  -> (Int -> String -> Raw.Observer -> IO (ObserverHandle key))
  -> Validated.Config input
  -> IO (Runtime key input payload)
resolve mkBuilderFn mkPipeHandleFn mkRecyclerHandleFn mkObserverFn validatedConfig = do
  let workloadsMap = Validated.workloads validatedConfig
  -- Distribute initial inputs equally across workloads, keyed by workload name.
  -- Both Maps share the same ascending key order, so zip + fromAscList is safe.
  let inputsByWorkload =
        Map.fromAscList $ zip
          (Map.keys workloadsMap)
          (partitionInputs
            (Map.size workloadsMap)
            (toList (Validated.initialInputs validatedConfig))
          )
  -- Resolve the name-keyed pools in dependency order: each resolver takes the
  -- already-resolved pools its entries reference. Observers stand alone, a
  -- recycler needs its pipe, a builder and a workload's targets need their pipe
  -- and recycler, and a forwarder bridges an observer to a recycler.
  resolvedObservers  <- resolveObservers  mkObserverFn (Validated.observers validatedConfig)
  resolvedPipes      <- resolvePipes      mkPipeHandleFn inputsByWorkload workloadsMap
  resolvedRecyclers  <- resolveRecyclers  mkRecyclerHandleFn resolvedPipes workloadsMap
  resolvedBuilders   <- resolveBuilders   mkBuilderFn resolvedPipes resolvedRecyclers workloadsMap
  resolvedWorkloads  <- resolveWorkloads  resolvedPipes resolvedRecyclers workloadsMap
  resolvedForwarders <- resolveForwarders resolvedRecyclers resolvedObservers workloadsMap
  -- Assemble the final runtime.
  pure Runtime
    { config     = validatedConfig
    , builders   = resolvedBuilders
    , pipes      = resolvedPipes
    , recyclers  = resolvedRecyclers
    , observers  = resolvedObservers
    , forwarders = resolvedForwarders
    , workloads  = resolvedWorkloads
    -- Collect all asyncs. Each pool entry carries a single async
    -- ('builderAsync', 'recyclerAsync', 'forwarderAsync', 'observerAsync').
    , asyncs     =    map builderAsync   (Map.elems resolvedBuilders)
                   ++ map recyclerAsync  (Map.elems resolvedRecyclers)
                   ++ map forwarderAsync (Map.elems resolvedForwarders)
                   ++ map observerAsync  (Map.elems resolvedObservers)
    }

--------------------------------------------------------------------------------
-- Named-pool resolution (builders, pipes, recyclers, observers).
--------------------------------------------------------------------------------

-- Definitions are listed builder, pipe, recycler, observer for consistency
-- with the field order. 'resolve' calls them in the reverse (dependency) order.

-- | Resolve one builder per workload into a name-keyed pool: spawn its build
-- loop over the workload's pipe and recycler (both by workload name). Each loop
-- takes inputs, builds a payload, records the tx's recyclable inputs with the
-- recycler ('Recycler.addToBacklog'), then enqueues the payload. Each 'Builder'
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
          strategy = Raw.builderRecycle (Validated.builder validatedWorkload)
      builderHandle <- mkBuilderFn ix wlName (Validated.builder validatedWorkload)
      -- The safe capability API handed to the builder loop: closures over this
      -- builder's pipe and recycler, never the raw resources. 'baAddPayload'
      -- holds the recyclable inputs with the recycler, then makes the payload
      -- dequeuable, in that order: a release must never precede its
      -- 'AddToBacklog' (see the Recycler invariants). Strategy wiring: no
      -- strategy holds nothing, on_build confirms right at the build
      -- ('Recycler.releaseOutputs', reason-free), the deferred strategies
      -- confirm later (at dequeue or on an observer confirm). 'baDropInputs'
      -- abandons a batch: 'Pipe.takeInputs' already removed it from the input
      -- queue, so skipping the recycler is all it takes to drop it.
      let recordBuild key consumed outputInputs = case strategy of
            Nothing -> pure ()
            Just Raw.RecycleOnBuild -> do
              Recycler.addToBacklog recycler key consumed outputInputs
              Recycler.releaseOutputs recycler key
            Just _ ->
              Recycler.addToBacklog recycler key consumed outputInputs
          api = BuilderApi
            { baTakeInputs = Pipe.takeInputs thePipe
            , baAddPayload = \key payload consumed outputInputs -> do
                recordBuild key consumed outputInputs
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
-- 'RecycleOnConfirm' is resolved separately, by 'resolveForwarders'.
resolveRecyclers
  :: Ord key
  -- | Recycler-events factory (index, name): the tracing handlers the recycler
  -- fires as payloads enter the backlog and as it recycles inputs.
  => (Int -> String -> IO (RecyclerHandle key input payload))
  -> Map String (Pipe.Pipe key input payload)
  -> Map String Validated.Workload
  -> IO (Map String (Recycler key input payload))
resolveRecyclers mkRecyclerHandleFn resolvedPipes workloadsMap =
  Map.fromAscList <$> mapM
    (\(ix, (wlName, _validatedWorkload)) -> do
      recyclerHandle <- mkRecyclerHandleFn ix wlName
      internal <- Recycler.mkRecycler
                    (resolvedPipes Map.! wlName)
                    (rhOnAddToBacklog recyclerHandle)
                    (rhOnAddToPipe recyclerHandle)
                    (rhOnReset recyclerHandle)
      worker <- Recycler.runRecycler internal wlName
      Async.link worker
      pure ( wlName
           , Recycler { recyclerName     = wlName
                      , recyclerInternal = internal
                      , recyclerRecover  = rhRecover recyclerHandle
                      , recyclerAsync    = worker
                      }
           )
    )
    -- Zero-based index and name provided to the recycler factory.
    (zip [0..] (Map.toAscList workloadsMap))

-- | Resolve the observers into a name-keyed pool, spawning each one's 'ohRun'
-- in a labeled, linked async. Each 'Observer' keeps its 'ObserverHandle' (as
-- 'observerHandle'), which the forwarders subscribe to on behalf of each
-- referencing workload's recycler.
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
-- Forwarders (observer to recycler).
--------------------------------------------------------------------------------

-- | Resolve the forwarders that bridge observers to recyclers. Every forwarder
-- is the same thing: a thread that reads one subscribed observer stream and
-- feeds each event through the workload's strategy wiring into the recycler
-- actions. The recycler knows no strategy, this wiring is the only place an
-- observer event is interpreted:
--
--   * 'Right' (confirmed) becomes 'Recycler.releaseOutputs' under on_confirm
--     and is ignored otherwise.
--   * 'Left' (orphaned) runs the workload's recovery action when one is
--     wired (a rollback invalidates the queued inputs, and the payloads
--     built from them, beyond the orphaned payload itself) and feeds its
--     result to 'Recycler.reset'. Under on_confirm the reset is KEYED:
--     built payloads stay held until their release, so the recycler applies
--     the reset only while the orphaned payload is still held, and a
--     foreign or duplicate orphan is ignored. The orphan burst of one
--     rollback then collapses at the backlog gate (at one recovery query
--     per orphan event). Under the optimistic strategies the payload was
--     already released at build or dequeue, so the reset is unkeyed and
--     unconditional, and the forwarder then discards the orphan burst the
--     rollback already delivered (one rollback, one recovery) while still
--     forwarding the confirms in it. Without a recovery a 'Left' becomes
--     'Recycler.releaseConsumed' under on_confirm, and is ignored otherwise.
--
-- A workload subscribes one forwarder per observer reference in its builder:
-- the 'Raw.RecycleOnConfirm' observer, and the observer its 'Raw.Recovery'
-- names explicitly. An on_confirm recovery without an observer adds no
-- reference (the default, the confirm subscription already carries the
-- orphans). Naming the confirm observer again explicitly creates a second
-- subscription, each event then delivered twice (duplicate confirms, and
-- keyed duplicate resets, are ignored at the unknown key).
--
-- These are the sole bridges from the observers (which know a transaction's
-- outcome) to the recyclers, which are observer-agnostic and drain only their
-- own event queues. Workloads with no strategy or no observer reference
-- contribute no forwarder. Returns the name-keyed 'forwarders' pool, each
-- entry named by its wiring path (see 'Forwarder'), whose asyncs 'resolve'
-- collects into 'asyncs'.
resolveForwarders
  -- | The recyclers to notify, keyed by workload name.
  :: Map String (Recycler key input payload)
  -- | Observers keyed by name (a strategy names the ones to subscribe to).
  -> Map String (Observer key)
  -- | Validated workloads (their recycle strategy and recovery select the
  -- observers).
  -> Map String Validated.Workload
  -> IO (Map String Forwarder)
resolveForwarders resolvedRecyclers resolvedObservers workloadsMap =
  fmap (Map.fromList . concat) $ mapM
    (\(wlName, validatedWorkload) ->
      case Raw.builderRecycle (Validated.builder validatedWorkload) of
        Nothing -> pure []
        Just strategy -> do
          -- All lookups are guaranteed present: recyclers are keyed by
          -- workload name, and 'Validated.validate' rejects an undefined
          -- observer reference.
          let recyclerEntry = resolvedRecyclers Map.! wlName
              recycler      = recyclerInternal recyclerEntry
              -- Strategy wiring: what a confirm means here (see the haddock
              -- above).
              confirmRight key = case strategy of
                Raw.RecycleOnConfirm _ -> Recycler.releaseOutputs recycler key
                _                      -> pure ()
              -- One subscription per observer reference, tagged with the
              -- config site that wired it ("recycle" or "recovery"): the
              -- tag makes the pool keys unique when both sites name the
              -- same observer (the documented double subscription).
              subscriptions =
                [ ("recycle", obsName)
                | Raw.RecycleOnConfirm obsName <- [strategy]
                ]
                ++ [ ("recovery", obsName)
                   | Just recovery <-
                       [ Raw.builderRecovery
                           (Validated.builder validatedWorkload)
                       ]
                   , Just obsName  <- [Raw.recoveryObserver recovery]
                   ]
          mapM
            (\(site, obsName) -> do
              let forwarderKey = wlName ++ "/" ++ site ++ "/" ++ obsName
              readEvent <- ohSubscribe
                (observerHandle (resolvedObservers Map.! obsName))
              let -- Strategy wiring: what an orphan means here (see the
                  -- haddock above).
                  step event = case event of
                    Right key -> confirmRight key
                    Left  key -> case recyclerRecover recyclerEntry of
                      Just recover -> do
                        fresh <- recover
                        case strategy of
                          -- on_confirm holds a payload until its release, so at
                          -- its orphan the key is still held and the reset can
                          -- be gated on it: the recycler drops a foreign or
                          -- duplicate one. No drain here, the backlog gate
                          -- absorbs the burst (at one recovery query per orphan
                          -- event).
                          Raw.RecycleOnConfirm _ ->
                            Recycler.reset recycler (Just key) fresh
                          -- The optimistic strategies released the payload at
                          -- build or dequeue, so an own orphan is no longer
                          -- held and the reset cannot be gated: unkeyed and
                          -- unconditional, followed by the drain that absorbs
                          -- the rollback's burst into one recovery.
                          _ -> do
                            Recycler.reset recycler Nothing fresh
                            drainOrphans
                      Nothing -> case strategy of
                        Raw.RecycleOnConfirm _ ->
                          Recycler.releaseConsumed recycler key
                        _ -> pure ()
                  -- Forward the events this subscription already delivered,
                  -- dropping orphans (the unkeyed reset that just ran covers
                  -- them), until it is momentarily empty. One rollback delivers
                  -- a burst of orphans, this absorbs it into one recovery. Only
                  -- the unkeyed reset path may drain: after a keyed reset the
                  -- recycler might have ignored it, and draining would then
                  -- discard genuine orphans.
                  drainOrphans = do
                    mEvent <- STM.atomically
                      ((Just <$> readEvent) `STM.orElse` pure Nothing)
                    case mEvent of
                      Nothing          -> pure ()
                      Just (Left _)    -> drainOrphans
                      Just (Right key) -> confirmRight key >> drainOrphans
              forwarder <- Async.async $ do
                -- Always labeled threads.
                tid <- myThreadId
                labelThread tid forwarderKey
                forever (STM.atomically readEvent >>= step)
              Async.link forwarder
              pure ( forwarderKey
                   , Forwarder { forwarderName     = forwarderKey
                               , forwarderObserver = obsName
                               , forwarderRecycler = wlName
                               , forwarderAsync    = forwarder
                               }
                   )
            )
            subscriptions
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
      -- Strategy wiring: only on_pull confirms a payload at dequeue
      -- ('Recycler.releaseOutputs', reason-free).
      confirmDequeued =
        case Raw.builderRecycle (Validated.builder validatedWorkload) of
          Just Raw.RecycleOnDequeue -> Recycler.releaseOutputs recycler
          _                         -> \_key -> pure ()
  (resolvedTargets, cache') <- foldlM
    (\(acc, cache) (tName, validatedTarget) -> do
      (resolved, cache'') <-
        resolveTarget cache thePipe confirmDequeued validatedTarget
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
-- build its rate-limited recycling fetch around the workload's dequeue wiring,
-- then build the 'Target' record.
resolveTarget
  :: LimiterCache
  -> Pipe.Pipe key input payload
  -- | The workload's dequeue wiring, fired with each dequeued payload's key
  -- (for on_pull it confirms the payload, otherwise a no-op).
  -> (key -> IO ())
  -> Validated.Target
  -> IO (Target key input payload, LimiterCache)
resolveTarget cache thePipe confirmDequeued validatedTarget = do
  (limiter, cache') <- getOrCreateLimiter cache validatedTarget
  let onEx = Validated.onExhaustion validatedTarget
      -- The recycling fetch: fetch from the pipe, fire the workload's dequeue
      -- wiring, deliver the payload.
      inner = Pipe.payloadFetcher thePipe limiter onEx
      fetcher = Pipe.PayloadFetcher
        { Pipe.fetchPayload = do
            (key, payload) <- Pipe.fetchPayload inner
            confirmDequeued key
            pure payload
        , Pipe.tryFetchPayload = do
            mKeyPayload <- Pipe.tryFetchPayload inner
            case mKeyPayload of
              Nothing             -> pure Nothing
              Just (key, payload) -> do
                confirmDequeued key
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

