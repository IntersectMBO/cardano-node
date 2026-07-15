{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | The 'Recycler': the single owner of closed-loop input recycling, kept out
-- of the 'Pipe' so the pipe stays a plain queue pair. Its /user/
-- ('Config.Runtime') only informs it of the three payload-lifecycle events
-- (build, dequeue or confirm\/orphan) via 'onBuild', 'onDequeue' and
-- 'onConfirm'. It never deals with a recycle strategy. Each hook just fires
-- (fire-and-forget) a 'PayloadLifecycle' onto the recycler's queue. The
-- recycle worker ('runRecycler') reads that queue on its own thread and does
-- all the work, applying the 'Raw.RecycleStrategy':
--
--   * 'Raw.RecycleOnBuild':   recycle a payload's produced outputs as soon
--                             as it is built.
--   * 'Raw.RecycleOnDequeue': recycle them when a worker dequeues the payload
--                             from the pipe.
--   * 'Raw.RecycleOnConfirm': recycle them when the node confirms the
--                             payload, or recover the consumed inputs if it
--                             is orphaned (rolled back).
--
-- The worker remembers, in its own /local/ map, the half-completed recycles. A
-- payload has two input sets: the produced /outputs/ (fed back on success)
-- and the /consumed/ inputs (recovered on an orphan). It is recycled once BOTH
-- its 'Built' event and its release event ('Dequeued' \/ 'Confirmed' \/
-- 'Orphaned') have arrived, and the worker handles them in either order: a
-- 'Slot' holds whichever came first until the other completes it, so no
-- recycle is lost no matter the order the two events are enqueued. The worker
-- is the sole writer to the pipe's input queue (which is what will later let
-- it shuffle a drained batch), and the map is worker-local, so no shared or
-- locked state is needed.
--
-- The worker emits two observable events for tracing: /Pending/ each time a
-- payload enters the backlog (it holds a payload's recyclable inputs pending
-- its confirm\/dequeue\/orphan match) and /AddToPipe/ each time it adds the
-- recycled inputs to the pipe (via 'Pipe.addInputs').
--
-- INVARIANT: each in-flight payload must have a unique @key@. The map is keyed
-- by @key@, so two live payloads sharing a key would clobber each other's
-- recyclable inputs. Callers satisfy this with a per-payload identifier, Main
-- uses the txId (distinct inputs ⇒ distinct txId, and the closed loop only
-- reuses a key after the previous payload under it has been recycled).
module Cardano.Benchmarking.PullFiction.Internal.Recycler
  ( -- * Recycler.
    Recycler, OnPendingEvent, OnAddToPipeEvent, mkRecycler
  , runRecycler
    -- * Lifecycle event hooks (fire-and-forget).
  , onBuild
  , onDequeue
  , onConfirm
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (myThreadId)
import GHC.Conc (labelThread)
import Numeric.Natural (Natural)
-----------
-- async --
-----------
import Control.Concurrent.Async qualified as Async
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
------------------
-- pull-fiction --
------------------
import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw
import Cardano.Benchmarking.PullFiction.Internal.Pipe qualified as Pipe

--------------------------------------------------------------------------------

-- | A point in a payload's lifecycle, fired (fire-and-forget) onto the
-- recycler's queue by 'Config.Runtime' and interpreted by the worker per
-- strategy. Each says /what happened/, not /how to recycle/.
data PayloadLifecycle key input
  = -- | A payload was built: keyed by @key@, with its consumed inputs and
    -- produced outputs.
    Built !key ![input] ![input]
    -- | A payload was dequeued from the pipe by a worker.
  | Dequeued !key
    -- | A payload was confirmed.
  | Confirmed !key
    -- | A payload was orphaned (rolled back).
  | Orphaned !key

-- | Handler fired by the recycle worker each time a payload enters the pending
-- backlog (built or released first, waiting for its match), with the inputs it
-- is now holding (the built outputs, or none when a release arrived first) and
-- the resulting pending count (e.g. for tracing). @pending@ is the number of
-- payloads the recycler is currently holding: one of their two lifecycle
-- signals (build, or a dequeue\/confirm\/orphan release) has arrived and they
-- await the other (the pending-recycle backlog).
-- It is NOT a pipe queue depth. Created by the caller and passed to
-- 'mkRecycler', mirroring the 'Pipe' event handlers.
type OnPendingEvent input = [input] -> Natural -> IO ()

-- | Handler fired by the recycle worker each time it adds recycled inputs to
-- the pipe's input queue, with those inputs and the resulting pending count
-- (e.g. for tracing). Created by the caller and passed to 'mkRecycler',
-- mirroring the 'Pipe' event handlers.
type OnAddToPipeEvent input = [input] -> Natural -> IO ()

-- | Recycling state for one 'Pipe'. Construct with 'mkRecycler' only. Holds no
-- slot map and no worker async: the map is the worker's own local state and
-- the async is returned by 'runRecycler'.
data Recycler key input payload = Recycler
  { -- | The workload's recycle strategy ('Nothing' is no recycling).
    recyclerStrategy :: !(Maybe Raw.RecycleStrategy)
    -- | The pipe whose input queue receives recycled inputs (via
    -- 'Pipe.addInputs').
  , recyclerPipe     :: !(Pipe.Pipe key input payload)
    -- | The fire-and-forget lifecycle-signal queue: hooks write, the worker
    -- reads. The worker handles a key's 'Built' and its release event in either
    -- order, so their relative enqueue order does not affect correctness.
  , recyclerInbox    :: !(STM.TQueue (PayloadLifecycle key input))
    -- | Fired by the worker when a payload enters the pending backlog, with the
    -- inputs it is now holding and the resulting pending count (e.g. for
    -- tracing).
  , recyclerOnPending   :: !(OnPendingEvent input)
    -- | Fired by the worker with the inputs it adds to the pipe and the
    -- resulting pending count, each time it recycles (e.g. for tracing).
  , recyclerOnAddToPipe :: !(OnAddToPipeEvent input)
  }

--------------------------------------------------------------------------------

-- | Build the recycler state for one pipe and strategy: just the event queue.
-- It spawns no worker: call 'runRecycler' for that.
mkRecycler
  :: Maybe Raw.RecycleStrategy
  -- ^ The workload's recycle strategy.
  -> Pipe.Pipe key input payload
  -- ^ Pipe to recycle into.
  -> OnPendingEvent input
  -- ^ Fired with the held inputs and pending count each time a payload enters
  -- the backlog.
  -> OnAddToPipeEvent input
  -- ^ Fired with the inputs and pending count each time the worker adds
  -- recycled inputs to the pipe.
  -> IO (Recycler key input payload)
mkRecycler strategy pipe onPending onAddToPipe = do
  inbox <- STM.newTQueueIO
  pure Recycler
    { recyclerStrategy    = strategy
    , recyclerPipe        = pipe
    , recyclerInbox       = inbox
    , recyclerOnPending   = onPending
    , recyclerOnAddToPipe = onAddToPipe
    }

-- | The worker's local record of a /half-completed/ recycle for one @key@. A
-- payload is recycled once BOTH its 'Built' event and its release event
-- ('Dequeued' \/ 'Confirmed' \/ 'Orphaned') have arrived. They can arrive in
-- either order, so the worker keeps whichever came first here until the other
-- completes the recycle. Nothing is lost regardless of enqueue order.
data Slot input
  = -- | 'Built' arrived first: its @consumed@ inputs and produced @outputs@,
    -- waiting for a release to pick which set to recycle.
    BuiltFirst ![input] ![input]
    -- | A release arrived first: the selector it will apply to
    -- @(consumed, outputs)@ once 'Built' lands (@snd@ = outputs on
    -- dequeue\/confirm, @fst@ = consumed inputs on orphan).
  | ReleasedFirst !(([input], [input]) -> [input])

-- | Spawn the recycle worker and return it (unlinked, so the caller links it,
-- as with the builder async).
--
-- The worker drains the lifecycle-signal queue and acts on every signal,
-- keeping its half-completed recycles in a worker-local map (only it touches
-- it). Per 'Raw.RecycleStrategy':
--
--   * 'RecycleOnBuild' recycles a tx's outputs as soon as 'Built' arrives.
--   * the deferred strategies recycle a tx once both its 'Built' and its
--     release event have arrived, in either order. 'Dequeued' \/ 'Confirmed'
--     recycle the outputs, 'Orphaned' recovers the consumed inputs.
--
-- It fires 'recyclerOnPending' when a payload enters the backlog and
-- 'recyclerOnAddToPipe' when inputs are added to the pipe, both with the
-- resulting pending count. It needs no observer or fetcher: the signals arrive
-- through the 'onBuild' \/ 'onDequeue' \/ 'onConfirm' hooks. When the strategy
-- does not recycle, no hook ever fires a signal, so the worker just parks on
-- the forever-empty queue.
runRecycler
  :: Ord key
  => Recycler key input payload
  -> String
  -- ^ Builder name (used to label the recycler thread).
  -> IO (Async.Async ())
runRecycler recycler name = Async.async $ do
  tid <- myThreadId
  labelThread tid (name ++ "/recycler")
  let -- Pending depth (the number of half-completed recycles) as a 'Natural'.
      depthOf s = fromIntegral (Map.size s)
      -- Add inputs to the pipe (skipping the enqueue when there are none,
      -- like 'Pipe.addInputs'), then fire the add-to-pipe handler after the
      -- fact with the resulting pending count. Always fired, so a deferred
      -- backlog exit is traced even when its recycled set is empty (pairing
      -- with 'hold').
      addToPipe inputs slots' = do
        case inputs of
          [] -> pure ()
          _  -> Pipe.addInputs (recyclerPipe recycler) inputs
        recyclerOnAddToPipe recycler inputs (depthOf slots')
      -- A payload just entered the pending backlog. Fire the pending handler
      -- with the inputs it is now holding and the resulting pending count
      -- (after the fact, like 'addToPipe').
      hold inputs slots' = recyclerOnPending recycler inputs (depthOf slots')
      -- 'Built' for a deferred strategy. If the release already arrived
      -- (ReleasedFirst), recycle its chosen set now, else hold the tx's
      -- outputs.
      onBuiltEvent key consumed outputs slots =
        case Map.lookup key slots of
          Just (ReleasedFirst select) -> do
            let slots' = Map.delete key slots
            addToPipe (select (consumed, outputs)) slots'
            pure slots'
          _ -> do
            let slots' = Map.insert key (BuiltFirst consumed outputs) slots
            hold outputs slots'
            pure slots'
      -- A release event, with the @select@ for its set. If 'Built' already
      -- arrived (BuiltFirst), recycle now, else hold the release (no inputs
      -- yet, they arrive with 'Built').
      onRelease key select slots =
        case Map.lookup key slots of
          Just (BuiltFirst consumed outputs) -> do
            let slots' = Map.delete key slots
            addToPipe (select (consumed, outputs)) slots'
            pure slots'
          _ -> do
            let slots' = Map.insert key (ReleasedFirst select) slots
            hold [] slots'
            pure slots'
      go slots = do
        event <- STM.atomically $ do
          ---------- STM START ----------
          STM.readTQueue (recyclerInbox recycler)
          ---------- STM ENDED ----------
        slots' <- case event of
          Built key consumed outputs ->
            case recyclerStrategy recycler of
              Just Raw.RecycleOnBuild -> addToPipe outputs slots >> pure slots
              _                       -> onBuiltEvent key consumed outputs slots
          Dequeued  key -> onRelease key snd slots
          Confirmed key -> onRelease key snd slots
          Orphaned  key -> onRelease key fst slots
        go slots'
  go (Map.empty :: Map.Map key (Slot input))

--------------------------------------------------------------------------------

-- | Record a payload's recyclable inputs with the recycler
-- (fire-and-forget): its @key@, the @consumed@ inputs and the produced
-- @outputs@. The deferred strategies ('RecycleOnDequeue' \/ 'RecycleOnConfirm')
-- recycle these once a matching 'onDequeue' \/ 'onConfirm' for the same @key@
-- also arrives, in either order (see 'runRecycler'). A no-op when the strategy
-- does not recycle.
onBuild
  :: Recycler key input payload
  -> key     -- ^ Key identifying the payload (e.g. its txId).
  -> [input] -- ^ Inputs consumed to build this payload.
  -> [input] -- ^ New inputs (outputs) produced by this payload.
  -> IO ()
onBuild recycler key consumedInputs outputInputs =
  case recyclerStrategy recycler of
    Nothing -> pure ()
    Just _  -> STM.atomically $ do
      ---------- STM START ----------
      STM.writeTQueue
        (recyclerInbox recycler)
        (Built key consumedInputs outputInputs)
      ---------- STM ENDED ----------

-- | Inform the recycler that a payload was dequeued from the pipe by a worker
-- (fire-and-forget), with its @key@. 'Config.Runtime' calls this on every
-- dequeue. Only 'Raw.RecycleOnDequeue' reacts (the others recycle at build or
-- confirm), so it enqueues nothing otherwise, keeping the fetch path free of
-- ignored events.
onDequeue :: Recycler key input payload -> key -> IO ()
onDequeue recycler key =
  case recyclerStrategy recycler of
    Just Raw.RecycleOnDequeue -> STM.atomically $ do
      ---------- STM START ----------
      STM.writeTQueue (recyclerInbox recycler) (Dequeued key)
      ---------- STM ENDED ----------
    _ -> pure ()

-- | Inform the recycler that a payload was confirmed or orphaned
-- (fire-and-forget). Only 'Raw.RecycleOnConfirm' reacts. The caller decides
-- when a payload is confirmed or orphaned. An observer bridge in
-- 'Config.Runtime' forwards the node's confirm\/orphan stream here.
onConfirm :: Recycler key input payload -> key -> Bool -> IO ()
onConfirm recycler key isOrphan =
  case recyclerStrategy recycler of
    Just (Raw.RecycleOnConfirm _) -> STM.atomically $ do
      ---------- STM START ----------
      STM.writeTQueue
        (recyclerInbox recycler)
        (if isOrphan then Orphaned key else Confirmed key)
      ---------- STM ENDED ----------
    _ -> pure ()

