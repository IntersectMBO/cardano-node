{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | The 'Recycler': the mechanism of closed-loop input recycling, kept out of
-- the 'Pipe' so the pipe stays a plain queue pair, and kept free of any recycle
-- strategy so it stays a plain mechanism. Its /user/ ('Config.Runtime') drives
-- it through four axiomatic actions, each firing (fire-and-forget) the
-- same-named 'PayloadLifecycle' instruction onto the recycler's queue:
--
--   * 'addToBacklog':    add a payload's two input sets, the /consumed/ inputs
--                        and the produced /outputs/, to the backlog.
--   * 'releaseOutputs':  release the held outputs to the input queue.
--                        Reason-free: the caller decides when a payload counts
--                        as confirmed (at build, at dequeue, or on an observer
--                        confirm), the recycler does not know.
--   * 'releaseConsumed': release the held consumed inputs to the input queue
--                        instead (the payload was discarded downstream, it will
--                        never confirm).
--   * 'reset':           drop every queued input and queued payload, then
--                        reseed the input queue with the caller's fresh inputs.
--                        Optionally gated by a key (see the unknown-key
--                        invariant below).
--
-- What to call and when, per recycle strategy, is wiring that lives entirely in
-- 'Config.Runtime'. The recycle worker ('runRecycler') reads the queue on its
-- own thread, keeps the backlog (the held input sets) in its own /local/ map,
-- and is the sole writer to the pipe's input queue (which is what will later
-- let it shuffle a drained batch), so no shared or locked state is needed.
--
-- The worker emits one observable event per action family for tracing:
-- /AddToBacklog/ each time it adds a payload's entry to the backlog
-- ('addToBacklog'), /AddToPipe/ each time a release adds recycled inputs to the
-- pipe (via 'Pipe.addInputs') and /Reset/ each time a reset drops the queued
-- inputs and payloads and reseeds the input queue.
--
-- INVARIANT: each in-flight payload must have a unique @key@. The map is keyed
-- by @key@, so two live payloads sharing a key would clobber each other's held
-- inputs. Callers satisfy this with a per-payload identifier, Main uses the
-- txId (distinct inputs imply distinct txId, and the closed loop only reuses a
-- key after the previous payload under it was recycled).
--
-- INVARIANT: a payload's 'addToBacklog' must be enqueued before any of its
-- releases, which the callers guarantee ('baAddPayload' holds before the
-- payload becomes dequeuable, and an observer can only see a submitted
-- payload). The worker therefore IGNORES a release for an unknown key: it is
-- foreign (the observer broadcast is unfiltered), a duplicate (a second
-- subscription), or already released. A keyed reset ('reset' with 'Just') obeys
-- the same rule: for an unknown key NO reset happens, the event is ignored. (To
-- keep these two ideas apart, comments here say the queues are /dropped/ by a
-- reset and an unknown-key event is /ignored/.) This is what lets the generator
-- share a chain with unrelated traffic without recycling, or resetting on,
-- other people's events.
module Cardano.Benchmarking.PullFiction.Internal.Recycler
  ( -- * Recycler.
    Recycler, mkRecycler
  , runRecycler
    -- * Event handlers.
  , OnAddToBacklogEvent
  , OnAddToPipeEvent
  , OnResetEvent
    -- * Actions (fire-and-forget).
  , addToBacklog
  , releaseOutputs
  , releaseConsumed
  , reset
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
import Cardano.Benchmarking.PullFiction.Internal.Pipe qualified as Pipe

--------------------------------------------------------------------------------

-- | A point in a payload's lifecycle, fired (fire-and-forget) onto the
-- recycler's queue by the same-named actions and interpreted by the worker.
-- Each says /what to do/, never /why/.
data PayloadLifecycle key input
  = -- | Add a payload's consumed inputs and produced outputs to the backlog,
    -- keyed by @key@.
    AddToBacklog !key ![input] ![input]
    -- | Release the held outputs of @key@ to the input queue.
  | ReleaseOutputs !key
    -- | Release the held consumed inputs of @key@ to the input queue.
  | ReleaseConsumed !key
    -- | Drop every queued input and payload, and reseed with these fresh
    -- inputs. With 'Just' a key, apply only while that key is held (an unknown
    -- key means no reset happens, the event is ignored). With 'Nothing', apply
    -- unconditionally.
  | Reset !(Maybe key) ![input]

-- | Handler fired by the recycle worker each time it adds a payload entry to
-- the backlog ('addToBacklog'), with the payload's key, its consumed inputs,
-- its produced outputs and the resulting backlog size (e.g. for tracing). The
-- whole entry is passed: what to render is the handler's decision, not the
-- recycler's. The final 'Natural' is the backlog size after this add: the
-- number of payloads the recycler is holding, awaiting their release. It is NOT
-- a pipe queue depth.
-- Created by the caller and passed to 'mkRecycler', mirrors 'Pipe' events.
type OnAddToBacklogEvent key input =
  key -> [input] -> [input] -> Natural -> IO ()

-- | Handler fired by the recycle worker each time it adds recycled inputs to
-- the pipe's input queue, with those inputs and the resulting backlog size
-- (e.g. for tracing).
-- Created by the caller and passed to 'mkRecycler', mirrors 'Pipe' events.
type OnAddToPipeEvent input = [input] -> Natural -> IO ()

-- | Handler fired by the recycle worker each time a reset drops the queued
-- inputs and payloads and reseeds the input queue, with the dropped inputs, the
-- dropped payloads (each with its key), the fresh inputs and the resulting
-- backlog size (always @0@, a reset clears the backlog) (e.g. for tracing).
-- Both dropped sets are passed in full: what to render (all of it, a count,
-- just the keys) is the handler's decision, not the recycler's.
-- Created by the caller and passed to 'mkRecycler', mirrors 'Pipe' events.
type OnResetEvent key input payload =
  [input] -> [(key, payload)] -> [input] -> Natural -> IO ()

-- | Recycling state for one 'Pipe'. Construct with 'mkRecycler' only. Holds no
-- backlog map and no worker async: the map is the worker's own local state and
-- the async is returned by 'runRecycler'.
data Recycler key input payload = Recycler
  { -- | The pipe whose input queue receives recycled inputs (via
    -- 'Pipe.addInputs').
    recyclerPipe           :: !(Pipe.Pipe key input payload)
    -- | Fired by the worker when it adds a payload's entry to the backlog,
    -- with the key, the consumed inputs, the produced outputs and the
    -- resulting backlog size (e.g. for tracing).
  , recyclerOnAddToBacklog :: !(OnAddToBacklogEvent key input)
    -- | Fired by the worker with the inputs it adds to the pipe and the
    -- resulting backlog size, each time it recycles (e.g. for tracing).
  , recyclerOnAddToPipe    :: !(OnAddToPipeEvent input)
    -- | Fired by the worker with the dropped inputs, the dropped payloads, the
    -- fresh inputs and the resulting backlog size, each time a reset drops the
    -- queued inputs and payloads and reseeds the input queue (e.g. for
    -- tracing).
  , recyclerOnReset        :: !(OnResetEvent key input payload)
    -- | The fire-and-forget lifecycle-signal queue: the actions write, the
    -- worker reads (FIFO, see the 'AddToBacklog' before release invariant
    -- above).
  , recyclerInbox          :: !(STM.TQueue (PayloadLifecycle key input))
  }

--------------------------------------------------------------------------------

-- | Build the recycler state for one pipe: just the event queue.
-- It spawns no worker: call 'runRecycler' for that.
mkRecycler
  -- | Pipe to recycle into.
  :: Pipe.Pipe key input payload
  -- | Fired with the added entry (key, consumed inputs, produced outputs) and
  -- the resulting backlog size each time a payload enters the backlog.
  -> OnAddToBacklogEvent key input
  -- | Fired with the inputs and backlog size each time the worker adds recycled
  -- inputs to the pipe.
  -> OnAddToPipeEvent input
  -- | Fired with the dropped inputs, the dropped payloads, the fresh inputs and
  -- the resulting backlog size each time a reset drops the queued inputs and
  -- payloads and reseeds the input queue.
  -> OnResetEvent key input payload
  -> IO (Recycler key input payload)
mkRecycler pipe onAddToBacklog onAddToPipe onReset = do
  inbox <- STM.newTQueueIO
  pure Recycler
    { recyclerPipe           = pipe
    , recyclerOnAddToBacklog = onAddToBacklog
    , recyclerOnAddToPipe    = onAddToPipe
    , recyclerOnReset        = onReset
    , recyclerInbox          = inbox
    }

-- | The worker's local record of one held payload: its consumed inputs and its
-- produced outputs, in that order, remembered at 'AddToBacklog' until a release
-- picks one set.
data Held input = Held ![input] ![input]

-- | Spawn the recycle worker and return it (unlinked, so the caller links it,
-- as with the builder async).
--
-- The worker drains the lifecycle-signal queue and acts on every event, keeping
-- the held input sets in a worker-local map (only it touches it):
-- 'AddToBacklog' holds, 'ReleaseOutputs' \/ 'ReleaseConsumed' recycle the
-- picked set and forget the key, 'Reset' drops the queued inputs and payloads
-- and reseeds the input queue with the fresh inputs it carries (gated on its
-- key being held, when keyed). A release, or a keyed reset, for an unknown key
-- is ignored (see the invariants in the module header).
--
-- It fires 'recyclerOnAddToBacklog' when it adds a payload to the backlog,
-- 'recyclerOnAddToPipe' when a release adds inputs to the pipe (both with the
-- resulting backlog size) and 'recyclerOnReset' when a reset drops the queued
-- inputs and payloads and reseeds the input queue. It needs no observer or
-- fetcher: the events arrive through the actions. When 'Config.Runtime' wires
-- no action calls for a workload, the worker just parks on the forever-empty
-- queue.
runRecycler
  :: Ord key
  => Recycler key input payload
  -- | Builder name (used to label the recycler thread).
  -> String
  -> IO (Async.Async ())
runRecycler recycler name = Async.async $ do
  tid <- myThreadId
  labelThread tid (name ++ "/recycler")
  let -- Backlog size (the number of held payloads) as a 'Natural'.
      depthOf s = fromIntegral (Map.size s)
      -- Recycle one set of a held payload and forget the key: add the set to
      -- the pipe ('Pipe.addInputs' is a no-op on an empty one), then fire the
      -- add-to-pipe handler.
      recycle key inputs backlog = do
        let backlog' = Map.delete key backlog
        Pipe.addInputs (recyclerPipe recycler) inputs
        -- Caller event: AddToPipe. Fired after the fact with the inputs and the
        -- resulting backlog size, either way (an empty recycle is traced too).
        (recyclerOnAddToPipe recycler) inputs (depthOf backlog')
        pure backlog'
      go backlog = do
        event <- STM.atomically $ do
          ---------- STM START ----------
          STM.readTQueue (recyclerInbox recycler)
          ---------- STM ENDED ----------
        backlog' <- case event of
          AddToBacklog key consumed outputs -> do
            let backlog' = Map.insert key (Held consumed outputs) backlog
            -- Caller event: AddToBacklog. The payload's entry just entered
            -- the backlog, fired after the fact with the key, both input
            -- sets and the resulting backlog size.
            (recyclerOnAddToBacklog recycler) key consumed outputs
              (depthOf backlog')
            pure backlog'
          -- The releases pick which held set returns to the input queue:
          -- the outputs or the consumed inputs. A release for an unknown
          -- key is ignored (see the module header).
          ReleaseOutputs key ->
            case Map.lookup key backlog of
              Just (Held _consumed outputs) -> recycle key outputs backlog
              Nothing                       -> pure backlog
          ReleaseConsumed key ->
            case Map.lookup key backlog of
              Just (Held consumed _outputs) -> recycle key consumed backlog
              Nothing                       -> pure backlog
          -- A keyed reset applies only while its key is held: like a release,
          -- an unknown key is foreign, a duplicate, or already superseded by an
          -- earlier reset that cleared the backlog. An unkeyed reset always
          -- applies.
          Reset maybeKey fresh -> do
            let applies = case maybeKey of
                  Nothing  -> True
                  Just key -> Map.member key backlog
            if not applies
              -- Ignored: NO reset happens, nothing is dropped from the queues,
              -- and the fresh inputs are discarded unused.
              then pure backlog
              -- Drop the queued inputs and the queued payloads built from them,
              -- reseed the input queue with the fresh inputs and clear the
              -- backlog (the reseed supersedes every held payload).
              else do
                droppedInputs <- Pipe.dropInputs (recyclerPipe recycler)
                -- Queued payloads are as stale as the queued inputs: they spend
                -- the lineage the reset abandons, so delivering them only feeds
                -- the targets transactions that no longer apply. Deliberately a
                -- separate STM transaction from the input drop above: fusing
                -- the two would gain no invariant (the builder's take, build
                -- and add span separate transactions anyway, so a payload built
                -- from pre-reset inputs can land after any boundary) and would
                -- catch less (a payload landing between the two flushes is
                -- swept by this later one). A payload the builder is completing
                -- right now can still slip in after this flush, which is
                -- harmless (a stale payload fails downstream).
                droppedPayloads <- Pipe.dropPayloads (recyclerPipe recycler)
                -- 'Pipe.addInputs' is a no-op on an empty fresh set.
                Pipe.addInputs (recyclerPipe recycler) fresh
                -- Caller event: Reset. Fired after the fact with the dropped
                -- inputs, the dropped payloads, the fresh inputs and the
                -- resulting backlog size, either way (a reset clears the
                -- backlog, so the size is 0).
                (recyclerOnReset recycler) droppedInputs droppedPayloads fresh 0
                pure Map.empty
        go backlog'
  go (Map.empty :: Map.Map key (Held input))

--------------------------------------------------------------------------------

-- | Add a payload's two input sets to the recycler's backlog (fire-and-forget):
-- the consumed inputs and the produced outputs, in that order. Exactly one
-- later release ('releaseOutputs' or 'releaseConsumed') picks the set to
-- recycle.
addToBacklog
  :: Recycler key input payload
  -- | Key identifying the payload (e.g. its txId).
  -> key
  -- | Inputs consumed to build this payload.
  -> [input]
  -- | New inputs (outputs) produced by this payload.
  -> [input]
  -> IO ()
addToBacklog recycler key consumedInputs outputInputs = do
  send recycler (AddToBacklog key consumedInputs outputInputs)

-- | Release the held outputs of a payload to the input queue (fire-and-forget).
-- Reason-free: 'Config.Runtime' decides when a payload counts as confirmed (at
-- build, at dequeue, or on an observer confirm), the recycler does not know.
releaseOutputs
  :: Recycler key input payload
  -- | Key identifying the payload (e.g. its txId).
  -> key
  -> IO ()
releaseOutputs recycler key = do
  send recycler (ReleaseOutputs key)

-- | Release the held consumed inputs of a payload to the input queue
-- (fire-and-forget): the payload was discarded downstream (e.g. a chain
-- rollback orphaned it), it will never confirm, so its consumed inputs come
-- back instead of its outputs.
releaseConsumed
  :: Recycler key input payload
  -- | Key identifying the payload (e.g. its txId).
  -> key
  -> IO ()
releaseConsumed recycler key = do
  send recycler (ReleaseConsumed key)

-- | Drop every queued input and queued payload, and reseed the input queue with
-- the given fresh inputs (fire-and-forget). The caller obtains the fresh inputs
-- however it likes (e.g. an on-chain UTxO re-query), the recycler only applies
-- them. The queued payloads go too because they were built from the inputs the
-- reset drops (see 'Pipe.dropPayloads').
--
-- With 'Just' a key the reset is gated: it applies only while that key is held
-- in the backlog (the caller is saying "reset because of this payload"), and
-- for an unknown key no reset happens at all, the event is ignored, following
-- the same rule as the releases (see the module header). With 'Nothing' the
-- reset always applies. Whether a meaningful key is available at the trigger is
-- the caller's wiring concern (see 'Config.Runtime').
reset
  :: Recycler key input payload
  -- | 'Just' the key of the payload whose event triggered the reset (gate the
  -- reset on it being held), or 'Nothing' to reset unconditionally.
  -> Maybe key
  -- | New inputs.
  -> [input]
  -> IO ()
reset recycler maybeKey fresh = do
  send recycler (Reset maybeKey fresh)

-- | Enqueue one lifecycle event onto the recycler's inbox.
send :: Recycler key input payload -> PayloadLifecycle key input -> IO ()
send recycler event = STM.atomically $ do
  ---------- STM START ----------
  STM.writeTQueue (recyclerInbox recycler) event
  ---------- STM ENDED ----------

