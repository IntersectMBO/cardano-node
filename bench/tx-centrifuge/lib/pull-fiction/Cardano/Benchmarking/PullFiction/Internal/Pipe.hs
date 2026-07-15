{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

-- | The 'Pipe': a generic pair of STM queues that moves items and nothing more.
-- It owns two private queues, an unbounded input queue (with a live depth
-- counter) and a bounded payload queue, and reports every queue change through
-- four caller-supplied event handlers (e.g. for tracing).
--
-- Work flows input queue → builder → payload queue → worker. The 'Pipe' knows
-- /nothing/ about recycling: the recycle strategy, its bookkeeping and the
-- recycle loop that closes inputs back onto the input queue all live in
-- 'Cardano.Benchmarking.PullFiction.Internal.Recycler', which drives this pipe
-- from the outside (it calls 'addInputs' to recycle). 'Config.Runtime' wraps
-- 'payloadFetcher' and calls the recycler's 'onDequeue' hook to observe pulls.
--
-- The input queue has 'addInputs' \/ 'takeInputs'. The payload queue has
-- 'addPayload' (in) and 'payloadFetcher' (out, rate-limited). Every payload
-- carries a @key@ that identifies it (e.g. its txId). The key rides through the
-- pipe untouched so that the fetcher's caller can correlate a pulled payload
-- with its own recycle bookkeeping, and so the enqueue \/ dequeue handlers can
-- put it in a trace. The pipe never interprets the key.
--
-- The 'Pipe' is purely synchronous and spawns no threads: every operation
-- returns once its STM work and event handler complete (a blocking fetch parks
-- the /caller's/ thread, it starts none of its own). All async behaviour lives
-- outside, never here (the builder loop and the recycler in 'Config.Runtime',
-- the observer announce loop and per-target fetch workers in @Main@).
--
-- The data constructor is hidden, build a 'Pipe' with 'mkPipe'.
module Cardano.Benchmarking.PullFiction.Internal.Pipe
  ( -- * Pipe.
    Pipe, mkPipe
    -- * Event handlers.
  , OnPayloadEvent
  , OnInputsEvent
    -- * Input queue operations (the queue itself is private).
  , takeInputs
  , addInputs
    -- * Payload queue operations (the queue itself is private).
  , addPayload
  , QueueStarved (..), PayloadFetcher (..), payloadFetcher
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM, when)
import Numeric.Natural (Natural)
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
------------------
-- pull-fiction --
------------------
import Cardano.Benchmarking.PullFiction.Clock qualified as Clock
import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw
import Cardano.Benchmarking.PullFiction.Internal.RateLimiter qualified as RL

--------------------------------------------------------------------------------

-- | Handler for an input-queue event, given the inputs that were added or
-- removed and the input-queue depth afterwards. Passing the inputs (rather than
-- just their count) lets the caller put their data in a trace. The count is
-- their @length@.
type OnInputsEvent input = [input] -> Natural -> IO ()

-- | Handler for a payload-queue event, given the payload's key and the
-- payload-queue depth afterwards. Passing the key (as 'OnInputsEvent' passes the
-- inputs) lets the caller put it in a trace.
type OnPayloadEvent key = key -> Natural -> IO ()

-- | Two private queues plus queue-event handlers.
-- Construct with 'mkPipe' only, the constructor is not exported.
data Pipe key input payload = Pipe
  { -- | Unbounded input queue (TQueue): must never block on write (the initial
    -- load at startup, recycle bursts at steady-state). Backpressure comes from
    -- 'pipePayloadQueue' instead.
    pipeInputQueue        :: !(STM.TQueue input)
    -- | Live element count of 'pipeInputQueue'. ('STM.TQueue' has no O(1)
    -- length, so we track it: starts at @0@ in 'mkPipe', @+@ on 'addInputs',
    -- @-@ on 'takeInputs', always in the same STM transaction as the queue
    -- itself.)
  , pipeInputDepth        :: !(STM.TVar Natural)
    -- | Bounded payload queue (capacity 'payloadQueueCapacity'). It's the sole
    -- source of backpressure. Each element is @(key, payload)@: the key rides
    -- along so the fetcher's caller can identify the pulled payload (e.g. for
    -- its recycle bookkeeping) and the dequeue handler can trace it.
  , pipePayloadQueue      :: !(STM.TBQueue (key, payload))
    -- | Fired after inputs are added to the input queue (by 'addInputs': the
    -- caller's initial load or a recycle).
  , pipeOnInputsEnqueued  :: !(OnInputsEvent input)
    -- | Fired after inputs are removed from the input queue ('takeInputs').
  , pipeOnInputsDequeued  :: !(OnInputsEvent input)
    -- | Fired after a payload is added to the payload queue ('addPayload').
  , pipeOnPayloadEnqueued :: !(OnPayloadEvent key)
    -- | Fired after a payload is removed from the payload queue (by
    -- 'payloadFetcher').
  , pipeOnPayloadDequeued :: !(OnPayloadEvent key)
  }

--------------------------------------------------------------------------------

-- | Capacity of 'pipePayloadQueue'. Bounded (unlike the input queue): the
-- builder blocks here when workers cannot consume fast enough, which is the
-- pipeline's sole source of backpressure. Large enough to absorb GC pauses at
-- high TPS (e.g. 100k TPS drains 8192 entries in ~80 ms).
payloadQueueCapacity :: Natural
payloadQueueCapacity = 8192

-- | Build an empty 'Pipe' with the four queue-event handlers (input enqueued\/
-- dequeued, payload enqueued\/dequeued). The only way to construct a 'Pipe', so
-- the queues and the depth counter are always created consistently.
--
-- The input queue starts empty. The caller loads the initial inputs afterwards
-- with 'addInputs', the same call the recycler uses, so an initial input is
-- indistinguishable from a recycled one. There is no \"initial input\" concept
-- below this call.
mkPipe
  :: OnInputsEvent input -- ^ Fired after inputs are enqueued (added).
  -> OnInputsEvent input -- ^ Fired after inputs are dequeued (taken).
  -> OnPayloadEvent key -- ^ Fired after a payload is enqueued.
  -> OnPayloadEvent key -- ^ Fired after a payload is dequeued (pulled).
  -> IO (Pipe key input payload)
mkPipe onInputEnqueued onInputDequeued onPayloadEnqueued onPayloadDequeued = do
  -- Input queue: unbounded (TQueue) so loading and recycling never block.
  -- It starts empty, the caller must load (initial) inputs through 'addInputs'.
  inputQueue <- STM.newTQueueIO
  inputDepth <- STM.newTVarIO (0 :: Natural)
  -- Payload queue: bounded (see 'payloadQueueCapacity'), the sole backpressure.
  payloadQueue <- STM.newTBQueueIO payloadQueueCapacity
  pure Pipe
    { -- STM fields.
      pipeInputQueue        = inputQueue
    , pipeInputDepth        = inputDepth
    , pipePayloadQueue      = payloadQueue
    -- Events.
    , pipeOnInputsEnqueued  = onInputEnqueued
    , pipeOnInputsDequeued  = onInputDequeued
    , pipeOnPayloadEnqueued = onPayloadEnqueued
    , pipeOnPayloadDequeued = onPayloadDequeued
    }

--------------------------------------------------------------------------------

-- | Add inputs to the input queue, bump the depth counter and fire the
-- input-enqueued handler with the added inputs and the resulting input-queue
-- depth. The write and the depth read share one STM transaction but the handler
-- runs after it. An empty input list is a no-op (no trace).
--
-- The only path onto the input queue, used both to load the initial inputs and
-- (by 'Internal.Recycler') to recycle. It treats every input the same,
-- regardless of where it came from.
addInputs :: Pipe key input payload -> [input] -> IO ()
addInputs pipe inputs = when (not (null inputs)) $ do
  inputDepth <- STM.atomically $ do
    ---------- STM START ----------
    mapM_ (STM.writeTQueue (pipeInputQueue pipe)) inputs
    STM.modifyTVar' (pipeInputDepth pipe) (+ fromIntegral (length inputs))
    STM.readTVar (pipeInputDepth pipe)
    ---------- STM ENDED ----------
  -- Fire event and return nothing.
  pipeOnInputsEnqueued pipe inputs inputDepth

-- | Take @n@ inputs for the builder: block until @n@ are available, remove them
-- from the input queue, and decrement the depth counter, all in one STM
-- transaction, then fire the input-dequeued handler with the taken inputs and
-- the resulting input-queue depth outside STM.
-- Blocking here is the intended backpressure when inputs are scarce (e.g. all
-- in flight).
takeInputs :: Pipe key input payload -> Natural -> IO [input]
takeInputs pipe n = do
  (inputs, inputDepth) <- STM.atomically $ do
    ---------- STM START ----------
    is <- replicateM (fromIntegral n) (STM.readTQueue (pipeInputQueue pipe))
    STM.modifyTVar' (pipeInputDepth pipe) (subtract (fromIntegral (length is)))
    depth <- STM.readTVar (pipeInputDepth pipe)
    pure (is, depth)
    ---------- STM ENDED ----------
  -- Fire event and return the inputs.
  pipeOnInputsDequeued pipe inputs inputDepth
  pure inputs

--------------------------------------------------------------------------------

-- | Add a built payload to the payload queue (the payload-side counterpart of
-- 'addInputs'), tagged with its @key@, then fire the payload-enqueued handler
-- with the payload-queue depth (measured via 'STM.lengthTBQueue' in the same
-- transaction as the write).
--
-- The pipe stores and forwards @(key, payload)@ untouched. What the key means
-- (e.g. a txId) and any recycle bookkeeping under it are the caller's concern
-- (see 'Internal.Recycler').
addPayload
  :: Pipe key input payload
  -> key     -- ^ Key identifying this payload (e.g. its txId).
  -> payload -- ^ The built payload to add for workers.
  -> IO ()
addPayload pipe key payload = do
  payloadDepth <- STM.atomically $ do
    ---------- STM START ----------
    STM.writeTBQueue (pipePayloadQueue pipe) (key, payload)
    STM.lengthTBQueue (pipePayloadQueue pipe)
    ---------- STM ENDED ----------
  pipeOnPayloadEnqueued pipe key payloadDepth

--------------------------------------------------------------------------------
-- Payload fetch (rate-limited).
--------------------------------------------------------------------------------

-- | Fatal exception thrown, in 'Raw.Error' on-exhaustion mode, when the payload
-- queue is empty but the rate limiter has authorized a fetch: the payload
-- builder cannot produce payloads fast enough for the configured TPS demand.
-- The caller must reduce TPS, add initial inputs, enlarge the payload queue, or
-- parallelise the builder. In 'Raw.Block' mode the fetch waits instead, the
-- blocking fetch parks until a payload arrives and if non-blocking returns
-- 'Nothing'.
data QueueStarved = QueueStarved !String
  deriving (Show)

instance Exception QueueStarved

-- | The two rate-limited ways to pull from a queue, produced by 'payloadFetcher'.
-- The payload queue itself is never exposed, so a payload can only leave the
-- pipe through one of these, always paced by the rate limiter and always firing
-- the payload-dequeued handler. Parameterised over the pulled element @a@
-- (here @(key, payload)@) so a caller can wrap it (e.g. 'Internal.Recycler'
-- strips the key after observing the pull).
data PayloadFetcher a = PayloadFetcher
  { -- | Claim a rate-limit slot, sleep for the computed delay, and return one
    -- element. On an empty queue it either parks until a payload arrives
    -- ('Raw.Block') or throws 'QueueStarved' ('Raw.Error').
    fetchPayload    :: IO a
    -- | Return @Just a@ if the rate limit allows and the queue is non-empty,
    -- 'Nothing' if ahead of schedule, or if the queue is empty in 'Raw.Block'
    -- mode. Throws 'QueueStarved' on an empty queue in 'Raw.Error'.
  , tryFetchPayload :: IO (Maybe a)
  }

-- | Build the rate-limited payload fetcher for one consumer (e.g. one target)
-- over this 'Pipe'\'s private payload queue, pacing every pull with the given
-- 'RL.RateLimiter' and handling an empty queue per 'Raw.OnExhaustion'.
--
-- Each successful pull removes one @(key, payload)@ from the queue in a single
-- rate-limit transaction (never parking inside STM while payloads flow), sleeps
-- for the computed delay /outside/ STM, then fires the payload-dequeued handler.
-- Because the payload queue is not exported, this is the only way out of the
-- pipe: a payload can never be delivered unpaced. Recycling is /not/ done here.
-- 'Config.Runtime' wraps this fetcher, calling the recycler's 'onDequeue' hook
-- to observe each pull.
payloadFetcher
  :: Pipe key input payload
  -> RL.RateLimiter   -- ^ Paces every pull (shared limiters allowed).
  -> Raw.OnExhaustion -- ^ What to do on an empty queue.
  -> PayloadFetcher (key, payload)
payloadFetcher pipe rateLimiter onExhaustion = PayloadFetcher
  { fetchPayload    = goBlocking
  , tryFetchPayload = goNonBlocking
  }
  where
    queue = pipePayloadQueue pipe
    -- Post-dequeue step (the payload has just been removed from the queue):
    -- report the new payload-queue depth through the dequeue handler.
    afterDequeue key = do
      -- Length of the payload queue is fetched in a different STM, not in sync.
      payloadDepth <- STM.atomically $ STM.lengthTBQueue queue
      pipeOnPayloadDequeued pipe key payloadDepth -- Dispatch event.
    ---------------
    -- Blocking. --
    ---------------
    goBlocking = do
      now <- Clock.getTime
      result <- STM.atomically $ do
        ---------- STM START ----------
        RL.waitToken now rateLimiter queue
        ---------- STM ENDED ----------
      case result of
        Just ((key, payload), delay) -> do
          -- Delays this thread only, not the (possibly shared) rate limiter.
          threadDelayNanos (Clock.toNanoSecs delay)
          -- Process the event and return.
          afterDequeue key
          pure (key, payload)
        -- The queue is empty.
        Nothing -> case onExhaustion of
          Raw.Error ->
            -- The payload queue is empty. The payload builder cannot keep up
            -- with the configured TPS demand. At this stage of the library we
            -- treat this as a fatal error rather than silently degrading
            -- throughput. The user must either reduce TPS, increase the number
            -- of initial inputs, or parallelise the builder.
            throwIO $ QueueStarved
              "fetchPayload: payload queue empty, cannot keep up with TPS."
          Raw.Block -> do
            -- Gate: park until the builder produces at least one payload.
            --
            -- 'peekTBQueue' retries (parks the thread via STM retry) until the
            -- queue is non-empty, then succeeds without consuming the item.
            -- This is event-driven: the thread uses zero CPU while parked and
            -- wakes as soon as the builder writes.
            --
            -- The stale-clock concern documented in 'RL.waitToken' does not
            -- apply here: 'goBlocking' captures a fresh timestamp on every
            -- iteration, so the rate limiter always sees an accurate clock.
            -- Fairness is likewise unaffected: the rate limiter's FIFO property
            -- comes from the atomic slot claiming inside 'waitToken', not from
            -- the retry mechanism.
            --
            -- Trade-off: when N workers are starved on the same queue, a single
            -- builder write wakes all N (GHC's STM wake-all). N-1 fail
            -- 'tryReadTBQueue' inside 'waitToken' and re-park. This is bounded
            -- by the number of targets per workload and is far cheaper than
            -- polling ('threadDelay' would cause N wakeups per requested sleep
            -- time regardless of builder activity).
            _ <- STM.atomically $ STM.peekTBQueue queue
            goBlocking
    -------------------
    -- Non-blocking. --
    -------------------
    goNonBlocking = do
      now <- Clock.getTime
      result <- STM.atomically $ do
        ---------- STM START ----------
        RL.tryWaitToken now rateLimiter queue
        ---------- STM ENDED ----------
      case result of
        -- Rate limited (ahead of schedule): no payload this round.
        Left _ -> pure Nothing
        -- Not rate limited and the queue was not empty.
        Right (Just (key, payload)) -> do
          -- Process the event and return.
          afterDequeue key
          pure (Just (key, payload))
        -- The queue is empty.
        Right Nothing -> case onExhaustion of
          Raw.Error ->
            -- The payload queue is empty. The payload builder cannot keep up
            -- with the configured TPS demand. At this stage of the library we
            -- treat this as a fatal error rather than silently degrading
            -- throughput. The user must either reduce TPS, increase the number
            -- of initial inputs, or parallelise the builder.
            throwIO $ QueueStarved
              "tryFetchPayload: payload queue empty, cannot keep up with TPS."
          Raw.Block -> pure Nothing

-- | Safely sleep for a duration in nanoseconds.
--
-- Converts nanoseconds to microseconds for 'threadDelay'. To prevent integer
-- overflow on 32-bit systems (where 'Int' maxes out at ~2147s), the delay is
-- clamped to 'maxBound :: Int', so even extremely low TPS (below ~0.0005)
-- sleeps for the maximum representable period rather than wrapping to a small or
-- negative value and triggering an accidental token burst.
-- Replaces: `threadDelay (fromIntegral (Clock.toNanoSecs nanos `div` 1_000))`.
threadDelayNanos :: Integer -> IO ()
threadDelayNanos nanos =
  let micros = nanos `div` 1_000
      clamped = fromIntegral (min (fromIntegral (maxBound :: Int)) micros)
  in when (clamped > 0) $ threadDelay clamped

