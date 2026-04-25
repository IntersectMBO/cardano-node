{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.PullFiction.WorkloadRunner
  ( TargetWorker, runWorkload
  , QueueStarved(..)
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (myThreadId, threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import GHC.Conc (labelThread)
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
import Cardano.Benchmarking.PullFiction.Config.Runtime qualified as Runtime
import Cardano.Benchmarking.PullFiction.Clock qualified as Clock
import Cardano.Benchmarking.PullFiction.Internal.RateLimiter qualified as RL

--------------------------------------------------------------------------------
-- Queue-starved exception.
--------------------------------------------------------------------------------

-- | Fatal exception thrown (in 'Raw.Error' on-exhaustion mode) when the payload
-- queue is empty and the rate limiter has authorized a fetch. This means the
-- payload builder cannot produce payloads fast enough for the configured TPS
-- demand. The caller must either reduce TPS, increase the number of initial
-- inputs, increase the payload queue capacity, or parallelise the builder.
--
-- In 'Raw.Block' mode, 'blockingFetch' retries until a token becomes
-- available; 'nonBlockingFetch' returns 'Nothing'.
data QueueStarved = QueueStarved !String
  deriving (Show)

instance Exception QueueStarved

--------------------------------------------------------------------------------
-- RateLimitedFetcher.
--------------------------------------------------------------------------------

-- | Rate-limited fetch interface over caller-supplied queues.
--
-- A 'RateLimitedFetcher' encapsulates rate-limiting state but has __no opinion
-- about which queue to read from__. The 'TBQueue' is a plain parameter on every
-- call, so the caller is free to pass:
--
-- * the same shared queue on every call,
-- * a different per-target queue for each worker,
-- * or even a different queue from one call to the next.
--
-- The fetcher never captures, stores, or inspects the queue; it only reads one
-- token from whatever queue it receives and applies the rate limit.
--
-- Two fetch modes are provided for pull-based protocols that distinguish
-- between "I need at least one item" and "give me more if available":
--
-- * 'blockingFetch': claims a rate-limit slot, sleeps for the required delay,
--   and returns the token. When the queue is empty, behaviour depends on the
--   'Raw.OnExhaustion' mode: 'Raw.Error' throws 'QueueStarved'; 'Raw.Block'
--   retries until a token becomes available.
--
-- * 'nonBlockingFetch': returns 'Nothing' when rate-limited (ahead of schedule)
--   or when the queue is empty in 'Raw.Block' mode. In 'Raw.Error' mode, throws
--   'QueueStarved' if not rate-limited but the queue is empty.
data RateLimitedFetcher token = RateLimitedFetcher
  { -- | Claim a rate-limit slot, sleep for the computed delay, return one token
    -- from the given queue. Retries or throws on empty queue depending on the
    -- 'Raw.OnExhaustion' mode.
    blockingFetch    :: STM.TBQueue token -> IO token
    -- | Return @Just token@ if the rate limit allows, 'Nothing' if ahead of
    -- schedule or if the queue is empty in 'Raw.Block' mode. Throws
    -- 'QueueStarved' on empty queue in 'Raw.Error' mode.
  , nonBlockingFetch :: STM.TBQueue token -> IO (Maybe token)
  }

-- | Build a 'RateLimitedFetcher' from a 'RL.RateLimiter'.
--
-- On the hot path both modes use non-blocking STM ('tryReadTBQueue', never
-- 'readTBQueue' / 'retry'), so no thread parks inside STM while tokens are
-- flowing. The sole exception is the 'Raw.Block' starvation fallback in
-- 'blockingFetch', which uses 'peekTBQueue' as an event-driven gate (see the
-- inline comment for the trade-off analysis). See
-- "Cardano.Benchmarking.PullFiction.Internal.RateLimiter" for the full design.
mkRateLimitedFetcher :: Raw.OnExhaustion
                     -> RL.RateLimiter
                     -> RateLimitedFetcher token
mkRateLimitedFetcher onExhaustion rateLimiter = RateLimitedFetcher
  { blockingFetch    = goBlocking
  , nonBlockingFetch = goNonBlocking
  }
  where
    goBlocking queue = do
      now <- Clock.getTime
      result <- STM.atomically $ RL.waitToken now rateLimiter queue
      case result of
        Just (token, delay) -> do
          -- Delays this thread and not the global RateLimiter.
          threadDelayNanos (Clock.toNanoSecs delay)
          pure token
        -- The queue is empty.
        Nothing -> case onExhaustion of
          Raw.Error ->
            -- The payload queue is empty. The payload builder cannot keep up
            -- with the configured TPS demand. At this stage of the library we
            -- treat this as a fatal error rather than silently degrading
            -- throughput; the user must either reduce TPS, increase the number
            -- of initial inputs, or parallelise the builder.
            throwIO $ QueueStarved
              "blockingFetch: payload queue empty, cannot keep up with TPS."
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
            goBlocking queue
    goNonBlocking queue = do
      now <- Clock.getTime
      result <- STM.atomically $ RL.tryWaitToken now rateLimiter queue
      case result of
        -- Rate limited, discard the nanoseconds and return.
        Left _ -> pure Nothing
        -- Not rate limited and the queue was not empty.
        Right (Just token) -> pure (Just token)
        -- The queue is empty.
        Right Nothing -> case onExhaustion of
          Raw.Error ->
            -- The payload queue is empty. The payload builder cannot keep up
            -- with the configured TPS demand. At this stage of the library we
            -- treat this as a fatal error rather than silently degrading
            -- throughput; the user must either reduce TPS, increase the number
            -- of initial inputs, or parallelise the builder.
            throwIO $ QueueStarved
              "nonBlockingFetch: payload queue empty, cannot keep up with TPS."
          Raw.Block -> pure Nothing

-- | Safely sleep for a duration in nanoseconds.
--
-- Converts nanoseconds to microseconds for 'threadDelay'. To prevent integer
-- overflow on 32-bit systems (where 'Int' maxes out at ~2147s), the delay is
-- clamped to 'maxBound :: Int'. This ensures that even with extremely low TPS
-- configurations (TPS below ~0.0005), the generator sleeps for the maximum
-- representable period rather than wrapping around to a small or negative value
-- and triggering an accidental token burst.
-- Replaces: `threadDelay (fromIntegral (Clock.toNanoSecs nanos `div` 1_000))`.
threadDelayNanos :: Integer -> IO ()
threadDelayNanos nanos =
  let micros = nanos `div` 1_000
      clamped = fromIntegral (min (fromIntegral (maxBound :: Int)) micros)
  in when (clamped > 0) $ threadDelay clamped

--------------------------------------------------------------------------------
-- Workload runner.
--------------------------------------------------------------------------------

-- | A worker callback that runs inside a labeled 'Async.Async'.
--
-- 'runWorkload' builds the rate-limited, recycling fetch functions for each
-- target and spawns a labeled async that calls this callback. The callback
-- receives:
--
-- 1. The fully resolved 'Runtime.Target' (carries addr, port, batch size,
--    and target name for error attribution).
-- 2. @fetchPayload@: blocking fetch that claims one rate-limit slot, reads a
--    @(payload, [input])@ pair from the payload queue, writes the @[input]@
--    component back to the workload's input queue, and returns the @payload@.
-- 3. @tryFetchPayload@: non-blocking variant that returns @Nothing@ when
--    rate-limited. On success, writes inputs back and returns the payload the
--    same way.
--
-- Both fetch functions handle the @[input]@ recycling automatically: whatever
-- inputs the builder pairs with the payload are written back to the input queue
-- after each fetch. The callback must not write to the input queue itself
-- (doing so would duplicate inputs). Its only responsibilities are delivering
-- the payload and any application-level bookkeeping.
--
-- The thread is already labeled @workloadName\/targetName@ by 'runWorkload'.
-- The callback body runs for the lifetime of the generator. It should not
-- create its own async or label its own thread; 'runWorkload' handles both.
type TargetWorker input payload
  =  Runtime.Target input payload -- ^ The resolved target.
  -> IO payload                   -- ^ Blocking fetch (rate-limited, recycles inputs).
  -> IO (Maybe payload)           -- ^ Non-blocking fetch (rate-limited, recycles inputs).
  -> IO ()                        -- ^ Worker body (runs inside labeled async).

-- | Run a load-generation workload: for each target, build rate-limited fetch
-- functions that recycle consumed inputs, spawn a labeled async, and call the
-- worker callback inside it.
--
-- Rate limiter creation and the shared\/independent decision are handled by
-- 'Runtime.resolve'. This function simply activates the pre-built limiters.
--
-- For each target the function:
--
-- 1. Builds a 'RateLimitedFetcher' from the target's 'Runtime.rateLimiter'.
-- 2. Wraps it with pipe recycling to produce @fetchPayload :: IO payload@ and
--    @tryFetchPayload :: IO (Maybe payload)@.
-- 3. Computes a thread label: @workloadName ++ \"\/\" ++ targetName@.
-- 4. Creates an 'Async.Async' that labels the thread, then runs the worker
--    callback.
--
-- Returns the list of worker asyncs (__unlinked__). Callers decide how to
-- monitor them: 'Main.hs' links them for immediate propagation; the test
-- harness polls synchronously so Tasty's 'withResource' can cache the
-- exception.
runWorkload
  :: Runtime.Workload input payload
  -> TargetWorker input payload
  -> IO [Async.Async ()]
runWorkload workload targetWorker =
  mapM
    (\target -> do
      let fetcher = mkRateLimitedFetcher
                      (Runtime.onExhaustion target)
                      (Runtime.rateLimiter target)
          pipe    = Runtime.targetPipe target
          -- Fetch one payload (blocking), write its [input] back, return payload.
          fetchPayload = do
            (payload, recycledInputs) <-
              (blockingFetch fetcher) (Runtime.pipePayloadQueue pipe)
            -- Recycling is a separate STM transaction from the fetch above.
            -- Merging both into one transaction would widen the critical
            -- section: the combined transaction would hold the rate-limiter
            -- TVars and the payload queue while also writing to the input
            -- queue, increasing contention and risking that a slow recycle
            -- (many inputs) stalls other workers competing for rate-limit
            -- slots. Keeping them separate means the fetch-and-claim is short;
            -- the recycle cannot delay other submissions.
            --
            -- Trade-off: between the two transactions, recycled inputs are held
            -- only in memory. If the thread is killed in this window, those
            -- inputs are lost. This is acceptable; recycling happens on
            -- delivery, not on downstream confirmation (see
            -- 'Runtime.pipeRecycle' for the full rationale).
            STM.atomically $ Runtime.pipeRecycle pipe recycledInputs
            pure payload
          -- Try to fetch one payload (non-blocking).
          tryFetchPayload = do
            result <- (nonBlockingFetch fetcher) (Runtime.pipePayloadQueue pipe)
            case result of
              Nothing -> pure Nothing
              Just (payload, recycledInputs) -> do
                -- See fetchPayload above for why recycle is a separate
                -- transaction.
                STM.atomically $ Runtime.pipeRecycle pipe recycledInputs
                pure (Just payload)
          -- Always labeled threads.
          threadLabel =
            Runtime.workloadName workload ++ "/" ++ Runtime.targetName target
      -- Return async (unlinked, caller decides monitoring strategy).
      async <- Async.async $ do
        tid <- myThreadId
        labelThread tid threadLabel
        targetWorker target fetchPayload tryFetchPayload
      pure async
    )
    (Map.elems (Runtime.targets workload))
