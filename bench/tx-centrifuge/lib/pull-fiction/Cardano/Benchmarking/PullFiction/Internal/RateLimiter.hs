{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

-- | Server-side GCRA rate limiter for pull-based token dispensing.
--
-- Computes delays but never sleeps — the caller is responsible for sleeping
-- outside the STM transaction (keeps the critical section short and the limiter
-- testable in pure STM).
--
-- The 'TBQueue' is an explicit parameter so that queue reads and rate-limit
-- accounting are atomic while the limiter stays decoupled from any particular
-- queue.
module Cardano.Benchmarking.PullFiction.Internal.RateLimiter
  ( RateLimiter, newTokenBucket, newUnlimited
  , waitToken, tryWaitToken
  ) where

--------------------------------------------------------------------------------

---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
------------------
-- pull-fiction --
------------------
import Cardano.Benchmarking.PullFiction.Clock qualified as Clock

--------------------------------------------------------------------------------

-- | 'TokenBucket' for a configured TPS ceiling, or 'Unlimited'.
data RateLimiter
  = TokenBucket
      -- | Emission interval T in nanoseconds (cached).
      !Integer
      -- | Start time (set on first claim).
      !(STM.TVar (Maybe Clock.TimeSpec))
      -- | Tokens sent so far.
      !(STM.TVar Integer)
  -- | No rate limit.
  | Unlimited

-- | Create a token-bucket rate limiter targeting @tps@ tokens per second.
--
-- Uses the Generic Cell Rate Algorithm (GCRA), also known as the virtual
-- scheduling algorithm (ITU-T I.371). Equivalent to Turner's leaky bucket as a
-- meter (Turner 1986, "New Directions in Communications", IEEE Comm. Mag.
-- 24(10)).
--
-- The algorithm tracks a /Theoretical Arrival Time/ (TAT), the earliest time
-- the next token is allowed:
--
-- @
--   TAT(0)     = now                  -- first token, no delay
--   TAT(N+1)   = max(TAT(N), now) + T -- T = emission interval = 1\/rate
--   allow      iff  TAT <= now + τ    -- τ = burst tolerance
-- @
--
-- With @τ = 0@ (the current implementation) no burst is allowed: each token
-- must wait until its scheduled time. Adding @τ > 0@ would permit up to @τ / T@
-- tokens to arrive ahead of schedule (the dual token-bucket formulation with
-- bucket depth @τ / T@).
--
-- TODO: Add a @maxBurst@ parameter to the rate limit config. The burst
-- tolerance becomes @τ = maxBurst * T@, and the admission check becomes
-- @TAT <= now + τ@.
--
-- The start time is captured on the first token claim, so any delay between
-- limiter creation and the first request does not cause a burst of catch-up
-- tokens.
--
-- Performance: @nanosPerToken@ (the emission interval @T@) is pre-computed
-- once at construction via @round (1e9 / tps)@. This trades a tiny rounding
-- error (at most +/-0.5 ns per token) for O(1) integer multiplication in
-- 'nextTokenTargetTime', avoiding 'Rational' division that would otherwise
-- dominate at high token counts.
newTokenBucket :: Double -> IO RateLimiter
newTokenBucket tps = do
  startVar <- STM.newTVarIO Nothing
  countVar <- STM.newTVarIO 0
  let !nanosPerToken = round (1_000_000_000 / tps) :: Integer
  pure (TokenBucket nanosPerToken startVar countVar)

-- | An unlimited rate limiter (never blocks on rate).
newUnlimited :: RateLimiter
newUnlimited = Unlimited

--------------------------------------------------------------------------------

-- | @targetTime(N) = startTime + N * nanosPerToken@.
-- Token 0 is special-cased in 'waitToken' (delay 0).
-- O(1) integer multiply + add — no division on the hot path.
nextTokenTargetTime :: Integer -> Clock.TimeSpec -> Integer -> Clock.TimeSpec
nextTokenTargetTime nanosPerToken startTime tokensSent =
  let !offset = Clock.fromNanoSecs (tokensSent * nanosPerToken)
  in  startTime + offset

--------------------------------------------------------------------------------

-- | Try to claim the next token. Runs entirely in STM, never retries.
--
-- @Just (token, delay)@ when a token is available; 'Nothing' when the queue is
-- empty (caller sleeps and retries).
--
-- __Fairness__: consume + slot-claim are one STM transaction, so concurrent
-- threads see a strictly increasing @tokensSent@ counter — FIFO-fair.
--
-- Never blocks inside STM, so the caller-captured @timeNow@ stays accurate
-- (no stale-clock TPS drift).
waitToken :: Clock.TimeSpec
          -> RateLimiter
          -> STM.TBQueue token
          -> STM.STM (Maybe (token, Clock.TimeSpec))
-- No TPS: try to read a token without blocking.
waitToken _ Unlimited queue = do
  maybeToken <- STM.tryReadTBQueue queue
  case maybeToken of
    Nothing    -> pure Nothing
    Just token -> pure (Just (token, 0))
-- With a TPS.
waitToken timeNow (TokenBucket nanosPerToken startTVar countTVar) queue = do
  maybeToken <- STM.tryReadTBQueue queue
  case maybeToken of
    Nothing    -> pure Nothing
    Just token -> do
      maybeStartTime <- STM.readTVar startTVar
      case maybeStartTime of
        -- Rate limiter running, claim a rate-limit slot.
        Just startTime -> do
          tokensSent <- STM.readTVar countTVar
          STM.writeTVar countTVar (tokensSent + 1)
          let !targetTime = nextTokenTargetTime
                              nanosPerToken startTime tokensSent
              !delay = max 0 (targetTime - timeNow)
          pure (Just (token, delay))
        -- First call, record start time.
        Nothing -> do
          STM.writeTVar startTVar (Just timeNow)
          STM.writeTVar countTVar 1
          pure (Just (token, 0))

-- | Non-blocking variant: checks the rate limit /first/ and returns
-- @Left delay@ without touching the queue when ahead of schedule.
--
-- @Right Nothing@: not rate-limited but queue empty.
-- @Right (Just token)@: token claimed.
tryWaitToken :: Clock.TimeSpec
             -> RateLimiter
             -> STM.TBQueue token
             -> STM.STM (Either Clock.TimeSpec (Maybe token))
-- No TPS.
tryWaitToken _  Unlimited queue = Right <$> STM.tryReadTBQueue queue
-- With a TPS.
tryWaitToken timeNow
             (TokenBucket nanosPerToken startTVar countTVar)
             queue = do
  maybeStartTime <- STM.readTVar startTVar
  case maybeStartTime of
     -- Rate limiter running, check if ahead of schedule.
    Just startTime -> do
      tokensSent <- STM.readTVar countTVar
      let !targetTime = nextTokenTargetTime
                          nanosPerToken startTime tokensSent
      if targetTime > timeNow
        -- Ahead of schedule.
        then pure (Left (targetTime - timeNow))
        -- Available headroom.
        else do
          maybeToken <- STM.tryReadTBQueue queue
          case maybeToken of
            Nothing    -> pure (Right Nothing)
            Just token -> do
              STM.writeTVar countTVar (tokensSent + 1)
              pure (Right (Just token))
    -- First call, no rate limit to check.
    Nothing -> do
      maybeToken <- STM.tryReadTBQueue queue
      case maybeToken of
        Nothing    -> pure (Right Nothing)
        Just token -> do
          -- Record the time only if a token was available.
          STM.writeTVar startTVar (Just timeNow)
          STM.writeTVar countTVar 1
          pure (Right (Just token))
