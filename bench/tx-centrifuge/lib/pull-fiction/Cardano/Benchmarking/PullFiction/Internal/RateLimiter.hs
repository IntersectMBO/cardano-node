{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

-- | Server-side rate limiter for pull-based token dispensing.
--
-- In traffic control terminology, a /rate limiter/ (or /policer/) enforces a
-- maximum admission rate on incoming requests. It is /reactive/: it does not
-- initiate or schedule transmissions; it responds to each request by either
-- admitting or delaying it against a configured ceiling. This contrasts with a
-- /traffic shaper/ (or /pacer/), which sits on the sender side and proactively
-- schedules outgoing emissions (RFC 2475, s. 2.3.3.3).
--
-- In a pull-based system the downstream consumer drives the conversation by
-- requesting tokens when it has capacity (e.g. Cardano's TxSubmission2
-- mini-protocol, where the node pulls transactions when its mempool has room).
-- The 'RateLimiter' enforces a tokens-per-second ceiling on dispensed tokens,
-- ensuring the generator does not exceed the configured rate regardless of how
-- aggressively or unevenly consumers poll. Because the generator never pushes,
-- sender-side shaping is not applicable; the appropriate discipline is
-- receiver-side rate limiting.
--
-- This module computes delays but never sleeps. Sleeping is the caller's
-- responsibility (the rate-limited fetcher in
-- "Cardano.Benchmarking.PullFiction.WorkloadRunner" applies the delay via
-- 'threadDelay' outside the STM transaction), keeping the STM critical section
-- short and the rate limiter testable in pure STM.
--
-- The 'TBQueue' is supplied as an explicit parameter to 'waitToken' and
-- 'tryWaitToken', so that queue reads and rate-limit accounting happen in a
-- single atomic STM transaction while keeping the limiter decoupled from any
-- particular queue.
module Cardano.Benchmarking.PullFiction.Internal.RateLimiter
  ( RateLimiter, newTokenBucket, newUnlimited
  , waitToken, tryWaitToken
  ) where

--------------------------------------------------------------------------------

---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
---------------------
-- pull-fiction --
---------------------
import Cardano.Benchmarking.PullFiction.Clock qualified as Clock

--------------------------------------------------------------------------------

-- | A rate limiter for pull-based (server-side) token dispensing.
--
-- Two constructors are provided: 'TokenBucket' for a configured TPS ceiling,
-- and 'Unlimited' for unconstrained throughput.
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
-- scheduling algorithm (ITU-T I.371). Equivalent to Turner's leaky bucket as
-- a meter (Turner 1986, "New Directions in Communications", IEEE Comm. Mag.
-- 24(10)).
--
-- The algorithm tracks a /Theoretical Arrival Time/ (TAT), the earliest
-- time the next token is allowed:
--
-- @
--   TAT(0)     = now                      -- first token, no delay
--   TAT(N+1)   = max(TAT(N), now) + T     -- T = emission interval = 1\/rate
--   allow      iff  TAT <= now + τ        -- τ = burst tolerance
-- @
--
-- With @τ = 0@ (the current implementation) no burst is allowed: each token
-- must wait until its scheduled time. Adding @τ > 0@ would permit up to
-- @τ / T@ tokens to arrive ahead of schedule (the dual token-bucket
-- formulation with bucket depth @τ / T@).
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

-- | Compute the target time for the next token given a pre-computed
-- nanoseconds-per-token interval.
--
-- @tokensSent@ is the number of tokens already dispatched. Token 0 is handled
-- by the first-call special case in 'waitToken' (dispatched immediately at
-- @startTime@, delay 0). For all subsequent tokens, @tokensSent@ equals the
-- 0-indexed position of the next token: token 1 has @tokensSent = 1@, so its
-- target time is @startTime + 1 * T@. In general:
--
-- @
--   targetTime(N) = startTime + N * nanosPerToken
-- @
--
-- This is a single O(1) integer multiply + add; see the performance note on
-- 'TokenBucket' for the precision/performance trade-off.
nextTokenTargetTime :: Integer -> Clock.TimeSpec -> Integer -> Clock.TimeSpec
nextTokenTargetTime nanosPerToken startTime tokensSent =
  let !offset = Clock.fromNanoSecs (tokensSent * nanosPerToken)
  in  startTime + offset

--------------------------------------------------------------------------------

-- | Try to claim the next token. Runs entirely in STM, never retries.
--
-- The 'TBQueue' is passed as a parameter so the caller controls which queue is
-- read; the rate limiter only tracks rate-limiting state.
--
-- Returns @Just (token, delay)@ when a token is available, where @delay@ is how
-- long the caller should sleep to respect the TPS rate (zero when behind
-- schedule). Returns 'Nothing' when the queue is empty; the caller (typically
-- the rate-limited fetcher in
-- "Cardano.Benchmarking.PullFiction.WorkloadRunner") is responsible for
-- sleeping and retrying.
--
-- __Fairness property__: the token is consumed and the rate-limit slot is
-- claimed in a single atomic STM transaction. This means that once a thread
-- obtains token N, no other thread can obtain an /earlier/ slot: the delay
-- assigned to token N is always <= the delay for token N+1. Threads that enter
-- concurrently are serialised by STM; each sees a strictly increasing
-- @tokensSent@ counter. The consume-before-delay order therefore provides
-- FIFO-fair scheduling: the thread that wins the STM commit gets the earliest
-- available slot, and no later arrival can jump ahead of it.
--
-- By never blocking inside STM the @timeNow@ timestamp (captured by the caller
-- just before entering 'STM.atomically') stays accurate, which prevents
-- stale-clock TPS drift that would occur if the transaction retried while waiting
-- for a queue write.
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

-- | Non-blocking token request with rate-limit check. Runs entirely in STM.
--
-- The 'TBQueue' is passed as a parameter so the caller controls which queue is
-- read; the rate limiter only tracks rate-limiting state.
--
-- Unlike 'waitToken' (which always tries to read a token), this function checks
-- the rate limit /first/ and returns @Left delay@ without touching the queue
-- when ahead of schedule. This is the primary path for non-blocking callers
-- that should not consume tokens faster than the target TPS.
--
-- Returns:
--
-- * @Left delay@ when rate-limited (ahead of schedule), where @delay@ is how
--   long until the next token slot.
-- * @Right Nothing@ when not rate-limited but the queue is empty.
-- * @Right (Just token)@ when not rate-limited and a token was claimed.
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
