{-# LANGUAGE NumericUnderscores #-}

module Cardano.Benchmarking.TxCentrifuge.Supervision
  ( superviseTarget
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, fromException, tryJust)
import GHC.Clock (getMonotonicTime)
import System.IO (hPutStrLn, stderr)

-- | Run one target's connection forever, reconnecting on any failure.
--
-- Never returns.  Exceptions are caught and retried rather than propagated, so
-- a wedged or unreachable relay costs that target's share of the load and
-- nothing else.  Asynchronous exceptions are not caught: 'Async.cancel' during
-- shutdown must still work.
--
-- Backoff doubles from 'superviseBackoffBase' and is capped at
-- 'superviseBackoffMax'.  The cap matters: an uncapped delay would grow past
-- the time it takes a relay to be restarted, so a recovered target would sit
-- idle long after it was usable.  A connection that survived at least
-- 'superviseHealthy' is treated as good and resets the backoff, so a relay
-- that flaps does not inherit the delay from an earlier outage.
--
-- Both transitions are traced.  Silence here would turn a permanently wedged
-- relay into an invisible capacity loss.
superviseTarget :: String -> IO () -> IO ()
superviseTarget name act = go superviseBackoffBase
  where
    go delay = do
      before <- getMonotonicTime
      outcome <- tryJust synchronousException act
      after <- getMonotonicTime
      let lasted = after - before
          reason = case outcome of
            Left ex -> show ex
            Right () -> "connection closed cleanly"
          delay'
            | lasted >= superviseHealthy = superviseBackoffBase
            | otherwise = min superviseBackoffMax (delay * 2)
      hPutStrLn stderr $
        "tx-centrifuge: target " ++ name ++ " dropped after "
          ++ show (round lasted :: Int) ++ "s: " ++ reason
          ++ "; reconnecting in " ++ show (round delay :: Int) ++ "s"
      threadDelay (round (delay * 1_000_000))
      hPutStrLn stderr $ "tx-centrifuge: target " ++ name ++ " reconnecting"
      go delay'

    -- async's AsyncCancelled also uses the SomeAsyncException hierarchy.
    -- Returning Nothing makes tryJust propagate cancellation instead of retrying.
    synchronousException :: SomeException -> Maybe SomeException
    synchronousException ex = case fromException ex :: Maybe SomeAsyncException of
      Just _ -> Nothing
      Nothing -> Just ex

-- | First reconnect delay, seconds.
superviseBackoffBase :: Double
superviseBackoffBase = 1

-- | Ceiling on the reconnect delay, seconds.  Keeps recovery prompt once a
-- wedged relay is restarted.
superviseBackoffMax :: Double
superviseBackoffMax = 60

-- | A connection lasting at least this long, seconds, counts as healthy and
-- resets the backoff.  Above the 60s KeepAlive server-agency limit, so a
-- wedged target, which always dies at that limit, never resets.
superviseHealthy :: Double
superviseHealthy = 120
