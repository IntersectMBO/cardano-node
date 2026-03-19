{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Cardano.Tracer.Handlers.Metrics.DDoSProtectionMiddleware(DDoSProtectionMiddlewareConfig(..), mkDDoSProtectionMiddleware) where
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Extra (threadDelay)
import           Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO,
                   writeTVar)
import           Control.Monad (void)
import           Network.Wai
import           Network.Wai.Middleware.RequestSizeLimit
import           Network.Wai.Middleware.Timeout
import           Network.Wai.RateLimit
import           Network.Wai.RateLimit.Backend (Backend (MkBackend))
import           Network.Wai.RateLimit.Strategy

data DDoSProtectionMiddlewareConfig = DDoSProtectionMiddlewareConfig {
    requestBodySizeLimitKB :: Word
  , requestRateWindowSec   :: Word
  , requestRateLimitSec    :: Word
  , responseTimeLimitSec   :: Word
}

-- | Simple request rate limiter backend that limits the rate of
--   requests based on the total number of requests.
totalRequestRateLimiterBackend :: IO (Backend ())
totalRequestRateLimiterBackend = do
  usage <- newTVarIO (0 :: Integer)

  let
    backendGetUsage :: () -> IO Integer
    backendGetUsage _ = readTVarIO usage

    backendIncAndGetUsage :: () -> Integer -> IO Integer
    backendIncAndGetUsage _ k = atomically $ modifyTVar' usage (+ k) >> readTVar usage

    backendExpireIn :: () -> Integer -> IO ()
    backendExpireIn _ s = void $ forkIO $ do
      threadDelay (fromIntegral (s * 1_000_000))
      atomically $ writeTVar usage 0

  pure $ MkBackend backendGetUsage backendIncAndGetUsage backendExpireIn

mkDDoSProtectionMiddleware :: DDoSProtectionMiddlewareConfig -> IO Middleware
mkDDoSProtectionMiddleware cfg = totalRequestRateLimiterBackend >>= \backend ->
  pure $
    -- request body size limiter
    requestSizeLimitMiddleware
      (setMaxLengthForRequest (const (pure (Just (fromIntegral cfg.requestBodySizeLimitKB * 1024))))
      defaultRequestSizeLimitSettings)
      .
    -- request rate limiter (fixed window)
    rateLimiting (fixedWindow backend
                    (fromIntegral cfg.requestRateWindowSec)
                    (fromIntegral cfg.requestRateLimitSec)
                    (const (pure ()))
                 )
      .
    -- response time limiter
    timeout (fromIntegral cfg.responseTimeLimitSec)
