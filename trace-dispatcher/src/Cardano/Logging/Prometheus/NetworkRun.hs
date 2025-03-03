{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


-- | Run a TCP server, with hardening against connection flooding
module Cardano.Logging.Prometheus.NetworkRun
       ( NetworkRunParams (..)
       , TimeoutServer
       , defaultRunParams
       , runTCPServer
       ) where

import           Cardano.Logging.Utils (threadLabelMe)

import           Control.Concurrent (forkFinally, forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue
import qualified Control.Exception as E
import           Control.Monad (forever, void, when)
import qualified Data.Foldable as F (sum)
import           Data.Hashable (hash)
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Network.Socket
import qualified System.TimeManager as T


-- | Parameters specifying how the TCP server should be run
data NetworkRunParams = NetworkRunParams
  { runSocketTimeout    :: !Int             -- ^ Release socket after inactivity (seconds)
  , runSocketGraceful   :: !Int             -- ^ Graceful closing of socket (milliseconds), 0 to disable
  , runRecvMaxSize      :: !Int             -- ^ Close socket if more than (runRecvMaxSize - 1) bytes received; choose a small power of 2
  , runRateLimit        :: !Double          -- ^ Limit requests per second (may be < 0.0), 0.0 to disable
  , runConnLimitGlobal  :: !Int             -- ^ Limit total number of incoming connections, 0 to disable
  , runConnLimitPerHost :: !Int             -- ^ Limit number of incoming connections from the same host, 0 to disable
  , runServerName       :: !String          -- ^ The server name - exclusively used for labeling GHC threads
  }

defaultRunParams :: String -> NetworkRunParams
defaultRunParams name = NetworkRunParams
  { runSocketTimeout    = 16
  , runSocketGraceful   = 1000
  , runRecvMaxSize      = 2048
  , runRateLimit        = 3.0
  , runConnLimitGlobal  = 12
  , runConnLimitPerHost = 3
  , runServerName       = name
  }


-- A server having the run params in scope, as well as an IO action to reset the timeout
type TimeoutServer a
    =  NetworkRunParams
    -> IO ()
    -> Socket
    -> IO a

-- | Runs a TCP server conforming to the run parameters.
--   Will bind to localhost / loopback device only if no host name is specified.
runTCPServer
  :: NetworkRunParams
  -> Maybe HostName
  -> PortNumber
  -> TimeoutServer a
  -> IO a
runTCPServer runParams (fromMaybe "127.0.0.1" -> host) portNo server = do
  threadLabelMe $ runServerName runParams ++ " server"
  addr <- resolve host portNo
  E.bracket (openTCPServerSocket addr) close $ \sock ->
    runTCPServerWithSocket runParams sock server

runTCPServerWithSocket
  :: NetworkRunParams
  -> Socket
  -> TimeoutServer a
  -> IO a
runTCPServerWithSocket runParams@NetworkRunParams{..} sock server = do
  rateLimiter     <- mkRateLimiter runServerName runRateLimit
  ConnLimiter{..} <- mkConnLimiter runConnLimitGlobal runConnLimitPerHost
  T.withManager (runSocketTimeout * 1000000) $ \mgr -> forever $ do
    waitForLimiter rateLimiter
    E.bracketOnError (accept sock) (close . fst) $ \(conn, peer) -> do
      noLimitHit <- canServeThisPeer peer
      if noLimitHit
        then void $ forkFinally (server' mgr conn) (const $ gclose conn >> releasePeer peer)
        else close conn
  where
    gclose = if runSocketGraceful > 0 then flip gracefulClose runSocketGraceful else close
    server' mgr conn = do
      threadLabelMe $ runServerName ++ " timeout server"
      T.withHandle mgr (return ()) $ \timeoutHandle ->
        server runParams (T.tickle timeoutHandle) conn

resolve :: HostName -> PortNumber -> IO AddrInfo
resolve host portNo =
  head <$> getAddrInfo (Just hints) (Just host) (Just $ show portNo)
  where
    hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }

openTCPServerSocket :: AddrInfo -> IO Socket
openTCPServerSocket addr = do
  sock <- openServerSocket
  listen sock 1024
  return sock
  where
    openServerSocket = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
#if !defined(openbsd_HOST_OS)
      when (addrFamily addr == AF_INET6) $ setSocketOption sock IPv6Only 1
#endif
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      return sock

newtype RateLimiter = RateLimiter {waitForLimiter :: IO ()}

mkRateLimiter :: String -> Double -> IO RateLimiter
mkRateLimiter _ 0.0 = pure $ RateLimiter (pure ())
mkRateLimiter serverName reqPerSecond = do
  lock <- newTBQueueIO queueSize
  void . forkIO $ do
    threadLabelMe $ serverName ++ " rate limiter"
    forever $ do
      atomically $ writeTBQueue lock ()
      threadDelay delay

  pure $ RateLimiter (void $ atomically $ readTBQueue lock)
  where
      delay     = round $ 1000000 / reqPerSecond
      queueSize = ceiling reqPerSecond

data ConnLimiter = ConnLimiter
  { canServeThisPeer  :: SockAddr -> IO Bool  -- ^ Can I serve this peer without hitting a limit?
  , releasePeer       :: SockAddr -> IO ()    -- ^ Release peer from the limiter after connection has been closed.
  }

mkConnLimiter :: Int -> Int -> IO ConnLimiter
mkConnLimiter 0 0 = pure $ ConnLimiter (const $ pure True) (const $ pure ())
mkConnLimiter global perHost = do
  lock <- newMVar IM.empty
  let
    canServeThisPeer (getPeerId -> peerId) =
      modifyMVar lock $ \intMap ->
        let
          intMap'   = IM.alter upsert peerId intMap
          count'    = F.sum intMap'
          canServe  = didntHitGlobalLimit count' && count' > F.sum intMap
        in pure (if canServe then intMap' else intMap, canServe)
    releasePeer (getPeerId -> peerId) =
      modifyMVar_ lock (pure . IM.alter removeOrDecrease peerId)

  pure ConnLimiter{..}
  where
    wontHitHostLimit    = if perHost == 0 then const True else (< perHost)
    didntHitGlobalLimit = if global == 0  then const True else (<= global)

    upsert, removeOrDecrease :: Maybe Int -> Maybe Int
    upsert = \case
      Just n  -> if wontHitHostLimit n then Just (n + 1) else Just n
      Nothing -> Just 1

    removeOrDecrease = \case
      Just n | n > 1  -> Just (n - 1)
      _               -> Nothing

    getPeerId :: SockAddr -> Int
    getPeerId = \case
        SockAddrInet _ h      -> hash h
        SockAddrInet6 _ _ h _ -> hash h
        SockAddrUnix s        -> hash s
