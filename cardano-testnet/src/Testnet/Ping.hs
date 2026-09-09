{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Ping
  ( pingNode
  , checkSprocket
  , waitForSprocket
  , waitForPortClosed
  , checkTcpPort
  , waitForTcpPort
  , randomFreePort
  , TestnetMagic
  , CNP.PingClientException
  ) where

import qualified Cardano.Network.Ping as CNP

import           Control.Exception.Safe
import           Control.Monad (when)
import qualified Control.Monad.Class.MonadTimer.SI as MT
import           Control.Monad.IO.Class
import qualified Control.Retry as R
import           Control.Tracer (nullTracer)
import           Data.Either
import           Data.IORef
import qualified Data.IP as IP
import           Data.Word (Word32)
import           Network.Socket (AddrInfo (..), PortNumber)
import qualified Network.Socket as Socket

import           Testnet.Process.RunIO (liftIOAnnotated)

import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

type TestnetMagic = Word32

-- | Ping the node once
pingNode :: MonadIO m
         => TestnetMagic -- ^ testnet magic
         -> IO.Sprocket  -- ^ node sprocket
         -> m (Either CNP.PingClientException ()) -- ^ 'Right ()' means success
pingNode networkMagic sprocket =
  liftIOAnnotated $
    CNP.pingClient nullTracer nullTracer nullTracer nullTracer (pingOpts networkMagic) (sprocketToAddrInfo sprocket)
  where
    pingOpts magic = CNP.PingOpts
      { CNP.pingOptsCount     = 1
      , CNP.pingOptsMagic     = CNP.NetworkMagic magic
      , CNP.pingOptsJson      = CNP.AsText
      , CNP.pingOptsQuiet     = True
      , CNP.pingOptsMode      = CNP.PingMode
      , CNP.pingOptsSRVPrefix = "_cardano._tcp"
      , CNP.pingOptsColor     = CNP.ColorAuto
      , CNP.pingOptsHashType  = CNP.FullHash
      }

-- | Wait for 'sprocket' to become ready. Periodically tries to connect to 'sprocket', with the provided interval.
-- If there was no success within 'timeout' period, return the last exception thrown during a connection
-- attempt.
waitForSprocket :: MonadIO m
                => MT.DiffTime -- ^ timeout
                -> MT.DiffTime -- ^ interval
                -> IO.Sprocket
                -> m (Either IOException ())
waitForSprocket timeout interval sprocket = waitFor timeout interval (checkSprocket sprocket)

-- | Check if the sprocket can be connected to. Returns an exception thrown during the connection attempt.
checkSprocket :: MonadIO m => IO.Sprocket -> m (Either IOException ())
checkSprocket sprocket = liftIOAnnotated $ do
  let AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress} = sprocketToAddrInfo sprocket
  bracket (Socket.socket addrFamily addrSocketType addrProtocol) Socket.close $ \sock -> do
    -- Capture only synchronous exceptions from the connection attempt.
    catch (Socket.connect sock addrAddress >> pure (pure ())) $ \e ->
      pure (Left e)

-- | Repeat the check until it succeeds or the timeout expires; returns the last result.
-- Seeded with a failure, not success, so a timeout that cancels the very first in-flight
-- check (before it can write a result) is not mistaken for a successful check.
waitFor :: MonadIO m
        => MT.DiffTime -- ^ timeout
        -> MT.DiffTime -- ^ interval
        -> IO (Either IOException ())
        -> m (Either IOException ())
waitFor timeout interval check = liftIOAnnotated $ do
  lastResult <- newIORef (Left $ userError "waitFor: timed out before any check completed")
  _ <- MT.timeout timeout $ loop lastResult
  readIORef lastResult
  where
    loop lastResult = do
      r <- check
      writeIORef lastResult r
      when (isLeft r) $ do
        MT.threadDelay interval
        loop lastResult

sprocketToAddrInfo :: IO.Sprocket -> AddrInfo
sprocketToAddrInfo sprocket = do
  let socketAbsPath = IO.sprocketSystemName sprocket
  Socket.AddrInfo
    [] Socket.AF_UNIX Socket.Stream
    Socket.defaultProtocol (Socket.SockAddrUnix socketAbsPath) Nothing

-- | Wait for a TCP endpoint to become ready. Periodically tries to connect to @(ip, port)@, with
-- the provided interval. If there was no success within 'timeout' period, return the last
-- exception thrown during a connection attempt.
waitForTcpPort :: MonadIO m
               => MT.DiffTime -- ^ timeout
               -> MT.DiffTime -- ^ interval
               -> IP.IP
               -> PortNumber
               -> m (Either IOException ())
waitForTcpPort timeout interval ip port = waitFor timeout interval (checkTcpPort ip port)

-- | Check if @(ip, port)@ can be connected to. Returns an exception thrown during the connection attempt.
checkTcpPort :: MonadIO m => IP.IP -> PortNumber -> m (Either IOException ())
checkTcpPort ip port = liftIOAnnotated $ do
  let sockAddr = IP.toSockAddr (ip, port)
      family = case ip of
        IP.IPv4 _ -> Socket.AF_INET
        IP.IPv6 _ -> Socket.AF_INET6
  bracket (Socket.socket family Socket.Stream Socket.defaultProtocol) Socket.close $ \sock ->
    -- Capture only synchronous exceptions from the connection attempt.
    catch (Socket.connect sock sockAddr >> pure (pure ())) $ \e ->
      pure (Left e)

-- | Find a free port to bind on 'ip', picking the matching address family.
-- Generalises hedgehog-extras' @randomPort@, which only binds 'AF_INET'.
randomFreePort :: MonadIO m => IP.IP -> m PortNumber
randomFreePort ip = liftIOAnnotated $
  bracket (Socket.socket family Socket.Stream Socket.defaultProtocol) Socket.close $ \sock -> do
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bind sock $ IP.toSockAddr (ip, 0)
    Socket.socketPort sock
  where
    family = case ip of
      IP.IPv4 _ -> Socket.AF_INET
      IP.IPv6 _ -> Socket.AF_INET6

-- | Wait until port gets closed.
waitForPortClosed
  :: MonadIO m
  => MT.DiffTime -- ^ timeout
  -> MT.DiffTime -- ^ check interval
  -> PortNumber
  -> m Bool -- ^ 'True' if port is closed, 'False' if timeout was reached before that
waitForPortClosed timeout interval portNumber = liftIOAnnotated $ do
  let retryPolicy = R.constantDelay (round @Double $ realToFrac interval) <> R.limitRetries (ceiling $ toRational timeout / toRational interval)
  fmap not . R.retrying retryPolicy (const pure) $ \_ ->
    liftIOAnnotated (IO.isPortOpen (fromIntegral portNumber))
