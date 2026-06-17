{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Ping
  ( pingNode
  , checkSprocket
  , waitForSprocket
  , waitForPortClosed
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
    CNP.pingClient nullTracer nullTracer (pingOpts networkMagic) (sprocketToAddrInfo sprocket)
  where
    pingOpts magic = CNP.PingOpts
      { CNP.pingOptsCount     = 1
      , CNP.pingOptsMagic     = CNP.NetworkMagic magic
      , CNP.pingOptsJson      = CNP.AsText
      , CNP.pingOptsQuiet     = True
      , CNP.pingOptsMode      = CNP.PingMode
      , CNP.pingOptsSRVPrefix = "_cardano._tcp"
      , CNP.pingOptsColor     = CNP.ColorAuto
      }

-- | Wait for 'sprocket' to become ready. Periodically tries to connect to 'sprocket', with the provided interval.
-- If there was no success within 'timeout' period, return the last exception thrown during a connection
-- attempt.
waitForSprocket :: MonadIO m
                => MT.DiffTime -- ^ timeout
                -> MT.DiffTime -- ^ interval
                -> IO.Sprocket
                -> m (Either IOException ())
waitForSprocket timeout interval sprocket = liftIOAnnotated $ do
  lastResult <- newIORef (Right ())
  _ <- MT.timeout timeout $ loop lastResult
  readIORef lastResult
  where
    loop lastResult = do
      r <- checkSprocket sprocket
      writeIORef lastResult r
      when (isLeft r) $ do
        -- repeat on error
        MT.threadDelay interval
        loop lastResult

-- | Check if the sprocket can be connected to. Returns an exception thrown during the connection attempt.
checkSprocket :: MonadIO m => IO.Sprocket -> m (Either IOException ())
checkSprocket sprocket = liftIOAnnotated $ do
  let AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress} = sprocketToAddrInfo sprocket
  bracket (Socket.socket addrFamily addrSocketType addrProtocol) Socket.close $ \sock -> do
    -- Capture only synchronous exceptions from the connection attempt.
    catch (Socket.connect sock addrAddress >> pure (pure ())) $ \e ->
      pure (Left e)

sprocketToAddrInfo :: IO.Sprocket -> AddrInfo
sprocketToAddrInfo sprocket = do
  let socketAbsPath = IO.sprocketSystemName sprocket
  Socket.AddrInfo
    [] Socket.AF_UNIX Socket.Stream
    Socket.defaultProtocol (Socket.SockAddrUnix socketAbsPath) Nothing

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
