{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- TODO TODO TODO: Add support for latency metrics, very useful for benchmarks.

-- | KeepAlive client for maintaining connection liveness.
--
-- This module provides a KeepAlive protocol client that sends periodic
-- keepalive messages to prevent idle connection timeouts.
--
-- == Usage
-- @
-- client <- mkClient 10  -- 10 seconds between keepalives
-- -- Use client with NodeToNode.connect
-- @
module Cardano.Benchmarking.TxCentrifuge.NodeToNode.KeepAlive
  ( KeepAliveClient
  , keepAliveClient
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Proxy (Proxy (..))
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
----------
-- time --
----------
import Data.Time.Clock (DiffTime)
-----------------------
-- ouroboros-network --
-----------------------
import Ouroboros.Network.ControlMessage qualified as ControlMsg
import Ouroboros.Network.KeepAlive qualified as KeepAlive
import Ouroboros.Network.Protocol.KeepAlive.Client qualified as KAClient
---------
-- stm --
---------
import Control.Concurrent.Class.MonadSTM.Strict qualified as StrictSTM
------------
-- random --
------------
import System.Random qualified as Random

--------------------------------------------------------------------------------

-- | KeepAlive client for maintaining connection liveness.
type KeepAliveClient = KAClient.KeepAliveClient IO ()

-- | Create a KeepAlive client that sends periodic keepalive messages.
--
-- The client runs indefinitely, sending keepalive cookies at the specified
-- interval (in seconds). This keeps the connection alive and allows the remote
-- peer to detect connection failures.
--
-- Note: This client does not track peer GSV (latency) metrics. For advanced
-- use cases requiring GSV tracking, construct the client directly using
-- 'Ouroboros.Network.KeepAlive.keepAliveClient' with appropriate parameters.
keepAliveClient
  -- | Interval between keepalive messages (in seconds).
  :: DiffTime
  -> IO KeepAliveClient
keepAliveClient interval = do
  rng <- Random.newStdGen
  dummyGSVMap <- StrictSTM.newTVarIO Map.empty
  pure $ KeepAlive.keepAliveClient
    mempty  -- tracer (no tracing in default client)
    rng
    (ControlMsg.continueForever (Proxy :: Proxy IO))
    ()  -- dummy peer address (GSV tracking not used)
    dummyGSVMap
    (KeepAlive.KeepAliveInterval interval)
