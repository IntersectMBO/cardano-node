{-# LANGUAGE NamedFieldPuns #-}

-- This top-level module will be used by the forwarder application.
-- Forwarder application collects 'TraceObject's and sends them to
-- the acceptor application.
module Trace.Forward.Forwarder
  ( runTraceForwarder
  ) where

import qualified Codec.Serialise as CBOR

import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import           Trace.Forward.Network.Forwarder (connectToAcceptor)
import           Trace.Forward.Utils

runTraceForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => IOManager                 -- ^ 'IOManager' from the external application.
  -> ForwarderConfiguration lo -- ^ Forwarder configuration.
  -> ForwardSink lo            -- ^ Forward "sink" that will be used to write tracing items.
  -> IO ()
runTraceForwarder iomgr config@ForwarderConfiguration{acceptorEndpoint} forwardSink =
  runActionInLoop (connectToAcceptor iomgr config forwardSink) acceptorEndpoint 1
