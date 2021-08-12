{-# LANGUAGE NamedFieldPuns #-}

-- This top-level module will be used by the forwarder application.
-- Forwarder application collects 'TraceObject's and sends them to
-- the acceptor application.
module Trace.Forward.Forwarder
  ( runTraceForwarder
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.STM.TBQueue (TBQueue)

import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import           Trace.Forward.Network.Forwarder (connectToAcceptor)
import           Trace.Forward.Utils (runActionInLoop)

runTraceForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => IOManager                 -- ^ 'IOManager' from the external application.
  -> ForwarderConfiguration lo -- ^ Forwarder configuration.
  -> TBQueue lo                -- ^ The queue from which the forwarder will take 'TraceObject's.
  -> IO ()
runTraceForwarder iomgr config@ForwarderConfiguration{acceptorEndpoint} loQueue =
  runActionInLoop (connectToAcceptor iomgr config loQueue) acceptorEndpoint 1
