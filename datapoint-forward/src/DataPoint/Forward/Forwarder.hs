{-# LANGUAGE NamedFieldPuns #-}

-- This top-level module will be used by the forwarder application.
-- Forwarder application collects 'DataPoint's and sends them to
-- the acceptor application.
module DataPoint.Forward.Forwarder
  ( runDataPointForwarder
  ) where

import           Ouroboros.Network.IOManager (IOManager)

import           DataPoint.Forward.Configuration (ForwarderConfiguration (..))
import           DataPoint.Forward.Network.Forwarder (connectToAcceptor)
import           DataPoint.Forward.Utils

runDataPointForwarder
  :: IOManager              -- ^ 'IOManager' from the external application.
  -> ForwarderConfiguration -- ^ Forwarder configuration.
  -> DataPointStore         -- ^ DataPoint store.
  -> IO ()
runDataPointForwarder iomgr config@ForwarderConfiguration{acceptorEndpoint} dpStore =
  runActionInLoop (connectToAcceptor iomgr config dpStore) acceptorEndpoint 1
