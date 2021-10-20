{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module will be used by the acceptor application.
--   Acceptor application asks 'DataPoint's from the forwarder application.
module DataPoint.Forward.Acceptor
  ( runDataPointAcceptor
  ) where

import           Ouroboros.Network.IOManager (IOManager)

import           DataPoint.Forward.Network.Acceptor (listenToForwarder)
import           DataPoint.Forward.Configuration (AcceptorConfiguration (..))
import           DataPoint.Forward.Utils (DataPointAsker, runActionInLoop)

runDataPointAcceptor
  :: IOManager             -- ^ 'IOManager' from the external application.
  -> AcceptorConfiguration -- ^ Acceptor configuration.
  -> DataPointAsker        -- ^ The structure we use to ask for 'DataPoint's explicitly.
  -> IO ()
runDataPointAcceptor iomgr config@AcceptorConfiguration{forwarderEndpoint} dpAsker =
  runActionInLoop (listenToForwarder iomgr config dpAsker) forwarderEndpoint 1
