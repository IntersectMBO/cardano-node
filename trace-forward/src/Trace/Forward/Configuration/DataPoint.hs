{-# LANGUAGE PackageImports #-}

module Trace.Forward.Configuration.DataPoint
  ( AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  ) where

import           Ouroboros.Network.Driver (TraceSendRecv)

import           Control.Concurrent.STM.TVar (TVar)
import           "contra-tracer" Control.Tracer (Tracer)

import           Trace.Forward.Protocol.DataPoint.Type

-- | Acceptor configuration, parameterized by trace item's type.
data AcceptorConfiguration = AcceptorConfiguration
  { -- | The tracer that will be used by the acceptor in its network layer.
    acceptorTracer :: !(Tracer IO (TraceSendRecv DataPointForward))
    -- | 'TVar' that can be used as a brake: if an external thread sets
    --   it to 'True', the acceptor will send 'MsgDone' message to the
    --   forwarder and their session will be closed.
  , shouldWeStop :: !(TVar Bool)
  }

-- | Forwarder configuration, parameterized by trace item's type.
newtype ForwarderConfiguration = ForwarderConfiguration
  { -- | The tracer that will be used by the forwarder in its network layer.
    forwarderTracer :: Tracer IO (TraceSendRecv DataPointForward)
  }
