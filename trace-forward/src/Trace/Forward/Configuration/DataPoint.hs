module Trace.Forward.Configuration.DataPoint
  ( AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  ) where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Tracer (Tracer)

import           Ouroboros.Network.Driver (TraceSendRecv)

import           Trace.Forward.Protocol.DataPoint.Type

-- | Acceptor configuration, parameterized by trace item's type.
data AcceptorConfiguration = AcceptorConfiguration
  { -- | The tracer that will be used by the acceptor in its network layer.
    acceptorTracer :: !(Tracer IO (TraceSendRecv DataPointForward))
    -- | The endpoint that will be used to listen to the forwarder.
  , forwarderEndpoint :: !FilePath
    -- | 'TVar' that can be used as a brake: if an external thread sets
    --   it to 'True', the acceptor will send 'MsgDone' message to the
    --   forwarder and their session will be closed.
  , shouldWeStop :: !(TVar Bool)
  }

-- | Forwarder configuration, parameterized by trace item's type.
data ForwarderConfiguration = ForwarderConfiguration
  { -- | The tracer that will be used by the forwarder in its network layer.
    forwarderTracer :: !(Tracer IO (TraceSendRecv DataPointForward))
    -- | The endpoint that will be used to connect to the acceptor.
  , acceptorEndpoint :: !FilePath
  }
