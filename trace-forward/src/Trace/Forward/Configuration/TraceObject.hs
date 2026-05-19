{-# LANGUAGE PackageImports #-}

module Trace.Forward.Configuration.TraceObject
  ( AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  ) where

import           Ouroboros.Network.Driver (TraceSendRecv)

import           Control.Concurrent.STM.TVar (TVar)
import           "contra-tracer" Control.Tracer (Tracer)

import           Trace.Forward.Protocol.TraceObject.Type

-- | Acceptor configuration, parameterized by trace item's type.
data AcceptorConfiguration lo = AcceptorConfiguration
  { -- | The tracer that will be used by the acceptor in its network layer.
    acceptorTracer :: !(Tracer IO (TraceSendRecv (TraceObjectForward lo)))
    -- | The request specifies how many 'TraceObject's will be requested.
  , whatToRequest :: !NumberOfTraceObjects
    -- | 'TVar' that can be used as a brake: if an external thread sets
    --   it to 'True', the acceptor will send 'MsgDone' message to the
    --   forwarder and their session will be closed.
  , shouldWeStop :: !(TVar Bool)
  }

-- | Forwarder configuration, parameterized by trace item's type.
data ForwarderConfiguration lo = ForwarderConfiguration
  { -- | The tracer that will be used by the forwarder in its network layer.
    forwarderTracer :: !(Tracer IO (TraceSendRecv (TraceObjectForward lo)))
    -- | The size of the internal queue for tracing items.
    --   Use a size suitable for the beginning of the session, to avoid queue
    --   overflows, because initially there is no connection with acceptor yet,
    --   and the number of tracing items after the node starts may be very big.
    --   At the same time choose a number that reduces memory usage in the node.
  , queueSize :: !Word
  }

