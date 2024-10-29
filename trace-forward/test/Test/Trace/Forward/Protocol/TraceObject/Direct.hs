{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Protocol.TraceObject.Direct
  ( direct
  ) where

import           Trace.Forward.Protocol.TraceObject.Acceptor
import           Trace.Forward.Protocol.TraceObject.Forwarder

direct :: Monad m
       => TraceObjectForwarder lo m a
       -> TraceObjectAcceptor lo m b
       -> m (a, b)
direct TraceObjectForwarder { recvMsgDone }
       (SendMsgDone mdone) =
  (,) <$> recvMsgDone <*> mdone
direct server@TraceObjectForwarder { recvMsgTraceObjectsRequest }
       (SendMsgTraceObjectsRequest blocking numOfTO mclient) = do
  reply <- recvMsgTraceObjectsRequest blocking numOfTO
  client <- mclient reply
  direct server client
