{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Protocol.DataPoint.Direct
  ( direct
  ) where

import           Trace.Forward.Protocol.DataPoint.Acceptor
import           Trace.Forward.Protocol.DataPoint.Forwarder
import           Trace.Forward.Protocol.DataPoint.Type

direct :: Monad m
       => DataPointForwarder m a
       -> DataPointAcceptor m b
       -> m (a, b)
direct DataPointForwarder { recvMsgDone }
       (SendMsgDone mdone) =
  (,) <$> recvMsgDone <*> mdone
direct DataPointForwarder { recvMsgDataPointsRequest }
       (SendMsgDataPointsRequest (dpNames :: [DataPointName]) mclient) = do
  (reply, server) <- recvMsgDataPointsRequest dpNames
  client <- mclient reply
  direct server client
