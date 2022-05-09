{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Protocol.TraceObject.Forwarder
  ( TraceObjectForwarder (..)
  , traceObjectForwarderPeer
  ) where

import           Data.Singletons

import           Network.TypedProtocol.Peer.Server

import           Trace.Forward.Protocol.TraceObject.Type

data TraceObjectForwarder lo m a = TraceObjectForwarder
  { -- | The acceptor sent us a request for new 'TraceObject's.
    recvMsgTraceObjectsRequest
      :: forall blocking.
         SingBlockingStyle blocking
      -> NumberOfTraceObjects
      -> m (BlockingReplyList blocking lo, TraceObjectForwarder lo m a)

    -- | The acceptor terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }

-- | Interpret a particular action sequence into the server side of the protocol.
--
traceObjectForwarderPeer
  :: Monad m
  => TraceObjectForwarder lo m a
  -> Server (TraceObjectForward lo) 'NonPipelined 'Empty 'StIdle m stm a
traceObjectForwarderPeer TraceObjectForwarder{recvMsgTraceObjectsRequest, recvMsgDone} =
  -- In the 'StIdle' state the forwarder is awaiting a request message
  -- from the acceptor.
  Await $ \case
    -- The acceptor sent us a request for new 'TraceObject's, so now we're
    -- in the 'StBusy' state which means it's the forwarder's turn to send
    -- a reply.
    MsgTraceObjectsRequest blocking request -> Effect $ do
      (reply, next) <- recvMsgTraceObjectsRequest blocking request
      return $ withSingI blocking $
               Yield (MsgTraceObjectsReply reply)
                     (traceObjectForwarderPeer next)

    -- The acceptor sent the done transition, so we're in the 'StDone' state
    -- so all we can do is stop using 'done', with a return value.
    MsgDone -> Effect $ Done <$> recvMsgDone
