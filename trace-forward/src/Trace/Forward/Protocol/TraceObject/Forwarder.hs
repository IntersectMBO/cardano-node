{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Protocol.TraceObject.Forwarder
  ( TraceObjectForwarder (..)
  , traceObjectForwarderPeer
  ) where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))

import           Trace.Forward.Protocol.TraceObject.Type

data TraceObjectForwarder lo m a = TraceObjectForwarder
  { -- | The acceptor sent us a request for new 'TraceObject's.
    recvMsgTraceObjectsRequest
      :: forall blocking.
         TokBlockingStyle blocking
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
  -> Peer (TraceObjectForward lo) 'AsServer 'StIdle m a
traceObjectForwarderPeer TraceObjectForwarder{recvMsgTraceObjectsRequest, recvMsgDone} =
  -- In the 'StIdle' state the forwarder is awaiting a request message
  -- from the acceptor.
  Await (ClientAgency TokIdle) $ \case
    -- The acceptor sent us a request for new 'TraceObject's, so now we're
    -- in the 'StBusy' state which means it's the forwarder's turn to send
    -- a reply.
    MsgTraceObjectsRequest blocking request -> Effect $ do
      (reply, next) <- recvMsgTraceObjectsRequest blocking request
      return $ Yield (ServerAgency (TokBusy blocking))
                     (MsgTraceObjectsReply reply)
                     (traceObjectForwarderPeer next)

    -- The acceptor sent the done transition, so we're in the 'StDone' state
    -- so all we can do is stop using 'done', with a return value.
    MsgDone -> Effect $ Done TokDone <$> recvMsgDone
