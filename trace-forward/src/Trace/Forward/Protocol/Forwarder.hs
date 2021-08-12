{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Protocol.Forwarder
  ( TraceForwarder (..)
  , traceForwarderPeer
  ) where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))

import           Trace.Forward.Protocol.Type

data TraceForwarder lo m a = TraceForwarder
  { -- | The acceptor sent us a request for node's info.
    recvMsgNodeInfoRequest
      :: m (NodeInfo, TraceForwarder lo m a)

    -- | The acceptor sent us a request for new 'TraceObject's.
  , recvMsgTraceObjectsRequest
      :: forall blocking.
         TokBlockingStyle blocking
      -> NumberOfTraceObjects
      -> m (BlockingReplyList blocking lo, TraceForwarder lo m a)

    -- | The acceptor terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }

-- | Interpret a particular action sequence into the server side of the protocol.
--
traceForwarderPeer
  :: Monad m
  => TraceForwarder lo m a
  -> Peer (TraceForward lo) 'AsServer 'StIdle m a
traceForwarderPeer TraceForwarder{recvMsgNodeInfoRequest, recvMsgTraceObjectsRequest, recvMsgDone} =
  -- In the 'StIdle' state the forwarder is awaiting a request message
  -- from the acceptor.
  Await (ClientAgency TokIdle) $ \case
    -- The acceptor sent us a request for node's info, so now we're
    -- in the 'StBusy' state which means it's the forwarder's turn to send
    -- a reply.
    MsgNodeInfoRequest -> Effect $ do
      (reply, next) <- recvMsgNodeInfoRequest
      return $ Yield (ServerAgency TokNodeInfoBusy)
                     (MsgNodeInfoReply reply)
                     (traceForwarderPeer next)

    -- The acceptor sent us a request for new 'TraceObject's, so now we're
    -- in the 'StBusy' state which means it's the forwarder's turn to send
    -- a reply.
    MsgTraceObjectsRequest blocking request -> Effect $ do
      (reply, next) <- recvMsgTraceObjectsRequest blocking request
      return $ Yield (ServerAgency (TokBusy blocking))
                     (MsgTraceObjectsReply reply)
                     (traceForwarderPeer next)

    -- The acceptor sent the done transition, so we're in the 'StDone' state
    -- so all we can do is stop using 'done', with a return value.
    MsgDone -> Effect $ Done TokDone <$> recvMsgDone
