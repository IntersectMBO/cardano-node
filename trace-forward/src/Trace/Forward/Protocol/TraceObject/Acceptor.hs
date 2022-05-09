{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the trace forwarding/accepting protocol
--   from the point of view of the client.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Trace.Forward.Protocol.TraceObject.Acceptor
  ( TraceObjectAcceptor(..)
  , traceObjectAcceptorPeer
  ) where

import           Network.TypedProtocol.Peer.Client

import           Trace.Forward.Protocol.TraceObject.Type

data TraceObjectAcceptor lo m a where
  SendMsgTraceObjectsRequest
    :: SingBlockingStyle blocking
    -> NumberOfTraceObjects
    -> (BlockingReplyList blocking lo -> m (TraceObjectAcceptor lo m a))
    -> TraceObjectAcceptor lo m a

  SendMsgDone
    :: m a
    -> TraceObjectAcceptor lo m a

-- | Interpret a particular action sequence into the client side of the protocol.
--
traceObjectAcceptorPeer
  :: Monad m
  => TraceObjectAcceptor lo m a
  -> Client (TraceObjectForward lo) 'NonPipelined 'Empty 'StIdle m stm a
traceObjectAcceptorPeer = \case
  SendMsgTraceObjectsRequest SingBlocking request next ->
    -- Send our message (request for new 'TraceObject's from the forwarder).
    Yield (MsgTraceObjectsRequest SingBlocking request) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder.
      Await $ \(MsgTraceObjectsReply reply) ->
        Effect $
          traceObjectAcceptorPeer <$> next reply

  SendMsgTraceObjectsRequest SingNonBlocking request next ->
    -- Send our message (request for new 'TraceObject's from the forwarder).
    Yield (MsgTraceObjectsRequest SingNonBlocking request) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder. It is assuming that the forwarder will reply
      -- immediately (even there are no 'TraceObject's).
      Await $ \(MsgTraceObjectsReply reply) ->
        Effect $
          traceObjectAcceptorPeer <$> next reply

  SendMsgDone getResult ->
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect $
      Yield MsgDone . Done
        <$> getResult
