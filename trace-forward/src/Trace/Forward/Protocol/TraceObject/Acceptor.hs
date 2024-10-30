{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | A view of the trace forwarding/accepting protocol
--   from the point of view of the client.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Trace.Forward.Protocol.TraceObject.Acceptor
  ( TraceObjectAcceptor(..)
  , traceObjectAcceptorPeer
  ) where

import           Data.Kind (Type)

import           Network.TypedProtocol.Peer.Client
import           Trace.Forward.Protocol.TraceObject.Type

type TraceObjectAcceptor :: Type -> (Type -> Type) -> Type -> Type
data TraceObjectAcceptor lo m a where
  SendMsgTraceObjectsRequest
    :: TokBlockingStyle blocking
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
  -> Client (TraceObjectForward lo) 'NonPipelined 'StIdle m a
traceObjectAcceptorPeer = \case
  SendMsgTraceObjectsRequest TokBlocking request next ->
    -- Send our message (request for new 'TraceObject's from the forwarder).
    Yield (MsgTraceObjectsRequest TokBlocking request) do
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder.
      Await \(MsgTraceObjectsReply reply) ->
        Effect do
          traceObjectAcceptorPeer <$> next reply

  SendMsgTraceObjectsRequest TokNonBlocking request next ->
    -- Send our message (request for new 'TraceObject's from the forwarder).
    Yield (MsgTraceObjectsRequest TokNonBlocking request) do
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder. It is assuming that the forwarder will reply
      -- immediately (even there are no 'TraceObject's).
      Await \(MsgTraceObjectsReply reply) ->
        Effect do
          traceObjectAcceptorPeer <$> next reply

  SendMsgDone getResult ->
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect do
      Yield MsgDone . Done
        <$> getResult
