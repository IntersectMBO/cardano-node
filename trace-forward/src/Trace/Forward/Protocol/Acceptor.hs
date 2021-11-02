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
module Trace.Forward.Protocol.Acceptor
  ( TraceAcceptor(..)
  , traceAcceptorPeer
  ) where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))

import           Trace.Forward.Protocol.Type

data TraceAcceptor lo m a where
  SendMsgTraceObjectsRequest
    :: TokBlockingStyle blocking
    -> NumberOfTraceObjects
    -> (BlockingReplyList blocking lo -> m (TraceAcceptor lo m a))
    -> TraceAcceptor lo m a

  SendMsgDone
    :: m a
    -> TraceAcceptor lo m a

-- | Interpret a particular action sequence into the client side of the protocol.
--
traceAcceptorPeer
  :: Monad m
  => TraceAcceptor lo m a
  -> Peer (TraceForward lo) 'AsClient 'StIdle m a
traceAcceptorPeer = \case
  SendMsgTraceObjectsRequest TokBlocking request next ->
    -- Send our message (request for new 'TraceObject's from the forwarder).
    Yield (ClientAgency TokIdle) (MsgTraceObjectsRequest TokBlocking request) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder.
      Await (ServerAgency (TokBusy TokBlocking)) $ \(MsgTraceObjectsReply reply) ->
        Effect $
          traceAcceptorPeer <$> next reply

  SendMsgTraceObjectsRequest TokNonBlocking request next ->
    -- Send our message (request for new 'TraceObject's from the forwarder).
    Yield (ClientAgency TokIdle) (MsgTraceObjectsRequest TokNonBlocking request) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder. It is assuming that the forwarder will reply
      -- immediately (even there are no 'TraceObject's).
      Await (ServerAgency (TokBusy TokNonBlocking)) $ \(MsgTraceObjectsReply reply) ->
        Effect $
          traceAcceptorPeer <$> next reply

  SendMsgDone getResult ->
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect $
      Yield (ClientAgency TokIdle) MsgDone . Done TokDone
        <$> getResult
