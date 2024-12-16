{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Trace.Forward.Protocol.TraceObject.Forwarder
  ( TraceObjectForwarder (..)
  , traceObjectForwarderPeer
  ) where

import           Data.Singletons
import           Network.TypedProtocol.Peer.Server
-- import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..), PeerRole (..))

import           Trace.Forward.Protocol.TraceObject.Type

data TraceObjectForwarder lo m a = TraceObjectForwarder
  { -- | The acceptor sent us a request for new 'TraceObject's.
    recvMsgTraceObjectsRequest
      :: forall blocking.
         TokBlockingStyle blocking
      -> NumberOfTraceObjects
      -> m (BlockingReplyList blocking lo)


    -- | The acceptor terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }

-- | Interpret a particular action sequence into the server side of the protocol.
--
traceObjectForwarderPeer
  :: forall m lo a
   . Monad m
  => TraceObjectForwarder lo m a
  -> Server (TraceObjectForward lo) 'NonPipelined 'StIdle m a
traceObjectForwarderPeer TraceObjectForwarder{recvMsgTraceObjectsRequest, recvMsgDone} = go
  where
  go :: Server (TraceObjectForward lo) 'NonPipelined StIdle m a
  go =
    -- In the 'StIdle' state the forwarder is awaiting a request message
    -- from the acceptor.
    Await \case
      -- The acceptor sent us a request for new 'TraceObject's, so now we're
      -- in the 'StBusy' state which means it's the forwarder's turn to send
      -- a reply.
      MsgTraceObjectsRequest blocking request -> Effect do
        reply <- recvMsgTraceObjectsRequest blocking request
        pure do
          withSingI blocking do
            Yield (MsgTraceObjectsReply reply) go

      -- The acceptor sent the done transition, so we're in the 'StDone' state
      -- so all we can do is stop using 'done', with a return value.
      MsgDone -> Effect do
        Done <$> recvMsgDone
