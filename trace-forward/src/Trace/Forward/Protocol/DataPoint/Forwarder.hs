{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Trace.Forward.Protocol.DataPoint.Forwarder
  ( DataPointForwarder (..)
  , dataPointForwarderPeer
  ) where

import           Network.TypedProtocol.Peer.Server

import           Trace.Forward.Protocol.DataPoint.Type

data DataPointForwarder m a = DataPointForwarder
  { -- | The acceptor sent us a request for new 'DataPoint's.
    recvMsgDataPointsRequest
      :: [DataPointName]
      -> m DataPointValues

    -- | The acceptor terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }

-- | Interpret a particular action sequence into the server side of the protocol.
--
dataPointForwarderPeer
  :: Monad m
  => DataPointForwarder m a
  -> Server DataPointForward 'NonPipelined 'StIdle m a
dataPointForwarderPeer DataPointForwarder{recvMsgDataPointsRequest, recvMsgDone} = go
  where
    go =
      -- In the 'StIdle' state the forwarder is awaiting a request message
      -- from the acceptor.
      Await \case
        -- The acceptor sent us a request for new 'DataPoint's, so now we're
        -- in the 'StBusy' state which means it's the forwarder's turn to send
        -- a reply.
        MsgDataPointsRequest request -> Effect do
          reply <- recvMsgDataPointsRequest request
          return $ Yield (MsgDataPointsReply reply)
                         go

        -- The acceptor sent the done transition, so we're in the 'StDone' state
        -- so all we can do is stop using 'done', with a return value.
        MsgDone -> Effect $ Done <$> recvMsgDone
