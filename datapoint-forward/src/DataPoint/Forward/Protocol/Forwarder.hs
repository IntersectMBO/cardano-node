{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module DataPoint.Forward.Protocol.Forwarder
  ( DataPointForwarder (..)
  , dataPointForwarderPeer
  ) where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))

import           DataPoint.Forward.Protocol.Type

data DataPointForwarder m a = DataPointForwarder
  { -- | The acceptor sent us a request for new 'DataPoint's.
    recvMsgDataPointsRequest
      :: [DataPointName]
      -> m (DataPointValues, DataPointForwarder m a)

    -- | The acceptor terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }

-- | Interpret a particular action sequence into the server side of the protocol.
--
dataPointForwarderPeer
  :: Monad m
  => DataPointForwarder m a
  -> Peer DataPointForward 'AsServer 'StIdle m a
dataPointForwarderPeer DataPointForwarder{recvMsgDataPointsRequest, recvMsgDone} =
  -- In the 'StIdle' state the forwarder is awaiting a request message
  -- from the acceptor.
  Await (ClientAgency TokIdle) $ \case
    -- The acceptor sent us a request for new 'DataPoint's, so now we're
    -- in the 'StBusy' state which means it's the forwarder's turn to send
    -- a reply.
    MsgDataPointsRequest request -> Effect $ do
      (reply, next) <- recvMsgDataPointsRequest request
      return $ Yield (ServerAgency TokBusy)
                     (MsgDataPointsReply reply)
                     (dataPointForwarderPeer next)

    -- The acceptor sent the done transition, so we're in the 'StDone' state
    -- so all we can do is stop using 'done', with a return value.
    MsgDone -> Effect $ Done TokDone <$> recvMsgDone
