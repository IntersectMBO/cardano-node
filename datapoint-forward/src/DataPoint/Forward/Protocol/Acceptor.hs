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
module DataPoint.Forward.Protocol.Acceptor
  ( DataPointAcceptor(..)
  , dataPointAcceptorPeer
  ) where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))

import           DataPoint.Forward.Protocol.Type

data DataPointAcceptor m a where
  SendMsgDataPointsRequest
    :: [DataPointName]
    -> (DataPointValues -> m (DataPointAcceptor m a))
    -> DataPointAcceptor m a

  SendMsgDone
    :: m a
    -> DataPointAcceptor m a

-- | Interpret a particular action sequence into the client side of the protocol.
--
dataPointAcceptorPeer
  :: Monad m
  => DataPointAcceptor m a
  -> Peer DataPointForward 'AsClient 'StIdle m a
dataPointAcceptorPeer = \case
  SendMsgDataPointsRequest request next ->
    -- Send our message (request for new 'DataPoint's from the forwarder).
    Yield (ClientAgency TokIdle) (MsgDataPointsRequest request) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder. It is assuming that the forwarder will reply
      -- immediately (even there are no 'DataPoint's).
      Await (ServerAgency TokBusy) $ \(MsgDataPointsReply reply) ->
        Effect $
          dataPointAcceptorPeer <$> next reply

  SendMsgDone getResult ->
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect $
      Yield (ClientAgency TokIdle) MsgDone . Done TokDone
        <$> getResult
