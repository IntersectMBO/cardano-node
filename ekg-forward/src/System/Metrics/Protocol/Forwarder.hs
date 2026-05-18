{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Metrics.Protocol.Forwarder (
    EKGForwarder (..)
  , ekgForwarderPeer
  ) where

import           Network.TypedProtocol.Peer.Server

import           System.Metrics.Protocol.Type

-- | Please note that the forwarder is a client from the __networking__
-- point of view: it establishes network connection with the acceptor.
-- But after the connection is established, the forwarder becomes a server
-- from the __interaction__ point of view: the acceptor sends a request for
-- new metrics, the forwarder replies to the acceptor.
--
data EKGForwarder req resp m a = EKGForwarder {
    -- | The acceptor sent us a request for new metrics.
    recvMsgReq  :: req -> m resp

    -- | The acceptor terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }

-- | Interpret a particular action sequence into the server side of the
-- 'EKGForward' protocol.
--
ekgForwarderPeer
  :: forall m req resp a. ()
  => Monad m
  => EKGForwarder req resp m a
  -> Server (EKGForward req resp) 'NonPipelined 'StIdle m a
ekgForwarderPeer EKGForwarder{..} = go
  where
    go :: Server (EKGForward req resp) 'NonPipelined 'StIdle m a
    go =
      -- In the 'StIdle' state the forwarder is awaiting a request message
      -- from the acceptor.
      Await \case
        MsgReq req -> Effect do
          resp <- recvMsgReq req
          return $ Yield (MsgResp resp) go

        -- The acceptor sent the done transition, so we're in the 'StDone' state
        -- so all we can do is stop using 'done', with a return value.
        MsgDone -> Effect $ Done <$> recvMsgDone
