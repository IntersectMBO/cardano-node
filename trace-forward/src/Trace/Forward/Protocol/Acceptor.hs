{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | A view of the Trace forwarding/accepting protocol from the point of view of the
-- client.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Trace.Forward.Protocol.Acceptor (
    TraceAcceptor(..)
  , traceAcceptorPeer
  ) where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))

import           Trace.Forward.Protocol.Type

-- | Please note that the acceptor is a server from the __networking__
-- point of view: the forwarder establishes network connection with the acceptor.
-- But after the connection is established, the acceptor becomes a client
-- from the __interaction__ point of view: it sends a request for new
-- 'LogObject's, the forwarder replies to the acceptor.
--
data TraceAcceptor req resp m a where
  SendMsgReq  :: req
              -> (resp -> m (TraceAcceptor req resp m a))
              -> TraceAcceptor req resp m a

  SendMsgDone :: m a -> TraceAcceptor req resp m a

-- | Interpret a particular action sequence into the client side of the
-- 'EKGForward' protocol.
--
traceAcceptorPeer
  :: Monad m
  => TraceAcceptor req resp m a
  -> Peer (TraceForward req resp) 'AsClient 'StIdle m a
traceAcceptorPeer = \case
  SendMsgReq req next ->
    -- Send our message (request for new 'LogObject's from the forwarder).
    Yield (ClientAgency TokIdle) (MsgReq req) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder.
      Await (ServerAgency TokBusy) $ \(MsgResp resp) ->
        Effect $
          traceAcceptorPeer <$> next resp

  SendMsgDone getResult ->
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect $
      Yield (ClientAgency TokIdle) MsgDone . Done TokDone
        <$> getResult
