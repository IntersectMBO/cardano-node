{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use <$>" -}

-- | A view of the EKG forwarding/accepting protocol from the point of view of the
-- client.
--
-- For execution, a conversion into the typed protocol is provided.
--
module System.Metrics.Protocol.Acceptor (
    EKGAcceptor(..)
  , ekgAcceptorPeer
  ) where

import           Network.TypedProtocol.Peer.Client
import           System.Metrics.Protocol.Type

-- | Please note that the acceptor is a server from the __networking__
-- point of view: the forwarder establishes network connection with the acceptor.
-- But after the connection is established, the acceptor becomes a client
-- from the __interaction__ point of view: it sends a request for new
-- metrics, the forwarder replies to the acceptor.
--
data EKGAcceptor req resp m a where
  SendMsgReq  :: req
              -> (resp -> m (EKGAcceptor req resp m a))
              -> EKGAcceptor req resp m a

  SendMsgDone :: m a -> EKGAcceptor req resp m a

-- | Interpret a particular action sequence into the client side of the
-- 'EKGForward' protocol.
--
ekgAcceptorPeer
  :: Monad m
  => EKGAcceptor req resp m a
  -> Client (EKGForward req resp) 'NonPipelined 'StIdle m a
ekgAcceptorPeer = \case
  SendMsgReq req next ->
    -- Send our message (request for the new metrics from the forwarder).
    Yield (MsgReq req) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder.
      Await $ \(MsgResp resp) ->
        Effect $
          ekgAcceptorPeer <$> next resp

  SendMsgDone getResult ->
      -- We do an actual transition using 'yield', to go from the 'StIdle' to
      -- 'StDone' state. Once in the 'StDone' state we can actually stop using
      -- 'done', with a return value.
      Effect $ do
        r <- getResult
        return $ Yield MsgDone (Done r)
