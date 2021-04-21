{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the Trace forwarding/accepting protocol from the point of view of the
-- client.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Trace.Forward.Protocol.Acceptor (
    TraceAcceptor(..)
  , traceAcceptorPeer
  , byteLimitsTraceForward
  , timeLimitsTraceForward
  ) where

import           Data.Time.Clock (DiffTime)

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))
import           Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..),
                                                  ProtocolTimeLimits (..))
import           Ouroboros.Network.Protocol.Limits (largeByteLimit, shortWait,
                                                    smallByteLimit, waitForever)

import           Trace.Forward.Protocol.Type

-- | Please note that the acceptor is a server from the __networking__
-- point of view: the forwarder establishes network connection with the acceptor.
-- But after the connection is established, the acceptor becomes a client
-- from the __interaction__ point of view: it sends a request for new
-- 'LogObject's, the forwarder replies to the acceptor.
--
data TraceAcceptor lo m a where
  SendMsgRequest
    :: TokBlockingStyle blocking
    -> Request
    -> (BlockingReplyList blocking lo -> m (TraceAcceptor lo m a))
    -> TraceAcceptor lo m a

  SendMsgDone
    :: m a
    -> TraceAcceptor lo m a

-- | Interpret a particular action sequence into the client side of the
-- 'EKGForward' protocol.
--
traceAcceptorPeer
  :: Monad m
  => TraceAcceptor lo m a
  -> Peer (TraceForward lo) 'AsClient 'StIdle m a
traceAcceptorPeer = \case
  SendMsgRequest TokBlocking request next ->
    -- Send our message (request for new 'LogObject's from the forwarder).
    Yield (ClientAgency TokIdle) (MsgRequest TokBlocking request) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder.
      Await (ServerAgency (TokBusy TokBlocking)) $ \(MsgReply reply) ->
        Effect $
          traceAcceptorPeer <$> next reply

  SendMsgRequest TokNonBlocking request next ->
    -- Send our message (request for new 'LogObject's from the forwarder).
    Yield (ClientAgency TokIdle) (MsgRequest TokNonBlocking request) $
      -- We're now into the 'StBusy' state, and now we'll wait for a reply
      -- from the forwarder. It is assuming that the forwarder will reply
      -- immediately (even there are no 'LogObject's).
      Await (ServerAgency (TokBusy TokNonBlocking)) $ \(MsgReply reply) ->
        Effect $
          traceAcceptorPeer <$> next reply

  SendMsgDone getResult ->
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect $
      Yield (ClientAgency TokIdle) MsgDone . Done TokDone
        <$> getResult

-- | Byte Limits.
byteLimitsTraceForward
  :: forall bytes lo.
     (bytes -> Word)
  -> ProtocolSizeLimits (TraceForward lo) bytes
byteLimitsTraceForward = ProtocolSizeLimits stateToLimit
 where
  stateToLimit
    :: forall lo
              (pr :: PeerRole)
              (st :: TraceForward lo).
       PeerHasAgency pr st
    -> Word
  stateToLimit (ServerAgency (TokBusy TokBlocking))    = largeByteLimit
  stateToLimit (ServerAgency (TokBusy TokNonBlocking)) = largeByteLimit
  stateToLimit (ClientAgency TokIdle)                  = smallByteLimit

-- | Time Limits.
timeLimitsTraceForward
  :: forall lo. ProtocolTimeLimits (TraceForward lo)
timeLimitsTraceForward = ProtocolTimeLimits stateToLimit
 where
  stateToLimit
    :: forall lo
              (pr :: PeerRole)
              (st :: TraceForward lo).
       PeerHasAgency pr st
    -> Maybe DiffTime
  stateToLimit (ServerAgency (TokBusy TokBlocking))    = waitForever
  stateToLimit (ServerAgency (TokBusy TokNonBlocking)) = shortWait
  stateToLimit (ClientAgency TokIdle)                  = waitForever
