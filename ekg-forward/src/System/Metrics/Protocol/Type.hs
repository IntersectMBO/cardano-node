{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the EKG forwarding/accepting protocol.
--
-- Since we are using a typed protocol framework this is in some sense /the/
-- definition of the protocol: what is allowed and what is not allowed.

module System.Metrics.Protocol.Type
  ( EKGForward (..)
  , Message (..)
  , SingEKGForward (..)
  ) where

import           Data.Singletons
import           Network.TypedProtocol.Core
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
-- IMPORTANT NOTE: the following terminology is used:
--
-- 1. From the protocol's point of view, two peers talk to each other:
--    the forwarder and the acceptor.
-- 2. The forwarder is an application that collects EKG metrics and sends
--    them to the acceptor by request.
-- 3. The acceptor is an application that receives EKG metrics from the
--    forwarder.
-- 4. You can this of the forwarder as a client, and the acceptor as a server:
--    4.1. The client is "initially active side", because it establishes the
--    connection with the server.
--    4.2. The server is "initially passive side", because it accepts the
--    connection from the client.
-- 5. But after the connection is established, the roles are REVERSED:
--    the acceptor becomes an active side because it asks the metrics, the
--    forwarder becomes a passive side because it waits for the request from
--    the acceptor, collects the metrics and sends them to the acceptor.
--
data EKGForward req resp where

  -- | Both acceptor and forwarder are in idle state. The acceptor can send a
  -- request for the metrics and the forwarder is waiting for a request.
  StIdle :: EKGForward req resp

  -- | The acceptor has sent a next request for the metrics. The acceptor is
  -- now waiting for a response, and the forwarder is busy getting ready to send a
  -- response with new metrics. It is assumed that the forwarder can send a reply
  -- immediately (with the current list of the metrics).
  StBusy :: EKGForward req resp

  -- | Both the acceptor and forwarder are in the terminal state. They're done.
  StDone :: EKGForward req resp

instance (ShowProxy req, ShowProxy resp)
      => ShowProxy (EKGForward req resp) where
  showProxy _ = concat
    [ "EKGForward ("
    , showProxy (Proxy :: Proxy req)
    , ") ("
    , showProxy (Proxy :: Proxy resp)
    , ")"
    ]

-- | Singleton type of EKGForward. Same as:
--
-- @
-- type SingEKGForward :: EKGForward req resp -> Type
-- type SingEKGForward = TypeRep
-- @
data SingEKGForward (st :: EKGForward req resp) where
  SingIdle :: SingEKGForward 'StIdle
  SingBusy :: SingEKGForward 'StBusy
  SingDone :: SingEKGForward 'StDone

type instance Sing = SingEKGForward

deriving instance Show (SingEKGForward st)
instance StateTokenI 'StIdle where stateToken = SingIdle
instance StateTokenI 'StBusy where stateToken = SingBusy
instance StateTokenI 'StDone where stateToken = SingDone

instance Protocol (EKGForward req resp) where

  -- | The messages in the EKG forwarding/accepting protocol.
  --
  data Message (EKGForward req resp) from to where
    -- | Request the list of metrics from the forwarder. State: Idle -> Busy.
    MsgReq  :: req  -> Message (EKGForward req resp) 'StIdle 'StBusy

    -- | Response with the list of metrics for the acceptor. State: Busy -> Idle.
    MsgResp :: resp -> Message (EKGForward req resp) 'StBusy 'StIdle

    -- | Terminating message. State: Idle -> Done.
    MsgDone ::         Message (EKGForward req resp) 'StIdle 'StDone

  -- | This is an explanation of our states, in terms of which party has agency
  -- in each state.
  --
  -- 1. When both peers are in Idle state, the acceptor can send a message
  --    to the forwarder (request for the new metrics),
  -- 2. When both peers are in Busy state, the forwarder is expected to send
  --    a reply to the acceptor (list of new metrics).
  --
  -- So we assume that, from __interaction__ point of view:
  -- 1. ClientAgency (from 'Network.TypedProtocol.Core') corresponds to acceptor's agency.
  -- 3. ServerAgency (from 'Network.TypedProtocol.Core') corresponds to forwarder's agency.
  --
  type StateAgency 'StIdle = 'ClientAgency
  type StateAgency 'StBusy = 'ServerAgency
  type StateAgency 'StDone = 'NobodyAgency

  type StateToken = SingEKGForward

deriving instance (Show req, Show resp)
               => Show (Message (EKGForward req resp) from to)
