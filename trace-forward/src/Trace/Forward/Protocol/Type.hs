{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the Trace forwarding/accepting protocol.
--
-- Since we are using a typed protocol framework this is in some sense /the/
-- definition of the protocol: what is allowed and what is not allowed.

module Trace.Forward.Protocol.Type
  ( TraceForward (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)
  ) where

import           Data.Proxy (Proxy(..))
import           Network.TypedProtocol.Core (Protocol (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
-- IMPORTANT NOTE: the following terminology is used:
--
-- 1. From the protocol's point of view, two peers talk to each other:
--    the forwarder and the acceptor.
-- 2. The forwarder is an application that collects 'LogObject's and sends
--    them to the acceptor by request.
-- 3. The acceptor is an application that receives 'LogObject's from the
--    forwarder.
-- 4. You can this of the forwarder as a client, and the acceptor as a server:
--    4.1. The client is "initially active side", because it establishes the
--    connection with the server.
--    4.2. The server is "initially passive side", because it accepts the
--    connection from the client.
-- 5. But after the connection is established, the roles are REVERSED:
--    the acceptor becomes an active side because it asks 'LogObject's, the
--    forwarder becomes a passive side because it waits for the request from
--    the acceptor, collects the 'LogObject's and sends them to the acceptor.
--
data TraceForward req resp where

  -- | Both acceptor and forwarder are in idle state. The acceptor can send a
  -- request for 'LogObject's and the forwarder is waiting for a request.
  StIdle :: TraceForward req resp

  -- | The acceptor has sent a next request for 'LogObject's. The acceptor is
  -- now waiting for a response, and the forwarder is busy getting ready to send a
  -- response with new 'LogObject's.
  StBusy :: TraceForward req resp

  -- | Both the acceptor and forwarder are in the terminal state. They're done.
  StDone :: TraceForward req resp

instance (ShowProxy req, ShowProxy resp)
      => ShowProxy (TraceForward req resp) where
  showProxy _ = concat
    [ "TraceForward ("
    , showProxy (Proxy :: Proxy req)
    , ") ("
    , showProxy (Proxy :: Proxy resp)
    , ")"
    ]

instance Protocol (TraceForward req resp) where

  -- | The messages in the Trace forwarding/accepting protocol.
  --
  data Message (TraceForward req resp) from to where
    -- | Request the list of 'LogObject's from the forwarder. State: Idle -> Busy.
    MsgReq  :: req  -> Message (TraceForward req resp) 'StIdle 'StBusy

    -- | Response with the list of 'LogObject's for the acceptor. State: Busy -> Idle.
    MsgResp :: resp -> Message (TraceForward req resp) 'StBusy 'StIdle

    -- | Terminating message. State: Idle -> Done.
    MsgDone ::         Message (TraceForward req resp) 'StIdle 'StDone

  -- | This is an explanation of our states, in terms of which party has agency
  -- in each state.
  --
  -- 1. When both peers are in Idle state, the acceptor can send a message
  --    to the forwarder (request for new 'LogObject's),
  -- 2. When both peers are in Busy state, the forwarder is expected to send
  --    a reply to the acceptor (list of new 'LogObject's).
  --
  -- So we assume that, from __interaction__ point of view:
  -- 1. ClientHasAgency (from 'Network.TypedProtocol.Core') corresponds to acceptor's agency.
  -- 3. ServerHasAgency (from 'Network.TypedProtocol.Core') corresponds to forwarder's agency.
  --
  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle

  data ServerHasAgency st where
    TokBusy :: ServerHasAgency 'StBusy

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  -- | Impossible cases.
  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

instance (Show req, Show resp)
      => Show (Message (TraceForward req resp) from to) where
  show MsgReq{}  = "MsgReq"
  show MsgResp{} = "MsgResp"
  show MsgDone{} = "MsgDone"

instance Show (ClientHasAgency (st :: TraceForward req resp)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: TraceForward req resp)) where
  show TokBusy = "TokBusy"
