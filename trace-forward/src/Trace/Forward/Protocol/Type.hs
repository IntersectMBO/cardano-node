{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the trace forwarding/accepting protocol.
--
-- Since we are using a typed protocol framework this is in some sense /the/
-- definition of the protocol: what is allowed and what is not allowed.

module Trace.Forward.Protocol.Type
  ( TraceForward (..)
  , TokBlockingStyle (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)
  , NumberOfTraceObjects (..)
  , BlockingReplyList (..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy (Proxy(..))
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import           Network.TypedProtocol.Core (Protocol (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
-- IMPORTANT NOTE: the following terminology is used:
--
-- 1. From the protocol's point of view, two peers talk to each other:
--    the forwarder and the acceptor.
-- 2. The forwarder is an application that collects 'TraceObject's and sends
--    them to the acceptor by request (with 'MsgTraceObjectsReply').
-- 3. The acceptor is an application that receives 'TraceObject's from the
--    forwarder.
-- 4. You can think of the acceptor as a client, and the forwarder as a server.
--    After the connection is established, the acceptor asks for 'TraceObject's,
--    the forwarder replies to it.

-- | The acceptor will send this request to the forwarder.
newtype NumberOfTraceObjects = NumberOfTraceObjects
  { nTraceObjects :: Word16
  } deriving (Eq, Generic, Show)

instance ShowProxy NumberOfTraceObjects
instance Serialise NumberOfTraceObjects

data TraceForward lo where

  -- | Both acceptor and forwarder are in idle state. The acceptor can send a
  -- request for a list of 'TraceObject's ('MsgTraceObjectsRequest');
  -- the forwarder is waiting for a request.
  -- It will replay with 'MsgTraceObjectsReply'.
  --
  -- Node's info is an important information about the node, such as
  -- its protocol, version, start time, etc. It is assuming that the node
  -- must provide this information.
  StIdle :: TraceForward lo

  -- | The acceptor has sent a next request for 'TraceObject's. The acceptor is
  -- now waiting for a reply, and the forwarder is busy getting ready to send a
  -- reply with new list of 'TraceObject's.
  --
  -- There are two sub-states for this, for blocking and non-blocking cases.
  StBusy :: StBlockingStyle -> TraceForward lo

  -- | Both the acceptor and forwarder are in the terminal state. They're done.
  StDone :: TraceForward lo

instance (ShowProxy lo)
      => ShowProxy (TraceForward lo) where
  showProxy _ = concat
    [ "TraceForward ("
    , showProxy (Proxy :: Proxy lo)
    , ")"
    ]

data StBlockingStyle where
  -- | In this sub-state the reply need not be prompt. There is no timeout.
  StBlocking    :: StBlockingStyle
  -- | In this sub-state the peer must reply. There is a timeout.
  StNonBlocking :: StBlockingStyle

-- | The value level equivalent of 'StBlockingStyle'.
--
-- This is also used in 'MsgTraceObjectsRequest' where it is interpreted (and can be encoded)
-- as a 'Bool' with 'True' for blocking, and 'False' for non-blocking.
data TokBlockingStyle (k :: StBlockingStyle) where
  TokBlocking    :: TokBlockingStyle 'StBlocking
  TokNonBlocking :: TokBlockingStyle 'StNonBlocking

deriving instance Eq   (TokBlockingStyle b)
deriving instance Show (TokBlockingStyle b)

-- | We have requests for lists of things. In the blocking case the
-- corresponding reply must be non-empty, whereas in the non-blocking case
-- an empty reply is fine.
--
data BlockingReplyList (blocking :: StBlockingStyle) lo where
  BlockingReply    :: NonEmpty lo  -> BlockingReplyList 'StBlocking    lo
  NonBlockingReply ::         [lo] -> BlockingReplyList 'StNonBlocking lo

deriving instance Eq   lo => Eq   (BlockingReplyList blocking lo)
deriving instance Show lo => Show (BlockingReplyList blocking lo)

instance Protocol (TraceForward lo) where

  -- | The messages in the trace forwarding/accepting protocol.
  --
  data Message (TraceForward lo) from to where
    -- | Request the list of 'TraceObject's from the forwarder.
    --   State: Idle -> Busy.
    --
    -- With 'TokBlocking' this is a a blocking operation: the reply will
    -- always have at least one 'TraceObject', and it does not expect a prompt
    -- reply: there is no timeout. This covers the case when there
    -- is nothing else to do but wait.
    --
    -- With 'TokNonBlocking' this is a non-blocking operation: the reply
    -- may be an empty list and this does expect a prompt reply.
    MsgTraceObjectsRequest
      :: TokBlockingStyle blocking
      -> NumberOfTraceObjects
      -> Message (TraceForward lo) 'StIdle ('StBusy blocking)

    -- | Reply with a list of 'TraceObject's for the acceptor.
    -- State: Busy -> Idle.
    MsgTraceObjectsReply
      :: BlockingReplyList blocking lo
      -> Message (TraceForward lo) ('StBusy blocking) 'StIdle

    -- | Terminating message. State: Idle -> Done.
    MsgDone
      :: Message (TraceForward lo) 'StIdle 'StDone

  -- | This is an explanation of our states, in terms of which party has agency
  -- in each state.
  --
  -- 1. When both peers are in Idle state, the acceptor can send a message
  --    to the forwarder (request for new 'TraceObject's),
  -- 2. When both peers are in Busy state, the forwarder is expected to send
  --    a reply to the acceptor (list of new 'TraceObject's).
  --
  -- So we assume that, from __interaction__ point of view:
  -- 1. ClientHasAgency (from 'Network.TypedProtocol.Core') corresponds to acceptor's agency.
  -- 3. ServerHasAgency (from 'Network.TypedProtocol.Core') corresponds to forwarder's agency.
  --
  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle

  data ServerHasAgency st where
    TokBusy :: TokBlockingStyle blocking -> ServerHasAgency ('StBusy blocking)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  -- | Impossible cases.
  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

instance Show lo
      => Show (Message (TraceForward lo) from to) where
  show MsgTraceObjectsRequest{} = "MsgTraceObjectsRequest"
  show MsgTraceObjectsReply{}   = "MsgTraceObjectsReply"
  show MsgDone{}                = "MsgDone"

instance Show (ClientHasAgency (st :: TraceForward lo)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: TraceForward lo)) where
  show TokBusy{} = "TokBusy"
