{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the trace forwarding/accepting protocol.
--

module Trace.Forward.Protocol.TraceObject.Type
  ( TraceObjectForward (..)
  , SingTraceObjectForward(..)
  , TokBlockingStyle (..)
  , Message (..)
  , NumberOfTraceObjects (..)
  , BlockingReplyList (..)
  , StBlockingStyle(..)
  ) where

import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import           Data.Kind (Type)
import           Data.Singletons
import           Codec.Serialise (Serialise (..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import           Network.TypedProtocol.Core -- (Protocol (..))

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
  }
  deriving stock
    (Eq, Generic, Show)
  deriving anyclass
    (ShowProxy, Serialise)

data TraceObjectForward lo where

  -- | Both acceptor and forwarder are in idle state. The acceptor can send a
  -- request for a list of 'TraceObject's ('MsgTraceObjectsRequest');
  -- the forwarder is waiting for a request.
  -- It will replay with 'MsgTraceObjectsReply'.
  --
  -- Node's info is an important information about the node, such as
  -- its protocol, version, start time, etc. It is assuming that the node
  -- must provide this information.
  StIdle :: TraceObjectForward lo

  -- | The acceptor has sent a next request for 'TraceObject's. The acceptor is
  -- now waiting for a reply, and the forwarder is busy getting ready to send a
  -- reply with new list of 'TraceObject's.
  --
  -- There are two sub-states for this, for blocking and non-blocking cases.
  StBusy :: StBlockingStyle -> TraceObjectForward lo

  -- | Both the acceptor and forwarder are in the terminal state. They're done.
  StDone :: TraceObjectForward lo

instance (ShowProxy lo)
      => ShowProxy (TraceObjectForward lo) where
  showProxy _ = concat
    [ "TraceObjectForward ("
    , showProxy (Proxy :: Proxy lo)
    , ")"
    ]

-- | Singleton type of TraceObjectForward.
type SingTraceObjectForward :: TraceObjectForward lo -> Type
data SingTraceObjectForward traceObj where
  SingIdle :: SingTraceObjectForward 'StIdle
  SingBusy :: TokBlockingStyle blockStyle -> SingTraceObjectForward ('StBusy blockStyle)
  SingDone :: SingTraceObjectForward 'StDone

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

type instance Sing = SingTraceObjectForward
type instance Sing = TokBlockingStyle

deriving stock
  instance Show (SingTraceObjectForward traceObj)
instance StateTokenI 'StIdle where stateToken = SingIdle
instance StateTokenI 'StDone where stateToken = SingDone
instance SingI blockStyle => StateTokenI ('StBusy blockStyle) where stateToken = SingBusy sing

instance SingI 'StBlocking    where sing = TokBlocking
instance SingI 'StNonBlocking where sing = TokNonBlocking

-- | We have requests for lists of things. In the blocking case the
-- corresponding reply must be non-empty, whereas in the non-blocking case
-- an empty reply is fine.
--
data BlockingReplyList (blocking :: StBlockingStyle) lo where
  BlockingReply    :: NonEmpty lo  -> BlockingReplyList 'StBlocking    lo
  NonBlockingReply ::         [lo] -> BlockingReplyList 'StNonBlocking lo

deriving instance Eq   lo => Eq   (BlockingReplyList blocking lo)
deriving instance Show lo => Show (BlockingReplyList blocking lo)

instance Protocol (TraceObjectForward lo) where

  -- | The messages in the trace forwarding/accepting protocol.
  --
  data Message (TraceObjectForward lo) from to where
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
      -> Message (TraceObjectForward lo) 'StIdle ('StBusy blocking)

    -- | Reply with a list of 'TraceObject's for the acceptor.
    -- State: Busy -> Idle.
    MsgTraceObjectsReply
      :: BlockingReplyList blocking lo
      -> Message (TraceObjectForward lo) ('StBusy blocking) 'StIdle

    -- | Terminating message. State: Idle -> Done.
    MsgDone
      :: Message (TraceObjectForward lo) 'StIdle 'StDone

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
  type StateAgency 'StIdle = 'ClientAgency
  type StateAgency ('StBusy blocking) = 'ServerAgency
  type StateAgency 'StDone = 'NobodyAgency

  type StateToken = SingTraceObjectForward

deriving stock
  instance Show lo => Show (Message (TraceObjectForward lo) from to)
