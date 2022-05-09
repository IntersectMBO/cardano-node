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

module Trace.Forward.Protocol.TraceObject.Type
  ( TraceObjectForward (..)
  , SingTraceObjectForward (..)
  , SingBlockingStyle (..)
  , Message (..)
  , NumberOfTraceObjects (..)
  , BlockingReplyList (..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Singletons
import           Data.Word (Word16)
import           GHC.Generics (Generic)

import           Network.TypedProtocol.Core
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

deriving instance Show (SingTraceObjectForward st)

data StBlockingStyle where
  -- | In this sub-state the reply need not be prompt. There is no timeout.
  StBlocking    :: StBlockingStyle
  -- | In this sub-state the peer must reply. There is a timeout.
  StNonBlocking :: StBlockingStyle


data SingTraceObjectForward (st :: TraceObjectForward lo) where
  SingIdle :: SingTraceObjectForward StIdle
  SingBusy :: SingBlockingStyle a
             -> SingTraceObjectForward (StBusy a)
  SingDone :: SingTraceObjectForward StDone

-- | The value level equivalent of 'StBlockingStyle'.
--
-- This is also used in 'MsgTraceObjectsRequest' where it is interpreted (and can be encoded)
-- as a 'Bool' with 'True' for blocking, and 'False' for non-blocking.
data SingBlockingStyle (b :: StBlockingStyle) where
  SingBlocking    :: SingBlockingStyle StBlocking
  SingNonBlocking :: SingBlockingStyle StNonBlocking

deriving instance Eq   (SingBlockingStyle b)
deriving instance Show (SingBlockingStyle b)
type instance Sing = SingBlockingStyle
instance SingI StBlocking    where sing = SingBlocking
instance SingI StNonBlocking where sing = SingNonBlocking

type instance Sing = SingTraceObjectForward
instance SingI StIdle     where sing = SingIdle
instance SingI b
      => SingI (StBusy b) where sing = SingBusy sing
instance SingI StDone     where sing = SingDone


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
      :: SingBlockingStyle blocking
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

  type StateAgency StIdle     = ClientAgency
  type StateAgency (StBusy _) = ServerAgency
  type StateAgency StDone     = NobodyAgency

  type StateToken = SingTraceObjectForward

deriving instance Show lo
               => Show (Message (TraceObjectForward lo) from to)
