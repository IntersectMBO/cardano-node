{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the 'DataPoint' forwarding/accepting protocol.
--

module Trace.Forward.Protocol.DataPoint.Type
  ( DataPointName
  , DataPointValue
  , DataPointValues
  , DataPointForward (..)
  , SingDataPointForward (..)
  , Message (..)
  ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import           Data.Singletons

import           Network.TypedProtocol.Core

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
-- IMPORTANT NOTE: the following terminology is used:
--
-- 1. From the protocol's point of view, two peers talk to each other:
--    the forwarder and the acceptor.
-- 2. The forwarder is an application that collects 'DataPoint's and sends
--    them to the acceptor by request (with 'MsgDataPointsReply').
-- 3. The acceptor is an application that receives 'DataPoint's from the
--    forwarder.
-- 4. You can think of the acceptor as a client, and the forwarder as a server.
--    After the connection is established, the acceptor asks for 'DataPoint's,
--    the forwarder replies to it.

type DataPointName   = Text
type DataPointValue  = LBS.ByteString
type DataPointValues = [(DataPointName, Maybe DataPointValue)]

data DataPointForward where

  -- | Both acceptor and forwarder are in idle state. The acceptor can send a
  -- request  for a list of 'DataPoint's ('MsgDataPointsRequest');
  -- the forwarder is waiting for a request, it will replay with 'MsgDataPointsReply'.
  StIdle :: DataPointForward

  -- | The acceptor has sent a next request for 'DataPoint's. The acceptor is
  -- now waiting for a reply, and the forwarder is busy getting ready to send a
  -- reply with new list of 'DataPoint's.
  StBusy :: DataPointForward

  -- | Both the acceptor and forwarder are in the terminal state. They're done.
  StDone :: DataPointForward

data SingDataPointForward (st :: DataPointForward) where
  SingIdle :: SingDataPointForward StIdle
  SingBusy :: SingDataPointForward StBusy
  SingDone :: SingDataPointForward StDone

deriving instance Show (SingDataPointForward (st :: DataPointForward))
type instance Sing = SingDataPointForward
instance SingI StIdle where sing = SingIdle
instance SingI StBusy where sing = SingBusy
instance SingI StDone where sing = SingDone

instance Protocol DataPointForward where

  -- | The messages in the trace forwarding/accepting protocol.
  --
  data Message DataPointForward from to where
    -- | Request the list of 'DataPoint's from the forwarder.
    --   State: Idle -> Busy.
    MsgDataPointsRequest
      :: [DataPointName]
      -> Message DataPointForward 'StIdle 'StBusy

    -- | Reply with a list of 'DataPoint's for the acceptor.
    -- State: Busy -> Idle.
    MsgDataPointsReply
      :: DataPointValues
      -> Message DataPointForward 'StBusy 'StIdle

    -- | Terminating message. State: Idle -> Done.
    MsgDone
      :: Message DataPointForward 'StIdle 'StDone

  type StateToken = SingDataPointForward

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StDone = NobodyAgency

deriving instance Show (Message DataPointForward from to)
