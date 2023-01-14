{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Trace.Forward.Protocol.DataPoint.Codec () where

import qualified Data.Aeson as A
import           Test.QuickCheck

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec

import           Trace.Forward.Protocol.DataPoint.Type

import           Test.Trace.Forward.Protocol.DataPoint.Item

instance Arbitrary (AnyMessageAndAgency DataPointForward) where
  arbitrary = oneof
    [ pure $ AnyMessageAndAgency (ClientAgency TokIdle) (MsgDataPointsRequest ["NodeInfo"])
    , pure $ AnyMessageAndAgency (ServerAgency TokBusy) (MsgDataPointsReply [("NodeInfo", Nothing)])
    , pure $ AnyMessageAndAgency (ServerAgency TokBusy) (MsgDataPointsReply [("NodeInfo", Just ni)])
    , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]
   where
    ni = A.encode $ TestNodeInfo
      { niName     = "core-1"
      , niVersion  = "1.30.1"
      , niCommit   = "abcdefg"
      , niProtocol = "Shelley"
      }

instance Eq (AnyMessage DataPointForward) where
  AnyMessage (MsgDataPointsRequest r1) == AnyMessage (MsgDataPointsRequest r2) = r1 == r2
  AnyMessage (MsgDataPointsReply r1)   == AnyMessage (MsgDataPointsReply r2)   = r1 == r2
  AnyMessage MsgDone                   == AnyMessage MsgDone                   = True
  _                                    == _                                    = False
