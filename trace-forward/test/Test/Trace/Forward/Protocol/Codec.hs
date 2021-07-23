{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Trace.Forward.Protocol.Codec () where

import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Test.QuickCheck

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec

import           Trace.Forward.Protocol.Type

import           Test.Trace.Forward.Protocol.TraceItem

instance Arbitrary NumberOfTraceObjects where
  arbitrary = NumberOfTraceObjects <$> arbitrary

ni1, ni2 :: NodeInfo
ni1 = NodeInfo
  { niName            = "core-1"
  , niProtocol        = "Shelley"
  , niVersion         = "1.28.0"
  , niCommit          = "cffa06c"
  , niStartTime       = UTCTime (fromGregorian 2021 7 24) ((22 * 3600) + (15 * 60) +  1)
  , niSystemStartTime = UTCTime (fromGregorian 2017 9 24) (( 1 * 3600) + (44 * 60) + 51)
  }
ni2 = NodeInfo
  { niName            = "core-2"
  , niProtocol        = "Shelley"
  , niVersion         = "1.28.0"
  , niCommit          = "cffa06c"
  , niStartTime       = UTCTime (fromGregorian 2021 7 24) ((22 * 3600) + (15 * 60) +  1)
  , niSystemStartTime = UTCTime (fromGregorian 2017 9 24) (( 1 * 3600) + (44 * 60) + 51)
  }

instance Arbitrary (AnyMessageAndAgency (TraceForward TraceItem)) where
  arbitrary = oneof
    [ pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgNodeInfoRequest
    , AnyMessageAndAgency (ClientAgency TokIdle) . MsgTraceObjectsRequest TokBlocking <$> arbitrary
    , AnyMessageAndAgency (ClientAgency TokIdle) . MsgTraceObjectsRequest TokNonBlocking <$> arbitrary
    , AnyMessageAndAgency (ServerAgency TokNodeInfoBusy) . MsgNodeInfoReply <$> oneof [pure ni1, pure ni2]
    , AnyMessageAndAgency (ServerAgency (TokBusy TokBlocking)) . MsgTraceObjectsReply . BlockingReply <$> arbitrary
    , AnyMessageAndAgency (ServerAgency (TokBusy TokNonBlocking)) . MsgTraceObjectsReply . NonBlockingReply <$> arbitrary
    , pure  $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
    ]

instance Eq (AnyMessage (TraceForward TraceItem)) where
  AnyMessage MsgNodeInfoRequest
    == AnyMessage MsgNodeInfoRequest = True
  AnyMessage (MsgNodeInfoReply r1)
    == AnyMessage (MsgNodeInfoReply r2) = r1 == r2
  AnyMessage (MsgTraceObjectsRequest TokBlocking r1)
    == AnyMessage (MsgTraceObjectsRequest TokBlocking r2) = r1 == r2
  AnyMessage (MsgTraceObjectsRequest TokNonBlocking r1)
    == AnyMessage (MsgTraceObjectsRequest TokNonBlocking r2) = r1 == r2
  AnyMessage (MsgTraceObjectsReply (BlockingReply r1))
    == AnyMessage (MsgTraceObjectsReply (BlockingReply r2)) = r1 == r2
  AnyMessage (MsgTraceObjectsReply (NonBlockingReply r1))
    == AnyMessage (MsgTraceObjectsReply (NonBlockingReply r2)) = r1 == r2
  AnyMessage MsgDone
    == AnyMessage MsgDone = True
  _ == _ = False
