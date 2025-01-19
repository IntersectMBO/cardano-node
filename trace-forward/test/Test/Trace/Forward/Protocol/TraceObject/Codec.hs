{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Test.Trace.Forward.Protocol.TraceObject.Codec () where

import           Network.TypedProtocol.Codec

import           Test.QuickCheck
import           Test.Trace.Forward.Protocol.TraceObject.Item

import           Trace.Forward.Protocol.TraceObject.Type

instance Arbitrary NumberOfTraceObjects where
  arbitrary = oneof
    [ pure $ NumberOfTraceObjects 1
    , pure $ NumberOfTraceObjects 10
    , pure $ NumberOfTraceObjects 100
    ]

instance Arbitrary (AnyMessage (TraceObjectForward TraceItem)) where
  arbitrary = oneof
    [ AnyMessage . MsgTraceObjectsRequest TokBlocking <$> arbitrary
    , AnyMessage . MsgTraceObjectsRequest TokNonBlocking <$> arbitrary
    , AnyMessage . MsgTraceObjectsReply . BlockingReply <$> arbitrary
    , AnyMessage . MsgTraceObjectsReply . NonBlockingReply <$> arbitrary
    , pure $ AnyMessage MsgDone
    ]

instance Eq (AnyMessage (TraceObjectForward TraceItem)) where
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
