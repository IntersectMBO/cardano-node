{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Test.Trace.Forward.Protocol.TraceObject.Codec () where

import           Test.QuickCheck

import           Network.TypedProtocol.Codec

import           Trace.Forward.Protocol.TraceObject.Type

import           Test.Trace.Forward.Protocol.TraceObject.Item

instance Arbitrary NumberOfTraceObjects where
  arbitrary = oneof
    [ pure $ NumberOfTraceObjects 1
    , pure $ NumberOfTraceObjects 10
    , pure $ NumberOfTraceObjects 100
    ]

instance Arbitrary (AnyMessage (TraceObjectForward TraceItem)) where
  arbitrary = oneof
    [ AnyMessage . MsgTraceObjectsRequest SingBlocking <$> arbitrary
    , AnyMessage . MsgTraceObjectsRequest SingNonBlocking <$> arbitrary
    , AnyMessage . MsgTraceObjectsReply . BlockingReply <$> arbitrary
    , AnyMessage . MsgTraceObjectsReply . NonBlockingReply <$> arbitrary
    , pure  $ AnyMessage MsgDone
    ]

instance Eq (AnyMessage (TraceObjectForward TraceItem)) where
  AnyMessage (MsgTraceObjectsRequest SingBlocking r1)
    == AnyMessage (MsgTraceObjectsRequest SingBlocking r2) = r1 == r2
  AnyMessage (MsgTraceObjectsRequest SingNonBlocking r1)
    == AnyMessage (MsgTraceObjectsRequest SingNonBlocking r2) = r1 == r2
  AnyMessage (MsgTraceObjectsReply (BlockingReply r1))
    == AnyMessage (MsgTraceObjectsReply (BlockingReply r2)) = r1 == r2
  AnyMessage (MsgTraceObjectsReply (NonBlockingReply r1))
    == AnyMessage (MsgTraceObjectsReply (NonBlockingReply r2)) = r1 == r2
  AnyMessage MsgDone
    == AnyMessage MsgDone = True
  _ == _ = False
