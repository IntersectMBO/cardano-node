{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Trace.Forward.Test.Types
  ( Endpoint (..)
  , TraceForwardText
  ) where

import           Data.Aeson.Types (Object, Value (..))
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import qualified Data.Vector as V
import           Test.QuickCheck

import           Network.TypedProtocol.Codec

import           Cardano.BM.Data.Aggregated (Aggregated (..), EWMA (..), Measurable (..))
import           Cardano.BM.Data.BackendKind (BackendKind (..))
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..),
                                          CounterType (..))
import           Cardano.BM.Data.LogItem (CommandValue (..), LoggerName, LogObject (..),
                                          LOContent (..), LOMeta (..), MonitorAction (..),
                                          PrivacyAnnotation (..))
import           Cardano.BM.Data.Severity (Severity (..))

import           Trace.Forward.Protocol.Type
import           Trace.Forward.ReqResp

data Endpoint = Pipe | Socket

-- | Response is parametrized by type because of 'LogObject'.
-- In practise we use 'LogObject Text' in most cases, so test
-- it with 'Response Text' as well.
type TraceForwardText = TraceForward Request (Response Text)

instance Arbitrary Request where
  arbitrary = GetLogObjects <$> arbitrary

instance Arbitrary (Response Text) where
  arbitrary = ResponseLogObjects <$> arbitrary

instance Arbitrary (LogObject Text) where
  arbitrary = LogObject
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary LoggerName where
  arbitrary = oneof
    [ return "trace.forward.test.LO.1"
    , return "trace.forward.test.LO.NAME.2"
    , return "trace.forward.test.LOGGER.NAME.3"
    ]

instance Arbitrary LOMeta where
  arbitrary = LOMeta
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Severity where
  arbitrary = oneof
    [ return Debug
    , return Info
    , return Notice
    , return Warning
    , return Error
    , return Critical
    , return Alert
    , return Emergency
    ]

instance Arbitrary PrivacyAnnotation where
  arbitrary = oneof
    [ return Confidential
    , return Public
    ]

instance Arbitrary UTCTime where
  arbitrary = oneof
    [ return $ UTCTime (fromGregorian 2021 03 01) (timeOfDayToTime $ TimeOfDay 2 25 50.553)
    , return $ UTCTime (fromGregorian 2021 04 02) (timeOfDayToTime $ TimeOfDay 6 42 51.553)
    ]

instance Arbitrary Object where
  arbitrary = do
    obj <- arbitrary
    return $ HM.fromList [("test.obj.1", obj)]

instance Arbitrary Value where
  arbitrary = oneof
    [ Object <$> arbitrary
    , Array <$> arbitrary
    , return $ String "String.Value.1"
    , return $ Number 12.3
    , return $ Bool True
    , return Null
    ]

instance Arbitrary (V.Vector Value) where
  arbitrary = oneof
    [ return $ V.fromList [String "String.Value.2"]
    , return $ V.fromList [Number 34.56, Bool False]
    ]

instance Arbitrary (LOContent Text) where
  arbitrary = oneof
    [ return $ LogMessage "test.LogMessage.1"
    , return $ LogError "test.LogError.Error.1"
    , LogValue "test.LogValue.1" <$> arbitrary
    , LogStructuredText <$> arbitrary <*> return "test.log.Structured"
    , LogStructured <$> arbitrary
    , ObserveClose . CounterState <$> arbitrary
    , ObserveOpen . CounterState <$> arbitrary
    , ObserveDiff . CounterState <$> arbitrary
    , return $ AggregatedMessage [("test.aggr.message.1", AggregatedEWMA (EmptyEWMA 1.0))]
    , return $ MonitoringEffect (MonitorAlert "test.Monitor.Alert.1")
    , return $ Command (DumpBufferedTo KatipBK)
    , return KillPill
    ]

instance Arbitrary Counter where
  arbitrary = Counter <$> arbitrary <*> return "test.Counter" <*> arbitrary

instance Arbitrary CounterType where
  arbitrary = oneof
    [ return MonotonicClockTime
    , return MemoryCounter
    , return SysInfo
    , return StatInfo
    , return IOCounter
    , return NetCounter
    , return RTSStats
    ]

instance Arbitrary Measurable where
  arbitrary = oneof
    [ Microseconds <$> arbitrary
    , Nanoseconds <$> arbitrary
    , Seconds <$> arbitrary
    , Bytes <$> arbitrary
    , PureD <$> arbitrary
    , PureI <$> arbitrary
    , Severity <$> arbitrary
    ]

instance Arbitrary (AnyMessageAndAgency TraceForwardText) where
  arbitrary = genTraceForward arbitrary arbitrary

genTraceForward
  :: Gen req
  -> Gen resp
  -> Gen (AnyMessageAndAgency (TraceForward req resp))
genTraceForward genReq genResp = oneof
  [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgReq <$> genReq
  , AnyMessageAndAgency (ServerAgency TokBusy) . MsgResp <$> genResp
  , return $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
  ]

instance ( Eq req
         , Eq resp
         ) => Eq (AnyMessage (TraceForward req resp)) where
  AnyMessage (MsgReq r1)  == AnyMessage (MsgReq r2)  = r1 == r2
  AnyMessage (MsgResp r1) == AnyMessage (MsgResp r2) = r1 == r2
  AnyMessage MsgDone      == AnyMessage MsgDone      = True
  _                       == _                       = False
