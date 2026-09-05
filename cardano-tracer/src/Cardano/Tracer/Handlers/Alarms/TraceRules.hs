{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The trace-severity alarm producer: raises an alarm for every received
-- trace message whose severity is at or above a configured threshold. Like
-- "Cardano.Tracer.Handlers.Alarms.Consumers", this module holds the rule
-- type, its config translation, and the pure matching logic; the IO driving
-- lives in "Cardano.Tracer.Handlers.Alarms.Registry".
module Cardano.Tracer.Handlers.Alarms.TraceRules
  ( TraceAlarmRule (..)
  , traceRuleFromConfig
  , traceAlarmSource
  , traceRuleRequest
  ) where

import           Cardano.Logging.Types (SeverityS, TraceObject (..))
import           Cardano.Tracer.Configuration (AlarmsTraceRuleConfig (..), alarmSeverityToText)
import           Cardano.Tracer.Handlers.Alarms.Types

import           Data.Aeson (object, (.=))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word (Word64)

data TraceAlarmRule = TraceAlarmRule
  { tarRuleId       :: !RuleId
  , tarSummary      :: !Text
  , tarThreshold    :: !SeverityS
  , tarSuppressSecs :: !Word64
  , tarLabels       :: !(Map Text Text)
  }

traceRuleFromConfig :: AlarmsTraceRuleConfig -> TraceAlarmRule
traceRuleFromConfig AlarmsTraceRuleConfig{..} = TraceAlarmRule
  { tarRuleId       = RuleId atrRuleId
  , tarSummary      = fromMaybe defaultSummary atrSummary
  , tarThreshold    = atrThreshold
  , tarSuppressSecs = max 1 (fromMaybe 300 atrSuppressForSecs)
  , tarLabels       = fromMaybe Map.empty atrLabels
  }
 where
  defaultSummary =
    "Trace message with severity at or above "
      <> alarmSeverityToText atrThreshold <> " received"

-- | The fixed trusted source identity for alarms raised by trace severity
--   rules. Internal producers never pass HTTP authentication, so the source
--   is a constant here, mirroring how external producers get their source
--   from their credential rather than choosing it themselves.
traceAlarmSource :: AlarmSource
traceAlarmSource = AlarmSource "trace"

-- | Pure mapping from one received trace message to an alarm submission, or
--   'Nothing' when the message is below the rule's threshold. The
--   @sourceEventId@ encodes a time window derived from the message's own
--   timestamp, so all matches from the same node and namespace within one
--   window share the store's idempotency key and collapse into one alarm --
--   flood prevention without any extra state.
traceRuleRequest :: TraceAlarmRule -> Text -> TraceObject -> Maybe IngressRequest
traceRuleRequest TraceAlarmRule{..} nodeName trObj
  | toSeverity trObj < tarThreshold = Nothing
  | otherwise = Just IngressRequest
      { irSourceEventId = Text.intercalate ":"
          [unRuleId tarRuleId, nodeName, namespace, Text.pack (show windowIndex)]
      , irRaisedAt      = toTimestamp trObj
      , irRuleId        = tarRuleId
      , irSeverity      = toSeverity trObj
      , irSummary       = tarSummary
      , irScope         = Map.singleton "nodeId" nodeName
      , irLabels        = Map.insert "namespace" namespace tarLabels
      , irDetails       = Just $ object
          [ "message"  .= toMachine trObj
          , "hostname" .= toHostname trObj
          , "threadId" .= toThreadId trObj
          ]
      }
 where
  namespace = Text.intercalate "." (toNamespace trObj)

  windowIndex :: Word64
  windowIndex = floor (utcTimeToPOSIXSeconds (toTimestamp trObj)) `div` tarSuppressSecs
