{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pure domain types for the alarm subsystem (see
-- @cardano-tracer/docs/alarm-system-concept.md@). Deliberately depends only
-- on "Cardano.Tracer.Configuration"/"Cardano.Logging" -- never on
-- "Cardano.Tracer.Environment" -- mirroring how @Cardano.Timeseries.Component@
-- is referenced by @Environment.hs@ but never references it back.
module Cardano.Tracer.Handlers.Alarms.Types
  ( AlarmSource (..)
  , RuleId (..)
  , AlarmCursor (..)
  , AlarmId
  , IngressRequest (..)
  , AlarmEvent (..)
  , AlarmFilter (..)
  , emptyAlarmFilter
  , filterFromConfig
  , matchesFilter
  , filterNarrows
  ) where

import           Cardano.Logging.Types (SeverityS)
import           Cardano.Tracer.Configuration (AlarmFilterConfig (..), alarmSeverityToText,
                   parseAlarmSeverityText)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

-- | The trusted identity of an alarm producer, derived from the producer
--   credential that authenticated the ingress request. Never taken from the
--   request body itself.
newtype AlarmSource = AlarmSource { unAlarmSource :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

newtype RuleId = RuleId { unRuleId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | A monotonic sequence number assigned by the store on insertion. Serves
--   as the pagination cursor for history reads.
newtype AlarmCursor = AlarmCursor Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Enum, Num)

-- | The server-assigned event identifier included in the wire envelope.
type AlarmId = Text

-- | The producer-submitted payload, once JSON-parsed. Only ever reads
--   producer-owned fields: @source@, @eventId@, and @receivedAt@ in the
--   design doc's envelope are assigned by the server, so a caller-supplied
--   value for any of them in the request body is simply not read here.
data IngressRequest = IngressRequest
  { irSourceEventId :: !Text
  , irRaisedAt      :: !UTCTime
  , irRuleId        :: !RuleId
  , irSeverity      :: !SeverityS
  , irSummary       :: !Text
  , irScope         :: !(Map Text Text)
  , irLabels        :: !(Map Text Text)
  , irDetails       :: !(Maybe Aeson.Value)
  }
  deriving stock (Eq, Show)

instance FromJSON IngressRequest where
  parseJSON = withObject "IngressRequest" \o -> do
    irSourceEventId <- o .: "sourceEventId"
    irRaisedAt      <- o .: "raisedAt"
    irRuleId        <- RuleId <$> o .: "ruleId"
    severityText    <- o .: "severity"
    irSeverity      <- parseSeverityOrFail severityText
    irSummary       <- o .: "summary"
    irScope         <- fromMaybe Map.empty <$> o .:? "scope"
    irLabels        <- fromMaybe Map.empty <$> o .:? "labels"
    irDetails       <- o .:? "details"
    pure IngressRequest{..}
   where
    parseSeverityOrFail t =
      maybe (fail ("unknown severity: " <> Text.unpack t)) pure (parseAlarmSeverityText t)

-- | The immutable, versioned alarm envelope. @eventId@\/@receivedAt@\/@source@
--   are assigned by the server; everything else is copied from the
--   producer's 'IngressRequest'.
data AlarmEvent = AlarmEvent
  { schemaVersion :: !Int
  , eventId       :: !AlarmId
  , sourceEventId :: !Text
  , raisedAt      :: !UTCTime
  , receivedAt    :: !UTCTime
  , source        :: !AlarmSource
  , ruleId        :: !RuleId
  , severity      :: !SeverityS
  , summary       :: !Text
  , scope         :: !(Map Text Text)
  , labels        :: !(Map Text Text)
  , details       :: !(Maybe Aeson.Value)
  }
  deriving stock (Eq, Show)

instance ToJSON AlarmEvent where
  toJSON AlarmEvent{..} = object
    [ "schemaVersion" .= schemaVersion
    , "eventId"       .= eventId
    , "sourceEventId" .= sourceEventId
    , "raisedAt"      .= raisedAt
    , "receivedAt"    .= receivedAt
    , "source"        .= unAlarmSource source
    , "ruleId"        .= unRuleId ruleId
    , "severity"      .= alarmSeverityToText severity
    , "summary"       .= summary
    , "scope"         .= scope
    , "labels"        .= labels
    , "details"       .= details
    ]

instance FromJSON AlarmEvent where
  parseJSON = withObject "AlarmEvent" \o -> do
    schemaVersion <- o .: "schemaVersion"
    eventId       <- o .: "eventId"
    sourceEventId <- o .: "sourceEventId"
    raisedAt      <- o .: "raisedAt"
    receivedAt    <- o .: "receivedAt"
    source        <- AlarmSource <$> o .: "source"
    ruleId        <- RuleId <$> o .: "ruleId"
    severityText  <- o .: "severity"
    severity      <- parseSeverityOrFail severityText
    summary       <- o .: "summary"
    scope         <- fromMaybe Map.empty <$> o .:? "scope"
    labels        <- fromMaybe Map.empty <$> o .:? "labels"
    details       <- o .:? "details"
    pure AlarmEvent{..}
   where
    parseSeverityOrFail t =
      maybe (fail ("unknown severity: " <> Text.unpack t)) pure (parseAlarmSeverityText t)

-- | A conjunctive selection over 'AlarmEvent's, used both for a static
--   consumer's configuration and for a reader credential's allowed
--   (ceiling) or requested filter. Scope\/label matching requires
--   the event to carry every key\/value pair listed here (a submap check),
--   not just an intersection.
data AlarmFilter = AlarmFilter
  { afSource      :: !(Maybe AlarmSource)
  , afRuleId      :: !(Maybe RuleId)
  , afMinSeverity :: !(Maybe SeverityS)
  , afScope       :: !(Map Text Text)
  , afLabels      :: !(Map Text Text)
  }
  deriving stock (Eq, Show)

emptyAlarmFilter :: AlarmFilter
emptyAlarmFilter = AlarmFilter Nothing Nothing Nothing Map.empty Map.empty

filterFromConfig :: AlarmFilterConfig -> AlarmFilter
filterFromConfig AlarmFilterConfig{..} = AlarmFilter
  { afSource      = AlarmSource <$> afcSource
  , afRuleId      = RuleId <$> afcRuleId
  , afMinSeverity = afcMinSeverity
  , afScope       = fromMaybe Map.empty afcScope
  , afLabels      = fromMaybe Map.empty afcLabels
  }

isSubmapOf :: (Ord k, Eq v) => Map k v -> Map k v -> Bool
isSubmapOf = Map.isSubmapOfBy (==)

matchesFilter :: AlarmFilter -> AlarmEvent -> Bool
matchesFilter AlarmFilter{..} ev =
     maybe True (== source ev)   afSource
  && maybe True (== ruleId ev)   afRuleId
  && maybe True (<= severity ev) afMinSeverity
  && afScope  `isSubmapOf` scope ev
  && afLabels `isSubmapOf` labels ev

-- | Is @requested@ at least as strict as @ceiling@ -- i.e. can it only narrow,
--   never broaden, what the credential allows? Used to reject a reader's
--   requested history filter that would see more than their credential
--   permits.
filterNarrows :: AlarmFilter -> AlarmFilter -> Bool
filterNarrows ceilingFilter requestedFilter =
     narrowsEq (afSource ceilingFilter) (afSource requestedFilter)
  && narrowsEq (afRuleId ceilingFilter) (afRuleId requestedFilter)
  && narrowsSeverity (afMinSeverity ceilingFilter) (afMinSeverity requestedFilter)
  && afScope  ceilingFilter `isSubmapOf` afScope  requestedFilter
  && afLabels ceilingFilter `isSubmapOf` afLabels requestedFilter
 where
  -- A single-value equality constraint can only be kept as-is, never
  -- dropped or changed, once the ceiling fixes it.
  narrowsEq :: Eq a => Maybe a -> Maybe a -> Bool
  narrowsEq Nothing  _        = True
  narrowsEq (Just c) (Just r) = c == r
  narrowsEq (Just _) Nothing  = False

  narrowsSeverity :: Maybe SeverityS -> Maybe SeverityS -> Bool
  narrowsSeverity Nothing  _        = True
  narrowsSeverity (Just c) (Just r) = r >= c
  narrowsSeverity (Just _) Nothing  = False
