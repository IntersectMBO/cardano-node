{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The timeseries-query alarm producer (concept-doc § Producers —
-- Timeseries rules). Periodically evaluates a @cardano-timeseries-io@
-- boolean query against the in-process 'TimeseriesHandle' and, per
-- output series, runs a small edge-triggered state machine:
--
--   false/missing → pending → publish
--        ^          |             |
--        `--------- '--- false ---'
--
-- Each output series has a deterministic key derived from its labels;
-- the @sourceEventId@ embeds that key so the alarm store dedupes
-- per-series edges. When @repeatEvery@ is configured, a still-true
-- series republishes at that interval with a fresh window-index in the
-- key.
--
-- Errors and missing data are traced as health information; they never
-- publish false alarms. Query execution is bounded by a timeout so a
-- pathological rule cannot starve normal timeseries ingestion.
module Cardano.Tracer.Handlers.Alarms.TimeseriesRules
  ( TimeseriesAlarmRule
  , tarRuleId
  , tarQuery
  , tarEvaluateEvery
  , timeseriesAlarmSource
  , timeseriesRuleFromConfig
  , SamplePoint (..)
  , decodeSamples
  , evaluateOnce
  , ruleRequests
  , SeriesState (..)
  ) where

import           Cardano.Logging.Types (SeverityS)
import           Cardano.Timeseries.API (Value (..))
import           Cardano.Timeseries.Domain.Instant (Instant (..))
import           Cardano.Tracer.Configuration (AlarmsTimeseriesRuleConfig (..))
import           Cardano.Tracer.Handlers.Alarms.Types

import           Data.Aeson (object, (.=))
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word (Word64)

-- | Runtime form of a timeseries rule. The mutable 'IORef' holds the
--   per-series state machines — inactive, pending-with-since, or
--   active-since-published.
data TimeseriesAlarmRule = TimeseriesAlarmRule
  { tarRuleId        :: !RuleId
  , tarSummary       :: !Text
  , tarSeverity      :: !SeverityS
  , tarQuery         :: !Text
  , tarEvaluateEvery :: !Word64                 -- ^ seconds
  , tarFor           :: !Word64                 -- ^ seconds; 0 means "publish on first true"
  , tarRepeatEvery   :: !(Maybe Word64)         -- ^ seconds between reminders while still true
  , tarLabels        :: !(Map Text Text)
  , tarSeriesState   :: !(IORef (Map SeriesKey SeriesState))
  }

-- | Canonical, deterministic key derived from a sample's labels. Encoded
--   as @k1=v1,k2=v2@ with keys sorted so semantically equal maps produce
--   the same key.
newtype SeriesKey = SeriesKey { unSeriesKey :: Text }
  deriving stock (Eq, Ord, Show)

-- | Per-series state.
data SeriesState
  = Inactive
    -- ^ Last observed sample was false or missing.
  | Pending    !UTCTime
    -- ^ Sample is truthy; waiting for the @for@ duration to elapse
    --   before publishing.
  | Active     !UTCTime
    -- ^ Sample stayed truthy for @for@ seconds and has been published
    --   at least once; the field is @publishedAt@ (last publish time).
  deriving stock (Eq, Show)

-- | One boolean sample decoded from a query response, tagged with its
--   labels (the source of 'SeriesKey').
data SamplePoint = SamplePoint
  { spLabels :: !(Map Text Text)
  , spTruth  :: !Bool
  }
  deriving stock (Eq, Show)

-- | The fixed trusted 'source' for alarms raised by timeseries rules.
--   Internal producer; never passes HTTP authentication.
timeseriesAlarmSource :: AlarmSource
timeseriesAlarmSource = AlarmSource "timeseries"

--------------------------------------------------------------------------------
-- Config → runtime
--------------------------------------------------------------------------------

timeseriesRuleFromConfig :: AlarmsTimeseriesRuleConfig -> IO TimeseriesAlarmRule
timeseriesRuleFromConfig AlarmsTimeseriesRuleConfig{..} = do
  stateRef <- newIORef Map.empty
  pure TimeseriesAlarmRule
    { tarRuleId        = RuleId atsRuleId
    , tarSummary       = fromMaybe defaultSummary atsSummary
    , tarSeverity      = atsSeverity
    , tarQuery         = atsQuery
    , tarEvaluateEvery = max 1 atsEvaluateEvery
    , tarFor           = fromMaybe 0 atsFor
    , tarRepeatEvery   = atsRepeatEvery
    , tarLabels        = fromMaybe Map.empty atsLabels
    , tarSeriesState   = stateRef
    }
 where
  defaultSummary = "Timeseries rule " <> atsRuleId <> " triggered"

--------------------------------------------------------------------------------
-- Series key
--------------------------------------------------------------------------------

-- | Deterministic canonical encoding of a label map. Sorted by key so
--   two 'Map's with the same content always produce the same key.
seriesKeyOf :: Map Text Text -> SeriesKey
seriesKeyOf labels =
  SeriesKey (Text.intercalate "," [ k <> "=" <> v | (k, v) <- Map.toAscList labels ])

--------------------------------------------------------------------------------
-- Evaluation → ingress requests
--------------------------------------------------------------------------------

-- | Apply one round of samples to the rule. Advances every touched
--   series through its state machine; returns one 'IngressRequest' per
--   series that just published (either a fresh edge or a scheduled
--   reminder).
--
-- Series that appear as 'False' or as missing (not in the samples map)
-- transition to 'Inactive'. Series that appear as 'True' advance
-- 'Inactive → Pending' or, having sat in 'Pending' for at least
-- @tarFor@ seconds, transition to 'Active' and publish.
--
-- Pure enough to be tested: takes @now@ as an argument, returns the new
-- 'SeriesState' map alongside the requests, and writes back to the
-- 'IORef' only in 'evaluateOnce'.
ruleRequests
  :: TimeseriesAlarmRule
  -> UTCTime                                -- ^ evaluation timestamp
  -> [SamplePoint]                          -- ^ decoded query response
  -> Map SeriesKey SeriesState              -- ^ previous state
  -> (Map SeriesKey SeriesState, [IngressRequest])
ruleRequests rule@TimeseriesAlarmRule{tarFor, tarRepeatEvery} now samples prev =
  let touched  = Map.fromList [ (seriesKeyOf (spLabels sp), sp) | sp <- samples ]
      allKeys  = Set.toAscList (Map.keysSet touched <> Map.keysSet prev)
      results  = map
                   (\k -> advance k (Map.lookup k touched) (Map.findWithDefault Inactive k prev))
                   allKeys
      newState = Map.fromList [ (k, s) | (k, s, _) <- results ]
      reqs     = [ req | (_, _, Just req) <- results ]
  in (newState, reqs)
 where
  advance :: SeriesKey
          -> Maybe SamplePoint
          -> SeriesState
          -> (SeriesKey, SeriesState, Maybe IngressRequest)
  advance key mSample state = case (state, isTrue mSample) of
    (_, False) ->
      (key, Inactive, Nothing)
    (Inactive, True) ->
      if tarFor == 0
        then let req = buildRequest rule key mSample now
             in (key, Active now, Just req)
        else (key, Pending now, Nothing)
    (Pending since, True) ->
      let elapsed = diffUTCTime now since
      in if realToFrac elapsed >= (fromIntegral tarFor :: Double)
           then let req = buildRequest rule key mSample now
                in (key, Active now, Just req)
           else (key, Pending since, Nothing)
    (Active publishedAt, True) ->
      case tarRepeatEvery of
        Nothing -> (key, Active publishedAt, Nothing)
        Just repeatSecs ->
          let elapsed = diffUTCTime now publishedAt
          in if realToFrac elapsed >= (fromIntegral repeatSecs :: Double)
               then let req = buildRequest rule key mSample now
                    in (key, Active now, Just req)
               else (key, Active publishedAt, Nothing)

  isTrue :: Maybe SamplePoint -> Bool
  isTrue = maybe False spTruth

buildRequest :: TimeseriesAlarmRule -> SeriesKey -> Maybe SamplePoint -> UTCTime -> IngressRequest
buildRequest TimeseriesAlarmRule{tarRuleId, tarSummary, tarSeverity, tarLabels}
             seriesKey mSample now =
  IngressRequest
    { irSourceEventId = "ts:" <> unRuleId tarRuleId <> ":" <> unSeriesKey seriesKey
                     <> ":" <> Text.pack (show (windowIndex now))
    , irRaisedAt      = now
    , irRuleId        = tarRuleId
    , irSeverity      = tarSeverity
    , irSummary       = tarSummary
    , irScope         = Map.empty
    , irLabels        = Map.union sampleLabels tarLabels
    , irDetails       = Just $ object
        [ "seriesKey" .= unSeriesKey seriesKey
        , "labels"    .= sampleLabels
        ]
    }
 where
  sampleLabels = maybe Map.empty spLabels mSample
  -- Millisecond-resolution window index so successive publishes (from
  -- 'repeatEvery') always get distinct source-event-ids.
  windowIndex :: UTCTime -> Integer
  windowIndex t = floor (realToFrac (utcTimeToPOSIXSeconds t) * (1000 :: Double))

-- | Decode a query result into per-series boolean samples.
--
-- Three top-level shapes are meaningful; anything else is a shape
-- error. Inside an instant vector two element shapes are accepted:
--
--   * 'Truth'\/'Falsity' — produced by explicitly boolean queries such
--     as @map (\\x -> x > 200) (m now)@;
--   * 'Scalar' — produced by comparison filters such as @m now > 200@,
--     which keep only the series satisfying the relation. Presence of
--     a series in the result means the condition holds for it, exactly
--     like a PromQL alert expression, so a surviving 'Scalar' decodes
--     as true. Series filtered out are absent from the vector and are
--     treated as missing (false) by the state machine.
decodeSamples :: Value -> Maybe [SamplePoint]
decodeSamples val = case val of
  Truth           -> Just [SamplePoint Map.empty True]
  Falsity         -> Just [SamplePoint Map.empty False]
  InstantVector v -> traverse decodeInstant v
  _               -> Nothing
 where
  decodeInstant :: Instant Value -> Maybe SamplePoint
  decodeInstant (Instant labels _ inner) = do
    truth <- case inner of
      Truth    -> Just True
      Falsity  -> Just False
      Scalar _ -> Just True
      _        -> Nothing
    pure (SamplePoint (Map.fromList (Set.toList labels)) truth)

-- | 'IO' wrapper around 'ruleRequests': reads the previous per-series
--   state atomically, computes the new state, writes it back, and
--   returns the requests to submit.
evaluateOnce
  :: TimeseriesAlarmRule
  -> UTCTime
  -> [SamplePoint]
  -> IO [IngressRequest]
evaluateOnce rule now samples =
  atomicModifyIORef' (tarSeriesState rule) \prev ->
    let (newState, reqs) = ruleRequests rule now samples prev
    in (newState, reqs)

