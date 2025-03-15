{-# LANGUAGE OverloadedStrings #-}

module Cardano.Logging.TraceDispatcherMessage
  (
    UnknownNamespaceKind (..)
  , TraceDispatcherMessage (..)
  ) where

import           Cardano.Logging.ConfigurationParser ()
import           Cardano.Logging.Types

import           Data.Aeson hiding (Error)
import           Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data UnknownNamespaceKind =
    UKFSeverity
  | UKFPrivacy
  | UKFDetails

instance Show UnknownNamespaceKind where
  show UKFSeverity = "severity"
  show UKFPrivacy = "privacy"
  show UKFDetails = "details"

data TraceDispatcherMessage =
    StartLimiting Text
    -- ^ This message indicates the start of frequency limiting
  | StopLimiting Text Int
    -- ^ This message indicates the stop of frequency limiting,
    -- and gives the number of messages that has been suppressed
  | RememberLimiting Text Int
    -- ^ This message remembers of ongoing frequency limiting,
    -- and gives the number of messages that has been suppressed
  | UnknownNamespace [Text] [Text] UnknownNamespaceKind
    -- ^ An internal error was detected
  | TracerInfo [Text] [Text] [Text]
    -- ^  The first array signifies the namespace of silent tracers
    --    The second array signifies the namespace tracers without metrics
    --    The third array gives the names of all tracers
  | MetricsInfo (Map.Map Text Int)
    -- ^  Outputs optional statistics about metrics frequency
  | TracerConsistencyWarnings [Text]
    -- ^  Consistency check found warnings
  | TracerInfoConfig TraceConfig
    -- ^  Trace the effective configuration as JSON
  deriving Show

instance LogFormatting TraceDispatcherMessage where
  forHuman (StartLimiting txt) = "Start of frequency limiting for " <> txt
  forHuman (StopLimiting txt num) = "Stop of frequency limiting for " <> txt <>
    ". Suppressed " <> textShow num <> " messages."
  forHuman (RememberLimiting txt num) = "Frequency limiting still active for " <> txt <>
    ". Suppressed so far " <> textShow num <> " messages."
  forHuman (UnknownNamespace nsPrefixNS nsInnerNS qk) = "Unknown namespace detected "
    <> Text.intercalate (Text.singleton '.') (nsPrefixNS ++ nsInnerNS)
    <> ". Used for querying " <> textShow qk <> "."
  forHuman (TracerInfo silent noMetrics allTracers) = "The tracing system has silent the following tracer,"
    <> " as they will never have any output according to the current config: "
    <> Text.intercalate (Text.singleton ' ') silent <> ". The following tracers will not emit metrics "
    <> Text.intercalate (Text.singleton ' ') noMetrics <> ". Here is a complete list of all tracers: "
    <> Text.intercalate (Text.singleton ' ') allTracers <> "."
  forHuman (MetricsInfo mmap) = "Number of metrics delivered, " <> textShow mmap
  forHuman (TracerConsistencyWarnings errs) = "Consistency check found error:  " <> textShow errs
  forHuman (TracerInfoConfig tc) = "Effective Tracer config is:  " <> Text.decodeUtf8 (toStrict (encode tc))


  forMachine _dtl StartLimiting {} = mconcat
        [ "kind" .= String "StartLimiting"
        ]
  forMachine _dtl (StopLimiting _txt num) = mconcat
        [ "kind" .= String "StopLimiting"
        , "numSuppressed" .= Number (fromIntegral num)
        ]
  forMachine _dtl (RememberLimiting _txt num) = mconcat
        [ "kind" .= String "RememberLimiting"
        , "numSuppressed" .= Number (fromIntegral num)
        ]
  forMachine _dtl (UnknownNamespace nsun nsleg query) = mconcat
        [ "kind" .= String "UnknownNamespace"
        , "unknownNamespace" .= String (Text.intercalate (Text.singleton '.') nsun)
        , "legalNamespace" .= String (Text.intercalate (Text.singleton '.') nsleg)
        , "querying" .= String (textShow query)
        ]
  forMachine _dtl (TracerInfo silent noMetrics allTracers) = mconcat
        [ "kind" .= String "TracerMeta"
        , "silentTracers" .= String (Text.intercalate (Text.singleton ' ') silent)
        , "noMetrics" .= String (Text.intercalate (Text.singleton ' ') noMetrics)
        , "allTracers" .= String (Text.intercalate (Text.singleton ' ') allTracers)
        ]
  forMachine _dtl (MetricsInfo mmap) = mconcat
        [ "kind" .= String "MetricsInfo"
        , "metrics count" .= String (textShow mmap)
        ]
  forMachine _dtl (TracerConsistencyWarnings errs) = mconcat
        [ "kind" .= String "TracerConsistencyWarnings"
        , "errors" .= String (textShow errs)
        ]
  forMachine _dtl (TracerInfoConfig tc) = mconcat
        [ "conf" .= toJSON tc
        ]


  asMetrics StartLimiting {} = []
  asMetrics (StopLimiting txt num)  = [IntM
                                        ("SuppressedMessages " <> txt)
                                        (fromIntegral num)]
  asMetrics RememberLimiting {} = []
  asMetrics UnknownNamespace {} = []
  asMetrics TracerInfo {}       = []
  asMetrics MetricsInfo {}      = []
  asMetrics TracerConsistencyWarnings {}     = []
  asMetrics TracerInfoConfig {} = []

internalRestriction :: Text
internalRestriction = "\nThis internal message can't be filtered by the current configuration"

instance MetaTrace TraceDispatcherMessage where
    namespaceFor StartLimiting {}    = Namespace [] ["StartLimiting"]
    namespaceFor StopLimiting {}     = Namespace [] ["StopLimiting"]
    namespaceFor RememberLimiting {} = Namespace [] ["RememberLimiting"]
    namespaceFor UnknownNamespace {} = Namespace [] ["UnknownNamespace"]
    namespaceFor TracerInfo {}       = Namespace [] ["TracerInfo"]
    namespaceFor MetricsInfo {}      = Namespace [] ["MetricsInfo"]
    namespaceFor TracerConsistencyWarnings {}     = Namespace [] ["TracerConsistencyWarnings"]
    namespaceFor TracerInfoConfig {} = Namespace [] ["TracerConfigInfo"]



    severityFor (Namespace _ ["StartLimiting"]) _    = Just Notice
    severityFor (Namespace _ ["StopLimiting"]) _     = Just Notice
    severityFor (Namespace _ ["RememberLimiting"]) _ = Just Notice
    severityFor (Namespace _ ["UnknownNamespace"]) _ = Just Error
    severityFor (Namespace _ ["TracerInfo"]) _       = Just Notice
    severityFor (Namespace _ ["MetricsInfo"]) _      = Just Debug
    severityFor (Namespace _ ["TracerConsistencyWarnings"]) _  = Just Error
    severityFor (Namespace _ ["TracerConfigInfo"]) _       = Just Notice
    severityFor _ _                                  = Nothing

    documentFor (Namespace _ ["StartLimiting"])    = Just $
      "This message indicates the start of frequency limiting" <> internalRestriction
    documentFor (Namespace _ ["StopLimiting"])     = Just $ mconcat
      [ "This message indicates the stop of frequency limiting,"
      , " and gives the number of messages that has been suppressed"
      ] <>  internalRestriction
    documentFor (Namespace _ ["RememberLimiting"]) = Just $ mconcat
      [ "^ This message remembers of ongoing frequency limiting,"
      , " and gives the number of messages that has been suppressed"
      ] <>  internalRestriction
    documentFor (Namespace _ ["UnknownNamespace"]) = Just $ mconcat
      [ "A value was queried for a namespaces from a tracer,"
      , "which is unknown. This indicates a bug in the tracer implementation."
      ] <>  internalRestriction
    documentFor (Namespace _ ["TracerInfo"]) = Just $ mconcat
      [ "Writes out tracers with metrics and silent tracers."
      ] <>  internalRestriction
    documentFor (Namespace _ ["MetricsInfo"]) = Just $ mconcat
      [ "Writes out numbers for metrics delivered."
      ] <>  internalRestriction
    documentFor (Namespace _ ["TracerConsistencyWarnings"]) = Just $ mconcat
      [ "Tracer consistency check found errors."
      ] <>  internalRestriction
    documentFor (Namespace _ ["TracerConfigInfo"]) = Just $ mconcat
      [ "Trace the tracer configuration which is effectively used."
      ] <>  internalRestriction
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["StartLimiting"])    =
      [("SuppressedMessages...", "Number of suppressed messages of a certain kind")]
    metricsDocFor _ = []


    allNamespaces = [
        Namespace [] ["StartLimiting"]
      , Namespace [] ["StopLimiting"]
      , Namespace [] ["RememberLimiting"]
      , Namespace [] ["UnknownNamespace"]
      , Namespace [] ["TracerInfo"]
      , Namespace [] ["MetricsInfo"]
      , Namespace [] ["TracerConsistencyWarnings"]
      , Namespace [] ["TracerConfigInfo"]
      ]

-- `text-2.1.2` provides `Text.show` which can replace this when
-- the lower bound for `text` is high enough.
textShow :: Show a => a -> Text
textShow = Text.pack . show
