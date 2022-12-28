module Cardano.Logging.TraceDispatcherMessage
  (
    UnknownNamespaceKind (..)
  , TraceDispatcherMessage (..)
  ) where

import Data.Aeson hiding (Error)
import Data.Text

import Cardano.Logging.Types


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
  deriving Show

instance LogFormatting TraceDispatcherMessage where
  forHuman (StartLimiting txt) = "Start of frequency limiting for " <> txt
  forHuman (StopLimiting txt num) = "Stop of frequency limiting for " <> txt <>
    ". Suppressed " <> pack (show num) <> " messages."
  forHuman (RememberLimiting txt num) = "Frequency limiting still active for " <> txt <>
    ". Suppressed so far " <> pack (show num) <> " messages."
  forHuman (UnknownNamespace nsUnknown nsLegal qk) = "Unknown namespace detected "
    <> intercalate (singleton '.') nsUnknown <> ". Used for querying " <> (pack . show) qk
    <> " a legal namespace would be " <> intercalate (singleton '.') nsLegal <> "."

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
        , "unknownNamespace" .= String (intercalate (singleton '.') nsun)
        , "legalNamespace" .= String (intercalate (singleton '.') nsleg)
        , "querying" .= String ((pack . show) query)
        ]

  asMetrics StartLimiting {} = []
  asMetrics (StopLimiting txt num)  = [IntM
                                        ("SuppressedMessages " <> txt)
                                        (fromIntegral num)]
  asMetrics RememberLimiting {} = []
  asMetrics UnknownNamespace {} = []

instance MetaTrace TraceDispatcherMessage where
    namespaceFor StartLimiting {}    = Namespace [] ["StartLimiting"]
    namespaceFor StopLimiting {}     = Namespace [] ["StopLimiting"]
    namespaceFor RememberLimiting {} = Namespace [] ["RememberLimiting"]
    namespaceFor UnknownNamespace {} = Namespace [] ["UnknownNamespace"]

    severityFor (Namespace _ ["StartLimiting"]) _    = Just Info
    severityFor (Namespace _ ["StopLimiting"]) _     = Just Info
    severityFor (Namespace _ ["RememberLimiting"]) _ = Just Info
    severityFor (Namespace _ ["UnknownNamespace"]) _ = Just Error

    documentFor (Namespace _ ["StartLimiting"])    = Just
      "This message indicates the start of frequency limiting"
    documentFor (Namespace _ ["StopLimiting"])     = Just
      "This message indicates the stop of frequency limiting,\
      \ and gives the number of messages that has been suppressed"
    documentFor (Namespace _ ["RememberLimiting"]) = Just
      "^ This message remembers of ongoing frequency limiting,\
       \ and gives the number of messages that has been suppressed"
    documentFor (Namespace _ ["UnknownNamespace"]) = Just
      "A value was queried for a namespaces from a tracer,\
      \which is unknown. This inicates a bug in the tracer implementation."

    allNamespaces = [
        Namespace [] ["StartLimiting"]
      , Namespace [] ["StopLimiting"]
      , Namespace [] ["RememberLimiting"]
      , Namespace [] ["UnknownNamespace"]
      ]