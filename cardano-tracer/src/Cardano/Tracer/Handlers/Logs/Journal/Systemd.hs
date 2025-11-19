{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tracer.Handlers.Logs.Journal.Systemd
  ( writeTraceObjectsToJournal
  ) where

import           Cardano.Logging (TraceObject (..))
import qualified Cardano.Logging as L
import           Cardano.Tracer.Configuration (LogFormat (..))
import           Cardano.Tracer.Handlers.Utils (normalizeNamespace)
import           Cardano.Tracer.Types (NodeName)

import           Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Format (defaultTimeLocale, formatTime)

import           Systemd.Journal (Priority (..), message, mkJournalField, priority,
                   sendJournalFields, syslogIdentifier)


-- | Store 'TraceObject's in Linux systemd's journal service.
writeTraceObjectsToJournal :: LogFormat -> NodeName -> [TraceObject] -> IO ()
writeTraceObjectsToJournal logFormat nodeName =
  mapM_ (sendJournalFields . mkJournalFields)
 where
  -- when no forHuman message is implemented for a trace, fallback to forMachine (same as for file handler)
  getMsg :: TraceObject -> T.Text
  getMsg = case logFormat of
    ForMachine -> toMachine
    ForHuman   -> \TraceObject{toHuman, toMachine} -> fromMaybe toMachine toHuman

  mkJournalFields trObj@TraceObject{toSeverity, toNamespace, toThreadId, toTimestamp} =
       syslogIdentifier nodeName
    <> message (getMsg trObj)
    <> priority (mkPriority toSeverity)
    <> HM.fromList
         [ (namespace, encodeUtf8 $ mkName toNamespace)
         , (thread,    encodeUtf8 $ T.filter isDigit toThreadId)
         , (time,      encodeUtf8 $ formatAsIso8601 toTimestamp)
         ]

  mkName (normalizeNamespace -> ns) = if T.null ns then "noname" else ns

  namespace = mkJournalField "namespace"
  thread    = mkJournalField "thread"
  time      = mkJournalField "time"

  formatAsIso8601 = T.pack . formatTime defaultTimeLocale "%F %T%12QZ"

  mkPriority L.Debug     = Debug
  mkPriority L.Info      = Info
  mkPriority L.Notice    = Notice
  mkPriority L.Warning   = Warning
  mkPriority L.Error     = Error
  mkPriority L.Critical  = Critical
  mkPriority L.Alert     = Alert
  mkPriority L.Emergency = Emergency
