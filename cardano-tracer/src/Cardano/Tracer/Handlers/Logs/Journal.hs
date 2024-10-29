{-# LANGUAGE CPP #-}
#ifdef SYSTEMD
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
#endif

module Cardano.Tracer.Handlers.Logs.Journal
  ( writeTraceObjectsToJournal
  ) where

#ifdef SYSTEMD
import qualified Cardano.Logging as L
#endif
import           Cardano.Logging (TraceObject (..))
import           Cardano.Tracer.Configuration (LogFormat (..))
import           Cardano.Tracer.Types (NodeName)

#ifdef SYSTEMD
import           Data.Char (isDigit)
import           Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
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

  mkName [] = "noname"
  mkName names = T.intercalate "." names

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
#else
-- It works only on Linux distributions with systemd support.
writeTraceObjectsToJournal :: LogFormat -> NodeName -> [TraceObject] -> IO ()
writeTraceObjectsToJournal _ _ _ = pure ()
#endif
