{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.Tracer.Handlers.Logs.Journal
  ( writeTraceObjectsToJournal
  ) where

#ifdef LINUX
import           Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Systemd.Journal (Priority (..), message, mkJournalField, priority,
                   sendJournalFields, syslogIdentifier)

import           Cardano.Logging (TraceObject (..))
import qualified Cardano.Logging as L

import           Cardano.Tracer.Types (NodeId (..))
#else
import           Cardano.Logging (TraceObject)

import           Cardano.Tracer.Types (NodeId)
#endif

#ifdef LINUX
-- | Store 'TraceObject's in Linux systemd's journal service.
writeTraceObjectsToJournal :: NodeId -> [TraceObject] -> IO ()
writeTraceObjectsToJournal (NodeId anId) = mapM_ (sendJournalFields . mkJournalFields)
 where
  mkJournalFields trOb@TraceObject{toHuman, toMachine} =
    case (toHuman, toMachine) of
      (Just msgForHuman, Nothing)            -> mkJournalFields' trOb msgForHuman
      (Nothing,          Just msgForMachine) -> mkJournalFields' trOb msgForMachine
      (Just _,           Just msgForMachine) -> mkJournalFields' trOb msgForMachine
      (Nothing,          Nothing)            -> HM.empty

  mkJournalFields' TraceObject{toSeverity, toNamespace, toThreadId, toTimestamp} msg =
       syslogIdentifier anId
    <> message msg
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
-- It works on Linux only.
writeTraceObjectsToJournal :: NodeId -> [TraceObject] -> IO ()
writeTraceObjectsToJournal _ _ = return ()
#endif
