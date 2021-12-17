{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.Tracer.Handlers.Logs.Journal
  ( writeNodeInfoToJournal
  , writeTraceObjectsToJournal
  ) where

#if defined(LINUX)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Systemd.Journal (Priority (..), message, mkJournalField,
                                  priority, sendJournalFields, syslogIdentifier)

import           Trace.Forward.Protocol.Type (NodeInfo (..))

import           Cardano.Logging (TraceObject (..))
import qualified Cardano.Logging as L

import           Cardano.Tracer.Types
#else
import           System.IO (hPutStrLn, stderr)

import           Cardano.Logging

import           Trace.Forward.Protocol.Type (NodeInfo)

import           Cardano.Tracer.Types
#endif

#if defined(LINUX)
writeNodeInfoToJournal
  :: NodeId
  -> NodeInfo
  -> IO ()
writeNodeInfoToJournal nodeId ni =
  sendJournalFields mkJournalFields
 where
  mkJournalFields =
       syslogIdentifier (niName ni <> T.pack (show nodeId))
    <> HM.fromList [ (nodeName,     encodeUtf8 $ niName ni)
                   , (protocol,     encodeUtf8 $ niProtocol ni)
                   , (version,      encodeUtf8 $ niVersion ni)
                   , (commit,       encodeUtf8 $ niCommit ni)
                   , (startTime,    encodeUtf8 . formatAsIso8601 $ niStartTime ni)
                   , (sysStartTime, encodeUtf8 . formatAsIso8601 $ niSystemStartTime ni)
                   ]

  nodeName     = mkJournalField "nodeName"
  protocol     = mkJournalField "protocol"
  version      = mkJournalField "version"
  commit       = mkJournalField "commit"
  startTime    = mkJournalField "startTime"
  sysStartTime = mkJournalField "systemStartTime"

writeTraceObjectsToJournal
  :: NodeId
  -> Text
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToJournal _ _ [] = return ()
writeTraceObjectsToJournal nodeId nodeName traceObjects =
  mapM_ (sendJournalFields . mkJournalFields) traceObjects
 where
  mkJournalFields trOb =
    let msgForHuman   = fromMaybe "" $ L.toHuman trOb
        msgForMachine = fromMaybe "" $ L.toMachine trOb
    in if | not . T.null $ msgForHuman   -> mkJournalFields' trOb msgForHuman
          | not . T.null $ msgForMachine -> mkJournalFields' trOb msgForMachine
          | otherwise                    -> HM.empty -- Both messages are empty!

  mkJournalFields' trOb msg =
       syslogIdentifier (nodeName <> T.pack (show nodeId))
    <> message msg
    <> priority (mkPriority $ L.toSeverity trOb)
    <> HM.fromList [ (namespace, encodeUtf8 (mkName $ L.toNamespace trOb))
                   , (thread,    encodeUtf8 $ L.toThreadId trOb)
                   , (time,      encodeUtf8 . formatAsIso8601 $ L.toTimestamp trOb)
                   ]

  mkName [] = "noname"
  mkName names = T.intercalate "." names

  namespace = mkJournalField "namespace"
  thread    = mkJournalField "thread"
  time      = mkJournalField "time"

formatAsIso8601 :: UTCTime -> Text
formatAsIso8601 = T.pack . formatTime defaultTimeLocale "%F %T%12QZ"

mkPriority :: L.SeverityS -> Priority
mkPriority L.Debug     = Debug
mkPriority L.Info      = Info
mkPriority L.Notice    = Notice
mkPriority L.Warning   = Warning
mkPriority L.Error     = Error
mkPriority L.Critical  = Critical
mkPriority L.Alert     = Alert
mkPriority L.Emergency = Emergency
#else
writeNodeInfoToJournal
  :: NodeId
  -> NodeInfo
  -> IO ()
writeNodeInfoToJournal _ _ =
  hPutStrLn stderr "Writing to systemd's journal is available on Linux only."

writeTraceObjectsToJournal
  :: NodeId
  -> NodeName
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToJournal _ _ _ =
  hPutStrLn stderr "Writing to systemd's journal is available on Linux only."
#endif
