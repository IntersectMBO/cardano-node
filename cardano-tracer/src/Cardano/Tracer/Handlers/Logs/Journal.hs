{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.Tracer.Handlers.Logs.Journal
  ( writeLogObjectsToJournal
  ) where

#if defined(LINUX)
import           Data.Aeson (ToJSON, Value (String), toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Systemd.Journal (Priority (..), message, mkJournalField,
                                  priority, sendJournalFields, syslogIdentifier)

import           Cardano.BM.Data.LogItem
import qualified Cardano.BM.Data.Severity as S

import           Cardano.Tracer.Types
#else
import           System.IO (hPutStrLn, stderr)

import           Cardano.BM.Data.LogItem

import           Cardano.Tracer.Types
#endif

#if defined(LINUX)
writeLogObjectsToJournal
  :: ToJSON a
  => NodeId
  -> NodeName
  -> [LogObject a]
  -> IO ()
writeLogObjectsToJournal _ _ [] = return ()
writeLogObjectsToJournal nodeId nodeName logObjects =
  mapM_ (sendJournalFields . mkJournalFieldsFromLogObject) logObjects
 where
  mkJournalFieldsFromLogObject (LogObject loname lometa loitem) =
       syslogIdentifier (nodeName <> T.pack (show nodeId))
    <> message (mkMessage loitem)
    <> priority (mkPriority $ severity lometa)
    <> HM.fromList [ (namespace, encodeUtf8 loname)
                   , (thread,    encodeUtf8 . T.pack . show . tid $ lometa)
                   , (time,      encodeUtf8 . formatAsIso8601 . tstamp $ lometa)
                   ]

  mkMessage item =
    case item of
      LogMessage logItem ->
        case toJSON logItem of
          String m -> m
          m        -> TL.toStrict $ encodeToLazyText m
      LogError m -> m
      LogStructured o -> TL.toStrict $ encodeToLazyText o
      LogStructuredText _o m -> m
      LogValue name' value ->
        if T.null name'
          then T.pack (show value)
          else name' <> " = " <> T.pack (show value)
      _ -> TL.toStrict $ encodeToLazyText item

  namespace = mkJournalField "namespace"
  thread    = mkJournalField "thread"
  time      = mkJournalField "time"

  formatAsIso8601 = T.pack . formatTime defaultTimeLocale "%F %T%12QZ"

mkPriority :: S.Severity -> Priority
mkPriority S.Debug     = Debug
mkPriority S.Info      = Info
mkPriority S.Notice    = Notice
mkPriority S.Warning   = Warning
mkPriority S.Error     = Error
mkPriority S.Critical  = Critical
mkPriority S.Alert     = Alert
mkPriority S.Emergency = Emergency
#else
writeLogObjectsToJournal
  :: NodeId
  -> NodeName
  -> [LogObject a]
  -> IO ()
writeLogObjectsToJournal _ _ _ =
  hPutStrLn stderr "Writing to systemd's journal is available on Linux only."
#endif
