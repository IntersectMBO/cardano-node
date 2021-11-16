{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Control.Monad (unless)
import           Control.Monad.Extra (ifM, unlessM)
import           Data.Aeson (ToJSON, (.=), object, toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))
import           System.Info.Extra (isWindows)

import           Cardano.Logging (Namespace, TraceObject (..))

import           Cardano.Tracer.Configuration (LogFormat (..))
import           Cardano.Tracer.Handlers.Logs.Utils (createLogAndSymLink,
                   doesSymLinkValid, symLinkName)
import           Cardano.Tracer.Types (NodeId (..))

-- | Append the list of 'TraceObject's to the latest log via symbolic link.
writeTraceObjectsToFile
  :: NodeId
  -> FilePath
  -> LogFormat
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile _ _ _ [] = return ()
writeTraceObjectsToFile nodeId rootDir format traceObjects =
  unless (null itemsToWrite) $ do
    pathToCurrentLog <- prepareLogsStructure nodeId rootDir format
    LBS.appendFile pathToCurrentLog . encodeUtf8 . TL.concat $ itemsToWrite
 where
  itemsToWrite =
    case format of
      ForHuman   -> mapMaybe traceObjectToText traceObjects
      ForMachine -> mapMaybe traceObjectToJSON traceObjects

-- | Prepare the structure for the log files:
--
--   /rootDir
--     /subDirForNode1
--       logs from node 1
--     /subDirForNode2
--       logs from node 2
--     ...
--     /subDirForNodeN
--       logs from node N
--
prepareLogsStructure
  :: NodeId
  -> FilePath
  -> LogFormat
  -> IO FilePath
prepareLogsStructure (NodeId anId) rootDir format = do
  -- The root directory (as a parent for subDirForLogs) will be created as well if needed.
  createDirectoryIfMissing True subDirForLogs
  ifM (doesFileExist pathToCurrentLog)
    (unlessM (doesSymLinkValid pathToCurrentLog) $ do
      removeFile pathToCurrentLog
      createLogAndSymLink subDirForLogs format)
    (createLogAndSymLink subDirForLogs format)
  return pathToCurrentLog
 where
  subDirForLogs = rootDir </> T.unpack anId
  -- This is a symlink to the current log file, please see rotation parameters.
  pathToCurrentLog = subDirForLogs </> symLinkName format

nl :: TL.Text
nl = if isWindows then "\r\n" else "\n"

traceObjectToText :: TraceObject -> Maybe TL.Text
traceObjectToText TraceObject{toHuman, toHostname, toNamespace, toSeverity, toThreadId, toTimestamp} =
  case toHuman of
    Nothing -> Nothing
    Just msgForHuman -> Just $
      "[" <> host <> ":" <> name <> ":" <> sev <> ":" <> thId <> "] [" <> time <> "] "
      <> TL.fromStrict msgForHuman
      <> nl
 where
  host = TL.pack toHostname
  name = mkName toNamespace
  sev  = TL.pack $ show toSeverity
  thId = TL.fromStrict $ T.filter isDigit toThreadId
  time = TL.pack $ formatTime defaultTimeLocale "%F %T%2Q %Z" toTimestamp

mkName :: Namespace -> TL.Text
mkName []    = "noname"
mkName names = TL.fromStrict $ T.intercalate "." names

-- | The type for converting 'TraceObject' to JSON.
data TraceObjectForJSON = TraceObjectForJSON
  { jAt   :: !UTCTime
  , jNS   :: !T.Text
  , jData :: !T.Text
  , jHost :: !T.Text
  , jSev  :: !T.Text
  , jTId  :: !T.Text
  }

instance ToJSON TraceObjectForJSON where
  toJSON toJ = object
    [ "at"     .= formatTime defaultTimeLocale "%FT%T%2Q%Z" (jAt toJ)
    , "ns"     .= jNS toJ
    , "data"   .= jData toJ
    , "host"   .= jHost toJ
    , "sev"    .= jSev toJ
    , "thread" .= jTId toJ
    ]

traceObjectToJSON :: TraceObject -> Maybe TL.Text
traceObjectToJSON TraceObject{toMachine, toTimestamp, toNamespace, toHostname, toSeverity, toThreadId} =
  case toMachine of
    Nothing -> Nothing
    Just msgForMachine -> Just . TL.append nl . encodeToLazyText $
      TraceObjectForJSON
        { jAt   = toTimestamp
        , jNS   = TL.toStrict $ mkName toNamespace
        , jData = msgForMachine
        , jHost = T.pack toHostname
        , jSev  = T.pack $ show toSeverity
        , jTId  = T.filter isDigit toThreadId
        }
