{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeNodeInfoToFile
  , writeTraceObjectsToFile
  ) where

import           Control.Monad (unless)
import           Control.Monad.Extra (ifM, unlessM)
import           Data.Aeson (ToJSON, (.=), object, toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))

import           Cardano.Logging

import           Trace.Forward.Protocol.Type (NodeInfo (..))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log
import           Cardano.Tracer.Types

-- | Log files structure can be represented like this (for example, with json format):
--
--   rootDir/
--     nodeFullId/
--       node-*-1.json
--       node-*-2.json
--       node-*-3.json
--       ...
--       node.json -> node-*-3.json
--
-- So 'TraceObject's and 'NodeInfo' received from the node will be stored in the logs
-- saved in the corresponding subdirectory inside of 'rootDir'.

writeNodeInfoToFile
  :: NodeId
  -> FilePath
  -> LogFormat
  -> NodeInfo
  -> IO ()
writeNodeInfoToFile nodeId rootDir format nodeInfo = do
  pathToCurrentLog <- prepareLogsStructure nodeId (niName nodeInfo) rootDir format
  LBS.appendFile pathToCurrentLog . encodeUtf8 $ preparedNodeInfo
 where
  preparedNodeInfo =
    case format of
      ForHuman   -> nodeInfoToText nodeInfo
      ForMachine -> nodeInfoToJSON nodeInfo

writeTraceObjectsToFile
  :: NodeId
  -> Text
  -> FilePath
  -> LogFormat
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile _ _ _ _ [] = return ()
writeTraceObjectsToFile nodeId nodeName rootDir format traceObjects = do
  pathToCurrentLog <- prepareLogsStructure nodeId nodeName rootDir format
  unless (null itemsToWrite) $
    LBS.appendFile pathToCurrentLog . encodeUtf8 . TL.concat $ itemsToWrite
 where
  itemsToWrite =
    case format of
      ForHuman   -> mapMaybe traceObjectToText traceObjects
      ForMachine -> mapMaybe traceObjectToJSON traceObjects

prepareLogsStructure
  :: NodeId
  -> Text
  -> FilePath
  -> LogFormat
  -> IO FilePath
prepareLogsStructure nodeId nodeName rootDir format = do
  -- Root directory (as a parent for subDirForLogs) will be created as well if needed.
  createDirectoryIfMissing True subDirForLogs
  ifM (doesFileExist pathToCurrentLog)
    (unlessM (doesSymLinkValid pathToCurrentLog) $ do
      removeFile pathToCurrentLog
      createLogAndSymLink subDirForLogs format)
    (createLogAndSymLink subDirForLogs format)
  return pathToCurrentLog
 where
  subDirForLogs = rootDir </> nodeFullId
  nodeFullId = T.unpack $ printNodeFullId nodeName nodeId
  -- This is a symlink to the current log file, please see rotation parameters.
  pathToCurrentLog = subDirForLogs </> symLinkName format

nl :: TL.Text
#if defined(mingw32_HOST_OS)
nl = "\r\n"
#else
nl = "\n"
#endif

nodeInfoToText :: NodeInfo -> TL.Text
nodeInfoToText NodeInfo{..} = TL.intercalate nl
  [ "Node info"
  , "  name: "              <> TL.fromStrict niName
  , "  protocol: "          <> TL.fromStrict niProtocol
  , "  version: "           <> TL.fromStrict niVersion
  , "  commit: "            <> TL.fromStrict niCommit
  , "  start time: "        <> TL.pack (show niStartTime)
  , "  system start time: " <> TL.pack (show niSystemStartTime)
  , nl
  ]

data NodeInfoForJSON = NodeInfoForJSON
  { jName            :: !T.Text
  , jProtocol        :: !T.Text
  , jVersion         :: !T.Text
  , jCommit          :: !T.Text
  , jStartTime       :: !UTCTime
  , jSystemStartTime :: !UTCTime
  }

instance ToJSON NodeInfoForJSON where
  toJSON NodeInfoForJSON{..} =
    object [ "nodeName"        .= jName
           , "protocol"        .= jProtocol
           , "version"         .= jVersion
           , "commit"          .= jCommit
           , "startTime"       .= formatTime defaultTimeLocale "%FT%T%2Q%Z" jStartTime
           , "systemStartTime" .= formatTime defaultTimeLocale "%FT%T%2Q%Z" jSystemStartTime
           ]

nodeInfoToJSON :: NodeInfo -> TL.Text
nodeInfoToJSON NodeInfo{..} = TL.append nl . encodeToLazyText $
  NodeInfoForJSON
    { jName            = niName
    , jProtocol        = niProtocol
    , jVersion         = niVersion
    , jCommit          = niCommit
    , jStartTime       = niStartTime
    , jSystemStartTime = niSystemStartTime
    }

traceObjectToText :: TraceObject -> Maybe TL.Text
traceObjectToText TraceObject{..} =
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

data TraceObjectForJSON = TraceObjectForJSON
  { jAt   :: !UTCTime
  , jNS   :: !T.Text
  , jData :: !T.Text
  , jHost :: !T.Text
  , jSev  :: !T.Text
  , jTId  :: !T.Text
  }

instance ToJSON TraceObjectForJSON where
  toJSON TraceObjectForJSON{..} =
    object [ "at"     .= formatTime defaultTimeLocale "%FT%T%2Q%Z" jAt
           , "ns"     .= jNS
           , "data"   .= jData
           , "host"   .= jHost
           , "sev"    .= jSev
           , "thread" .= jTId
           ]

traceObjectToJSON :: TraceObject -> Maybe TL.Text
traceObjectToJSON TraceObject{..} =
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
