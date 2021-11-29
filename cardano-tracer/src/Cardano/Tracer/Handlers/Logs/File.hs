{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Control.Concurrent.Extra (Lock, withLock)
import           Control.Monad (unless)
import           Control.Monad.Extra (ifM, unlessM)
import           Data.Aeson (ToJSON, (.=), object, toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))

import           Cardano.Logging (Namespace, TraceObject (..))

import           Cardano.Tracer.Configuration (LogFormat (..))
import           Cardano.Tracer.Handlers.Logs.Utils (createLogAndSymLink,
                   doesSymLinkValid, symLinkName)
import           Cardano.Tracer.Types (NodeId (..))

-- | Append the list of 'TraceObject's to the latest log via symbolic link.
--
-- It is technically possible that, during writing in the current log,
-- the rotator's thread will check if the current log is full and, if so,
-- the symbolic link will be switched to the new log file and writing can
-- be interrupted. To prevent it, we use 'Lock'.
writeTraceObjectsToFile
  :: NodeId
  -> Lock
  -> FilePath
  -> LogFormat
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile nodeId currentLogLock rootDir ForHuman traceObjects = do
  let itemsToWrite = mapMaybe traceObjectToText traceObjects
  unless (null itemsToWrite) $ do
    pathToCurrentLog <- prepareLogsStructure nodeId rootDir ForHuman
    let preparedLine = TE.encodeUtf8 $ T.concat itemsToWrite
    withLock currentLogLock $
      BS.appendFile pathToCurrentLog preparedLine

writeTraceObjectsToFile nodeId currentLogLock rootDir ForMachine traceObjects = do
  let itemsToWrite = mapMaybe traceObjectToJSON traceObjects
  unless (null itemsToWrite) $ do
    pathToCurrentLog <- prepareLogsStructure nodeId rootDir ForMachine
    let preparedLine = TLE.encodeUtf8 $ TL.concat itemsToWrite
    withLock currentLogLock $
      LBS.appendFile pathToCurrentLog preparedLine

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
  ifM (doesFileExist pathToCurrentLog)
    (unlessM (doesSymLinkValid pathToCurrentLog) $ do
      removeFile pathToCurrentLog
      createLogAndSymLink subDirForLogs format)
    $ do
      -- The root directory (as a parent for subDirForLogs) will be created as well if needed.
      createDirectoryIfMissing True subDirForLogs
      createLogAndSymLink subDirForLogs format
  return pathToCurrentLog
 where
  subDirForLogs = rootDir </> T.unpack anId
  -- This is a symlink to the current log file, please see rotation parameters.
  pathToCurrentLog = subDirForLogs </> symLinkName format

nl :: T.Text
#ifdef UNIX
nl = "\n"
#else
nl = "\r\n"
#endif

traceObjectToText :: TraceObject -> Maybe T.Text
traceObjectToText TraceObject{toHuman, toHostname, toNamespace, toSeverity, toThreadId, toTimestamp} =
  case toHuman of
    Nothing -> Nothing
    Just msgForHuman -> Just $
      "[" <> host <> ":" <> name <> ":" <> sev <> ":" <> thId <> "] [" <> time <> "] "
      <> msgForHuman <> nl
 where
  host = T.pack toHostname
  name = mkName toNamespace
  sev  = T.pack $ show toSeverity
  thId = T.filter isDigit toThreadId
  time = T.pack $ formatTime defaultTimeLocale "%F %T%2Q %Z" toTimestamp

mkName :: Namespace -> T.Text
mkName []    = "noname"
mkName names = T.intercalate "." names

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
    Just msgForMachine -> Just . TL.append (TL.fromStrict nl) . encodeToLazyText $
      TraceObjectForJSON
        { jAt   = toTimestamp
        , jNS   = mkName toNamespace
        , jData = msgForMachine
        , jHost = T.pack toHostname
        , jSev  = T.pack $ show toSeverity
        , jTId  = T.filter isDigit toThreadId
        }
