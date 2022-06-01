{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Control.Concurrent.Extra (Lock, withLock)
import           Control.Monad (unless)
import           Control.Monad.Extra (ifM)
import           Data.Aeson (Value, (.=), decodeStrict', pairs)
import           Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
import           System.Directory.Extra (listFiles)
import           System.FilePath ((</>))

import           Cardano.Logging (Namespace, TraceObject (..))

import           Cardano.Tracer.Configuration (LogFormat (..))
import           Cardano.Tracer.Handlers.Logs.Utils (createEmptyLog, isItLog)
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
writeTraceObjectsToFile nodeId currentLogLock rootDir format traceObjects = do
  rootDirAbs <- makeAbsolute rootDir
  let converter = case format of
                    ForHuman   -> traceObjectToText
                    ForMachine -> traceObjectToJSON
  let itemsToWrite = mapMaybe converter traceObjects
  unless (null itemsToWrite) $ do
    pathToCurrentLog <- getPathToCurrentlog nodeId rootDirAbs format
    let preparedLine = TE.encodeUtf8 $ T.concat itemsToWrite
    withLock currentLogLock $
      BS.appendFile pathToCurrentLog preparedLine

-- | Returns the path to the current log. Prepares the structure for the log files if needed:
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
getPathToCurrentlog
  :: NodeId
  -> FilePath
  -> LogFormat
  -> IO FilePath
getPathToCurrentlog (NodeId anId) rootDirAbs format =
  ifM (doesDirectoryExist subDirForLogs)
    getPathToCurrentLogIfExists
    prepareLogsStructure
 where
  subDirForLogs = rootDirAbs </> T.unpack anId

  getPathToCurrentLogIfExists = do
    logsWeNeed <- filter (isItLog format) <$> listFiles subDirForLogs
    if null logsWeNeed
      then createEmptyLog subDirForLogs format
      -- We can sort the logs by timestamp, the biggest one is the latest one.
      else return $ subDirForLogs </> maximum logsWeNeed

  prepareLogsStructure = do
    -- The root directory (as a parent for subDirForLogs) will be created as well if needed.
    createDirectoryIfMissing True subDirForLogs
    createEmptyLog subDirForLogs format

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

traceObjectToJSON :: TraceObject -> Maybe T.Text
traceObjectToJSON TraceObject{toMachine, toTimestamp, toNamespace, toHostname, toSeverity, toThreadId} =
  case toMachine of
    Nothing  -> Nothing
    Just msg -> Just $ toAsJSON msg <> nl
 where
  toAsJSON msgForMachine =
      TE.decodeUtf8
    . LBS.toStrict
    . encodingToLazyByteString
    . pairs $ "at"     .= formatTime defaultTimeLocale "%F %H:%M:%S%4QZ" toTimestamp
           <> "ns"     .= mkName toNamespace
           <> "data"   .= case decodeStrict' $ TE.encodeUtf8 msgForMachine of
                            Just (v :: Value) -> v
                            Nothing -> ""
           <> "sev"    .= T.pack (show toSeverity)
           <> "thread" .= T.filter isDigit toThreadId
           <> "host"   .= T.pack toHostname
