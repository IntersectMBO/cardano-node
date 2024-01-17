{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.Extra (Lock, withLock)
import Control.Monad (unless)
import Control.Monad.Extra (ifM)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
import System.Directory.Extra (listFiles)
import System.FilePath ((</>))
import System.IO (openFile, Handle, IOMode(AppendMode))

import Cardano.Logging (TraceObject (..))

import Data.Map (Map)
import Data.Map qualified as Map
import Cardano.Tracer.Configuration
import Cardano.Tracer.Handlers.Logs.Utils
import Cardano.Tracer.Types
import Cardano.Tracer.Utils (nl)

-- | Append the list of 'TraceObject's to the latest log via symbolic link.
--
-- It is technically possible that, during writing in the current log,
-- the rotator's thread will check if the current log is full and, if so,
-- the symbolic link will be switched to the new log file and writing can
-- be interrupted. To prevent it, we use 'Lock'.
writeTraceObjectsToFile
  :: MVar (Map FilePath Handle)
  -> NodeName
  -> Lock
  -> FilePath
  -> LogFormat
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile handleMapMVar nodeName currentLogLock rootDir format traceObjects = do
  -- Store this in config (double check)
  rootDirAbs <- makeAbsolute rootDir

  let converter :: TraceObject -> T.Text
      converter = case format of
                    ForHuman   -> traceTextForHuman
                    ForMachine -> traceTextForMachine

      itemsToWrite :: [T.Text]
      itemsToWrite = map converter traceObjects

      preparedLine :: BS.ByteString
      preparedLine = TE.encodeUtf8 $ T.append nl (T.intercalate nl itemsToWrite)

  unless (null itemsToWrite) do
    pathToCurrentLog <- getPathToCurrentlog nodeName rootDirAbs format

    handleMap <- readMVar handleMapMVar
    case Map.lookup pathToCurrentLog handleMap of
      Nothing -> do
        modifyMVar_ handleMapMVar \oldHandleMap -> do
          newHandle <- openFile pathToCurrentLog AppendMode
          let
            newHandleMap :: Map FilePath Handle
            newHandleMap = Map.insert pathToCurrentLog newHandle oldHandleMap
          pure newHandleMap
      Just handle -> do
        BS.hPutStrLn handle preparedLine

    -- withLock currentLogLock do
    --   BS.appendFile pathToCurrentLog preparedLine

-- Next Rotation / Filenames ... or Handles?

-- + absolute root dir in tracer config?
-- + handle set locked in MVar; replacing Lock
-- + initialize handles (open files) upfront or lazily?
-- + propagate handle set to all relevant functions (e.g. rotator)
-- + Goal: don’t open/close logfile(s) on each write

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
  :: NodeName
  -> FilePath
  -> LogFormat
  -> IO FilePath
getPathToCurrentlog nodeName rootDirAbs format =
  ifM (doesDirectoryExist subDirForLogs)
    getPathToCurrentLogIfExists
    prepareLogsStructure
 where
  subDirForLogs = rootDirAbs </> T.unpack nodeName

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

traceTextForHuman :: TraceObject -> T.Text
traceTextForHuman TraceObject{toHuman, toMachine} =
    fromMaybe toMachine toHuman

traceTextForMachine :: TraceObject -> T.Text
traceTextForMachine TraceObject{toMachine} = toMachine
