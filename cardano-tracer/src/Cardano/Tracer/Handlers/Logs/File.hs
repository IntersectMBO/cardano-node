{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Cardano.Logging (TraceObject (..))
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils (nl)

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.MVar (readMVar)
import           Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.Directory (makeAbsolute)
import           System.FilePath ((</>))
import           System.IO (hFlush)

-- | Append the list of 'TraceObject's to the latest log via symbolic link.
--
-- It is technically possible that, during writing in the current log,
-- the rotator's thread will check if the current log is full and, if so,
-- the symbolic link will be switched to the new log file and writing can
-- be interrupted. To prevent it, we use 'Lock'.
writeTraceObjectsToFile
  :: HandleRegistry
  -> LoggingParams
  -> NodeName
  -> Lock
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile registry@(Registry registryMVar) loggingParams@LoggingParams{logRoot, logFormat} nodeName currentLogLock traceObjects = do
  let converter :: TraceObject -> T.Text
      converter = case logFormat of
        ForHuman   -> traceTextForHuman
        ForMachine -> traceTextForMachine

      itemsToWrite :: [T.Text]
      itemsToWrite = map converter traceObjects

      preparedLines :: BS8.ByteString
      preparedLines = TE.encodeUtf8 (nl `T.append` T.intercalate nl itemsToWrite)

  unless (null itemsToWrite) do
    readMVar registryMVar >>= \handleMap -> do
      case Map.lookup (nodeName, loggingParams) handleMap of
        Nothing -> do
          rootDirAbs <- makeAbsolute logRoot

          let subDirForLogs :: FilePath
              subDirForLogs = rootDirAbs </> T.unpack nodeName

          createEmptyLogRotation currentLogLock nodeName loggingParams registry subDirForLogs logFormat
          handles <- readMVar registryMVar
          let handle = fst (fromJust (Map.lookup (nodeName, loggingParams) handles))
          BS8.hPutStr handle preparedLines
          hFlush handle
        Just (handle, _filePath) -> do
          BS8.hPutStr handle preparedLines
          hFlush handle

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
-- getPathToCurrentlog
--   :: Lock
--   -> NodeName
--   -> HandleRegistry
--   -> FilePath
--   -> LogFormat
--   -> IO FilePath
-- getPathToCurrentlog currentLogLock nodeName reg@(Registry registry) rootDirAbs format = do
--   readMVar registry >>= \handles ->
--     case Map.lookup nodeName handles of
--       Nothing -> do
--         createEmptyLogRotation currentLogLock nodeName reg subDirForLogs format
--         handles <- readMVar registry
--         pure $ snd (fromJust (Map.lookup nodeName handles))
--       Just (_handle, filePath) ->
--         pure filePath
--  where
--   subDirForLogs = rootDirAbs </> T.unpack nodeName

traceTextForHuman :: TraceObject -> T.Text
traceTextForHuman TraceObject{toHuman, toMachine} =
    fromMaybe toMachine toHuman

traceTextForMachine :: TraceObject -> T.Text
traceTextForMachine TraceObject{toMachine} = toMachine
