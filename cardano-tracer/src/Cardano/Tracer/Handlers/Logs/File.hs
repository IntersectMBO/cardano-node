{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Cardano.Logging (TraceObject (..))
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils (nl, readRegistry)

import           Control.Concurrent.Extra (Lock)
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
writeTraceObjectsToFile registry loggingParams@LoggingParams{logRoot, logFormat} nodeName currentLogLock traceObjects = do
  let converter :: TraceObject -> T.Text
      converter = case logFormat of
        ForHuman   -> traceTextForHuman
        ForMachine -> traceTextForMachine

      itemsToWrite :: [T.Text]
      itemsToWrite = map converter traceObjects

      preparedLines :: BS8.ByteString
      preparedLines = TE.encodeUtf8 (nl `T.append` T.intercalate nl itemsToWrite)

  unless (null itemsToWrite) do
    readRegistry registry >>= \handleMap -> do
      case Map.lookup (nodeName, loggingParams) handleMap of
        Nothing -> do
          rootDirAbs <- makeAbsolute logRoot

          let subDirForLogs :: FilePath
              subDirForLogs = rootDirAbs </> T.unpack nodeName

          createEmptyLogRotation currentLogLock nodeName loggingParams registry subDirForLogs logFormat
          handles <- readRegistry registry
          let handle = fst (fromJust (Map.lookup (nodeName, loggingParams) handles))
          BS8.hPutStr handle preparedLines
          hFlush handle
        Just (handle, _filePath) -> do
          BS8.hPutStr handle preparedLines
          hFlush handle

traceTextForHuman :: TraceObject -> T.Text
traceTextForHuman TraceObject{toHuman, toMachine} =
  fromMaybe toMachine toHuman

traceTextForMachine :: TraceObject -> T.Text
traceTextForMachine TraceObject{toMachine} = toMachine
