{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.Rotator
  ( runLogsRotator
  ) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.MVar
import Control.Concurrent.Extra (Lock)
import Control.Monad (forM_, forever, unless, when)
import Control.Monad.Extra (whenJust, whenM)
import Data.List (nub, sort)
import Data.List.Extra (dropEnd)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Text qualified as Text
import Data.Word (Word32, Word64)
import System.Directory (doesDirectoryExist, makeAbsolute, removeFile)
import System.Directory.Extra (listDirectories, listFiles)
import System.FilePath ((</>))
import System.IO (Handle, hTell)
import System.Time.Extra (sleep)

import Cardano.Tracer.Configuration
import Cardano.Tracer.Environment
import Cardano.Tracer.Handlers.Logs.Utils (createOrUpdateEmptyLog, getTimeStampFromLog, isItLog)
import Cardano.Tracer.MetaTrace
import Cardano.Tracer.Utils (showProblemIfAny)
import Cardano.Tracer.Types

-- | Runs rotation mechanism for the log files.
runLogsRotator :: TracerEnv -> IO ()
runLogsRotator TracerEnv
  { teConfig = TracerConfig{rotation, verbosity, logging}
  , teCurrentLogLock
  , teTracer
  , teRegistry
  } =
  whenJust rotation \rotParams -> do
    traceWith teTracer TracerStartedLogRotator
    launchRotator loggingParamsForFiles rotParams verbosity teRegistry teCurrentLogLock
 where
  loggingParamsForFiles :: [LoggingParams]
  loggingParamsForFiles = nub (NE.filter filesOnly logging)

  filesOnly :: LoggingParams -> Bool
  filesOnly LoggingParams{logMode} = logMode == FileMode

launchRotator
  :: [LoggingParams]
  -> RotationParams
  -> Maybe Verbosity
  -> HandleRegistry
  -> Lock
  -> IO ()
launchRotator [] _ _ _ _ = return ()
launchRotator loggingParamsForFiles
              rotParams@RotationParams{rpFrequencySecs} verb registry currentLogLock = forever do
  showProblemIfAny verb do
    forM_ loggingParamsForFiles do
      checkRootDir currentLogLock registry rotParams
  sleep $ fromIntegral rpFrequencySecs

-- | All the logs with 'TraceObject's received from particular node
--   will be stored in a separate subdirectory in the root directory.
--
--   Each subdirectory contains a symbolic link, we can use it to write
--   log items to the latest log file. When we create the new log file,
--   this symbolic link is switched to it.
checkRootDir
  :: Lock
  -> HandleRegistry
  -> RotationParams
  -> LoggingParams
  -> IO ()
checkRootDir currentLogLock registry@(Registry mvarRegistry) rotParams loggingParams@LoggingParams{logRoot, logFormat} = do
  logRootAbs <- makeAbsolute logRoot
  whenM (doesDirectoryExist logRootAbs) do
    logsSubDirs <- listDirectories logRootAbs

    handles <- readMVar mvarRegistry

    -- There are no nodes' subdirs yet (or they were deleted), or they
    -- don't contain files with open handles so no rotation can be
    -- performed for now.
    forConcurrently_ logsSubDirs \logSubDir -> do

      let nodeName :: NodeName
          nodeName = Text.pack logSubDir

      case Map.lookup (nodeName, loggingParams) handles of
        Nothing ->
          pure ()
        Just (handle, filePath) -> do
          let
            nodeName' :: NodeName
            nodeName' = Text.pack filePath
          checkLogs currentLogLock handle nodeName' loggingParams registry rotParams logFormat (logRootAbs </> logSubDir)

-- | We check the log files:
--   1. If there are too big log files.
--   2. If there are too old log files.
checkLogs
  :: Lock
  -> Handle
  -> NodeName
  -> LoggingParams
  -> HandleRegistry
  -> RotationParams
  -> LogFormat
  -> FilePath
  -> IO ()
checkLogs currentLogLock handle nodeName loggingParams registry
          RotationParams{rpLogLimitBytes, rpMaxAgeMinutes, rpKeepFilesNum} format subDirForLogs = do

  logs <- map (subDirForLogs </>) . filter (isItLog format) <$> listFiles subDirForLogs
  unless (null logs) do
    -- Since logs' names contain timestamps, we can sort them: the maximum one is the latest log,
    -- and this is the current log (i.e. the log we're writing 'TraceObject's in).
    let fromOldestToNewest = sort logs
        -- Usage of partial function 'last' is safe here (we already checked the list isn't empty).
        -- Only previous logs should be checked if they are outdated.
        allOtherLogs = dropEnd 1 fromOldestToNewest
    checkIfCurrentLogIsFull currentLogLock handle nodeName loggingParams registry format rpLogLimitBytes subDirForLogs
    checkIfThereAreOldLogs allOtherLogs rpMaxAgeMinutes rpKeepFilesNum

-- | If the current log file is full (it's size is too big), the new log will be created.
checkIfCurrentLogIsFull
  :: Lock
  -> Handle
  -> NodeName
  -> LoggingParams
  -> HandleRegistry
  -> LogFormat
  -> Word64
  -> FilePath
  -> IO ()
checkIfCurrentLogIsFull currentLogLock handle nodeName loggingParams registry format maxSizeInBytes subDirForLogs =
  whenM logIsFull do
    createOrUpdateEmptyLog currentLogLock nodeName loggingParams registry subDirForLogs format

 where
  logIsFull :: IO Bool
  logIsFull = do
    size <- hTell handle
    return $! fromIntegral size >= maxSizeInBytes

-- | If there are too old log files - they will be removed.
--   Please note that some number of log files can be kept in any case.
checkIfThereAreOldLogs
  :: [FilePath]
  -> Word64
  -> Word32
  -> IO ()
checkIfThereAreOldLogs [] _ _ = return ()
checkIfThereAreOldLogs fromOldestToNewest maxAgeInMinutes keepFilesNum = do
  -- N ('keepFilesNum') newest files have to be kept in any case.
  let logsWeHaveToCheck = dropEnd (fromIntegral keepFilesNum) fromOldestToNewest
  unless (null logsWeHaveToCheck) $
    checkOldLogs logsWeHaveToCheck =<< getCurrentTime
 where
  checkOldLogs [] _ = return ()
  checkOldLogs (oldestLog:otherLogs) now' =
    case getTimeStampFromLog oldestLog of
      Just ts -> do
        let !oldestLogAge = toSeconds $ now' `diffUTCTime` ts
        when (oldestLogAge >= maxAgeInSecs) $ do
          removeFile oldestLog
          checkOldLogs otherLogs now'
        -- If 'oldestLog' isn't outdated (yet), other logs aren't
        -- outdated too (because they are newer), so we shouldn't check them.
      Nothing ->
        -- Something is wrong with log's name, continue.
        checkOldLogs otherLogs now'

  maxAgeInSecs = fromIntegral maxAgeInMinutes * 60
  toSeconds age = fromEnum age `div` 1000000000000
