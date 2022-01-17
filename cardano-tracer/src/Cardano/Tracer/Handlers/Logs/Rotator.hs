{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.Rotator
  ( runLogsRotator
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.Extra (Lock)
import           Control.Monad (forM_, forever, unless, when)
import           Control.Monad.Extra (whenJust, whenM)
import           Data.List (nub, sort)
import           Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NE
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Word (Word16, Word32, Word64)
import           System.Directory (doesDirectoryExist, getFileSize, removeFile)
import           System.Directory.Extra (listDirectories, listFiles)
import           System.FilePath ((</>), takeDirectory)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils (createLogAndUpdateSymLink,
                   getTimeStampFromLog, isItLog)
import           Cardano.Tracer.Utils (showProblemIfAny)

-- | Runs rotation mechanism for the log files.
runLogsRotator
  :: TracerConfig
  -> Lock
  -> IO ()
runLogsRotator TracerConfig{rotation, logging, verbosity} currentLogLock =
  whenJust rotation $ \rotParams ->
    launchRotator loggingParamsForFiles rotParams verbosity currentLogLock
 where
  loggingParamsForFiles = nub . NE.filter filesOnly $ logging
  filesOnly LoggingParams{logMode} = logMode == FileMode

launchRotator
  :: [LoggingParams]
  -> RotationParams
  -> Maybe Verbosity
  -> Lock
  -> IO ()
launchRotator [] _ _ _ = return ()
launchRotator loggingParamsForFiles
              rotParams@RotationParams{rpFrequencySecs} verb currentLogLock = forever $ do
  showProblemIfAny verb $
    forM_ loggingParamsForFiles $ checkRootDir currentLogLock rotParams
  sleep $ fromIntegral rpFrequencySecs

-- | All the logs with 'TraceObject's received from particular node
--   will be stored in a separate subdirectory in the root directory.
--
--   Each subdirectory contains a symbolic link, we can use it to write
--   log items to the latest log file. When we create the new log file,
--   this symbolic link is switched to it.
checkRootDir
  :: Lock
  -> RotationParams
  -> LoggingParams
  -> IO ()
checkRootDir currentLogLock rotParams LoggingParams{logRoot, logFormat} =
  whenM (doesDirectoryExist logRoot) $
    listDirectories logRoot >>= \case
      [] ->
        -- There are no nodes' subdirs yet (or they were deleted),
        -- so no rotation can be performed for now.
        return ()
      logsSubDirs -> do
        let fullPathsToSubDirs = map (logRoot </>) logsSubDirs
        forConcurrently_ fullPathsToSubDirs $
          checkLogs currentLogLock rotParams logFormat

-- | We check the log files:
--   1. If there are too big log files.
--   2. If there are too old log files.
checkLogs
  :: Lock
  -> RotationParams
  -> LogFormat
  -> FilePath
  -> IO ()
checkLogs currentLogLock
          RotationParams{rpLogLimitBytes, rpMaxAgeHours, rpKeepFilesNum} format subDirForLogs = do
  logs <- map (subDirForLogs </>) . filter (isItLog format) <$> listFiles subDirForLogs
  unless (null logs) $ do
    -- Since logs' names contain timestamps, we can sort them: the maximum one is the latest log,
    -- and this is the current log (i.e. the log we're writing 'TraceObject's in).
    let fromOldestToNewest = sort logs
        -- Usage of partial function 'last' is safe here (we already checked the list isn't empty).
        currentLog = last fromOldestToNewest
        -- Only previous logs should be checked if they are outdated.
        allOtherLogs = dropEnd 1 fromOldestToNewest
    checkIfCurrentLogIsFull currentLogLock currentLog format rpLogLimitBytes
    checkIfThereAreOldLogs allOtherLogs rpMaxAgeHours rpKeepFilesNum

-- | If the current log file is full (it's size is too big), the new log will be created.
checkIfCurrentLogIsFull
  :: Lock
  -> FilePath
  -> LogFormat
  -> Word64
  -> IO ()
checkIfCurrentLogIsFull currentLogLock pathToCurrentLog format maxSizeInBytes =
  whenM logIsFull $
    createLogAndUpdateSymLink currentLogLock (takeDirectory pathToCurrentLog) format
 where
  logIsFull = do
    size <- getFileSize pathToCurrentLog
    return $ fromIntegral size >= maxSizeInBytes

-- | If there are too old log files - they will be removed.
--   Please note that some number of log files can be kept in any case.
checkIfThereAreOldLogs
  :: [FilePath]
  -> Word16
  -> Word32
  -> IO ()
checkIfThereAreOldLogs [] _ _ = return ()
checkIfThereAreOldLogs fromOldestToNewest maxAgeInHours keepFilesNum = do
  -- N ('keepFilesNum') newest files have to be kept in any case.
  let logsWeHaveToCheck = dropEnd (fromIntegral keepFilesNum) fromOldestToNewest
  unless (null logsWeHaveToCheck) $
    checkOldLogs logsWeHaveToCheck =<< getCurrentTime
 where
  checkOldLogs [] _ = return ()
  checkOldLogs (oldestLog:otherLogs) now' =
    case getTimeStampFromLog oldestLog of
      Just ts -> do
        let oldestLogAge = toSeconds $ now' `diffUTCTime` ts
        when (oldestLogAge >= maxAgeInSecs) $ do
          removeFile oldestLog
          checkOldLogs otherLogs now'
        -- If 'oldestLog' isn't outdated (yet), other logs aren't
        -- outdated too (because they are newer), so we shouldn't check them.
      Nothing ->
        -- Something is wrong with log's name, continue.
        checkOldLogs otherLogs now'

  maxAgeInSecs = fromIntegral maxAgeInHours * 3600
  toSeconds age = fromEnum age `div` 1000000000000
