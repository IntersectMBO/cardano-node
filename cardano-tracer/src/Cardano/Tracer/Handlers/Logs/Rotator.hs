{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.Rotator
  ( runLogsRotator
  ) where

import           Control.Exception (SomeException, try)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad (forM_, forever, unless, when)
import           Control.Monad.Extra (whenJust, whenM)
import "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)
import           Data.List (nub, sort)
import           Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NE
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Word (Word64)
import           System.Directory
import           System.Directory.Extra (listDirectories, listFiles)
import           System.FilePath ((</>), takeDirectory)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log

runLogsRotator :: TracerConfig -> IO ()
runLogsRotator TracerConfig{rotation, logging} =
  whenJust rotation $
    launchRotator loggingParamsForFiles
 where
  loggingParamsForFiles = nub . NE.filter filesOnly $ logging
  filesOnly LoggingParams{logMode} = logMode == FileMode

-- | All the logs with 'TraceObject's received from particular node
-- will be stored in a separate directory, so they can be checked
-- concurrently.
launchRotator
  :: [LoggingParams]
  -> RotationParams
  -> IO ()
launchRotator [] _ = return ()
launchRotator loggingParamsForFiles rotParams = forever $ do
  try (forM_ loggingParamsForFiles $ checkRootDir rotParams) >>= \case
    Left (e :: SomeException) ->
      logTrace $ "cardano-tracer, problem with logs rotation: " <> show e
    Right _ -> return ()
  sleep 15.0
 where
  logTrace = traceWith $ showTracing stdoutTracer

checkRootDir
  :: RotationParams
  -> LoggingParams
  -> IO ()
checkRootDir rotParams LoggingParams{logRoot, logFormat} =
  whenM (doesDirectoryExist logRoot) $
    -- All the logs received from particular node will be stored in corresponding subdir.
    listDirectories logRoot >>= \case
      [] ->
        -- There are no nodes' subdirs yet (or they were deleted),
        -- so no rotation can be performed for now.
        return ()
      nodesSubDirs -> do
        let fullPathsToSubDirs = map (logRoot </>) nodesSubDirs  
        forConcurrently_ fullPathsToSubDirs $ checkLogs rotParams logFormat

checkLogs
  :: RotationParams
  -> LogFormat
  -> FilePath
  -> IO ()
checkLogs RotationParams{rpLogLimitBytes, rpMaxAgeHours, rpKeepFilesNum} format subDirForLogs = do
  logs <- map (subDirForLogs </>) . filter (isItLog format) <$> listFiles subDirForLogs
  checkIfCurrentLogIsFull logs format rpLogLimitBytes
  checkIfThereAreOldLogs logs rpMaxAgeHours rpKeepFilesNum

checkIfCurrentLogIsFull
  :: [FilePath]
  -> LogFormat
  -> Word64
  -> IO ()
checkIfCurrentLogIsFull [] _ _ = return ()
checkIfCurrentLogIsFull logs format maxSizeInBytes =
  whenM (logIsFull pathToCurrentLog) $
    createLogAndUpdateSymLink (takeDirectory pathToCurrentLog) format
 where
  logIsFull logName = do
    size <- getFileSize logName
    return $ fromIntegral size >= maxSizeInBytes
  -- Since logs' names contain timestamps, the maximum one is the latest log,
  -- or current log (i.e. the log we write 'TraceObject's in).
  pathToCurrentLog = maximum logs

checkIfThereAreOldLogs
  :: [FilePath]
  -> Word
  -> Word
  -> IO ()
checkIfThereAreOldLogs [] _ _ = return ()
checkIfThereAreOldLogs logs maxAgeInHours keepFilesNum = do
  -- Logs' names contain timestamp, so we can sort them.  
  let fromOldestToNewest = sort logs
      -- N ('keepFilesNum') newest files have to be kept in any case.
      logsWeHaveToCheck = dropEnd (fromIntegral keepFilesNum) fromOldestToNewest
  unless (null logsWeHaveToCheck) $ do
    now <- getCurrentTime
    checkOldLogs now logsWeHaveToCheck
 where
  checkOldLogs _ [] = return ()
  checkOldLogs now' (oldestLog:otherLogs) =
    case getTimeStampFromLog oldestLog of
      Just ts -> do
        let oldestLogAge = toSeconds $ now' `diffUTCTime` ts
        when (oldestLogAge >= maxAgeInSecs) $ do
          removeFile oldestLog
          checkOldLogs now' otherLogs
        -- If 'oldestLog' isn't outdated (yet), other logs aren't
        -- outdated too (because they are newer), so we shouldn't check them.
      Nothing ->
        -- Something is wrong with log's name, continue.
        checkOldLogs now' otherLogs

  maxAgeInSecs = fromIntegral maxAgeInHours * 3600
  toSeconds age = fromEnum age `div` 1000000000000
