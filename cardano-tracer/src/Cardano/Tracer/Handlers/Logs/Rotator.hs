{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.Rotator
  ( runLogsRotator
  ) where

import           Control.Exception.Safe (Exception (..), catchIO)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad (filterM, forM_, forever, when)
import           Data.List (find, nub, sort)
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Word (Word64)
import           System.Directory (doesDirectoryExist, getFileSize,
                                   getSymbolicLinkTarget, createFileLink,
                                   listDirectory, removeFile)
import           System.FilePath ((</>), takeDirectory, takeFileName)
import           System.IO (hPutStrLn, stderr)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log

runLogsRotator :: TracerConfig -> IO ()
runLogsRotator TracerConfig{..} =
  case rotation of
    Nothing -> return () -- No rotation parameters are defined.
    Just rotParams -> launchRotator rotParams rootDirsWithFormats
 where
  rootDirsWithFormats = nub . map getRootAndFormat . filter fileParamsOnly $ logging
  fileParamsOnly   LoggingParams{..} = logMode == FileMode
  getRootAndFormat LoggingParams{..} = (logRoot, logFormat)

-- | All the logs with 'LogObject's received from particular node
-- will be stored in a separate directory, so they can be checked
-- concurrently.
launchRotator
  :: RotationParams
  -> [(FilePath, LogFormat)]
  -> IO ()
launchRotator _ [] = return ()
launchRotator rotParams rootDirsWithFormats = forever $ do
  forM_ rootDirsWithFormats $ checkRootDir rotParams
  threadDelay 10000000

checkRootDir
  :: RotationParams
  -> (FilePath, LogFormat)
  -> IO ()
checkRootDir rotParams (rootDir, format) =
  catchIO checkLogsInRootDir' $ \e ->
    hPutStrLn stderr $ "Cannot write log items to file: " <> displayException e
 where
  checkLogsInRootDir' =
    listDirectory rootDir >>= \case
      [] ->
        -- There are no nodes' subdirs yet (or they were deleted),
        -- so no rotation can be performed.
        return ()
      maybeSubDirs -> do
        let fullPathsToSubDirs = map (rootDir </>) maybeSubDirs
        -- All the logs received from particular node will be stored in a subdir.
        filterM doesDirectoryExist fullPathsToSubDirs >>= \case
          [] ->
            -- There are no subdirs with logs here, so no rotation can be performed.
            return ()
          subDirs ->
            forConcurrently_ subDirs $ checkLogsFromNode rotParams format

checkLogsFromNode
  :: RotationParams
  -> LogFormat
  -> FilePath
  -> IO ()
checkLogsFromNode rotParams format subDirForLogs =
  listDirectory subDirForLogs >>= \case
    [] ->
      -- There are no logs in this subdir (probably they were deleted),
      -- so no rotation can be performed.
      return ()
    [oneFile] ->
      -- At least two files must be there: one log and symlink.
      -- So if there is only one file, it's a weird situation,
      -- (probably invalid symlink only), we have to try to fix it.
      fixLog (subDirForLogs </> oneFile) format
    logs -> do
      let fullPathsToLogs = map (subDirForLogs </>) logs
      checkLogs rotParams format fullPathsToLogs

fixLog
  :: FilePath
  -> LogFormat
  -> IO ()
fixLog oneFile format =
  isItSymLink oneFile format >>= \case
    True -> do
      -- It is a single symlink, but corresponding log was deleted.
      removeFile oneFile
      createLogAndSymLink (takeDirectory oneFile) format
    False ->
      when (isItLog format oneFile) $
        -- It is a single log, but its symlink was deleted.
        createFileLink oneFile $ symLinkName format

checkLogs
  :: RotationParams
  -> LogFormat
  -> [FilePath]
  -> IO ()
checkLogs RotationParams{..} format logs = do
  checkIfCurrentLogIsFull logs format rpLogLimitBytes
  checkIfThereAreOldLogs logs format rpMaxAgeHours rpKeepFilesNum

checkIfCurrentLogIsFull
  :: [FilePath]
  -> LogFormat
  -> Word64
  -> IO ()
checkIfCurrentLogIsFull [] _ _ = return ()
checkIfCurrentLogIsFull logs format maxSizeInBytes =
  case find (\logPath -> takeFileName logPath == symLinkName format) logs of
    Just symLink ->
      doesSymLinkValid symLink >>= \case
        True -> do
          itIsFull <- isItFull =<< getPathToLatestLog symLink
          when itIsFull $ createLogAndUpdateSymLink subDirForLogs format
        False -> do
          -- Remove invalid symlink.
          removeFile symLink
    Nothing ->
      -- There is no symlink we need, so skip check for now:
      -- this symlink will be created when the new 'LogObject's will be received.
      return ()
 where
  subDirForLogs = takeDirectory $ head logs -- All these logs are in the same subdir.

  getPathToLatestLog symlink = do
    logName <- getSymbolicLinkTarget symlink
    return $ subDirForLogs </> logName

  isItFull logName = do
    sz <- getFileSize logName
    return $ fromIntegral sz >= maxSizeInBytes

checkIfThereAreOldLogs
  :: [FilePath]
  -> LogFormat
  -> Word
  -> Word
  -> IO ()
checkIfThereAreOldLogs [] _ _ _ = return ()
checkIfThereAreOldLogs logs format maxAgeInHours keepFilesNum = do
  now <- getCurrentTime
  let logsWeNeed = filter (isItLog format) logs
      -- Sort by name with timestamp, so the latest logs will always be in the end.
      oldLogs = sort . filter (oldLog now) $ logsWeNeed
      remainingLogsNum = length logsWeNeed - length oldLogs
  if remainingLogsNum >= fromIntegral keepFilesNum
    then mapM_ removeFile oldLogs
    else removeSomeOldLogs oldLogs remainingLogsNum
 where
  oldLog now' logName =
    case getTimeStampFromLog logName of
      Just timeStamp ->
        let logAge = now' `diffUTCTime` timeStamp
        in toSeconds logAge >= maxAgeInSecs
      Nothing -> False

  maxAgeInSecs = fromIntegral maxAgeInHours * 3600
  toSeconds age = fromEnum age `div` 1000000000000

  removeSomeOldLogs [] _ = return ()
  removeSomeOldLogs oldLogs remainingLogsNum = do
    -- Too many logs are old, so make sure we keep enough latest logs.
    let oldLogsNumToKeep = fromIntegral keepFilesNum - remainingLogsNum
    -- Reverse logs to place the latest ones in the beginning and drop
    -- 'oldLogsNumToKeep' to keep them.
    let oldLogsToRemove = drop oldLogsNumToKeep . reverse $ oldLogs
    -- If the total num of old logs is less than 'keepFilesNum', all
    -- of them will be kept.
    mapM_ removeFile oldLogsToRemove
