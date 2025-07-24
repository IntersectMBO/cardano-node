{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.Rotator
  ( runLogsRotator
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment (TracerEnv (..), handleNoop, forever'tilShutdown)
import           Cardano.Tracer.Handlers.Logs.Utils (createOrUpdateEmptyLog, getTimeStampFromLog,
                   isItLog)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types (HandleRegistry, HandleRegistryKey, NodeName)
import           Cardano.Tracer.Utils (showProblemIfAny, readRegistry)

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.Chan.Unagi (dupChan)
import           Control.Concurrent.Extra (Lock)
import           Control.Monad (forM_, unless, when)
import           Control.Monad.Extra (whenJust, whenM)
import           Data.Foldable (for_)
import           Data.List (nub, sort)
import           Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Word (Word32, Word64)
import           System.Directory (doesDirectoryExist, makeAbsolute, removeFile)
import           System.Directory.Extra (listDirectories, listFiles)
import           System.FilePath (takeFileName, (</>))
import           System.IO (Handle, hTell)
import           System.Time.Extra (sleep)

-- | Runs rotation mechanism for the log files.
runLogsRotator :: TracerEnv -> IO ()
runLogsRotator tracerEnv@TracerEnv { teConfig = TracerConfig{rotation}, teTracer } = do
  whenJust rotation \rot -> do
    traceWith teTracer TracerStartedLogRotator
    launchRotator tracerEnv rot

launchRotator
  :: TracerEnv
  -> RotationParams
  -> IO ()
launchRotator tracerEnv rot@RotationParams{rpFrequencySecs} = do
  whenNonEmpty loggingParamsForFiles do
    outChan <- dupChan teInChan
    forever'tilShutdown handleNoop outChan do
      showProblemIfAny verbosity do
        forM_ loggingParamsForFiles \loggingParam -> do
          checkRootDir teCurrentLogLock teRegistry rot loggingParam
      sleep (fromIntegral rpFrequencySecs)
  where
  whenNonEmpty :: Applicative f => [a] -> f () -> f ()
  whenNonEmpty = unless . null

  TracerEnv
    { teConfig = TracerConfig{verbosity, logging}
    , teCurrentLogLock
    , teRegistry
    , teInChan
    } = tracerEnv

  loggingParamsForFiles :: [LoggingParams]
  loggingParamsForFiles = nub (NE.filter filesOnly logging)

  filesOnly :: LoggingParams -> Bool
  filesOnly LoggingParams{logMode} = logMode == FileMode

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
checkRootDir currentLogLock registry rotParams loggingParams@LoggingParams{logRoot} = do
  logRootAbs <- makeAbsolute logRoot
  whenM (doesDirectoryExist logRootAbs) do
    logsSubDirs <- listDirectories logRootAbs

    handles <- readRegistry registry

    -- There are no nodes' subdirs yet (or they were deleted), or they
    -- don't contain files with open handles so no rotation can be
    -- performed for now.
    forConcurrently_ logsSubDirs \logSubDir -> do

      let nodeName :: NodeName
          nodeName = Text.pack (takeFileName logSubDir)

          key :: HandleRegistryKey
          key = (nodeName, loggingParams)

      for_ @Maybe (Map.lookup key handles) \(handle, _filePath) ->
        checkLogs currentLogLock handle key registry rotParams (logRootAbs </> logSubDir)

-- | We check the log files:
--   1. If there are too big log files.
--   2. If there are too old log files.
checkLogs
  :: Lock
  -> Handle
  -> HandleRegistryKey
  -> HandleRegistry
  -> RotationParams
  -> FilePath
  -> IO ()
checkLogs currentLogLock handle key@(_, LoggingParams{logFormat = format}) registry
          RotationParams{rpLogLimitBytes, rpMaxAgeMinutes, rpKeepFilesNum} subDirForLogs = do

  logs <- map (subDirForLogs </>) . filter (isItLog format) <$> listFiles subDirForLogs
  unless (null logs) do
    -- Since logs' names contain timestamps, we can sort them: the maximum one is the latest log,
    -- and this is the current log (i.e. the log we're writing 'TraceObject's in).
    let fromOldestToNewest = sort logs
        -- Usage of partial function 'last' is safe here (we already checked the list isn't empty).
        -- Only previous logs should be checked if they are outdated.
        allOtherLogs = dropEnd 1 fromOldestToNewest
    checkIfCurrentLogIsFull currentLogLock handle key registry rpLogLimitBytes subDirForLogs
    checkIfThereAreOldLogs allOtherLogs rpMaxAgeMinutes rpKeepFilesNum

-- | If the current log file is full (it's size is too big), the new log will be created.
checkIfCurrentLogIsFull
  :: Lock
  -> Handle
  -> HandleRegistryKey
  -> HandleRegistry
  -> Word64
  -> FilePath
  -> IO ()
checkIfCurrentLogIsFull currentLogLock handle key registry maxSizeInBytes subDirForLogs =
  whenM logIsFull do
     createOrUpdateEmptyLog currentLogLock key registry subDirForLogs

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
