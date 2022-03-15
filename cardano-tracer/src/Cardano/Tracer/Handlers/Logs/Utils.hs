{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.Logs.Utils
  ( createLogAndSymLink
  , createLogAndUpdateSymLink
  , doesSymLinkValid
  , getTimeStampFromLog
  , isItLog
  , isItSymLink
  , symLinkName
  ) where

import           Control.Monad.Extra (whenM)
import           Control.Concurrent.Extra (Lock, withLock)
import qualified Data.ByteString as BS
import           Data.Maybe (isJust)
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Data.Text as T
import           System.Directory (createFileLink, doesFileExist, getSymbolicLinkTarget,
                   pathIsSymbolicLink, renamePath, removeFile)
import           System.FilePath ((<.>), (</>), takeBaseName, takeExtension,
                   takeFileName)

import           Cardano.Tracer.Configuration (LogFormat (..))

logPrefix :: String
logPrefix = "node-"

logExtension :: LogFormat -> String
logExtension ForHuman   = ".log"
logExtension ForMachine = ".json"

symLinkName :: LogFormat -> FilePath
symLinkName format = "node" <.> logExtension format

symLinkNameTmp :: LogFormat -> FilePath
symLinkNameTmp format = symLinkName format <.> "tmp"

isItSymLink :: LogFormat -> FilePath -> IO Bool
isItSymLink format fileName =
  if takeFileName fileName == symLinkName format
    then pathIsSymbolicLink fileName
    else return False

doesSymLinkValid :: FilePath -> IO Bool
doesSymLinkValid pathToSymLink =
  doesFileExist =<< getSymbolicLinkTarget pathToSymLink

-- | An example of the valid log name: 'node-2021-11-29T09-55-04.json'.
isItLog :: LogFormat -> FilePath -> Bool
isItLog format pathToLog = hasProperPrefix && hasTimestamp && hasProperExt
 where
  fileName = takeFileName pathToLog
  hasProperPrefix = T.pack logPrefix `T.isPrefixOf` T.pack fileName
  hasTimestamp = isJust timeStamp
  hasProperExt = takeExtension fileName == logExtension format

  timeStamp :: Maybe UTCTime
  timeStamp = parseTimeM True defaultTimeLocale timeStampFormat $ T.unpack maybeTimestamp

  maybeTimestamp = T.drop (length logPrefix) . T.pack . takeBaseName $ fileName

-- | Create a new log file and a symlink to it, from scratch.
createLogAndSymLink :: FilePath -> LogFormat -> IO FilePath
createLogAndSymLink subDirForLogs format = do
  pathToNewLog <- createEmptyLog subDirForLogs format
  whenM (doesFileExist symLink) $ removeFile symLink
  createFileLink pathToNewLog symLink
  return pathToNewLog
 where
  symLink = subDirForLogs </> symLinkName format

-- | Create a new log file and move existing symlink
--   from the old log file to the new one.
--
-- It is technically possible that, during checking if the current log is full,
-- another thread is writing in the current log (via its symbolic link). If so,
-- we cannot switch to the new log file to avoid writing interruption. That's why
-- we use 'Lock'.
createLogAndUpdateSymLink
  :: Lock
  -> FilePath
  -> LogFormat
  -> IO ()
createLogAndUpdateSymLink currentLogLock subDirForLogs format = do
  newLog <- createEmptyLog subDirForLogs format
  whenM (doesFileExist tmpSymLink) $ removeFile tmpSymLink
  createFileLink newLog tmpSymLink
  withLock currentLogLock $
    renamePath tmpSymLink realSymLink -- Atomic operation (uses POSIX.rename function).
 where
  tmpSymLink  = subDirForLogs </> symLinkNameTmp format
  realSymLink = subDirForLogs </> symLinkName format

-- | Create an empty log file (with the current timestamp in the name).
createEmptyLog :: FilePath -> LogFormat -> IO FilePath
createEmptyLog subDirForLogs format = do
  ts <- formatTime defaultTimeLocale timeStampFormat <$> getCurrentTime
  let logName = logPrefix <> ts <.> logExtension format
      pathToLog = subDirForLogs </> logName
  BS.writeFile pathToLog BS.empty
  return pathToLog

getTimeStampFromLog :: FilePath -> Maybe UTCTime
getTimeStampFromLog pathToLog =
  parseTimeM True defaultTimeLocale timeStampFormat timeStamp
 where
  timeStamp = drop (length logPrefix) . takeBaseName . takeFileName $ pathToLog

timeStampFormat :: String
timeStampFormat = "%Y-%m-%dT%H-%M-%S"
