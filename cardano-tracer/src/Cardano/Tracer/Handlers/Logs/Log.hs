{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.Logs.Log
  ( createLogAndSymLink
  , createLogAndUpdateSymLink
  , doesSymLinkValid
  , getTimeStampFromLog
  , isItLog
  , isItSymLink
  , symLinkName
  ) where

import           Control.Monad.Extra (whenM)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (isJust)
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Data.Text as T
import           System.Directory
import           System.FilePath

import           Cardano.Tracer.Configuration

logPrefix :: String
logPrefix = "node-"

logExtension :: LogFormat -> String
logExtension ForHuman   = ".log"
logExtension ForMachine = ".json"

symLinkName :: LogFormat -> FilePath
symLinkName format = "node" <.> logExtension format

symLinkNameTmp :: LogFormat -> FilePath
symLinkNameTmp format = symLinkName format <.> "tmp"

isItSymLink
  :: LogFormat
  -> FilePath
  -> IO Bool
isItSymLink format fileName =
  if takeFileName fileName == symLinkName format
    then pathIsSymbolicLink fileName
    else return False

doesSymLinkValid :: FilePath -> IO Bool
doesSymLinkValid pathToSymLink = do
  logName <- getSymbolicLinkTarget pathToSymLink
  -- Target log is always in the same subdir.
  let subDirForLogs = takeDirectory pathToSymLink
      pathToLog = subDirForLogs </> logName
  -- If the symlink is valid, 'pathToLog' does exist.
  doesFileExist pathToLog

isItLog :: LogFormat -> FilePath -> Bool
isItLog format pathToLog = hasProperPrefix && hasTimestamp && hasProperExt
 where
  fileName        = takeFileName pathToLog
  hasProperPrefix = T.pack logPrefix `T.isPrefixOf` T.pack fileName
  hasTimestamp    = isJust timeStamp

  timeStamp :: Maybe UTCTime
  timeStamp = parseTimeM True defaultTimeLocale timeStampFormat $ T.unpack maybeTimestamp

  maybeTimestamp  = T.drop (length logPrefix) . T.pack . takeBaseName $ fileName
  hasProperExt    = takeExtension fileName == logExtension format

-- | Create a new log file and a symlink to it, from scratch.
createLogAndSymLink :: FilePath -> LogFormat -> IO ()
createLogAndSymLink subDirForLogs format =
  createLog subDirForLogs format >>= flip createFileLink symLink
 where
  symLink = subDirForLogs </> symLinkName format

-- | Create a new log file and move existing symlink
-- from the old log file to the new one.
createLogAndUpdateSymLink :: FilePath -> LogFormat -> IO ()
createLogAndUpdateSymLink subDirForLogs format = do
  newLog <- createLog subDirForLogs format
  whenM (doesFileExist tmpSymLink) $
    removeFile tmpSymLink
  createFileLink newLog tmpSymLink
  renamePath tmpSymLink realSymLink -- Atomic operation, uses POSIX.rename.
 where
  tmpSymLink  = subDirForLogs </> symLinkNameTmp format
  realSymLink = subDirForLogs </> symLinkName format

createLog :: FilePath -> LogFormat -> IO FilePath
createLog subDirForLogs format = do
  ts <- formatTime defaultTimeLocale timeStampFormat <$> getCurrentTime
  let logName = logPrefix <> ts <.> logExtension format
      pathToLog = subDirForLogs </> logName
  LBS.writeFile pathToLog LBS.empty -- Create an empty log file.
  return pathToLog

-- | This function is applied to the log we already checked,
-- so we definitely know it contains timestamp.
getTimeStampFromLog :: FilePath -> Maybe UTCTime
getTimeStampFromLog pathToLog =
  parseTimeM True defaultTimeLocale timeStampFormat timeStamp
 where
  timeStamp = drop (length logPrefix) . takeBaseName . takeFileName $ pathToLog

timeStampFormat :: String
timeStampFormat = "%Y-%m-%dT%H-%M-%S"
