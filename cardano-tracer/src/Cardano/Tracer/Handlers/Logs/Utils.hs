{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.Logs.Utils
  ( createEmptyLog
  , createEmptyLogRotation
  , getTimeStampFromLog
  , isItLog
  ) where

import           Cardano.Tracer.Configuration (LogFormat (..))

import           Control.Concurrent.Extra (Lock, withLock)
import           Control.Monad (void)
import qualified Data.ByteString as BS
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           System.FilePath (takeBaseName, takeExtension, takeFileName, (<.>), (</>))

logPrefix :: String
logPrefix = "node-"

logExtension :: LogFormat -> String
logExtension ForHuman   = ".log"
logExtension ForMachine = ".json"

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

createEmptyLogRotation
  :: Lock
  -> FilePath
  -> LogFormat
  -> IO ()
createEmptyLogRotation currentLogLock subDirForLogs format =
  withLock currentLogLock $
    void $ createEmptyLog subDirForLogs format

-- | Create an empty log file (with the current timestamp in the name).
createEmptyLog :: FilePath -> LogFormat -> IO FilePath
createEmptyLog subDirForLogs format = do
  ts <- formatTime defaultTimeLocale timeStampFormat . systemToUTCTime <$> getSystemTime
  let pathToLog = subDirForLogs </> logPrefix <> ts <.> logExtension format
  BS.writeFile pathToLog BS.empty
  return pathToLog

getTimeStampFromLog :: FilePath -> Maybe UTCTime
getTimeStampFromLog pathToLog =
  parseTimeM True defaultTimeLocale timeStampFormat timeStamp
 where
  timeStamp = drop (length logPrefix) . takeBaseName . takeFileName $ pathToLog

timeStampFormat :: String
timeStampFormat = "%Y-%m-%dT%H-%M-%S"
