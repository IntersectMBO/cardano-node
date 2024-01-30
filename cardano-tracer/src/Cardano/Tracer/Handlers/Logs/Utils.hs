{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.Logs.Utils
  ( createOrUpdateEmptyLog
  , createEmptyLogRotation
  , getTimeStampFromLog
  , isItLog
  , logExtension
  , logPrefix
  , timeStampFormat
  ) where

import Control.Concurrent.Extra (Lock, withLock)
import Control.Concurrent.MVar (modifyMVar_, tryReadMVar)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (<.>), (</>))
import System.IO (openFile, Handle, IOMode (AppendMode, WriteMode), hClose)

import Cardano.Tracer.Configuration (LogFormat (..), LoggingParams (..))
import Cardano.Tracer.Types (HandleRegistry, Registry(Registry), NodeName)
import Cardano.Tracer.Utils

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
  -> NodeName
  -> LoggingParams
  -> HandleRegistry
  -> FilePath
  -> LogFormat
  -> IO ()
createEmptyLogRotation currentLogLock nodeName loggingParams registry subDirForLogs format = do
  -- The root directory (as a parent for subDirForLogs) will be created as well if needed.
  createDirectoryIfMissing True subDirForLogs
  createOrUpdateEmptyLog currentLogLock nodeName loggingParams registry subDirForLogs format

-- | Create an empty log file (with the current timestamp in the name).
createOrUpdateEmptyLog :: Lock -> NodeName -> LoggingParams -> HandleRegistry -> FilePath -> LogFormat -> IO ()
createOrUpdateEmptyLog currentLogLock nodeName loggingParams (Registry registry) subDirForLogs format = do
  withLock currentLogLock do
    ts <- formatTime defaultTimeLocale timeStampFormat . systemToUTCTime <$> getSystemTime
    let pathToLog = subDirForLogs </> logPrefix <> ts <.> logExtension format

    modifyMVar_ registry \handles -> do

      case Map.lookup (nodeName, loggingParams) handles of
        Nothing -> 
          undefined 
        Just (handle, _filePath) -> 
          hClose handle

      newHandle <- openFile pathToLog WriteMode
      let newMap = Map.insert (nodeName, loggingParams) (newHandle, pathToLog) handles
      pure newMap

getTimeStampFromLog :: FilePath -> Maybe UTCTime
getTimeStampFromLog pathToLog =
  parseTimeM True defaultTimeLocale timeStampFormat timeStamp
 where
  timeStamp = drop (length logPrefix) . takeBaseName . takeFileName $ pathToLog

timeStampFormat :: String
timeStampFormat = "%Y-%m-%dT%H-%M-%S"
