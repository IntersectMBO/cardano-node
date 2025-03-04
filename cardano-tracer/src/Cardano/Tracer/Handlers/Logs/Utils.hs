{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.Logs.Utils
  ( createOrUpdateEmptyLog
  , createEmptyLogRotationAndSymlink
  , createEmptyLogRotationAndUpdateSymlink
  , getTimeStampFromLog
  , isItLog
  , logExtension
  , logPrefix
  , timeStampFormat
  ) where

import           Cardano.Tracer.Configuration (LogFormat (..), LoggingParams (..))
import           Cardano.Tracer.Types (HandleRegistry, HandleRegistryKey, HandleRegistryMap)
import           Cardano.Tracer.Utils (modifyRegistry_)

import           Control.Monad.Extra (whenM)
-- import           Control.Concurrent.MVar (MVar) -- newMVar, swapMVar, readMVar, tryReadMVar, modifyMVar_)
import           Control.Concurrent.Extra (Lock, withLock)
import           Data.Foldable (for_)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
-- import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           System.Directory (createDirectoryIfMissing, createFileLink, doesFileExist, renamePath, removeFile)
import           System.FilePath (takeBaseName, takeExtension, takeFileName, (<.>), (</>))
import           System.IO (IOMode (WriteMode), hClose, openFile)

logPrefix :: String
logPrefix = "node-"

logExtension :: LogFormat -> String
logExtension ForHuman   = ".log"
logExtension ForMachine = ".json"

-- | An example of the valid log name: 'node-2021-11-29T09-55-04.json'.
isItLog :: LogFormat -> FilePath -> Bool
isItLog format pathToLog = and
  [ hasProperPrefix
  , hasTimestamp
  , hasProperExt
  ]
 where
  fileName = takeFileName pathToLog
  hasProperPrefix = T.pack logPrefix `T.isPrefixOf` T.pack fileName
  hasTimestamp = isJust timeStamp
  hasProperExt = takeExtension fileName == logExtension format

  timeStamp :: Maybe UTCTime
  timeStamp = parseTimeM True defaultTimeLocale timeStampFormat $ T.unpack maybeTimestamp

  maybeTimestamp = T.drop (length logPrefix) . T.pack . takeBaseName $ fileName

symLinkName :: LogFormat -> FilePath
symLinkName format = "node" <.> logExtension format

symLinkNameTmp :: LogFormat -> FilePath
symLinkNameTmp format = symLinkName format <.> "tmp"

createEmptyLogRotationAndSymlink
  :: Lock
  -> HandleRegistryKey
  -> HandleRegistry
  -> FilePath
  -> IO ()
createEmptyLogRotationAndSymlink currentLogLock key@(_, LoggingParams{logFormat = format}) registry subDirForLogs = do
  -- The root directory (as a parent for subDirForLogs) will be created as well if needed.
  withLock currentLogLock do
    newLog <- createOrUpdateEmptyLog key registry subDirForLogs
    let
      symLink :: FilePath
      symLink = subDirForLogs </> symLinkName format

    appendFile "/tmp/mylog" ("one: symlink: " ++ symLink ++ "\n\n")
    whenM (doesFileExist symLink) do
      appendFile "/tmp/mylog" ("one: symlink exists, rm: " ++ symLink ++ "\n\n")
      removeFile symLink
    createFileLink newLog symLink
    appendFile "/tmp/mylog" ("one: createFileLink newLog (" ++ newLog ++ ") symLink (" ++ symLink ++ ")\n\n")

createEmptyLogRotationAndUpdateSymlink
  :: Lock
  -> HandleRegistryKey
  -> HandleRegistry
  -> FilePath
  -> IO ()
createEmptyLogRotationAndUpdateSymlink currentLogLock key@(_, LoggingParams{logFormat = format}) registry subDirForLogs = do
  -- The root directory (as a parent for subDirForLogs) will be created as well if needed.
  createDirectoryIfMissing True subDirForLogs
  withLock currentLogLock do
    newLog <- createOrUpdateEmptyLog key registry subDirForLogs
    let
      symLink, tmpLink :: FilePath
      symLink = subDirForLogs </> symLinkName    format
      tmpLink = subDirForLogs </> symLinkNameTmp format
    appendFile "/tmp/mylog" ("two: symlink: " ++ symLink ++ "\n\n")
    appendFile "/tmp/mylog" ("two: tmplink: " ++ tmpLink ++ "\n")
    whenM (doesFileExist tmpLink) do
      appendFile "/tmp/mylog" ("two: tmplink exists, rm: " ++ tmpLink ++ "\n\n")
      removeFile tmpLink
    createFileLink newLog tmpLink
    appendFile "/tmp/mylog" ("two: createFileLink newLog (" ++ newLog ++ ") tmpLink (" ++ tmpLink ++ ")\n\n")
    renamePath tmpLink symLink
    appendFile "/tmp/mylog" ("two: renamePath tmpLink (" ++ tmpLink ++ ") symLink (" ++ symLink ++ ")\n\n")

-- | Create an empty log file (with the current timestamp in the name).
createOrUpdateEmptyLog :: HandleRegistryKey -> HandleRegistry -> FilePath -> IO FilePath
createOrUpdateEmptyLog key@(_, LoggingParams{logFormat = format}) registry subDirForLogs = do
  formattedTime :: String <-
    formatTime defaultTimeLocale timeStampFormat . systemToUTCTime <$> getSystemTime
  let
    pathToLog :: FilePath
    pathToLog = subDirForLogs </> logPrefix <> formattedTime <.> logExtension format
  modifyRegistry_ registry \(handles :: HandleRegistryMap) -> do

    for_ @Maybe (Map.lookup key handles) \(handle, _filePath) ->
      hClose handle

    newHandle <- openFile pathToLog WriteMode
    let newMap :: HandleRegistryMap
        newMap = Map.insert key (newHandle, pathToLog) handles
    pure newMap
  pure pathToLog

getTimeStampFromLog :: FilePath -> Maybe UTCTime
getTimeStampFromLog pathToLog = let
  timeStamp :: String
  timeStamp = drop (length logPrefix) . takeBaseName . takeFileName $ pathToLog
    in
  parseTimeM True defaultTimeLocale timeStampFormat timeStamp

timeStampFormat :: String
timeStampFormat = "%Y-%m-%dT%H-%M-%S"
