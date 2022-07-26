{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.RTView.System
  ( getPathToBackupDir
  , getPathToChartColorsDir
  , getPathToChartsConfig
  , getPathToThemeConfig
  , getPathsToNotificationsSettings
  , getPathsToSSLCerts
  , getProcessId
  ) where

import           Data.Word (Word32)
import           Graphics.UI.Threepenny.Core (UI, liftIO)
import qualified System.Directory as D
import           System.FilePath ((</>))

#if defined(mingw32_HOST_OS)
import           System.Win32.Process (getCurrentProcessId)
#else
import           System.Posix.Process (getProcessID)
import           System.Posix.Types (CPid (..))
#endif

getProcessId :: UI Word32
getProcessId =
#if defined(mingw32_HOST_OS)
  liftIO getCurrentProcessId
#else
  do CPid pid <- liftIO getProcessID
     return $ fromIntegral pid
#endif

getPathToChartsConfig, getPathToThemeConfig :: IO FilePath
getPathToChartsConfig = getPathToConfig "charts"
getPathToThemeConfig  = getPathToConfig "theme"

getPathToConfig :: FilePath -> IO FilePath
getPathToConfig configName = do
  configDir <- getPathToConfigDir
  return $ configDir </> configName

getPathsToSSLCerts :: IO (FilePath, FilePath)
getPathsToSSLCerts = do
  configDir <- getPathToConfigDir
  let pathToSSLSubDir = configDir </> "ssl"
  D.createDirectoryIfMissing True pathToSSLSubDir
  return ( pathToSSLSubDir </> "cert.pem"
         , pathToSSLSubDir </> "key.pem"
         )

getPathsToNotificationsSettings :: IO (FilePath, FilePath)
getPathsToNotificationsSettings = do
  configDir <- getPathToConfigDir
  let pathToNotifySubDir = configDir </> "notifications"
  D.createDirectoryIfMissing True pathToNotifySubDir
  return ( pathToNotifySubDir </> "email"
         , pathToNotifySubDir </> "events"
         )

getPathToConfigDir :: IO FilePath
getPathToConfigDir = do
  configDir <- D.getXdgDirectory D.XdgConfig ""
  let pathToRTViewConfigDir = configDir </> rtViewRootDir
  D.createDirectoryIfMissing True pathToRTViewConfigDir
  return pathToRTViewConfigDir

getPathToBackupDir :: IO FilePath
getPathToBackupDir = do
  dataDir <- D.getXdgDirectory D.XdgData ""
  let pathToRTViewBackupDir = dataDir </> rtViewRootDir </> "backup"
  D.createDirectoryIfMissing True pathToRTViewBackupDir
  return pathToRTViewBackupDir

getPathToChartColorsDir :: IO FilePath
getPathToChartColorsDir = do
  configDir <- getPathToConfigDir
  let pathToColorsSubDir = configDir </> "color"
  D.createDirectoryIfMissing True pathToColorsSubDir
  return pathToColorsSubDir

rtViewRootDir :: FilePath
rtViewRootDir = "cardano-rt-view"
