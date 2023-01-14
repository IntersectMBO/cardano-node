{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.System
  ( getPathToBackupDir
  , getPathToChartColorsDir
  , getPathToChartsConfig
  , getPathToLogsLiveViewFontConfig
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

import           Cardano.Tracer.Environment

getProcessId :: UI Word32
getProcessId =
#if defined(mingw32_HOST_OS)
  liftIO getCurrentProcessId
#else
  do CPid pid <- liftIO getProcessID
     return $ fromIntegral pid
#endif

getPathToChartsConfig
  , getPathToThemeConfig
  , getPathToLogsLiveViewFontConfig :: TracerEnv -> IO FilePath
getPathToChartsConfig = getPathToConfig "charts"
getPathToThemeConfig  = getPathToConfig "theme"
getPathToLogsLiveViewFontConfig = getPathToConfig "llvFontSize"

getPathToConfig :: FilePath -> TracerEnv -> IO FilePath
getPathToConfig configName TracerEnv{teRTViewStateDir} = do
  configDir <- getPathToConfigDir teRTViewStateDir
  return $ configDir </> configName

getPathsToSSLCerts :: TracerEnv -> IO (FilePath, FilePath)
getPathsToSSLCerts TracerEnv{teRTViewStateDir} = do
  configDir <- getPathToConfigDir teRTViewStateDir
  let pathToSSLSubDir = configDir </> "ssl"
  D.createDirectoryIfMissing True pathToSSLSubDir
  return ( pathToSSLSubDir </> "cert.pem"
         , pathToSSLSubDir </> "key.pem"
         )

getPathsToNotificationsSettings :: Maybe FilePath -> IO (FilePath, FilePath)
getPathsToNotificationsSettings rtvSD = do
  configDir <- getPathToConfigDir rtvSD
  let pathToNotifySubDir = configDir </> "notifications"
  D.createDirectoryIfMissing True pathToNotifySubDir
  return ( pathToNotifySubDir </> "email"
         , pathToNotifySubDir </> "events"
         )

getPathToChartColorsDir :: TracerEnv -> IO FilePath
getPathToChartColorsDir TracerEnv{teRTViewStateDir} = do
  configDir <- getPathToConfigDir teRTViewStateDir
  let pathToColorsSubDir = configDir </> "color"
  D.createDirectoryIfMissing True pathToColorsSubDir
  return pathToColorsSubDir

getPathToConfigDir :: Maybe FilePath -> IO FilePath
getPathToConfigDir rtvSD = do
  configDir <- getStateDir rtvSD D.XdgConfig
  let pathToRTViewConfigDir = configDir </> rtViewRootDir
  D.createDirectoryIfMissing True pathToRTViewConfigDir
  return pathToRTViewConfigDir

getPathToBackupDir :: TracerEnv -> IO FilePath
getPathToBackupDir TracerEnv{teRTViewStateDir} = do
  dataDir <- getStateDir teRTViewStateDir D.XdgData
  let pathToRTViewBackupDir = dataDir </> rtViewRootDir </> "backup"
  D.createDirectoryIfMissing True pathToRTViewBackupDir
  return pathToRTViewBackupDir

getStateDir
  :: Maybe FilePath
  -> D.XdgDirectory
  -> IO FilePath
getStateDir Nothing xdgDir = D.getXdgDirectory xdgDir ""
getStateDir (Just stateDir) _ = return stateDir

rtViewRootDir :: FilePath
rtViewRootDir = "cardano-rt-view"
