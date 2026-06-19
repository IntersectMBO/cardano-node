{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.System
  ( getPathToBackupDir
  , getPathToChartColorsDir
  , getPathToChartsConfig
  , getPathToLogsLiveViewFontConfig
  , getPathToThemeConfig
  , getPathsToNotificationsSettings
  ) where

import qualified System.Directory as D
import           System.FilePath ((</>))

import           Cardano.Tracer.Environment

getPathToChartsConfig
  , getPathToThemeConfig
  , getPathToLogsLiveViewFontConfig :: TracerEnv -> IO FilePath
getPathToChartsConfig = getPathToConfig "charts"
getPathToThemeConfig  = getPathToConfig "theme"
getPathToLogsLiveViewFontConfig = getPathToConfig "llvFontSize"

getPathToConfig :: FilePath -> TracerEnv -> IO FilePath
getPathToConfig configName TracerEnv{teStateDir} = do
  configDir <- getPathToConfigDir teStateDir
  return $ configDir </> configName

getPathsToNotificationsSettings :: Maybe FilePath -> IO (FilePath, FilePath)
getPathsToNotificationsSettings rtvSD = do
  configDir <- getPathToConfigDir rtvSD
  let pathToNotifySubDir = configDir </> "notifications"
  D.createDirectoryIfMissing True pathToNotifySubDir
  return ( pathToNotifySubDir </> "email"
         , pathToNotifySubDir </> "events"
         )

getPathToChartColorsDir :: TracerEnv -> IO FilePath
getPathToChartColorsDir TracerEnv{teStateDir} = do
  configDir <- getPathToConfigDir teStateDir
  let pathToColorsSubDir = configDir </> "color"
  D.createDirectoryIfMissing True pathToColorsSubDir
  return pathToColorsSubDir

getPathToConfigDir :: Maybe FilePath -> IO FilePath
getPathToConfigDir rtvSD = do
  configDir <- getStateDir rtvSD D.XdgConfig
  let pathToConfigDir = configDir </> tracerStateRootDir
  D.createDirectoryIfMissing True pathToConfigDir
  return pathToConfigDir

getPathToBackupDir :: TracerEnv -> IO FilePath
getPathToBackupDir TracerEnv{teStateDir} = do
  dataDir <- getStateDir teStateDir D.XdgData
  let pathToBackupDir = dataDir </> tracerStateRootDir </> "backup"
  D.createDirectoryIfMissing True pathToBackupDir
  return pathToBackupDir

getStateDir
  :: Maybe FilePath
  -> D.XdgDirectory
  -> IO FilePath
getStateDir Nothing xdgDir = D.getXdgDirectory xdgDir ""
getStateDir (Just stateDir) _ = return stateDir

tracerStateRootDir :: FilePath
tracerStateRootDir = "cardano-tracer"
