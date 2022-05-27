{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.RTView.System
  ( getPathToChartsConfig
  , getPathToThemeConfig
  , getProcessId
  ) where

import           Data.Word (Word32)
import           Graphics.UI.Threepenny.Core
import           System.Directory
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
  configDir <- getXdgDirectory XdgConfig ""
  let pathToRTViewConfigDir = configDir </> "cardano-rt-view"
  createDirectoryIfMissing True pathToRTViewConfigDir
  return $ pathToRTViewConfigDir </> configName
