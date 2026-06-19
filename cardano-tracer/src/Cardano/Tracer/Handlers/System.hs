module Cardano.Tracer.Handlers.System
  ( getPathsToNotificationsSettings
  ) where

import qualified System.Directory as D
import           System.FilePath ((</>))

getPathsToNotificationsSettings :: Maybe FilePath -> IO (FilePath, FilePath)
getPathsToNotificationsSettings rtvSD = do
  configDir <- getPathToConfigDir rtvSD
  let pathToNotifySubDir = configDir </> "notifications"
  D.createDirectoryIfMissing True pathToNotifySubDir
  return ( pathToNotifySubDir </> "email"
         , pathToNotifySubDir </> "events"
         )

getPathToConfigDir :: Maybe FilePath -> IO FilePath
getPathToConfigDir rtvSD = do
  configDir <- getStateDir rtvSD D.XdgConfig
  let pathToConfigDir = configDir </> tracerStateRootDir
  D.createDirectoryIfMissing True pathToConfigDir
  return pathToConfigDir

getStateDir
  :: Maybe FilePath
  -> D.XdgDirectory
  -> IO FilePath
getStateDir Nothing xdgDir = D.getXdgDirectory xdgDir ""
getStateDir (Just stateDir) _ = return stateDir

tracerStateRootDir :: FilePath
tracerStateRootDir = "cardano-tracer"
