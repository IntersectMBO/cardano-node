module Cardano.Tracer.Test.Utils
  ( doesDirectoryEmpty
  , false
  , propRunInLogsStructure
  , propRunInLogsStructure2
  ) where

import           System.Directory.Extra (listDirectories)
import           System.FilePath ((</>), (<.>), dropDrive, takeBaseName)
import           System.IO.Extra (newTempDir, newTempFile)
import           System.Info.Extra (isMac, isWindows)
import           Test.Tasty.QuickCheck

false :: String -> IO Property
false msg = return . counterexample msg $ property False

propRunInLogsStructure
  :: (FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure testAction = ioProperty $ do
  (rootDir, _)   <- newTempDir
  (localSock, _) <- newTempFile
  testAction rootDir (prepareLocalSock localSock)

propRunInLogsStructure2
  :: (FilePath -> FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure2 testAction = ioProperty $ do
  (rootDir, _)    <- newTempDir
  (localSock1, _) <- newTempFile
  (localSock2, _) <- newTempFile
  testAction rootDir (prepareLocalSock localSock1) (prepareLocalSock localSock2)

prepareLocalSock :: FilePath -> FilePath
prepareLocalSock localSock
  | isWindows = pipeForWindows
  | isMac     = sockForMac
  | otherwise = localSock
 where
  pipeForWindows = "\\\\.\\pipe\\" <> dropDrive localSock
  sockForMac = "/tmp" </> takeBaseName localSock <.> "pipe"

doesDirectoryEmpty :: FilePath -> IO Bool
doesDirectoryEmpty = fmap null . listDirectories
