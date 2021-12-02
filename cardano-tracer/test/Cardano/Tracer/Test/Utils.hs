module Cardano.Tracer.Test.Utils
  ( doesDirectoryEmpty
  , false
  , propRunInLogsStructure
  , propRunInLogsStructure2
  , removeDirectoryContent
  ) where

import           Control.Exception (finally)
import           System.Directory.Extra (getTemporaryDirectory,
                   listDirectories, removeFile, removePathForcibly)
import           System.FilePath (dropDrive)
import           System.IO.Extra (newTempDirWithin, newTempFileWithin)
import           System.Info.Extra (isWindows)
import           Test.Tasty.QuickCheck

false :: String -> IO Property
false msg = return . counterexample msg $ property False

propRunInLogsStructure
  :: (FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure testAction = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  (rootDir, deleteDir) <- newTempDirWithin tmpDir
  (localSock, _) <- newTempFileWithin tmpDir
  let preparedLocalSock = if isWindows then forWindows localSock else localSock
  testAction rootDir preparedLocalSock
    `finally` (removeFile preparedLocalSock >> deleteDir)

propRunInLogsStructure2
  :: (FilePath -> FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure2 testAction = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  (rootDir, deleteDir) <- newTempDirWithin tmpDir
  (localSock1, _) <- newTempFileWithin tmpDir
  (localSock2, _) <- newTempFileWithin tmpDir
  let preparedLocalSock1 = if isWindows then forWindows localSock1 else localSock1
      preparedLocalSock2 = if isWindows then forWindows localSock2 else localSock2
  testAction rootDir preparedLocalSock1 preparedLocalSock2
    `finally` (removeFile preparedLocalSock1 >> removeFile preparedLocalSock2 >> deleteDir)

forWindows :: FilePath -> FilePath
forWindows localSock = "\\\\.\\pipe\\" <> dropDrive localSock

removeDirectoryContent :: FilePath -> IO ()
removeDirectoryContent dir = listDirectories dir >>= mapM_ removePathForcibly

doesDirectoryEmpty :: FilePath -> IO Bool
doesDirectoryEmpty = fmap null . listDirectories
