{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tracer.Test.Utils
  ( false
  , propRunInLogsStructure
  , propRunInLogsStructure2
  , removeDirectoryContent
  , doesDirectoryEmpty
  ) where

import           Control.Exception (finally)
import           System.Directory
import           System.Directory.Extra
import           System.IO.Extra
import           Test.Tasty.QuickCheck

false :: String -> IO Property
false msg = return . counterexample msg $ property False

propRunInLogsStructure
  :: (FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure testAction = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  (rootDir, deleteDir) <- newTempDirWithin tmpDir
  (localSock, deleteSock) <- newTempFileWithin tmpDir
  testAction rootDir localSock `finally` deleteSock >> deleteDir

propRunInLogsStructure2
  :: (FilePath -> FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure2 testAction = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  (rootDir, deleteDir) <- newTempDirWithin tmpDir
  (localSock1, deleteSock1) <- newTempFileWithin tmpDir
  (localSock2, deleteSock2) <- newTempFileWithin tmpDir
  testAction rootDir localSock1 localSock2
    `finally` deleteSock1 >> deleteSock2 >> deleteDir

removeDirectoryContent :: FilePath -> IO ()
removeDirectoryContent dir = listContents dir >>= mapM_ removePathForcibly

doesDirectoryEmpty :: FilePath -> IO Bool
doesDirectoryEmpty dir = listContents dir >>= return . null
