{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Test.Utils
  ( module Cardano.Tracer.Test.Utils
  , module Data.Functor.Identity
  ) where

import           Cardano.Tracer.Test.TestSetup
import           Cardano.Logging.Types (HowToConnect)
import qualified Cardano.Logging.Types as Net

import           Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import           Data.Functor.Identity
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Vector.Algorithms.Merge
import           System.Directory.Extra (listDirectories)
import           System.FilePath (dropDrive, dropExtension)
import           System.Info.Extra (isMac, isWindows)
import           System.IO.Extra (newTempDirWithin)

import qualified Test.QuickCheck as QuickCheck
import           Test.Tasty.QuickCheck

unI :: Identity a -> a
unI (Identity x) = x

false :: String -> IO Property
false msg = return . counterexample msg $ property False

propRunInLogsStructure
  :: TestSetup Identity -> (FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure TestSetup{..} testAction = ioProperty do
  (rootDir, _) <- newTempDirWithin (unI tsWorkDir)
  testAction rootDir
    (prepareLocalSock $ unI tsSockInternal)

propRunInLogsStructure2
  :: TestSetup Identity -> (FilePath -> FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure2 TestSetup{..} testAction = ioProperty $ do
  (rootDir, _) <- newTempDirWithin (unI tsWorkDir)
  testAction rootDir
    (prepareLocalSock . replaceExtension "1.sock" $ unI tsSockInternal)
    (prepareLocalSock . replaceExtension "2.sock" $ unI tsSockInternal)

propRunInLogsStructureLocal
  :: TestSetup Identity -> (FilePath -> HowToConnect -> IO Property)
  -> Property
propRunInLogsStructureLocal TestSetup{..} testAction = ioProperty $ do
  (rootDir, _) <- newTempDirWithin (unI tsWorkDir)
  testAction rootDir
    (Net.LocalPipe (prepareLocalSock $ unI tsSockInternal))

propRunInLogsStructureLocal2
  :: TestSetup Identity -> (FilePath -> HowToConnect -> HowToConnect -> IO Property)
  -> Property
propRunInLogsStructureLocal2 TestSetup{..} testAction = ioProperty $ do
  (rootDir, _) <- newTempDirWithin (unI tsWorkDir)
  testAction rootDir
    (Net.LocalPipe . prepareLocalSock . replaceExtension "1.sock" $ unI tsSockInternal)
    (Net.LocalPipe . prepareLocalSock . replaceExtension "2.sock" $ unI tsSockInternal)

propRunInLogsStructurePort
  :: TestSetup Identity -> (FilePath -> HowToConnect -> IO Property)
  -> Property
propRunInLogsStructurePort TestSetup{..} testAction = ioProperty do
  (rootDir, _) <- newTempDirWithin (unI tsWorkDir)
  testAction rootDir howToConnect
    where
  howToConnect :: HowToConnect
  howToConnect = Net.RemoteSocket "localhost" 22557

propRunInLogsStructurePort2
  :: TestSetup Identity -> (FilePath -> HowToConnect -> HowToConnect -> IO Property)
  -> Property
propRunInLogsStructurePort2 TestSetup{..} testAction = ioProperty do
  (rootDir, _) <- newTempDirWithin (unI tsWorkDir)
  testAction rootDir howToConnect1 howToConnect2
    where
  howToConnect1, howToConnect2 :: HowToConnect
  howToConnect1 = Net.RemoteSocket "localhost" 22555
  howToConnect2 = Net.RemoteSocket "localhost" 22556

prepareLocalSock :: FilePath -> FilePath
prepareLocalSock localSock
  | isWindows = "\\\\.\\pipe\\build\\" <> dropDrive localSock
  | isMac     = replaceExtension "pipe" localSock
  | otherwise = localSock

replaceExtension :: String -> FilePath -> FilePath
replaceExtension new f = dropExtension f <> "." <> new

isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty = fmap null . listDirectories

vectorOf :: Gen a -> Gen (Vector a)
vectorOf gen = sized \n -> do
  len <- chooseInt (0, n)
  sizedVectorOf len gen

sizedVectorOf :: Int -> Gen a -> Gen (Vector a)
sizedVectorOf n a = Vector.fromListN n <$> QuickCheck.vectorOf n a

vectorSort :: Ord a => Vector a -> Vector a
vectorSort = Vector.modify sort

-- | Basic Lock synchronisation.
--
-- From: https://hackage.haskell.org/package/concurrent-extra-0.7.0.12/docs/Control-Concurrent-Lock.html

type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

acquire :: Lock -> IO ()
acquire = takeMVar

release :: Lock -> IO ()
release = (`putMVar` ())
