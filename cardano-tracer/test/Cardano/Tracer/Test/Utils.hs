{-# LANGUAGE RecordWildCards #-}
module Cardano.Tracer.Test.Utils
  ( module Cardano.Tracer.Test.Utils
  , module Data.Functor.Identity
  ) where

import           Cardano.Tracer.Test.TestSetup

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
propRunInLogsStructure TestSetup{..} testAction = ioProperty $ do
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
