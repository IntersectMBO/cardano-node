{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Testnet.Conf
  ( ProjectBase(..)
  , YamlFilePath(..)
  , Conf(..)
  , mkConf
  ) where

import           System.FilePath.Posix ((</>))

import qualified Hedgehog.Extras.Test.Base as H
import qualified System.FilePath.Posix as FP
import qualified System.Random as IO

newtype ProjectBase = ProjectBase
  { projectBase :: FilePath
  } deriving (Eq, Show)

newtype YamlFilePath = YamlFilePath
  { projectBase :: FilePath
  } deriving (Eq, Show)

data Conf = Conf
  { tempAbsPath :: FilePath
  , tempRelPath :: FilePath
  , tempBaseAbsPath :: FilePath
  , logDir :: FilePath
  , base :: FilePath
  , socketDir :: FilePath
  , configurationTemplate :: FilePath
  , testnetMagic :: Int
  } deriving (Eq, Show)

mkConf :: ProjectBase -> YamlFilePath -> FilePath -> Maybe Int -> H.Integration Conf
mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tempAbsPath maybeMagic = do
  testnetMagic <- H.noteShowIO $ maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  socketDir <- H.noteShow $ tempRelPath </> "socket"
  logDir <- H.noteTempFile tempAbsPath "logs"

  return $ Conf {..}
