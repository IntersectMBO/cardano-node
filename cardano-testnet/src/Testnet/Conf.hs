{-# LANGUAGE DuplicateRecordFields #-}

module Testnet.Conf
  ( ProjectBase(..)
  , YamlFilePath(..)
  , Conf(..)
  , mkConf
  ) where

import           System.FilePath ((</>))

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified System.FilePath.Posix as FP
import qualified System.Random as IO

newtype ProjectBase = ProjectBase
  { projectBase :: FilePath
  } deriving (Eq, Show)

newtype YamlFilePath = YamlFilePath
  { unYamlFilePath :: FilePath
  } deriving (Eq, Show)

data Conf = Conf
  { tempAbsPath :: FilePath
  , tempRelPath :: FilePath
  , tempBaseAbsPath :: FilePath
  , logDir :: FilePath
  , base :: FilePath
  , socketDir :: FilePath
  , configurationTemplate :: Maybe FilePath
  , testnetMagic :: Int
  } deriving (Eq, Show)

mkConf :: ProjectBase -> Maybe YamlFilePath -> FilePath -> Maybe Int -> H.Integration Conf
mkConf (ProjectBase base') mConfigTemplate tempAbsPath' maybeMagic = do
  testnetMagic' <- H.noteShowIO $ maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  tempBaseAbsPath' <- H.noteShow $ FP.takeDirectory tempAbsPath'
  tempRelPath' <- H.noteShow $ FP.makeRelative tempBaseAbsPath' tempAbsPath'
  socketDir' <- H.createSubdirectoryIfMissing tempBaseAbsPath' $ tempRelPath' </> "socket"
  logDir' <- H.createDirectoryIfMissing $ tempAbsPath' </> "logs"
  let configTemplate = unYamlFilePath <$> mConfigTemplate

  return $ Conf
    { tempAbsPath = tempAbsPath'
    , tempRelPath = tempRelPath'
    , tempBaseAbsPath = tempBaseAbsPath'
    , logDir = logDir'
    , base = base'
    , socketDir = socketDir'
    , configurationTemplate = configTemplate
    , testnetMagic = testnetMagic'
    }
