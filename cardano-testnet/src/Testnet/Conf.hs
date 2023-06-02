{-# LANGUAGE DuplicateRecordFields #-}

module Testnet.Conf
  ( ProjectBase(..)
  , YamlFilePath(..)
  , Conf(..)
  , mkConf
  ) where

import           Testnet.Util.Runtime

import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Random as IO

newtype ProjectBase = ProjectBase
  { projectBase :: FilePath
  } deriving (Eq, Show)

newtype YamlFilePath = YamlFilePath
  { unYamlFilePath :: FilePath
  } deriving (Eq, Show)

data Conf = Conf
  { tempAbsPath :: TmpAbsolutePath
  , configurationTemplate :: Maybe FilePath
  } deriving (Eq, Show)

mkConf :: Maybe YamlFilePath -> FilePath -> Maybe Int -> H.Integration Conf
mkConf mConfigTemplate tempAbsPath' maybeMagic = do
  _testnetMagic' <- H.noteShowIO $ maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  let configTemplate = unYamlFilePath <$> mConfigTemplate

  return $ Conf
    { tempAbsPath = TmpAbsolutePath tempAbsPath'
    , configurationTemplate = configTemplate
    }


