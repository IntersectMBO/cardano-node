{-# LANGUAGE DuplicateRecordFields #-}

module Testnet.Conf
  ( ProjectBase(..)
  , YamlFilePath(..)
  , Conf(..)
  , mkConf
  ) where

import           Testnet.Runtime

import qualified Hedgehog.Extras.Test.Base as H
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

mkConf :: Maybe YamlFilePath -> FilePath -> H.Integration Conf
mkConf mConfigTemplate tempAbsPath' = do
  let configTemplate = unYamlFilePath <$> mConfigTemplate

  return $ Conf
    { tempAbsPath = TmpAbsolutePath tempAbsPath'
    , configurationTemplate = configTemplate
    }


