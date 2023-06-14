{-# LANGUAGE DuplicateRecordFields #-}

module Testnet.Conf
  ( YamlFilePath(..)
  , Conf(..)
  , mkConf
  ) where

import           Testnet.Runtime

import qualified Hedgehog.Extras.Test.Base as H

newtype YamlFilePath = YamlFilePath
  { unYamlFilePath :: FilePath
  } deriving (Eq, Show)

newtype Conf = Conf
  { tempAbsPath :: TmpAbsolutePath
  } deriving (Eq, Show)

mkConf :: FilePath -> H.Integration Conf
mkConf tempAbsPath' =
  return $ Conf
    { tempAbsPath = TmpAbsolutePath tempAbsPath'
    }


