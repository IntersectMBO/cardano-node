{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Testnet.Conf
  ( ProjectBase(..)
  , YamlFilePath(..)
  , Conf(..)
  , mkConf
  ) where

import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Random as IO

newtype ProjectBase = ProjectBase
  { projectBase :: FilePath
  } deriving (Eq, Show)

newtype YamlFilePath = YamlFilePath
  { projectBase :: FilePath
  } deriving (Eq, Show)

data Conf = Conf
  { tempAbsPath :: FilePath
  , base :: FilePath
  , configurationTemplate :: FilePath
  , testnetMagic :: Int
  } deriving (Eq, Show)

mkConf :: ProjectBase -> YamlFilePath -> FilePath -> Maybe Int -> H.Integration Conf
mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tempAbsPath maybeMagic = do
  testnetMagic <- H.noteShowIO $ maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  return $ Conf {base, configurationTemplate, tempAbsPath, testnetMagic}
