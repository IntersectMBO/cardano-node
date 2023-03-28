{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Testnet.Conf
  ( ProjectBase(..)  -- Todo remove inline
  , YamlFilePath(..) -- Todo remove inline
  , Conf(..) -- Todo remove inline
  , mkConf -- Todo remove inline
  ) where

import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Random as IO

newtype ProjectBase = ProjectBase  -- from the project that surounds the cabal package
  { projectBase :: FilePath
  } deriving (Eq, Show)

newtype YamlFilePath = YamlFilePath   -- from the project that surounds the cabal package
  { projectBase :: FilePath
  } deriving (Eq, Show)

data Conf = Conf
  { tempAbsPath :: FilePath
  , base :: FilePath -- -- from the project that surounds the cabal package
  , configurationTemplate :: FilePath -- from the project that surounds the cabal package
  , testnetMagic :: Int
  } deriving (Eq, Show)

mkConf :: ProjectBase -> YamlFilePath -> FilePath -> Maybe Int -> H.Integration Conf
mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tmpPath maybeMagic = do
  testnetMagic <- maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  let tempAbsPath = tmpPath
  H.noteShow_ testnetMagic
  return $ Conf {..}
