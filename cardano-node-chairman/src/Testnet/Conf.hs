{-# LANGUAGE RecordWildCards #-}

module Testnet.Conf
  ( Conf(..)
  , mkConf
  ) where

import           Control.Monad
import           Data.Eq
import           Data.Function
import           Data.Int
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath)
import           Text.Show

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.FilePath.Posix as FP

data Conf = Conf
  { tempAbsPath :: FilePath
  , tempRelPath :: FilePath
  , tempBaseAbsPath :: FilePath
  , logDir :: FilePath
  , base :: FilePath
  , socketDir :: FilePath
  , testnetMagic :: Int
  } deriving (Eq, Show)

mkConf :: FilePath -> Int -> H.Integration Conf
mkConf tempAbsPath testnetMagic = do
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  base <- H.noteShowM H.getProjectBase
  socketDir <- H.noteShow $ tempRelPath </> "socket"
  logDir <- H.noteTempFile tempAbsPath "/logs"

  return $ Conf {..}
