{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Testnet.Conf
  ( Conf(..)
  , mkConf

  , SubmitApiConfig(..)
  ) where

import           Control.Monad
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath)
import           Text.Show

import qualified Hedgehog.Extras.Test.Base as H
import qualified System.FilePath.Posix as FP
import qualified System.Random as IO

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

mkConf :: FilePath -> FilePath -> FilePath -> Maybe Int -> H.Integration Conf
mkConf base configurationTemplate tempAbsPath maybeMagic = do
  testnetMagic <- H.noteShowIO $ maybe (IO.randomRIO (1000, 2000)) return maybeMagic
  tempBaseAbsPath <- H.noteShow $ FP.takeDirectory tempAbsPath
  tempRelPath <- H.noteShow $ FP.makeRelative tempBaseAbsPath tempAbsPath
  socketDir <- H.noteShow $ tempRelPath </> "socket"
  logDir <- H.noteTempFile tempAbsPath "logs"

  return $ Conf {..}

data SubmitApiConfig = SubmitApiConfig
  { tempBaseAbsPath :: FilePath
  , base :: FilePath
  , configFile :: FilePath
  , sprocket  :: Sprocket
  , testnetMagic :: Int
  , stdoutFile :: FilePath
  , stderrFile :: FilePath
  } deriving (Eq, Show)
