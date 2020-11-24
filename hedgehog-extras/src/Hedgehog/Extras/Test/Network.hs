{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Extras.Test.Network
  ( doesFileExists
  , isPortOpen
  , doesSocketExist
  , assertFileExists
  , assertPortOpen
  , assertSocketExists
  , doesSprocketExist
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Int
import           Data.Semigroup
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket, sprocketSystemName)
import           System.IO (FilePath)
import           Text.Show

import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.NamedPipe as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Directory as IO

-- | Test if a file exists
doesFileExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Bool
doesFileExists = GHC.withFrozenCallStack . H.evalIO . IO.doesFileExist

-- | Test if a port is open
isPortOpen :: (MonadTest m, MonadIO m, HasCallStack) => Int -> m Bool
isPortOpen port = GHC.withFrozenCallStack $ do
  H.note_ $ "Port: " <> show port
  H.evalIO $ IO.isPortOpen port

-- | Test if a socket file exists
doesSocketExist :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Bool
doesSocketExist = GHC.withFrozenCallStack . H.evalIO . IO.doesSocketExist

-- | Assert that a file exists
assertFileExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertFileExists = GHC.withFrozenCallStack . H.assertM . doesFileExists

-- | Assert that a port is open
assertPortOpen :: (MonadTest m, MonadIO m, HasCallStack) => Int -> m ()
assertPortOpen = GHC.withFrozenCallStack . H.assertM . isPortOpen

-- | Assert that a socket file exists is open
assertSocketExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertSocketExists = GHC.withFrozenCallStack . H.assertM . doesSocketExist

-- | Test if the sprocket exists
doesSprocketExist :: (MonadTest m, MonadIO m, HasCallStack) => Sprocket -> m Bool
doesSprocketExist socket = GHC.withFrozenCallStack $ do
  waitResult <- H.evalIO . try $ if OS.isWin32
    then IO.doesNamedPipeExist (sprocketSystemName socket)
    else IO.doesSocketExist (sprocketSystemName socket)
  case waitResult of
    Right result -> return result
    Left (e :: IOException) -> do
      H.annotate $ "Error: " <> show e
      return False
