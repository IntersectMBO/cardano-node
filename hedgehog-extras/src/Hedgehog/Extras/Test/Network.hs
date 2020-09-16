module Hedgehog.Extras.Test.Network
  ( doesFileExists
  , isPortOpen
  , doesSocketExist
  , assertFileExists
  , assertPortOpen
  , assertSocketExists
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Bool
import           Data.Function
import           Data.Int
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           System.IO (FilePath)

import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Directory as IO

-- | Test if a file exists
doesFileExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Bool
doesFileExists = GHC.withFrozenCallStack . H.evalIO . IO.doesFileExist

-- | Test if a port is open
isPortOpen :: (MonadTest m, MonadIO m, HasCallStack) => Int -> m Bool
isPortOpen = GHC.withFrozenCallStack . H.evalIO . IO.isPortOpen

-- | Test if a socket file exists
doesSocketExist :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Bool
doesSocketExist = GHC.withFrozenCallStack . H.evalIO . IO.doesSocketExist

-- | Assert that a file exists
assertFileExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertFileExists = H.assertM . doesFileExists

-- | Assert that a port is open
assertPortOpen :: (MonadTest m, MonadIO m, HasCallStack) => Int -> m ()
assertPortOpen = H.assertM . isPortOpen

-- | Assert that a socket file exists is open
assertSocketExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertSocketExists = H.assertM . doesSocketExist
