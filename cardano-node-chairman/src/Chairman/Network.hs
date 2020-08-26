module Chairman.Network
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
import           System.IO (FilePath)

import qualified Chairman.Base as H
import qualified Chairman.IO.Network.Socket as IO
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified System.Directory as IO

doesFileExists :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m Bool
doesFileExists = GHC.withFrozenCallStack . H.evalIO . IO.doesFileExist

isPortOpen :: (MonadIO m, HasCallStack) => Int -> H.PropertyT m Bool
isPortOpen = GHC.withFrozenCallStack . H.evalIO . IO.isPortOpen

doesSocketExist :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m Bool
doesSocketExist = GHC.withFrozenCallStack . H.evalIO . IO.doesSocketExist

assertFileExists :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m ()
assertFileExists = H.assertM . doesFileExists

assertPortOpen :: (MonadIO m, HasCallStack) => Int -> H.PropertyT m ()
assertPortOpen = H.assertM . isPortOpen

assertSocketExists :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m ()
assertSocketExists = H.assertM . doesSocketExist
