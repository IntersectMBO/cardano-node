module Hedgehog.Extras.Test.Concurrent
  ( threadDelay
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Function (($), (.))
import           Data.Int
import           Hedgehog (MonadTest)

import qualified Control.Concurrent as IO
import qualified GHC.Stack as GHC
import qualified Hedgehog as H

-- Delay the thread by 'n' milliseconds.
threadDelay :: (MonadTest m, MonadIO m) => Int -> m ()
threadDelay n = GHC.withFrozenCallStack . H.evalIO $ IO.threadDelay n
