module Test.Common.Base
  ( propertyOnce
  , threadDelay
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Function (($), (.))
import           Data.Int
import           System.IO (IO)

import qualified Control.Concurrent as IO
import qualified Hedgehog as H

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce = H.withTests 1 . H.property

threadDelay :: Int -> H.PropertyT IO ()
threadDelay n = liftIO $ IO.threadDelay n
