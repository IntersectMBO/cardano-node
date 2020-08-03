module Test.Common.Base
  ( propertyOnce
  , failWithCustom
  , threadDelay
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Either (Either (..))
import           Data.Function (($), (.))
import           Data.Int
import           Data.Maybe (Maybe (..))
import           Data.Monoid (Monoid (..))
import           Data.String (String)
import           GHC.Stack (CallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)
import           System.IO (IO)

import qualified Control.Concurrent as IO
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce = H.withTests 1 . H.property

threadDelay :: Int -> H.PropertyT IO ()
threadDelay = liftIO . IO.threadDelay

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg = liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)
