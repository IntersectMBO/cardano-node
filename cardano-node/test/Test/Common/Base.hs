module Test.Common.Base
  ( propertyOnce
  , failWithCustom
  , threadDelay
  , workspace
  , moduleWorkspace
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Bool
import           Data.Either (Either (..))
import           Data.Function (($), (.))
import           Data.Functor
import           Data.Int
import           Data.Maybe (Maybe (..), fromMaybe, listToMaybe)
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
import           Data.String (String)
import           Data.Tuple
import           GHC.Stack (CallStack, HasCallStack, callStack, getCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)
import           System.IO (FilePath, IO)

import qualified Control.Concurrent as IO
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Directory as IO
import qualified System.IO.Temp as IO

cardanoCliPath :: FilePath
cardanoCliPath = "cardano-cli"

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce = H.withTests 1 . H.property

threadDelay :: Int -> H.PropertyT IO ()
threadDelay = liftIO . IO.threadDelay

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg = liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace :: HasCallStack => FilePath -> (FilePath -> H.PropertyT IO ()) -> H.PropertyT IO ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  liftIO $ IO.createDirectoryIfMissing True prefixPath
  ws <- liftIO $ IO.createTempDirectory prefixPath "test"
  H.annotate $ "Workspace: " <> cardanoCliPath <> "/" <> ws
  f ws
  liftIO $ IO.removeDirectoryRecursive ws

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the prefix as "$prefixPath/$moduleName" but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
moduleWorkspace :: HasCallStack => FilePath -> (FilePath -> H.PropertyT IO ()) -> H.PropertyT IO ()
moduleWorkspace prefixPath f = GHC.withFrozenCallStack $ do
  let srcModule = fromMaybe "UnknownModule" (fmap (GHC.srcLocModule . snd) (listToMaybe (getCallStack callStack)))
  workspace (prefixPath <> "/" <> srcModule) f
