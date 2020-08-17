{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Chairman.Base
  ( propertyOnce
  , failWithCustom
  , threadDelay
  , workspace
  , moduleWorkspace
  , createDirectoryIfMissing
  , copyFile
  , noteShow
  , noteShowM
  , noteShowIO
  , noteTempFile
  , assertByDeadlineIO
  , showUTCTimeSeconds
  , Integration
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Bool
import           Data.Either (Either (..))
import           Data.Eq
import           Data.Function (($), (.))
import           Data.Int
import           Data.Maybe (Maybe (..), listToMaybe, maybe)
import           Data.Monoid (Monoid (..))
import           Data.Ord
import           Data.Semigroup (Semigroup (..))
import           Data.String (String)
import           Data.Time.Clock (UTCTime)
import           Data.Tuple
import           GHC.Stack (CallStack, HasCallStack, callStack, getCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)
import           Prelude (floor)
import           System.IO (FilePath, IO)
import           Text.Show

import qualified Control.Concurrent as IO
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Clock.POSIX as DTC
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Directory as IO
import qualified System.Info as IO
import qualified System.IO.Temp as IO

type Integration a = H.PropertyT (ResourceT IO) a

propertyOnce :: HasCallStack => Integration () -> H.Property
propertyOnce = H.withTests 1 . H.property . hoist runResourceT

threadDelay :: Int -> Integration ()
threadDelay = H.evalM . liftIO . IO.threadDelay

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
workspace :: HasCallStack => FilePath -> (FilePath -> Integration ()) -> Integration ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  systemTemp <- H.evalM . liftIO $ IO.getCanonicalTemporaryDirectory
  let systemPrefixPath = systemTemp <> "/" <> prefixPath
  H.evalM . liftIO $ IO.createDirectoryIfMissing True systemPrefixPath
  ws <- H.evalM . liftIO $ IO.createTempDirectory systemPrefixPath "test"
  H.annotate $ "Workspace: " <> ws
  f ws
  when (IO.os /= "mingw32") . H.evalM . liftIO $ IO.removeDirectoryRecursive ws

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the prefix as "$prefixPath/$moduleName" but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
moduleWorkspace :: HasCallStack => FilePath -> (FilePath -> Integration ()) -> Integration ()
moduleWorkspace prefixPath f = GHC.withFrozenCallStack $ do
  let srcModule = maybe "UnknownModule"  (GHC.srcLocModule . snd) (listToMaybe (getCallStack callStack))
  workspace (prefixPath <> "/" <> srcModule) f

createDirectoryIfMissing :: HasCallStack => FilePath -> Integration ()
createDirectoryIfMissing filePath = H.evalM . liftIO $ IO.createDirectoryIfMissing True filePath

copyFile :: HasCallStack => FilePath -> FilePath -> Integration ()
copyFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copy from " <> show src <> " to " <> show dst
  H.evalM . liftIO $ IO.copyFile src dst

noteShow :: (HasCallStack, Show a) => a -> Integration a
noteShow a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  H.annotateShow b
  return b

noteShowM :: (HasCallStack, Show a) => Integration a -> Integration a
noteShowM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  H.annotateShow b
  return b

noteShowIO :: (HasCallStack, Show a) => IO a -> Integration a
noteShowIO a = GHC.withFrozenCallStack $ do
  !b <- H.evalM . liftIO $ a
  H.annotateShow b
  return b

-- | Return the test file path after annotating it relative to the project root directory
noteTempFile :: (Monad m, HasCallStack) => FilePath -> FilePath -> H.PropertyT m FilePath
noteTempFile tempDir filePath = GHC.withFrozenCallStack $ do
  let relPath = tempDir <> "/" <> filePath
  H.annotate relPath
  return relPath

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
assertByDeadlineIO :: (MonadIO m, HasCallStack) => UTCTime -> IO Bool -> H.PropertyT m ()
assertByDeadlineIO deadline f = GHC.withFrozenCallStack $ do
  success <- liftIO f
  unless success $ do
    currentTime <- liftIO DTC.getCurrentTime
    if currentTime < deadline
      then do
        liftIO $ IO.threadDelay 1000000
        assertByDeadlineIO deadline f
      else do
        H.annotateShow currentTime
        failWithCustom GHC.callStack Nothing "Condition not met by deadline"

-- | Show 'UTCTime' in seconds since epoch
showUTCTimeSeconds :: UTCTime -> String
showUTCTimeSeconds time = show @Int64 (floor (DTC.utcTimeToPOSIXSeconds time))
