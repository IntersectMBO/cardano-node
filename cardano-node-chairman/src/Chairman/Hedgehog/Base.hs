{-# LANGUAGE BangPatterns #-}

module Chairman.Hedgehog.Base
  ( propertyOnce

  , threadDelay

  , workspace
  , moduleWorkspace

  , noteShow
  , noteShow_
  , noteShowM
  , noteShowM_
  , noteShowIO
  , noteShowIO_
  , noteTempFile

  , failWithCustom
  , failMessage

  , assertByDeadlineIO
  , assertM
  , assertIO

  , Integration
  , release
  ) where

import           Chairman.Monad
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (ReleaseKey, ResourceT, runResourceT)
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
import           GHC.Stack (CallStack, HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)
import           System.IO (FilePath, IO)
import           Text.Show

import qualified Control.Concurrent as IO
import qualified Control.Monad.Trans.Resource as IO
import qualified Data.Time.Clock as DTC
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
threadDelay n = GHC.withFrozenCallStack . H.evalM . liftIO $ IO.threadDelay n

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg = liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failMessage :: MonadTest m => CallStack -> String -> m a
failMessage cs = failWithCustom cs Nothing

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
  systemTemp <- H.evalIO IO.getCanonicalTemporaryDirectory
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
  let srcModule = maybe "UnknownModule" (GHC.srcLocModule . snd) (listToMaybe (GHC.getCallStack GHC.callStack))
  workspace (prefixPath <> "/" <> srcModule) f

noteWithCallstack :: MonadTest m => CallStack -> String -> m ()
noteWithCallstack cs a = H.writeLog $ H.Annotation (getCaller cs) a

noteShow :: (HasCallStack, Show a) => a -> Integration a
noteShow a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  noteWithCallstack GHC.callStack (show b)
  return b

noteShow_ :: (HasCallStack, Show a) => a -> Integration ()
noteShow_ a = GHC.withFrozenCallStack $ noteWithCallstack GHC.callStack (show a)

noteShowM :: (HasCallStack, Show a) => Integration a -> Integration a
noteShowM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (show b)
  return b

noteShowM_ :: (HasCallStack, Show a) => Integration a -> Integration ()
noteShowM_ a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (show b)
  return ()

noteShowIO :: (HasCallStack, Show a) => IO a -> Integration a
noteShowIO f = GHC.withFrozenCallStack $ do
  !a <- H.evalM . liftIO $ f
  noteWithCallstack GHC.callStack (show a)
  return a

noteShowIO_ :: (HasCallStack, Show a) => IO a -> Integration ()
noteShowIO_ f = GHC.withFrozenCallStack $ do
  !a <- H.evalM . liftIO $ f
  noteWithCallstack GHC.callStack (show a)
  return ()

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
        failMessage GHC.callStack "Condition not met by deadline"

assertM :: (MonadIO m, HasCallStack) => H.PropertyT m Bool -> H.PropertyT m ()
assertM f = GHC.withFrozenCallStack $ f >>= H.assert

assertIO :: (MonadIO m, HasCallStack) => IO Bool -> H.PropertyT m ()
assertIO f = GHC.withFrozenCallStack $ H.evalIO (forceM f) >>= H.assert

release :: MonadIO m => ReleaseKey -> H.PropertyT m ()
release k = GHC.withFrozenCallStack . H.evalIO $ IO.release k
