{-# LANGUAGE BangPatterns #-}

module Hedgehog.Extras.Test.Base
  ( propertyOnce

  , workspace
  , moduleWorkspace

  , note
  , note_
  , noteM
  , noteM_
  , noteIO
  , noteIO_

  , noteShow
  , noteShow_
  , noteShowM
  , noteShowM_
  , noteShowIO
  , noteShowIO_

  , noteEach
  , noteEach_
  , noteEachM
  , noteEachM_
  , noteEachIO
  , noteEachIO_

  , noteTempFile

  , failWithCustom
  , failMessage

  , assertByDeadlineIO
  , assertByDeadlineIOFinally
  , assertM
  , assertIO

  , Integration
  , release
  ) where

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (ReleaseKey, ResourceT, runResourceT)
import           Data.Bool
import           Data.Either (Either (..))
import           Data.Eq
import           Data.Foldable
import           Data.Function (($), (.))
import           Data.Maybe (Maybe (..), listToMaybe, maybe)
import           Data.Monoid (Monoid (..))
import           Data.Ord
import           Data.Semigroup (Semigroup (..))
import           Data.String (String)
import           Data.Time.Clock (UTCTime)
import           Data.Traversable
import           Data.Tuple
import           GHC.Stack (CallStack, HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Stock.CallStack
import           Hedgehog.Extras.Stock.Monad
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
import qualified System.IO as IO
import qualified System.IO.Temp as IO

type Integration a = H.PropertyT (ResourceT IO) a

-- | Run a property with only one test.  This is intended for allowing hedgehog
-- to run unit tests.
propertyOnce :: HasCallStack => Integration () -> H.Property
propertyOnce = H.withTests 1 . H.property . hoist runResourceT

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
workspace :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  systemTemp <- H.evalIO IO.getCanonicalTemporaryDirectory
  let systemPrefixPath = systemTemp <> "/" <> prefixPath
  H.evalIO $ IO.createDirectoryIfMissing True systemPrefixPath
  ws <- H.evalIO $ IO.createTempDirectory systemPrefixPath "test"
  H.annotate $ "Workspace: " <> ws
  liftIO $ IO.writeFile (ws <> "/module") callerModuleName
  f ws
  when (IO.os /= "mingw32") . H.evalIO $ IO.removeDirectoryRecursive ws

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the prefix as "$prefixPath/$moduleName" but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
moduleWorkspace :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
moduleWorkspace prefixPath f = GHC.withFrozenCallStack $ do
  let srcModule = maybe "UnknownModule" (GHC.srcLocModule . snd) (listToMaybe (GHC.getCallStack GHC.callStack))
  workspace (prefixPath <> "/" <> srcModule) f

-- | Annotate the given string at the context supplied by the callstack.
noteWithCallstack :: MonadTest m => CallStack -> String -> m ()
noteWithCallstack cs a = H.writeLog $ H.Annotation (getCaller cs) a

-- | Annotate with the given string.
note :: (MonadTest m, HasCallStack) => String -> m String
note a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  noteWithCallstack GHC.callStack b
  return b

-- | Annotate the given string returning unit.
note_ :: (MonadTest m, HasCallStack) => String -> m ()
note_ a = GHC.withFrozenCallStack $ noteWithCallstack GHC.callStack a

-- | Annotate the given string in a monadic context.
noteM :: (MonadTest m, MonadCatch m, HasCallStack) => m String -> m String
noteM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack b
  return b

-- | Annotate the given string in a monadic context returning unit.
noteM_ :: (MonadTest m, MonadCatch m, HasCallStack) => m String -> m ()
noteM_ a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack b
  return ()

-- | Annotate the given string in IO.
noteIO :: (MonadTest m, MonadIO m, HasCallStack) => IO String -> m String
noteIO f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack a
  return a

-- | Annotate the given string in IO returning unit.
noteIO_ :: (MonadTest m, MonadIO m, HasCallStack) => IO String -> m ()
noteIO_ f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack a
  return ()

-- | Annotate the given value.
noteShow :: (MonadTest m, HasCallStack, Show a) => a -> m a
noteShow a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  noteWithCallstack GHC.callStack (show b)
  return b

-- | Annotate the given value returning unit.
noteShow_ :: (MonadTest m, HasCallStack, Show a) => a -> m ()
noteShow_ a = GHC.withFrozenCallStack $ noteWithCallstack GHC.callStack (show a)

-- | Annotate the given value in a monadic context.
noteShowM :: (MonadTest m, MonadCatch m, HasCallStack, Show a) => m a -> m a
noteShowM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (show b)
  return b

-- | Annotate the given value in a monadic context returning unit.
noteShowM_ :: (MonadTest m, MonadCatch m, HasCallStack, Show a) => m a -> m ()
noteShowM_ a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (show b)
  return ()

-- | Annotate the given value in IO.
noteShowIO :: (MonadTest m, MonadIO m, HasCallStack, Show a) => IO a -> m a
noteShowIO f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack (show a)
  return a

-- | Annotate the given value in IO returning unit.
noteShowIO_ :: (MonadTest m, MonadIO m, HasCallStack, Show a) => IO a -> m ()
noteShowIO_ f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack (show a)
  return ()

-- | Annotate the each value in the given traversable.
noteEach :: (MonadTest m, HasCallStack, Show a, Traversable f) => f a -> m (f a)
noteEach as = GHC.withFrozenCallStack $ do
  for_ as $ noteWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable returning unit.
noteEach_ :: (MonadTest m, HasCallStack, Show a, Traversable f) => f a -> m ()
noteEach_ as = GHC.withFrozenCallStack $ for_ as $ noteWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable in a monadic context.
noteEachM :: (MonadTest m, HasCallStack, Show a, Traversable f) => m (f a) -> m (f a)
noteEachM f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ noteWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable in a monadic context returning unit.
noteEachM_ :: (MonadTest m, HasCallStack, Show a, Traversable f) => m (f a) -> m ()
noteEachM_ f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ noteWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable in IO.
noteEachIO :: (MonadTest m, MonadIO m, HasCallStack, Show a, Traversable f) => IO (f a) -> m (f a)
noteEachIO f = GHC.withFrozenCallStack $ do
  !as <- H.evalIO f
  for_ as $ noteWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable in IO returning unit.
noteEachIO_ :: (MonadTest m, MonadIO m, HasCallStack, Show a, Traversable f) => IO (f a) -> m ()
noteEachIO_ f = GHC.withFrozenCallStack $ do
  !as <- H.evalIO f
  for_ as $ noteWithCallstack GHC.callStack . show

-- | Return the test file path after annotating it relative to the project root directory
noteTempFile :: (MonadTest m, HasCallStack) => FilePath -> FilePath -> m FilePath
noteTempFile tempDir filePath = GHC.withFrozenCallStack $ do
  let relPath = tempDir <> "/" <> filePath
  H.annotate relPath
  return relPath

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
assertByDeadlineIO :: (MonadTest m, MonadIO m, HasCallStack) => UTCTime -> IO Bool -> m ()
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

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- The action 'g' is run after expiration of the deadline, but before failure allowing for
-- additional annotations to be presented.
--
-- Expiration of the deadline results in an assertion failure
assertByDeadlineIOFinally :: (MonadTest m, MonadIO m, HasCallStack) => UTCTime -> IO Bool -> m () -> m ()
assertByDeadlineIOFinally deadline f g = GHC.withFrozenCallStack $ do
  success <- liftIO f
  unless success $ do
    currentTime <- liftIO DTC.getCurrentTime
    if currentTime < deadline
      then do
        liftIO $ IO.threadDelay 1000000
        assertByDeadlineIO deadline f
      else do
        H.annotateShow currentTime
        g
        failMessage GHC.callStack "Condition not met by deadline"

-- | Run the monadic action 'f' and assert the return value is 'True'.
assertM :: (MonadTest m, HasCallStack) => m Bool -> m ()
assertM f = GHC.withFrozenCallStack $ f >>= H.assert

-- | Run the IO action 'f' and assert the return value is 'True'.
assertIO :: (MonadTest m, MonadIO m, HasCallStack) => IO Bool -> m ()
assertIO f = GHC.withFrozenCallStack $ H.evalIO (forceM f) >>= H.assert

-- | Release the given release key.
release :: (MonadTest m, MonadIO m) => ReleaseKey -> m ()
release k = GHC.withFrozenCallStack . H.evalIO $ IO.release k
