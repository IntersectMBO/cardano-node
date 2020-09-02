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
  , createFileLink
  , noteShow
  , noteShow_
  , noteShowM
  , noteShowM_
  , noteShowIO
  , noteShowIO_
  , noteTempFile
  , assertByDeadlineIO
  , showUTCTimeSeconds
  , listDirectory
  , readFile
  , writeFile
  , lbsReadFile
  , lbsWriteFile
  , rewriteJson
  , assertM
  , assertIO
  , Integration
  , forceM
  , release
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (ReleaseKey, ResourceT, runResourceT)
import           Data.Aeson
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
import qualified Control.Monad.Trans.Resource as IO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Clock.POSIX as DTC
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Directory as IO
import qualified System.Info as IO
import qualified System.IO as IO
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

noteShow :: (HasCallStack, Show a) => a -> Integration a
noteShow a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  H.annotateShow b
  return b

noteShow_ :: (HasCallStack, Show a) => a -> Integration ()
noteShow_ = void . noteShow

noteShowM :: (HasCallStack, Show a) => Integration a -> Integration a
noteShowM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  H.annotateShow b
  return b

noteShowM_ :: (HasCallStack, Show a) => Integration a -> Integration ()
noteShowM_ = void . noteShowM

noteShowIO :: (HasCallStack, Show a) => IO a -> Integration a
noteShowIO a = GHC.withFrozenCallStack $ do
  !b <- H.evalM . liftIO $ a
  H.annotateShow b
  return b

noteShowIO_ :: (HasCallStack, Show a) => IO a -> Integration ()
noteShowIO_ = void . noteShowIO

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

createDirectoryIfMissing :: HasCallStack => FilePath -> Integration ()
createDirectoryIfMissing filePath = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating directory if missing: " <> filePath
  H.evalIO $ IO.createDirectoryIfMissing True filePath

copyFile :: HasCallStack => FilePath -> FilePath -> Integration ()
copyFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copying from " <> show src <> " to " <> show dst
  H.evalM . liftIO $ IO.copyFile src dst

createFileLink :: HasCallStack => FilePath -> FilePath -> Integration ()
createFileLink src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating link from " <> show dst <> " to " <> show src
  H.evalM . liftIO $ IO.copyFile src dst

listDirectory :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m [FilePath]
listDirectory p = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Listing directory: " <> p
  H.evalIO $ IO.listDirectory p

writeFile :: (MonadIO m, HasCallStack) => FilePath -> String -> H.PropertyT m ()
writeFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ IO.writeFile filePath contents

readFile :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m String
readFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ IO.readFile filePath

lbsWriteFile :: (MonadIO m, HasCallStack) => FilePath -> LBS.ByteString -> H.PropertyT m ()
lbsWriteFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ LBS.writeFile filePath contents

lbsReadFile :: (MonadIO m, HasCallStack) => FilePath -> H.PropertyT m LBS.ByteString
lbsReadFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ LBS.readFile filePath

rewriteJson :: (MonadIO m, HasCallStack) => FilePath -> (Value -> Value) -> H.PropertyT m ()
rewriteJson filePath f = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Rewriting JSON file: " <> filePath
  lbs <- forceM $ lbsReadFile filePath
  case eitherDecode lbs of
    Right iv -> lbsWriteFile filePath (encode (f iv))
    Left msg -> failWithCustom GHC.callStack Nothing msg

assertM :: (MonadIO m, HasCallStack) => H.PropertyT m Bool -> H.PropertyT m ()
assertM = (>>= H.assert)

assertIO :: (MonadIO m, HasCallStack) => IO Bool -> H.PropertyT m ()
assertIO f = H.evalIO f >>= H.assert

forceM :: (Monad m, NFData a) => m a -> m a
forceM = (force <$!>)

release :: MonadIO m => ReleaseKey -> H.PropertyT m ()
release = H.evalIO . IO.release
