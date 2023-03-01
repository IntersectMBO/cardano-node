module Test.Utilities
  ( diffVsGoldenFile,
    diffFileVsGoldenFile,
  ) where

import           Cardano.Prelude (ConvertText (..), HasCallStack)

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Algorithm.Diff (PolyDiff (Both), getGroupedDiff)
import           Data.Algorithm.DiffOutput (ppDiff)
import           GHC.Stack (callStack)
import qualified GHC.Stack as GHC
import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras.Test as H
import           Hedgehog.Extras.Test.Base (failMessage)
import qualified Hedgehog.Internal.Property as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

-- | Whether the test should create the golden files if the file does ont exist.
createFiles :: Bool
createFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "CREATE_GOLDEN_FILES"
  return $ value == Just "1"

-- | Diff contents against the golden file.  If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the gold file not exist it would be created.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
--
-- TODO: Improve the help output by saying the difference of
-- each input.
diffVsGoldenFile
  :: HasCallStack
  => (MonadIO m, MonadTest m)
  => String   -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> m ()
diffVsGoldenFile actualContent referenceFile = GHC.withFrozenCallStack $ do
  fileExists <- liftIO $ IO.doesFileExist referenceFile

  if fileExists
    then do
      referenceLines <- map toS . lines <$> H.readFile referenceFile
      let difference = getGroupedDiff actualLines referenceLines
      case difference of
        [Both{}] -> pure ()
        _        -> failMessage callStack $ ppDiff difference
    else if createFiles
      then do
        -- CREATE_GOLDEN_FILES is set, so we create any golden files that don't
        -- already exist.
        H.note_ $ "Creating golden file " <> referenceFile
        H.writeFile referenceFile actualContent
      else do
        H.note_ $ mconcat
          [ "Golden file " <> referenceFile
          , " does not exist.  To create, run with CREATE_GOLDEN_FILES=1"
          ]
        H.failure
  where
    actualLines = Prelude.lines actualContent

-- | Diff file against the golden file.  If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the gold file not exist it would be created.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
diffFileVsGoldenFile
  :: HasCallStack
  => (MonadIO m, MonadTest m)
  => FilePath -- ^ Actual file
  -> FilePath -- ^ Reference file
  -> m ()
diffFileVsGoldenFile actualFile referenceFile = GHC.withFrozenCallStack $ do
  contents <- H.readFile actualFile
  diffVsGoldenFile contents referenceFile
