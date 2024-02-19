module Cardano.Testnet.Test.Golden.Util
  ( checkTextEnvelopeFormat
  , equivalence
  , execCardanoTestnet
  , execDetailCardanoTestnet
  , tryExecCardanoTestnet
  , propertyOnce
  , withSnd
  , noteInputFile
  , noteTempFile
  ) where

import           Cardano.Api

import           Control.Monad.Catch
import           Data.Function ((&))
import qualified Data.List as List
import           Data.Monoid (Last (..))
import           GHC.Stack (CallStack, HasCallStack)
import qualified GHC.Stack as GHC
import qualified System.Exit as IO
import qualified System.Process as IO
import           System.Process (CreateProcess)

import qualified Hedgehog as H
import           Hedgehog.Extras (ExecConfig)
import qualified Hedgehog.Extras as H
import           Hedgehog.Extras.Test (ExecConfig (..))
import           Hedgehog.Internal.Property (Diff, MonadTest, liftTest, mkTest)
import qualified Hedgehog.Internal.Property as H
import           Hedgehog.Internal.Show (ValueDiff (ValueSame), mkValue, showPretty, valueDiff)
import           Hedgehog.Internal.Source (getCaller)

-- | Execute cardano-testnet via the command line.
--
-- Waits for the process to finish and returns the stdout.
execCardanoTestnet
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the cardano-testnet command
  -> m String
  -- ^ Captured stdout
execCardanoTestnet = GHC.withFrozenCallStack $ H.execFlex "cardano-testnet" "CARDANO_TESTNET"

-- | Execute cardano-testnet via the command line, expecting it to fail.
--
-- Waits for the process to finish and returns the exit code, stdout and stderr.
execDetailCardanoTestnet
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the cardano-testnet command
  -> m (IO.ExitCode, String, String)
  -- ^ Captured stdout
execDetailCardanoTestnet = GHC.withFrozenCallStack $ execDetailFlex H.defaultExecConfig "cardano-testnet" "CARDANO_TESTNET"

procFlex'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the cardano-testnet command
  -> m CreateProcess
  -- ^ Captured stdout
procFlex' execConfig pkg binaryEnv arguments = GHC.withFrozenCallStack . H.evalM $ do
  bin <- H.binFlex pkg binaryEnv
  return (IO.proc bin arguments)
    { IO.env = getLast $ execConfigEnv execConfig
    , IO.cwd = getLast $ execConfigCwd execConfig
    }

execDetailFlex
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -> String
  -> [String]
  -> m (IO.ExitCode, String, String)
execDetailFlex execConfig pkgBin envBin arguments = GHC.withFrozenCallStack $ do
  cp <- procFlex' execConfig pkgBin envBin arguments
  H.annotate . ("Command: " <>) $ case IO.cmdspec cp of
    IO.ShellCommand cmd -> cmd
    IO.RawCommand cmd args -> cmd <> " " <> List.unwords args
  H.evalIO $ IO.readCreateProcessWithExitCode cp ""

tryExecCardanoTestnet
  :: [String]
  -- ^ Arguments to the cardano-testnet command
  -> H.PropertyT IO (Either H.Failure String)
  -- ^ Captured stdout, or error in case of failures
tryExecCardanoTestnet args =
  GHC.withFrozenCallStack (H.execFlex "cardano-testnet" "CARDANO_TESTNET") args
    & H.unPropertyT
    & H.unTest
    & runExceptT
    & lift
    & H.TestT
    & H.PropertyT

-- | Checks that the 'tvType' and 'tvDescription' are equivalent between two files.
checkTextEnvelopeFormat
  :: (MonadTest m, MonadIO m, HasCallStack)
  => TextEnvelopeType
  -> FilePath
  -> FilePath
  -> m ()
checkTextEnvelopeFormat tve reference created = GHC.withFrozenCallStack $ do
  eRefTextEnvelope <- H.evalIO $ readTextEnvelopeOfTypeFromFile tve reference
  refTextEnvelope <- handleTextEnvelope eRefTextEnvelope

  eCreatedTextEnvelope <- H.evalIO $ readTextEnvelopeOfTypeFromFile tve created
  createdTextEnvelope <- handleTextEnvelope eCreatedTextEnvelope

  typeTitleEquivalence refTextEnvelope createdTextEnvelope
 where
   handleTextEnvelope :: MonadTest m
                      => Either (FileError TextEnvelopeError) TextEnvelope
                      -> m TextEnvelope
   handleTextEnvelope (Right refTextEnvelope) = return refTextEnvelope
   handleTextEnvelope (Left fileErr) = failWithCustom GHC.callStack Nothing . docToString . prettyError $ fileErr

   typeTitleEquivalence :: (MonadTest m, HasCallStack) => TextEnvelope -> TextEnvelope -> m ()
   typeTitleEquivalence (TextEnvelope refType refTitle _)
                        (TextEnvelope createdType createdTitle _) = GHC.withFrozenCallStack $ do
     equivalence refType createdType
     equivalence refTitle createdTitle

--------------------------------------------------------------------------------
-- Helpers, Error rendering & Clean up
--------------------------------------------------------------------------------

cardanoTestnetPath :: FilePath
cardanoTestnetPath = "cardano-testnet"

-- | Return the input file path after annotating it relative to the project root directory
noteInputFile :: (MonadTest m, HasCallStack) => FilePath -> m FilePath
noteInputFile filePath = GHC.withFrozenCallStack $ do
  H.annotate $ cardanoTestnetPath <> "/" <> filePath
  return filePath

-- | Return the test file path after annotating it relative to the project root directory
noteTempFile :: (MonadTest m, HasCallStack) => FilePath -> FilePath -> m FilePath
noteTempFile tempDir filePath = GHC.withFrozenCallStack $ do
  let relPath = tempDir <> "/" <> filePath
  H.annotate $ cardanoTestnetPath <> "/" <> relPath
  return relPath

-- | Return the supply value with the result of the supplied function as a tuple
withSnd :: (a -> b) -> a -> (a, b)
withSnd f a = (a, f a)

-- These were lifted from hedgehog and slightly modified

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce =  H.withTests 1 . H.withShrinks 0 . H.property

-- | Check for equivalence between two types and perform a file cleanup on failure.
equivalence
  :: (MonadTest m, Eq a, Show a, HasCallStack)
  => a
  -> a
  -> m ()
equivalence x y = do
  ok <- H.eval (x == y)
  if ok
    then H.success
    else failDiffCustom GHC.callStack x y

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg =
  liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Fails with an error that shows the difference between two values.
failDiffCustom :: (MonadTest m, Show a) => CallStack -> a -> a -> m ()
failDiffCustom cS x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      GHC.withFrozenCallStack $
        failWithCustom cS Nothing $
        Prelude.unlines [
            "Failed"
          , "━━ lhs ━━"
          , showPretty x
          , "━━ rhs ━━"
          , showPretty y
          ]

    Just vdiff@(ValueSame _) ->
      GHC.withFrozenCallStack $
        failWithCustom cS (Just $
          H.Diff "━━━ Failed ("  "" "no differences" "" ") ━━━" vdiff) ""

    Just vdiff ->
      GHC.withFrozenCallStack $
        failWithCustom cS (Just $
          H.Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" vdiff) ""
