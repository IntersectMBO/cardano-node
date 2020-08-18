{-# LANGUAGE TypeApplications #-}

module Test.OptParse
  ( checkTextEnvelopeFormat
  , assertEndsWithSingleNewline
  , assertFileLines
  , assertFileOccurences
  , assertFilesExist
  , equivalence
  , execCardanoCLI
  , formatIso8601
  , propertyOnce
  , workspace
  , moduleWorkspace
  , withSnd
  , newFileWithContents
  , noteEval
  , noteEvalM
  , noteInputFile
  , noteTempFile
  , readFile
  ) where

import           Cardano.Api.TextView (TextView (..), TextViewError, TextViewType (..))
import           Cardano.Api.Typed (FileError, displayError, readTextEnvelopeOfTypeFromFile)
import           Cardano.Prelude hiding (lines, readFile, stderr, stdout)
import           Hedgehog.Internal.Property (Diff, MonadTest, liftTest, mkTest)
import           Hedgehog.Internal.Show (ValueDiff (ValueSame), mkValue, showPretty, valueDiff)
import           Hedgehog.Internal.Source (getCaller)
import           Prelude (String)
import           System.Directory (doesFileExist)

import qualified Control.DeepSeq as CSD
import qualified Control.Exception as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DT
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import qualified Prelude
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.IO as IO
import qualified System.IO.Temp as IO
import qualified System.Process as IO

-- | Format argument for a shell CLI command.
--
-- This includes automatically embedding string in double quotes if necessary, including any necessary escaping.
--
-- Note, this function does not cover all the edge cases for shell processing, so avoid use in production code.
argQuote :: String -> String
argQuote arg = if ' ' `elem` arg || '"' `elem` arg || '$' `elem` arg
  then "\"" <> escape arg <> "\""
  else arg
  where escape :: String -> String
        escape ('"':xs) = '\\':'"':escape xs
        escape ('\\':xs) = '\\':'\\':escape xs
        escape ('\n':xs) = '\\':'n':escape xs
        escape ('\r':xs) = '\\':'r':escape xs
        escape ('\t':xs) = '\\':'t':escape xs
        escape ('$':xs) = '\\':'$':escape xs
        escape (x:xs) = x:escape xs
        escape "" = ""

-- | Execute cardano-cli via the command line.
--
-- Waits for the process to finish and returns the stdout.
execCardanoCLI
  :: HasCallStack
  => [String]
  -- ^ Arguments to the CLI command
  -> H.PropertyT IO String
  -- ^ Captured stdout
execCardanoCLI arguments = do
  maybeCardanoCli <- liftIO $ IO.lookupEnv "CARDANO_CLI"
  (exitResult, stdout, stderr) <- case maybeCardanoCli of
    Just cardanoCli -> liftIO $ IO.readProcessWithExitCode cardanoCli arguments ""
    Nothing -> liftIO $ IO.readProcessWithExitCode "cabal" ("exec":"--":"cardano-cli":arguments) ""
  case exitResult of
    IO.ExitFailure exitCode -> failWithCustom callStack Nothing . Prelude.unlines $
      [ "Process exited with non-zero exit-code"
      , "━━━━ command ━━━━"
      , "cardano-cli " <> intercalate " " (fmap argQuote arguments)
      , "━━━━ stdout ━━━━"
      , stdout
      , "━━━━ stderr ━━━━"
      , stderr
      , "━━━━ exit code ━━━━"
      , show @Int exitCode
      ]
    IO.ExitSuccess -> return stdout

-- | Checks that the 'tvType' and 'tvDescription' are equivalent between two files.
checkTextEnvelopeFormat
  :: HasCallStack
  => TextViewType
  -> FilePath
  -> FilePath
  -> H.PropertyT IO ()
checkTextEnvelopeFormat tve reference created = do

  eRefTextEnvelope <- liftIO $ readTextEnvelopeOfTypeFromFile tve reference
  refTextEnvelope <- handleTextEnvelope eRefTextEnvelope

  eCreatedTextEnvelope <- liftIO $ readTextEnvelopeOfTypeFromFile tve created
  createdTextEnvelope <- handleTextEnvelope eCreatedTextEnvelope

  typeTitleEquivalence refTextEnvelope createdTextEnvelope
 where
   handleTextEnvelope :: Either (FileError TextViewError) TextView -> H.PropertyT IO TextView
   handleTextEnvelope (Right refTextEnvelope) = return refTextEnvelope
   handleTextEnvelope (Left fileErr) = failWithCustom callStack Nothing . displayError $ fileErr

   typeTitleEquivalence :: TextView -> TextView -> H.PropertyT IO ()
   typeTitleEquivalence (TextView refType refTitle _) (TextView createdType createdTitle _) = do
     equivalence refType createdType
     equivalence refTitle createdTitle

--------------------------------------------------------------------------------
-- Helpers, Error rendering & Clean up
--------------------------------------------------------------------------------

cardanoCliPath :: FilePath
cardanoCliPath = "cardano-cli"

-- | Evaluate the value 'f' and annotate the value returned.
noteEval :: (Show a, Monad m, HasCallStack) => a -> H.PropertyT m a
noteEval a = withFrozenCallStack (H.annotateShow a >> pure a)

-- | Run the computation 'f' and annotate the value returned.
noteEvalM :: (Show a, Monad m, HasCallStack) => H.PropertyT m a -> H.PropertyT m a
noteEvalM f = withFrozenCallStack $ do
  a <- f
  H.annotateShow a
  return a

-- | Return the input file path after annotating it relative to the project root directory
noteInputFile :: (Monad m, HasCallStack) => FilePath -> H.PropertyT m FilePath
noteInputFile filePath = withFrozenCallStack $ do
  H.annotate $ cardanoCliPath <> "/" <> filePath
  return filePath

-- | Return the test file path after annotating it relative to the project root directory
noteTempFile :: (Monad m, HasCallStack) => FilePath -> FilePath -> H.PropertyT m FilePath
noteTempFile tempDir filePath = withFrozenCallStack $ do
  let relPath = tempDir <> "/" <> filePath
  H.annotate $ cardanoCliPath <> "/" <> relPath
  return relPath

-- | Create a new file with the given text contents at the specified path
newFileWithContents :: MonadIO m => FilePath -> String -> m FilePath
newFileWithContents filePath contents = liftIO $ IO.writeFile filePath contents >> return filePath

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace :: HasCallStack => FilePath -> (FilePath -> H.PropertyT IO ()) -> H.PropertyT IO ()
workspace prefixPath f = withFrozenCallStack $ do
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
moduleWorkspace prefixPath f = withFrozenCallStack $ do
  let srcModule = M.fromMaybe "UnknownModule" (fmap (GHC.srcLocModule . snd) (M.listToMaybe (getCallStack callStack)))
  workspace (prefixPath <> "/" <> srcModule) f

-- | Return the supply value with the result of the supplied function as a tuple
withSnd :: (a -> b) -> a -> (a, b)
withSnd f a = (a, f a)

-- | Format the given time as an ISO 8601 date-time string
formatIso8601 :: DT.UTCTime -> String
formatIso8601 = DT.formatTime DT.defaultTimeLocale (DT.iso8601DateFormat (Just "%H:%M:%SZ"))

-- | Assert the file contains the given number of occurences of the given string
readFile :: HasCallStack => FilePath -> H.PropertyT IO String
readFile filename = withFrozenCallStack $ H.evalM . liftIO $ E.evaluate . CSD.force =<< IO.readFile filename

-- | Checks if all files gives exists. If this fails, all files are deleted.
assertFilesExist :: HasCallStack => [FilePath] -> H.PropertyT IO ()
assertFilesExist [] = return ()
assertFilesExist (file:rest) = do
  exists <- liftIO $ doesFileExist file
  if exists == True
    then withFrozenCallStack $ assertFilesExist rest
    else failWithCustom callStack Nothing (file <> " has not been successfully created.")

-- | Assert the file contains the given number of occurences of the given string
assertFileOccurences :: HasCallStack => Int -> String -> FilePath -> H.PropertyT IO ()
assertFileOccurences n s fp = withFrozenCallStack $ do
  contents <- H.evalM . liftIO $ IO.readFile fp

  length (filter (s `L.isInfixOf`) (L.lines contents)) H.=== n

-- | Assert the file contains the given number of occurences of the given string
assertFileLines :: HasCallStack => (Int -> Bool) -> FilePath -> H.PropertyT IO ()
assertFileLines p fp = withFrozenCallStack $ do
  contents <- H.evalM . liftIO $ IO.readFile fp

  let lines = L.lines contents

  let len = case reverse lines of
        "":xs -> length xs
        xs -> length xs

  when (not (p len)) $ do
    failWithCustom callStack Nothing (fp <> " has an unexpected number of lines")

-- | Assert the file contains the given number of occurences of the given string
assertEndsWithSingleNewline :: HasCallStack => FilePath -> H.PropertyT IO ()
assertEndsWithSingleNewline fp = withFrozenCallStack $ do
  contents <- H.evalM . liftIO $ IO.readFile fp

  case reverse contents of
    '\n':'\n':_ -> failWithCustom callStack Nothing (fp <> " ends with too many newlines.")
    '\n':_ -> return ()
    _ -> failWithCustom callStack Nothing (fp <> " must end with newline.")

-- These were lifted from hedgehog and slightly modified

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce =  H.withTests 1 . H.property

-- | Check for equivalence between two types and perform a file cleanup on failure.
equivalence
  :: (Eq a, Show a, HasCallStack)
  => a
  -> a
  -> H.PropertyT IO ()
equivalence x y = do
  ok <- H.eval (x == y)
  if ok
    then H.success
    else failDiffCustom callStack x y

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg =
  liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Fails with an error that shows the difference between two values.
failDiffCustom :: Show a => CallStack -> a -> a -> H.PropertyT IO ()
failDiffCustom cS x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      withFrozenCallStack $
        failWithCustom cS Nothing $
        Prelude.unlines $ [
            "Failed"
          , "━━ lhs ━━"
          , showPretty x
          , "━━ rhs ━━"
          , showPretty y
          ]

    Just vdiff@(ValueSame _) ->
      withFrozenCallStack $
        failWithCustom cS (Just $
          H.Diff "━━━ Failed ("  "" "no differences" "" ") ━━━" vdiff) ""

    Just vdiff ->
      withFrozenCallStack $
        failWithCustom cS (Just $
          H.Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" vdiff) ""
