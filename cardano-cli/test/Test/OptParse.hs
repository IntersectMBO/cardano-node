module Test.OptParse
  ( checkTextEnvelopeFormat
  , assertFilesExist
  , evalCardanoCLIParser
  , execCardanoCLIParser
  , fileCleanup
  , propertyOnce
  ) where

import           Cardano.Prelude
import           Prelude (String)
import qualified Prelude as Prelude

import           System.IO.Error
import           Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Data.Text as Text
import           Options.Applicative (ParserHelp(..), ParserResult(..))
import qualified Options.Applicative as Opt
import           Options.Applicative.Help.Chunk
import           Options.Applicative.Help.Pretty
import           System.Directory (doesFileExist, removeFile)

import           Cardano.Api.TextView (TextView(..), TextViewFileError, TextViewType(..),
                   readTextViewFileOfType, renderTextViewFileError)
import           Cardano.CLI.Parsers (opts, pref)
import           Cardano.CLI.Run (ClientCommand(..),
                   renderClientCommandError, runClientCommand)

import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H
import           Hedgehog.Internal.Property (Diff, MonadTest, failWith, liftTest, mkTest)
import           Hedgehog.Internal.Show (ValueDiff(ValueSame), mkValue, showPretty, valueDiff)
import           Hedgehog.Internal.Source (getCaller)

-- | Purely evalutate the cardano-cli parser.
-- e.g evalCardanoCLIParser ["shelley"] would be equivalent to cabal exec cardano-cli shelley
-- without running underlying IO.
evalCardanoCLIParser :: [String] -> Opt.ParserResult ClientCommand
evalCardanoCLIParser args = Opt.execParserPure pref opts args

-- | This takes a 'ParserResult', which is pure, and executes it.
execCardanoCLIParser
  :: HasCallStack
  => [FilePath]
  -- ^ Files to clean up on failure
  -> String
  -- ^ Name of command, used in error rendering
  -> Opt.ParserResult ClientCommand
  -- ^ ParserResult to execute
  -> H.PropertyT IO ()
execCardanoCLIParser fps cmdName pureParseResult =
  case pureParseResult of

    -- The pure 'ParserResult' succeeds and we can then execute the result.
    -- This would be equivalent to `cabal exec cardano-cli ...`
    Success cmd -> execClientCommand callStack fps cmd

    -- The pure `ParserResult` failed and we clean up any created files
    -- and fail with `optparse-applicative`'s error message
    Failure failure -> let (parserHelp, _exitCode, cols) = Opt.execFailure failure cmdName
                           helpMessage = renderHelp cols parserHelp cmdName

                       in liftIO (fileCleanup fps) >> failWithCustom callStack Nothing helpMessage


    CompletionInvoked compl -> do msg <- lift $ Opt.execCompletion compl cmdName
                                  liftIO (fileCleanup fps) >> failWithCustom callStack Nothing msg

-- | Executes a `ClientCommand`. If successful the property passes
-- if not, the property fails and the error is rendered.
execClientCommand
  :: CallStack
  -- ^ CallStack allows us to render the error
  -- at the appropriate function call site
  -> [FilePath]
  -- ^ Files to clean up on failure
  -> ClientCommand
  -> H.PropertyT IO ()
execClientCommand cS fps cmd = do e <- lift . runExceptT $ runClientCommand cmd
                                  case e of
                                    Left cmdErrors -> do
                                      liftIO (fileCleanup fps)
                                      failWithCustom cS Nothing . Text.unpack $ renderClientCommandError cmdErrors
                                    Right _ -> H.success

-- | Checks that the 'tvType' and 'tvTitle' are equivalent between two files.
checkTextEnvelopeFormat
  :: HasCallStack
  => [FilePath]
  -- ^ Files to clean up on failure
  -> TextViewType
  -> FilePath
  -> FilePath
  -> H.PropertyT IO ()
checkTextEnvelopeFormat fps tve reference created = do

  eRefTextEnvelope <- liftIO $ readTextViewFileOfType tve reference
  refTextEnvelope <- handleTextEnvelope eRefTextEnvelope

  eCreatedTextEnvelope <- liftIO $ readTextViewFileOfType tve created
  createdTextEnvelope <- handleTextEnvelope eCreatedTextEnvelope

  typeTitleEquivalence refTextEnvelope createdTextEnvelope
 where
   handleTextEnvelope :: Either TextViewFileError TextView -> H.PropertyT IO TextView
   handleTextEnvelope (Right refTextEnvelope) = return refTextEnvelope
   handleTextEnvelope (Left tvfErr) = do
     liftIO $ fileCleanup fps
     failWithCustom callStack Nothing . Text.unpack $ renderTextViewFileError tvfErr

   typeTitleEquivalence :: TextView -> TextView -> H.PropertyT IO ()
   typeTitleEquivalence (TextView refType refTitle _) (TextView createdType createdTitle _) = do
     equivalence callStack fps refType createdType
     equivalence callStack fps refTitle createdTitle

--------------------------------------------------------------------------------
-- Helpers, Error rendering & Clean up
--------------------------------------------------------------------------------

-- | Checks if all files gives exists. If this fails, all files are deleted.
assertFilesExist :: [FilePath] -> H.PropertyT IO ()
assertFilesExist [] = return ()
assertFilesExist allFiles@(file:rest) = do
  exists <- liftIO $ doesFileExist file
  if exists == True
  then assertFilesExist rest
  else liftIO (fileCleanup allFiles) >> failWith Nothing (file <> " has not been successfully created.")

fileCleanup :: [FilePath] -> IO ()
fileCleanup fps = mapM_ (\fp -> removeFile fp `catch` fileExists) fps
 where
   fileExists e
     | isDoesNotExistError e = return ()
     | otherwise = throwIO e

-- These were lifted from opt-parsers-applicative and slightly modified

customHelpText :: Opt.ParserHelp -> Doc
customHelpText (ParserHelp e s h _ b f) = extractChunk . vsepChunks $ [e, s, h, b, f]


-- | Convert a help text to 'String'.
renderHelp :: Int -> Opt.ParserHelp -> String -> String
renderHelp cols pHelp testName =
  "Failure in: " ++ testName ++ "\n\n" ++ ((`displayS` "") . renderPretty 1.0 cols $ customHelpText pHelp)

-- These were lifted from hedgehog and slightly modified

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce =  H.withTests 1 . H.property

-- | Check for equivalence between two types and perform a file cleanup on failure.
equivalence
  :: (Eq a, Show a)
  => CallStack
  -> [FilePath]
  -- ^ Files to clean up on failure.
  -> a
  -> a
  -> H.PropertyT IO ()
equivalence cS fps x y = do
  ok <- H.eval (x == y)
  if ok then
    H.success
  else do
    liftIO $ fileCleanup fps
    failDiffCustom cS x y

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
