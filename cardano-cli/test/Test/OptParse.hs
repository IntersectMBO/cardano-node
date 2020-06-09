module Test.OptParse
  ( doFilesExist
  , evalCardanoCLIParser
  , execCardanoCLIParser
  , fileCleanup
  , propertyOnce
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           System.IO.Error
import           Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Data.Text as Text
import           Options.Applicative (ParserHelp(..), ParserResult(..))
import qualified Options.Applicative as Opt
import           Options.Applicative.Help.Chunk
import           Options.Applicative.Help.Pretty
import           System.Directory (doesFileExist, removeFile)

import           Cardano.CLI.Parsers (opts, pref)
import           Cardano.CLI.Run (ClientCommand(..),
                   renderClientCommandError, runClientCommand)

import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)

-- | Purely evalutate the cardano-cli parser.
-- e.g evalCardanoCLIParser ["shelley"] would be equivalent to cabal exec cardano-cli shelley
-- without running underlying IO.
evalCardanoCLIParser :: [String] -> Opt.ParserResult ClientCommand
evalCardanoCLIParser args = Opt.execParserPure pref opts args

-- | This takes a 'ParserResult', which is pure, and executes it.
execCardanoCLIParser
  :: [FilePath]
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
    Success cmd -> execClientCommand fps cmd

    -- The pure `ParserResult` failed and we clean up any created files
    -- and fail with `optparse-applicative`'s error message
    Failure failure -> let (parserHelp, _exitCode, cols) = Opt.execFailure failure cmdName
                           helpMessage = renderHelp cols parserHelp cmdName

                       in liftIO (fileCleanup fps) >> failWith Nothing helpMessage


    CompletionInvoked compl -> do msg <- lift $ Opt.execCompletion compl cmdName
                                  liftIO (fileCleanup fps) >> failWith Nothing msg


-- | Executes a `ClientCommand`. If successful the property passes
-- if not, the property fails and the error is rendered.
execClientCommand
  :: [FilePath]
  -- ^ Files to clean up on failure
  -> ClientCommand
  -> H.PropertyT IO ()
execClientCommand fps cmd = do e <- lift . runExceptT $ runClientCommand cmd
                               case e of
                                 Left cmdErrors -> do
                                   liftIO (fileCleanup fps)
                                   failWith Nothing . Text.unpack $ renderClientCommandError cmdErrors
                                 Right _ ->
                                   H.success

--------------------------------------------------------------------------------
-- Error rendering & Clean up
--------------------------------------------------------------------------------

-- | Checks if all files gives exists. If this fails, all files are deleted.
doFilesExist :: [FilePath] -> H.PropertyT IO ()
doFilesExist [] = return ()
doFilesExist allFiles@(file:rest) = do
  exists <- liftIO $ doesFileExist file
  if exists == True
  then doFilesExist rest
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

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce =  H.withTests 1 . H.property
