module Test.Scenarios.OptParseTesters
  ( cardanoCLI
  , executeClientCommandParser
  , fileCleanup
  , renderHelp
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Data.Text as Text
import           Options.Applicative
import qualified Options.Applicative as Opt
import           Options.Applicative.Help.Chunk
import           Options.Applicative.Help.Pretty
import           System.Directory (removeFile)

import           Cardano.CLI.Parsers (opts, pref)
import           Cardano.CLI.Run (ClientCommand(..),
                   renderClientCommandError, runClientCommand)

import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)

-- | Purely evalutate the cardano-cli parser.
-- e.g cardanoCLI ["shelley"] would be equivalent to cabal exec cardano-cli shelley
cardanoCLI :: [String] -> Opt.ParserResult ClientCommand
cardanoCLI args = Opt.execParserPure pref opts args

customHelpText :: ParserHelp -> Doc
customHelpText (ParserHelp e s h _ b f) = extractChunk . vsepChunks $ [e, s, h, b, f]

-- | Convert a help text to 'String'.
renderHelp :: Int -> ParserHelp -> String -> String
renderHelp cols pHelp testName =
  ((`displayS` "") . renderPretty 1.0 cols $ customHelpText pHelp) ++ "\n\n" ++ testName

-- | This will render incorrectly entered cli commands
executeClientCommandParser :: String -> Opt.ParserResult ClientCommand -> H.PropertyT IO ()
executeClientCommandParser _ (Success cmd) = evaluateExceptT cmd
executeClientCommandParser cmdName (Failure failure) = do
  let (parserHelp, _exitCode, cols) = execFailure failure cmdName
      helpMessage = renderHelp cols parserHelp cmdName
  failWith Nothing helpMessage
executeClientCommandParser cmdName (CompletionInvoked compl) = do
    msg <- lift $ execCompletion compl cmdName
    failWith Nothing msg

evaluateExceptT :: ClientCommand -> H.PropertyT IO ()
evaluateExceptT cmd = do e <- lift . runExceptT $ runClientCommand cmd
                         case e of
                           Left cmdErrors -> failWith Nothing . Text.unpack $ renderClientCommandError cmdErrors
                           Right _ -> H.success

-- Can be improved to catch exceptions
fileCleanup :: [FilePath] -> IO ()
fileCleanup fps = mapM_ removeFile fps
