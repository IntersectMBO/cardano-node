module Cardano.Timeseries.CLI(Mode(..), Command(..), parseCommand) where
import           Data.Text (Text)
import           Options.Applicative

data Mode = Execute Text | Interactive

data Command = Command {
  store :: FilePath,
  mode :: Mode
}

parseExecute :: Parser Mode
parseExecute = Execute <$> option str (short 'x' <> long "execute" <> metavar "QUERY" <> help "Execute the query")

parseInteractive :: Parser Mode
parseInteractive = flag' Interactive (short 'i' <> long "interactive" <> help "Enter REPL")

parseMode :: Parser Mode
parseMode = parseExecute <|> parseInteractive

parseStore :: Parser FilePath
parseStore = argument str (metavar "FILE")

parseCommand :: ParserInfo Command
parseCommand = info (Command <$> parseStore <*> parseMode <**> helper)
  (fullDesc <> progDesc "Run a query against a metric store")
