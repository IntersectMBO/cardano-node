module Cardano.Timeseries.CLI(Mode(..), Command(..), parseCommand) where
import           Data.Text (Text)
import           Options.Applicative

data Mode = ExecuteQuery Text | ExecuteFile FilePath | Interactive

data Command = Command {
  store :: FilePath,
  mode :: Mode
}

parseExecuteQuery :: Parser Mode
parseExecuteQuery = ExecuteQuery <$> option str (short 'q' <> long "query" <> metavar "QUERY" <> help "Execute the query")

parseExecuteFile :: Parser Mode
parseExecuteFile = ExecuteFile <$> option str (short 'f' <> long "file" <> metavar "FILE" <> help "Execute the file")

parseInteractive :: Parser Mode
parseInteractive = flag' Interactive (short 'i' <> long "interactive" <> help "Enter REPL")

parseMode :: Parser Mode
parseMode = parseExecuteQuery <|> parseExecuteFile <|> parseInteractive

parseStore :: Parser FilePath
parseStore = argument str (metavar "FILE")

parseCommand :: ParserInfo Command
parseCommand = info (Command <$> parseStore <*> parseMode <**> helper)
  (fullDesc <> progDesc "Run a query against a metric store")
