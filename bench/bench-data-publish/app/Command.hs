{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Command
        ( Command (..)
        , Config(..)
        , DBCredentials(..)
        , envVarDBPass
        , envVarDBURI
        , parseCommandLine
        ) where

import           Options.Applicative as Opt


data Command
  = Publish Bool FilePath
  | Import FilePath
  | ImportAll FilePath
  | List
  | Bootstrap String
  | UpdateViews String
  deriving Show

data DBCredentials
  = PostgresURI String
  | DBCreds {
        dbName :: String
      , dbUser :: Maybe String
      , dbPass :: Maybe String
      , dbHost :: Maybe String
      , dbPort :: Maybe Int
      }
  | NoCreds
  deriving Show

data Config
  = Config {
        appCommand  :: Command
      , appDB       :: DBCredentials
      , appDBSchema :: String
      , appForce    :: Bool
      }
  deriving Show

envVarDBPass, envVarDBURI :: String
envVarDBPass    = "BENCH_DATA_PASS"
envVarDBURI     = "BECNH_DATA_PGURI"


parseCommandLine :: IO Config
parseCommandLine =
    Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parseConfig mempty

parseConfig :: Parser Config
parseConfig
  = Config
      <$> parseCommand
      <*> parseDBCredentials
      <*> parseDBSchema
      <*> parseForce
  where
    parseCommand = subparser $ mconcat
      [ cmdParser "import"
          (Import <$> parseRunDirArg)
          "Import/update specified run"
      , cmdParser "import-all"
          (ImportAll <$> strArgument (metavar "PATH" <> help "Path containing benchmarking runs" <> completer (bashCompleter "directory")))
          "Import/update all runs contained in directory"
      , cmdParser "list"
          (pure List)
          "List all runs"
      , cmdParser "publish"
          (Publish True <$> parseRunDirArg)
          "Publish specified run to API"
      , cmdParser "unpublish"
          (Publish False <$> parseRunDirArg)
          "Unpublish specified run from API"
      , cmdParser "bootstrap"
          (Bootstrap <$> strArgument (metavar "ROLE" <> help "Anonymous/read-only role on the DB"))
          "Bootstrap schema onto DB, CLEARING PREVIOUS SCHEMA"
      , cmdParser "update-views"
          (UpdateViews <$> strArgument (metavar "ROLE" <> help "Anonymous/read-only role on the DB"))
          "Update API facing views in the schema only, not touching any tables or stored data"
      ]
    cmdParser cmd parser description = command cmd $ info parser $ progDesc description

parseDBCredentials :: Parser DBCredentials
parseDBCredentials =
    postgresUri <|> parseCreds <|> pure NoCreds
  where
    postgresUri
      = PostgresURI <$> strOption
        ( long "pg-uri"
        <> metavar "URI"
        <> help ("postgres[ql]:// URI of DB (default: $" ++ envVarDBURI ++ ")")
        )
    parseCreds
      = DBCreds
        <$> strOption (long "db" <> help "DB name")
        <*> parseCredAttribute 'u' "DB user name (default: <your login name>)"
        <*> parseCredAttribute 'p' ("DB password (default: $" ++ envVarDBPass ++ ")")
        <*> parseCredAttribute 'h' "DB host (default: localhost)"
        <*> optional (option auto $ short 'P' <> help "DB port (default: 5432)")

parseCredAttribute :: Char -> String -> Parser (Maybe String)
parseCredAttribute c h
  = optional $ strOption (short c <> help h)

parseDBSchema :: Parser String
parseDBSchema
  = strOption
    (short 's' <> long "schema" <> metavar "SCHEMA" <> help "DB schema to use")

parseForce :: Parser Bool
parseForce
  = switch
    (short 'f' <> help "Force destructive operations (e.g. bootstrap)")

parseRunDirArg :: Parser FilePath
parseRunDirArg
  = strArgument
    ( metavar "FILE|PATH"
    <> help "Path of a benchmarking run or its meta.json"
    <> completer (bashCompleter "file")
    )
