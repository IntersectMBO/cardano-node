{-# LANGUAGE ScopedTypeVariables #-}

module TraceSchemaGen.ValidateTraceSchemas
  ( Config (..)
  , checkInputs
  , defaultConfig
  , defaultMessagesDir
  , defaultMetaSchema
  , listJsonFilesRecursive
  , main
  , parseArgs
  , runValidator
  , validatorArgs
  ) where

import Control.Monad (unless, when)
import Data.List (isSuffixOf)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)

defaultMetaSchema :: FilePath
defaultMetaSchema = "bench/trace-schemas/meta.schema.json"

defaultMessagesDir :: FilePath
defaultMessagesDir = "bench/trace-schemas/messages"

data Config = Config
  { cfgMetaSchema :: FilePath
  , cfgMessagesDir :: FilePath
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { cfgMetaSchema = defaultMetaSchema
    , cfgMessagesDir = defaultMessagesDir
    }

main :: IO ()
main = do
  config <- parseArgs defaultConfig =<< getArgs
  checkInputs config
  schemaFiles <- listJsonFilesRecursive (cfgMessagesDir config)
  when (null schemaFiles) $ do
    putStrLn $ "No schema files found under " <> cfgMessagesDir config
    exitFailure

  runValidator "Validating meta schema..." (validatorArgs ["--check-metaschema", cfgMetaSchema config])
  runValidator
    ("Validating " <> show (length schemaFiles) <> " trace schema files...")
    (validatorArgs (["--schemafile", cfgMetaSchema config] <> schemaFiles))

  putStrLn $
    "Validated "
      <> show (length schemaFiles)
      <> " trace schema file(s) against "
      <> cfgMetaSchema config
  exitSuccess

parseArgs :: Config -> [String] -> IO Config
parseArgs = go
 where
  go cfg [] = pure cfg
  go cfg ("--meta-schema" : path : rest) = go cfg {cfgMetaSchema = path} rest
  go cfg ("--messages-dir" : path : rest) = go cfg {cfgMessagesDir = path} rest
  go _ ["--help"] = printHelp >> exitSuccess
  go _ ["-h"] = printHelp >> exitSuccess
  go _ unknown = do
    putStrLn $ "Unrecognized arguments: " <> unwords unknown
    printHelp
    exitFailure

printHelp :: IO ()
printHelp =
  putStrLn $
    unlines
      [ "Usage: runghc bench/trace-schemas/scripts/schema-gen/ValidateTraceSchemas.hs [options]"
      , ""
      , "Options:"
      , "  --meta-schema PATH   Path to the meta schema."
      , "  --messages-dir PATH  Directory containing trace schema files."
      ]

checkInputs :: Config -> IO ()
checkInputs config = do
  metaExists <- doesFileExist (cfgMetaSchema config)
  unless metaExists $ do
    putStrLn $ "Meta schema not found: " <> cfgMetaSchema config
    exitFailure

  messagesExists <- doesDirectoryExist (cfgMessagesDir config)
  unless messagesExists $ do
    putStrLn $ "Messages directory not found: " <> cfgMessagesDir config
    exitFailure

listJsonFilesRecursive :: FilePath -> IO [FilePath]
listJsonFilesRecursive root = do
  entries <- listDirectory root
  paths <- mapM descend entries
  pure (concat paths)
 where
  descend name = do
    let path = root </> name
    isDir <- doesDirectoryExist path
    if isDir
      then listJsonFilesRecursive path
      else pure [path | ".json" `isSuffixOf` path]

validatorArgs :: [String] -> [String]
validatorArgs args =
  [ "run"
  , "nixpkgs#check-jsonschema"
  , "--"
  ]
    <> args

runValidator :: String -> [String] -> IO ()
runValidator message args = do
  putStrLn message
  (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc "nix" args) ""
  case exitCode of
    ExitSuccess -> do
      unless (null stdoutText) (putStr stdoutText)
      unless (null stderrText) (putStr stderrText)
    ExitFailure _ -> do
      unless (null stdoutText) (putStr stdoutText)
      unless (null stderrText) (putStr stderrText)
      exitFailure
