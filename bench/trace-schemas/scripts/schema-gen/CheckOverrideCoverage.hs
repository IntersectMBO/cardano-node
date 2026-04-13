{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.List (isPrefixOf, isSuffixOf, sort)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.Process (proc, readCreateProcessWithExitCode)

generatedRoots :: [FilePath]
generatedRoots =
  [ "bench/trace-schemas/messages"
  , "bench/trace-schemas/types"
  ]

overridesRoot :: FilePath
overridesRoot = "bench/trace-schemas/overrides"

newtype Config = Config
  { cfgRange :: Maybe String
  }

defaultConfig :: Config
defaultConfig = Config {cfgRange = Nothing}

main :: IO ()
main = do
  config <- parseArgs defaultConfig =<< getArgs
  generatedChanged <- listChangedPaths config generatedRoots
  overrideChanged <- Set.fromList <$> listChangedPaths config [overridesRoot]

  let generatedChangedSet = Set.fromList (filter isGeneratedJson generatedChanged)
  let requiredOverrideFiles = Set.map generatedToOverride generatedChangedSet
  let missing = sort (Set.toList (Set.difference requiredOverrideFiles overrideChanged))

  when (null generatedChangedSet) $ do
    putStrLn "No generated trace-schema files changed."
    exitSuccess

  unless (null missing) $ do
    putStrLn "Generated schema files changed without matching override sidecar updates:"
    mapM_ (\fp -> putStrLn $ "  " <> fp) missing
    putStrLn ""
    putStrLn "Update corresponding files under bench/trace-schemas/overrides/."
    exitFailure

  putStrLn "Override coverage check passed."
  exitSuccess

parseArgs :: Config -> [String] -> IO Config
parseArgs = go
 where
  go cfg [] = pure cfg
  go cfg ("--range" : r : rest) = go cfg {cfgRange = Just r} rest
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
      [ "Usage: runghc bench/trace-schemas/scripts/schema-gen/CheckOverrideCoverage.hs [options]"
      , ""
      , "Options:"
      , "  --range GIT_RANGE   Diff range to inspect, e.g. origin/master...HEAD"
      ]

listChangedPaths :: Config -> [FilePath] -> IO [FilePath]
listChangedPaths config paths = do
  let baseArgs =
        case cfgRange config of
          Just r -> ["diff", "--name-only", r, "--"]
          Nothing -> ["diff", "--name-only", "--"]
      args = baseArgs <> paths
  (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc "git" args) ""
  case exitCode of
    ExitSuccess -> pure (filter (not . null) (lines stdoutText))
    ExitFailure _ -> do
      unless (null stdoutText) (putStr stdoutText)
      unless (null stderrText) (putStr stderrText)
      exitFailure

isGeneratedJson :: FilePath -> Bool
isGeneratedJson path =
  any (`isPrefixOf` path) generatedRoots
    && ".schema.json" `isSuffixOf` path

generatedToOverride :: FilePath -> FilePath
generatedToOverride generatedPath =
  case stripPrefixPath "bench/trace-schemas/" generatedPath of
    Just rel ->
      let withoutSuffix = take (length rel - length ".json") rel
       in overridesRoot <> "/" <> withoutSuffix <> ".override.json"
    Nothing -> generatedPath

stripPrefixPath :: FilePath -> FilePath -> Maybe FilePath
stripPrefixPath prefix path =
  if prefix `isPrefixOf` path
    then Just (drop (length prefix) path)
    else Nothing
