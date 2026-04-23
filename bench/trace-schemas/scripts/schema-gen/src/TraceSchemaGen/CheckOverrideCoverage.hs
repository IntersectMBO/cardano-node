{-# LANGUAGE ScopedTypeVariables #-}

module TraceSchemaGen.CheckOverrideCoverage
  ( Config (..)
  , defaultConfig
  , generatedRoots
  , generatedToOverride
  , isGeneratedJson
  , listChangedPaths
  , main
  , overridesRoot
  ) where

import Control.Monad (unless, when)
import Data.List (isPrefixOf, isSuffixOf, sort, stripPrefix)
import Options.Applicative
  ( Parser
  , ParserInfo
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , optional
  , progDesc
  , strOption
  , (<**>)
  )
import qualified Data.Set as Set
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (dropExtension)
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
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {cfgRange = Nothing}

main :: IO ()
main = do
  config <- execParser parserInfo
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

configParser :: Parser Config
configParser =
  Config
    <$> optional
      ( strOption
          ( long "range"
         <> metavar "GIT_RANGE"
         <> help "Diff range to inspect, e.g. origin/master...HEAD"
          )
      )

parserInfo :: ParserInfo Config
parserInfo =
  info
    (configParser <**> helper)
    ( fullDesc
   <> progDesc "Check that changed generated schemas have matching override updates"
    )

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
  case stripPrefix "bench/trace-schemas/" generatedPath of
    Just rel ->
      let withoutSuffix = dropExtension rel
       in overridesRoot <> "/" <> withoutSuffix <> ".override.json"
    Nothing -> generatedPath
