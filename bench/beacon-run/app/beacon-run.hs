{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO --
-- * disered output formats
-- * publishing option?
-- * running as a service?
-- * haskell92 support in consensus


-- | This program compares two versions of Consensus through the 'db-analyser' tool.
--
-- Given two versions 'A' and 'B', which can be specified as branches or
-- commits, this program performs the following steps:
--
-- 0. Install versions 'A' and 'B' 'db-analyzer'. We assume version 'A' to be
--    the "baseline" version (see function 'compareMeasurements' below).
--
-- 1. Run a given benchmark (analysis) using both versions of 'db-analyzer'.
--    This benchmark is expected to produce a CVS file which contain data points
--    per slot. Each data point represents the measurement of a metric we're
--    interested in, for instance time spent applying a block, or the memory
--    that was consumed by said operation. Each column of the CSV file represents
--    either the slots that were analyzed or the measurements of a metric.
--
-- 2. Compare both CSV files obtained in the previous step and summarize the
--    results. The results are summarized using text and plots. See below for
--    more details on how we compare the two benchmarks.
--
-- * Analysis
--
-- At the moment we only compare the results of the 'benchmark-ledger-ops'
-- 'db-analyser' analysis. See the documentation of this flag for more details.
-- We might add other 'db-analyser' analysis in the future.
--
-- * Caveats
--
-- - The tool is fragile because it assumes the resulting CSV file has certain
--   headers, which depend on the output of 'db-analyser'. If the latter tool
--   changes the name of a header this tool will fail. Using 'db-analyser' as a
--   library might help mitigating this problem, however we first need to assess
--   the added value of this tool.
-- - Works on Unix systems only.
--
-- * TODOs
--
-- - [ ] Create a markdown report.
-- - [ ] Return an error if the threshold is exceeded.
-- - [ ] Allow to configure metrics information (eg "lower is better", pretty name, etc).
-- - [ ] Perform a statistical analysis on the measurements.
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket_)
import           Control.Monad (foldM_, forM_, unless, when)
import           Control.Monad.Extra (ifM, unlessM)
import           Data.Aeson (eitherDecodeFileStrict, eitherDecodeStrict', encodeFile)
import           Data.Either (rights)
import           Data.List (sort)
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.Time.Clock (getCurrentTime)
import           Data.Version (showVersion)
import           Text.Printf
import           Text.Read (readMaybe)

import           Network.HostName
import           System.Directory
import           System.Environment (getExecutablePath)
import           System.FilePath

import           Cardano.Beacon.Chain
import           Cardano.Beacon.CLI
import           Cardano.Beacon.Compare
import           Cardano.Beacon.Console
import           Cardano.Beacon.Run
import           Cardano.Beacon.Types

import qualified Paths_beacon_run as Paths (version)


--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn appHeader
  (options, commands) <- getOpts

  hostName <-
    let machId = optMachineId options
    in if null machId then getHostName else pure machId
  let env = envEmpty options { optMachineId = hostName }

  ifM (doesDirectoryExist $ optBeaconDir options)
    (runCommands env commands)
    (printFatalAndDie $ "beacon data directory missing: " ++ optBeaconDir options)


-- constants

chainRegisterFilename :: FilePath
chainRegisterFilename = "chain" </> "chain-register.json"


runCommands :: RunEnvironment -> [BeaconCommand] -> IO ()
runCommands env cmds
  | null lockFile = evalCommands
  | otherwise = do
      waitForLock True
      bracket_ createLock removeLock evalCommands
  where
    evalCommands  = foldM_ runCommand env cmds
    lockFile      = optLockFile $ runOptions env
    createLock    = getExecutablePath >>= writeFile lockFile
    removeLock    = removeFile lockFile

    waitForLock firstCheck = do
      locked <- doesFileExist lockFile
      when locked $ do
        when firstCheck $
          printStyled StyleInfo $ "waiting for lock to be released: " ++ lockFile
        threadDelay 1_000_000
        waitForLock False

runCommand :: RunEnvironment -> BeaconCommand -> IO RunEnvironment
runCommand env BeaconListChains = do
  env' <- runCommand env BeaconLoadChains
  case runChains env' of
    Nothing -> printStyled StyleWarning $
         "in: " ++ registerFile ++ "\n"
      ++ "    no registered chain fragments found"
    Just cs -> do
      mapM_ (printStyled StyleNone) $ renderChainsInfo cs
      printStyled StyleInfo $
           "in:    " ++ registerFile ++ "\n"
        ++ "found: " ++ show (countChains cs) ++ " registered chain fragment(s)"
  pure env'
  where
    registerFile = envBeaconDir env </> chainRegisterFilename

runCommand env BeaconLoadChains =
  case runChains env of
    Nothing -> do
      chains <- loadChainsInfo $ envBeaconDir env </> chainRegisterFilename
      pure $ if countChains chains > 0
        then env{ runChains = Just chains }
        else env
    Just{} -> pure env

runCommand env (BeaconLoadCommit ref) = do
  result <- shellCurlGitHubAPI env $ "/repos/IntersectMBO/ouroboros-consensus/commits/" ++ ref
  case eitherDecodeStrict' result of
    Left{} ->
      printFatalAndDie $ "could not find commit for ref '" ++ ref ++ "' on GitHub"
    Right ci -> do
      printStyled StyleInfo $ "found commit on GitHub: " ++ ciCommitSHA1 ci
      pure env{ runCommit = Just ci }

runCommand env@Env{ runCommit = Nothing } cmd@(BeaconBuild ver) = do
  env' <- runCommand env (BeaconLoadCommit $ verGitRef ver)
  runCommand env' cmd
runCommand env (BeaconBuild ver) = do
  install <- shellNixBuildVersion env ver
  printStyled StyleNone $ "installed binary is: " ++ installPath install
  pure env{ runInstall = Just install }

runCommand env@Env{ runChains = Nothing } cmd@(BeaconDoRun bChain _ _) = do
  env' <- runCommand env BeaconLoadChains
  case runChains env' >>= lookupChain bChain of
    Nothing -> printFatalAndDie $ "requested chain " ++ show bChain ++ " is not registered"
    Just{}  -> runCommand env' cmd
runCommand env@Env{ runInstall = Nothing } cmd@(BeaconDoRun _ ver _) = do
  env' <- runCommand env (BeaconBuild ver)
  runCommand env' cmd
runCommand env@Env{..} (BeaconDoRun bChain ver count) = do
  printStyled StyleInfo "performing run..."

  meta <- mkMeta <$> getCurrentTime
  shellRunDbAnalyser env beaconChain currentData
  encodeFile currentMeta meta
  shellMergeMetaAndData env currentMeta currentData currentRun

  forM_ [currentMeta, currentData]
    removeFile
  _ <- runCommand env (BeaconStoreRun currentRun)

  if count > 1
    then runCommand env (BeaconDoRun bChain ver (count - 1))
    else pure env{ runInstall = Nothing }
  where
    currentData   = envBeaconDir env </> "beacon-slotdata.json"
    currentMeta   = envBeaconDir env </> "beacon-metadata.json"
    currentRun    = envBeaconDir env </> "beacon-result.json"
    beaconChain   = fromJust $ runChains >>= lookupChain bChain
    mkMeta date = BeaconRunMeta {
        commit  = fromJust runCommit
      , version = ver
      , chain   = bChain
      , nixPath = installNixPath $ fromJust runInstall
      , host    = optMachineId runOptions
      , ..
      }

runCommand env (BeaconStoreRun file) = do
  run <- eitherDecodeFileStrict file
  case run of
    Left err -> printFatalAndDie $
      "doesn't seem to be a beacon run result JSON: " ++ file
      ++ "\n" ++ show err
    Right (BeaconRun meta _) -> do
      let slugDir = runDir </> toSlug meta
      createDirectoryIfMissing True slugDir
      target <- nextUnusedFilename slugDir
      let dest = slugDir </> target
      printStyled StyleNone $ "moving file to: " ++ dest
      renameFile file dest
  pure env
  where
    runDir = envBeaconDir env </> "run"

runCommand env (BeaconCompare slugA slugB) = do
  readA <- eitherDecodeFileStrict $ runDir </> slugA </> "run-01.json"
  readB <- eitherDecodeFileStrict $ runDir </> slugB </> "run-01.json"

  case (readA, readB) of
    (Right runA, Right runB) -> doCompare runA runB
    _ -> printFatalAndDie "could not read / parse specified slugs"

  pure env
  where
    runDir = envBeaconDir env </> "run"

runCommand env (BeaconVariance slug) = do
  results <- listDirectory $ runDir </> slug
  parses  <- mapM (\f -> eitherDecodeFileStrict $ runDir </> slug </> f) results
  doVariance $ rights parses
  pure env
  where
    runDir = envBeaconDir env </> "run"



nextUnusedFilename :: FilePath -> IO FilePath
nextUnusedFilename inSlugDir = do
  fileNamesDesc <- reverse . sort <$> listDirectory inSlugDir
  pure
    $ indexedName
    $ maybe 1 (+ 1)
    $ getAlt
    $ mconcat
    $ map (Alt . parseFileName) fileNamesDesc
  where
    indexedName :: Int -> FilePath
    indexedName = printf "run-%03d.json" . max 1 . min 999

    parseFileName fn
      | length fn /= 12 || takeExtension fn /= ".json" = Nothing
      | otherwise = readMaybe $ take 3 $ drop 4 fn

appHeader :: String
appHeader = unlines
  [ "┳┓"
  , "┣┫┏┓┏┓┏┏┓┏┓"
  , "┻┛┗ ┗┻┗┗┛┛┗     v" ++ showVersion Paths.version
  , "Benchmarking, exploration, and analysis of Consensus"
  ]
