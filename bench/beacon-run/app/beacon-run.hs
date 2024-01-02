{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Arrow ((>>>))
import           Control.Concurrent (threadDelay)
import           Control.Exception (assert, bracket_)
import           Control.Monad (foldM_, unless, when)
import           Control.Monad.Extra (ifM, unlessM)
import           Data.Aeson (eitherDecodeFileStrict, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.List (findIndex, foldl')
import           Data.Maybe (fromJust)
import           Data.Ord (Down (Down), comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Algorithms.Merge (sortBy)
import           Data.Version (showVersion)
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart.Cairo
import           Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Easy as Chart

import           System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import           System.Environment (getExecutablePath)
import           System.FilePath
import           System.IO (IOMode (ReadMode), openFile)

import           Cardano.Beacon.Chain
import           Cardano.Beacon.CLI
import           Cardano.Beacon.Console
import           Cardano.Beacon.Run
import           Cardano.Beacon.SlotDataPoint
import           Cardano.Beacon.Types

import qualified Paths_beacon_run as Paths (version)


--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn appHeader
  (options, commands) <- getOpts

  let env = envEmpty options

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
        threadDelay $ 1_000_000
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

runCommand env@Env{ runChains = Nothing } cmd@(BeaconRun chain _ _) = do
  env' <- runCommand env BeaconLoadChains
  case runChains env' >>= lookupChain chain of
    Nothing -> printFatalAndDie $ "requested chain " ++ show chain ++ " is not registered"
    Just{}  -> runCommand env' cmd
runCommand env@Env{ runInstall = Nothing } cmd@(BeaconRun _ ver _) = do
  env' <- runCommand env (BeaconBuild ver)
  runCommand env' cmd
runCommand env (BeaconRun chain ver count) = do
  printStyled StyleInfo "performing run..."
  shellRunDbAnalyser env beaconChain currentResult

  sdps <- eitherDecodeFileStrict currentResult
  case sdps of
    Left err -> printStyled StyleWarning $ "error reading slot data points: " ++ err
    Right (xs :: [SlotDataPoint]) -> do
      printStyled StyleInfo $ "acquired " ++ show (length xs) ++ " slot data points, the last one being:\n"
        ++ show (last xs)
  if count > 1
    then runCommand env (BeaconRun chain ver (count - 1))
    else pure env{ runInstall = Nothing }
  where
    currentResult = envBeaconDir env </> "beacon-result.json"
    beaconChain   = fromJust $ runChains env >>= lookupChain chain



{-
main :: IO ()
main = do
    opts <- getOpts
    --
    -- Obtain benchmarks data
    --
    -- TODO: we could consider using db-analyzer as a library instead.
    csvPathA <- installBenchmarkingProg (versionA opts) >>= runBenchmarks opts
    csvPathB <- installBenchmarkingProg (versionB opts) >>= runBenchmarks opts

    --
    -- Process benchmarks data
    --
    csvA <- parseBenchmarkLedgerOpsCsv $ benchmarkRunDataPath csvPathA
    csvB <- parseBenchmarkLedgerOpsCsv $ benchmarkRunDataPath csvPathB

    unless (csvA .@ slot == csvB .@ slot) $ die "Slot columns must be the same!"
      -- TODO: show a couple of differences.

    compareMeasurements opts mut_forecast csvA csvB
    compareMeasurements opts mut_blockApply csvA csvB

--------------------------------------------------------------------------------
-- Csv with headers file abstraction
--------------------------------------------------------------------------------

-- | Data from a CSV file, consisting of headers and columns.
--
-- INVARIANT:
-- - length headers <= length columns
-- - for all 0<= i, j < length columns, length (columns !! i) == length (columns !! j)
--
-- TODO: We might want to hide this constructor so that we can check the invariants.
data Csv = Csv { headers :: ![Text], columns :: ![Vector Double] }

mkCsv :: [Text] -> [Vector Double] -> Csv
mkCsv hs cs = assert (length hs <= length cs)
            $ assert (and [ length (cs !! i) == length (cs !! (i+1)) | i <- [0 .. length cs - 2] ])
            $ Csv hs cs

-- | Get the column that corresponds to the given header.
--
-- Throws a runtime exception if the column does not exists in the CSV data.
(.@) ::
     Csv
  -> Text
  -- ^ Field to look up.
  -> Vector Double
df .@ t = case findIndex (== t) (headers df) of
  Nothing -> error $ "Could not find field " <> show t <> " in " <> show (headers df)
  Just i  -> columns df !! i

infixl 9 .@

--------------------------------------------------------------------------------
-- Output data processing functions
--------------------------------------------------------------------------------

-- | Given a comma-separated values (CSV) file, parse its header and columns.
--
-- PRECONDITION: the input file should use '\t' as delimiter for values.
--
-- RETURNS: a tuple such that:
-- - The first element contains the headers.
-- - The second element contains one vector per-column of the input CSV file.
--
-- THROWS: a failure exception ('die') if the CSV file could not be parsed.
--
-- TODO: make the function more robust by introducing extra type safety.
parseBenchmarkLedgerOpsCsv :: FilePath -> IO Csv
parseBenchmarkLedgerOpsCsv csvDataPath = do
    csvData <- BL.readFile csvDataPath
    -- TODO: this is a bit fragile because we assume that the benchmarking ledger
    -- ops analysis uses tabs as separator. This might be ok if we run the
    -- analysis within this program, because we control the separator (assuming
    -- it's configurable).
    let decodingOpts = Csv.defaultDecodeOptions {
        Csv.decDelimiter = fromIntegral (ord '\t')
      }
    case Csv.decodeWith decodingOpts Csv.HasHeader csvData of
      Left err  -> die err
      Right res -> do
        -- Create empty vectors per each column.
        csvFileHandle <- openFile csvDataPath ReadMode
        headers <- Text.splitOn "\t" <$> Text.IO.hGetLine csvFileHandle
        pure $ mkCsv headers (transposeCsv res)
  where
    transposeCsv :: Vector [a] -> [Vector a]
    transposeCsv vec =
        fmap (V.fromList . reverse) $ foldl' addRow [] vec
      where
        addRow :: [[a]] -> [a] -> [[a]]
        addRow acc []          = acc
        addRow [] (x:xs)       = [x]: addRow [] xs
        addRow (rs:rss) (x:xs) = (x:rs) : addRow rss xs

--------------------------------------------------------------------------------
-- Output data analysis functions
--------------------------------------------------------------------------------

-- Fields that we assume present in the csv files. NOTE: This is brittle, but
-- works for now.
--
-- TODO: We might consider including this as part of the program
-- option/configuration. Alternatively, the CSV fields can be obtained from
-- `db-analyser` if we use it as a library.
slot, mut_forecast, mut_blockApply :: Text
slot = "slot"
mut_forecast = "mut_forecast"
mut_blockApply = "mut_blockApply"

-- | Compare two measurements (benchmarks).
--
-- At the moment we perform a very simple comparison between the benchmark
-- results of versions 'A' and 'B'. We will refine the comparison process in
-- later versions. Per each slot 's', and each metric 'm' at that slot (eg block
-- processing time), we compute the relative change between measurements 'A'
-- and 'B':
--
-- > d_s A B = (m_s_B - m_s_A) / (max m_s_A m_s_B)
--
-- where 'm_s_v' is the measurement of metric 'm' at slot 's' for version 'v'.
--
-- Given the way we compute this ratio, 'd_s A B' will be positive if the
-- measurement for version 'B' is larger than the measurement for version 'A',
-- and conversely, 'd_s A B' will be negative if the measurement for version 'A' is
-- larger than the corresponding measurement for 'B'.
--
-- For instance, if we're measuring block application time, and 'd_100' is '0.6'
-- this means that version 'B' took 60% more time to apply a block in that
-- particular run.
--
-- We use the maximum betweeen 'm_s_A' and 'm_s_B' as quotient to guarantee that
-- a change from 'm_s_A' to 'm_s_B' has the same magnitude as a change in the
-- opposite direction. In other words:
--
-- > d_s A B = - (d_s B A)
--
-- TODO: Describe what we do with the comparison results.
compareMeasurements :: BenchmarksCompareOptions -> Text -> Csv -> Csv -> IO ()
compareMeasurements opts header csvA csvB = do
    -- TODO: Make this configurable.
    let threshold = 0.8

    let abRelChange = relChangeAscending csvA csvB

    putStrLn $ "Comparison for " <> Text.unpack header

    -- TODO: Bigger is better or smaller is better depends on the metric. We should make this configurable.
    abRelChange `shouldBeBelow` threshold

    let n = 10 :: Int

    putStrLn $ "Top " <> show n <> " measurements smaller than baseline (" <> (versionA opts).dbAnalyser <> ")"
    printPairs slot header $ V.take 10 $ relativeChange abRelChange

    putStrLn $ "Top " <> show n <> " measurements larger than baseline ("  <> (versionA opts).dbAnalyser <> ")"
    printPairs slot header $ V.take 10 $ V.reverse $ relativeChange abRelChange

    -- Filter the slots that have a difference above the give threshold.
    let outliers = Set.fromList
                 $ V.toList
                 $ filterSlots (\v -> v <= -threshold || v >= threshold ) abRelChange
    -- TODO: We might avoid an 'n * log n' runtime if we augment the CSV file with the relative change.

    when (emitPlots opts) $
      plotMeasurements
      (ChartTitle (Text.unpack header))
      header
      (Just outliers)
      csvA
      csvB
      $ toSlug (
      Text.unpack header
        <> "-"
        <> (versionA opts).dbAnalyser
        <> "_vs_"
        <> (versionB opts).dbAnalyser
      ) <> ".png"
    where
      -- Given two runs and a column name, return the relative change, sorted in
      -- ascending order.
      relChangeAscending ::
           Csv
        -> Csv
        -> RelativeChange
      relChangeAscending dfA dfB =
            RelativeChange
          $ sortAscendingWithSlot dfA
          $ fmap relChange
          $ V.zip (dfA .@ header) (dfB .@ header)
        where
          relChange (a, b) = (b - a) / max a b

-- | Check that the relative change is above the given threshold.
shouldBeAbove :: RelativeChange -> Double -> IO ()
shouldBeAbove dr threshold =
  check (threshold < maxRelativeChange dr)

shouldBeBelow :: RelativeChange -> Double -> IO ()
shouldBeBelow dr threshold =
  check (maxRelativeChange dr < threshold)

-- | Check that the relative change is above the given threshold.
check :: Bool -> IO ()
check b =
  unless b $ do
      -- TODO: Add an option to return an error at the end if the above condition is true.
      printWarning "Distance treshold exceeded!"

-- | Relative change per-slot. See 'relChangeDescending'.
--
-- TODO: The first component in the vector represents a slot. We might want to
-- change this type.
--
-- INVARIANT:
--
-- - the vector is sorted in ascending order on its second component.
--
-- TODO: we might want to add a smart constructor for this.
newtype RelativeChange = RelativeChange { relativeChange :: Vector (Double, Double) }

maxRelativeChange :: RelativeChange -> Double
maxRelativeChange = snd . V.last . relativeChange

minRelativeChange :: RelativeChange -> Double
minRelativeChange = snd . (V.! 0) . relativeChange

-- | Keep only the slots that satisfy the given predicate on the second component.
filterSlots :: (Double -> Bool) -> RelativeChange -> Vector Double
filterSlots f RelativeChange { relativeChange } =
    V.map fst $ V.filter (f . snd) relativeChange

sortDescendingWithSlot :: Ord a => Csv -> Vector a -> Vector (Double, a)
sortDescendingWithSlot df = V.zip (df .@ slot)
                          >>> V.modify (sortBy (comparing (Down . snd)))

sortAscendingWithSlot :: Ord a => Csv -> Vector a -> Vector (Double, a)
sortAscendingWithSlot df = V.zip (df .@ slot)
                          >>> V.modify (sortBy (comparing snd))

--------------------------------------------------------------------------------
-- Output data plotting functions
--------------------------------------------------------------------------------

newtype ChartTitle = ChartTitle String

plotMeasurements ::
     ChartTitle
  -> Text
     -- ^ Header to print.
  -> Maybe (Set Double)
     -- ^ Slots from the CSV files to plot ('Nothing' means print all the slots).
  -> Csv
  -> Csv
  -> FilePath
  -> IO ()
plotMeasurements (ChartTitle title) header mSlots csvA csvB outfile = do
    let slotXvalue csv = V.toList
                       $ V.filter (onlySlotsIn mSlots . fst)
                       $ V.zip (csv .@ slot) (csv .@ header)
        slotXvalueA = slotXvalue csvA
        slotXvalueB = slotXvalue csvB
    Chart.Cairo.toFile Chart.def outfile $ do
      Chart.layout_title .= title
      Chart.setColors [Chart.opaque Chart.blue, Chart.opaque Chart.red]
      Chart.plot (Chart.points (Text.unpack header <> " A") slotXvalueA)
      Chart.plot (Chart.points (Text.unpack header <> " B") slotXvalueB)
  where
    onlySlotsIn Nothing      _ = True
    onlySlotsIn (Just slots) s = s `Set.member` slots
-}

--------------------------------------------------------------------------------
-- Printing functions
--------------------------------------------------------------------------------

printPairs :: (Foldable t, Show a, Show b) => Text -> Text -> t (a, b) -> IO ()
printPairs fstHeader sndHeader xs = do
    putStrLn $ show fstHeader <> ", " <> show sndHeader
    mapM_ printPair xs
  where
    printPair (a, b) = putStrLn $ "" <> show a <> ", " <> show b <> ""

appHeader :: String
appHeader = unlines
  [ "┳┓"
  , "┣┫┏┓┏┓┏┏┓┏┓"
  , "┻┛┗ ┗┻┗┗┛┛┗     v" ++ showVersion Paths.version
  , "Benchmarking, exploration, and analysis of Consensus"
  ]
