{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- TODO: remove this once this tool is refactored into smaller sub-modules.
{-# OPTIONS_GHC -Wno-unused-top-binds -fno-warn-unused-imports #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use camelCase" -}
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
import           Control.Exception (assert, bracket_)
import           Control.Monad (unless, when)
import           Control.Monad.Extra (ifM, unlessM)
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import qualified Data.Csv as Csv
import           Data.List (findIndex, foldl')
import           Data.Ord (Down (Down), comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Slugger (toSlug)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Algorithms.Merge (sortBy)
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart.Cairo
import           Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Easy as Chart
import           Options.Applicative (Parser, auto, execParser, flag, fullDesc,
                     help, helper, info, long, metavar, option, progDesc,
                     strOption, value, (<**>))
import qualified System.Console.ANSI as Console
import           System.Directory (doesFileExist)
import           System.Exit (die)
import           System.IO (IOMode (ReadMode), openFile)
import           System.Process (callCommand)


main :: IO ()
main = putStrLn "good"

{- }
-- | Should the program overwrite the benchmark data?
data OverwriteData = OverwriteData | DoNotOverwriteData

-- | Should the program plot the benchmark data?
data ShouldEmitPlots = EmitPlots | DoNotEmitPlots
  deriving Eq

emitPlots :: BenchmarksCompareOptions -> Bool
emitPlots = (EmitPlots ==) . doPlotting

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

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

printWarning :: String -> IO ()
printWarning str =
    bracket_
      (Console.setSGR [Console.SetColor Console.Foreground Console.Vivid Console.Red])
      (Console.setSGR [Console.Reset])
      (print str)

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

--------------------------------------------------------------------------------
-- Functions needed to install and run benchmarks
--------------------------------------------------------------------------------

-- TODO: Make this configurable.
echo :: EchoCommand
echo = -- EchoCommand
   DoNotEchoCommand

data Version = Version {
    dbAnalyser :: String
    -- | Compiler version used to compile db-analyser.
    --
    -- This comes from the 'ouroboros-consensus' 'nix' setup.
    -- Since relying on this is brittle anyway, we do not define a type for it, and rely instead on a free-form string.
  , compiler   :: String
  }

data InstallInfo = InstallInfo { installPath :: String, installedVersion :: Version }

installBenchmarkingProg ::
     Version
  -> IO InstallInfo
installBenchmarkingProg version = do
    let installCmd =  "nix build "
                   <> "github:input-output-hk/ouroboros-consensus/"
                   <> version.dbAnalyser
                   <> "#hydraJobs.x86_64-linux.native."
                   <> version.compiler
                   <> ".exesNoAsserts.ouroboros-consensus-cardano.db-analyser"
                   <> " -o " <> installDir
        installDir = "db-analyser-" <> version.dbAnalyser <> "-" <> version.compiler
        executable = installDir <> "/bin/db-analyser"
    ifM (doesFileExist executable)
        (putStrLn $ "File " <> executable <> " exists. No installation required.")
        (callCommandEchoing echo installCmd)
    pure $ InstallInfo {
             installPath = executable
           , installedVersion = version
           }

newtype BenchmarkRunDataPath = BenchmarkRunDataPath { benchmarkRunDataPath :: String }

-- | Run the benchmarks and return the file path where the benchmarks are stored.
runBenchmarks ::
     BenchmarksCompareOptions
  -> InstallInfo
  -> IO BenchmarkRunDataPath
runBenchmarks opts InstallInfo { installPath, installedVersion } = do
    unlessM (doesFileExist outfile) run
    pure $ BenchmarkRunDataPath { benchmarkRunDataPath = outfile }
  where
    outfile = toSlug (
      "ledger-ops-cost-"
        <> vToFilePath installedVersion
        <> "-from_" <> show (analyseFromSlot opts)
        <> "-nr_blocks_" <> show (numBlocksToProcess opts)
      ) <> ".csv"

    vToFilePath version =
      version.dbAnalyser <> "-" <> version.compiler

    newLine = " \\\n"

    run =
      callCommandEchoing echo
        $ "./" <> installPath <> " cardano"                    <> newLine
        <> "\t --config " <> nodeHome opts <> configPath opts  <> newLine
        <> "\t --db "     <> nodeHome opts <> dbPath opts      <> newLine
        <> "\t --analyse-from " <> show (analyseFromSlot opts) <> newLine
        <> "\t --benchmark-ledger-ops"                         <> newLine
        <> "\t --out-file " <> outfile                         <> newLine
        <> "\t --num-blocks-to-process " <> show (numBlocksToProcess opts) <> newLine
        <> "\t --only-immutable-db"                            <> newLine
        <> "\t +RTS -T -RTS"

data EchoCommand = EchoCommand | DoNotEchoCommand

callCommandEchoing ::
     EchoCommand
     -- | Command to run
  -> String
  -> IO ()
callCommandEchoing EchoCommand      cmd = putStrLn cmd >> callCommandEchoing DoNotEchoCommand cmd
callCommandEchoing DoNotEchoCommand cmd = callCommand cmd

--------------------------------------------------------------------------------
-- Printing functions
--------------------------------------------------------------------------------

printPairs :: (Foldable t, Show a, Show b) => Text -> Text -> t (a, b) -> IO ()
printPairs fstHeader sndHeader xs = do
    putStrLn $ show fstHeader <> ", " <> show sndHeader
    mapM_ printPair xs
  where
    printPair (a, b) = putStrLn $ "" <> show a <> ", " <> show b <> ""

--------------------------------------------------------------------------------
-- Command line parsing
--------------------------------------------------------------------------------

getOpts :: IO BenchmarksCompareOptions
getOpts = execParser
        $ info (parseOpts <**> helper)
        $ mconcat
          [ fullDesc
          , progDesc "Compare benchmarks."
          ]

parseOpts :: Parser BenchmarksCompareOptions
parseOpts =   BenchmarksCompareOptions
          <$> parseVersion "versionA"
          <*> parseVersion "versionB"
          <*> parseNodeHome
          <*> parseConfigPath
          <*> parseDBPath
          <*> parseAnalyseFrom
          <*> parseNumBlocksToProcess
          <*> parseOverwriteData
          <*> parseDoPlotting
  where
    parseVersion :: String -> Parser Version
    parseVersion name =  Version
                <$> strOption
                    ( mconcat
                    [ long name
                    , help "Version to compare"
                    ])
                 <*> parseCompilerVersion
      where
        parseCompilerVersion :: Parser String
        parseCompilerVersion =
          strOption
            ( mconcat
              [ long name
              , help ("Compiler version (eg \"haskell810\", \"haskell96\". See the 'ouroboros-consensus' 'nix' setup for options. ")
                <> metavar "GHC_VERSION"
              ]
            )

    parseNodeHome :: Parser FilePath
    parseNodeHome = strOption
                    ( mconcat
                      [ long "node-home"
                      , help "File path to the 'mainnet' data directory of the node."
                      ]
                    )

    parseConfigPath :: Parser FilePath
    parseConfigPath = strOption
                      ( mconcat
                        [ long "config-path"
                        , value "/configuration/cardano/mainnet-config.json"
                        , help "File path to the config.json file. Relative to node-home. When not passed this defaults to /configuration/cardano/mainnet-config.json"
                        ]
                      )

    parseDBPath :: Parser FilePath
    parseDBPath = strOption
                    ( mconcat
                      [ long "db-path"
                      , value "/mainnet/db"
                      , help "File path to the db file analyzed by db-analyzer. Relative to node-home. When not passed this defaults to mainnet/db"
                      ]
                    )

    parseAnalyseFrom :: Parser Int
    parseAnalyseFrom = option auto
                     $ mconcat
                       [ long "analyse-from"
                       ]

    parseNumBlocksToProcess :: Parser Int
    parseNumBlocksToProcess = option auto
                            $ mconcat
                            [ long "num-blocks-to-process"
                            ]

    parseOverwriteData :: Parser OverwriteData
    parseOverwriteData = flag OverwriteData DoNotOverwriteData
                       $ mconcat
                         [ long "overwrite-data"
                         , help "Overwrite previous benchmark data"
                         ]

    parseDoPlotting :: Parser ShouldEmitPlots
    parseDoPlotting = flag DoNotEmitPlots EmitPlots
                      $ mconcat
                      [ long "do-plotting"
                      , help "Whether to plot and compare db-analyser data, or not."
                      ]
-}