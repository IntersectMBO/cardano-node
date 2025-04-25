{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-redundant-constraints -Wwarn=unused-top-binds -Wwarn=partial-fields -Wno-orphans #-}

import           Cardano.Api (ExceptT, SlotNo (..), runExceptT)

import           Cardano.Analysis.API.Ground (JsonInputFile (..))
import           Cardano.Analysis.Reducer
import           Cardano.Analysis.Reducer.Util
import           Cardano.Unlog.BackendDB
import           Cardano.Unlog.BackendFile (readRunLogsBare)
import           Cardano.Unlog.LogObject (LogObject (..), RunLogs (..), hlLogs, rlLogs)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util hiding (toDouble)

import           Prelude hiding (log, seq)

import           Data.Bifunctor
import qualified Data.ByteString as B
import           Data.Either
import           Data.Function (on)
import           Data.List (find, isSuffixOf)
import           Data.List.Extra (splitOn)
import           Data.Map.Strict as Map (restrictKeys, (!))
import           Data.Maybe
import           Data.Reducer
import qualified Data.Set as Set (fromList, null)
import           Data.Word
import           Options.Applicative as Opt
import           System.FilePath (splitDirectories, takeExtension)

import           Database.Sqlite.Easy hiding (Text)
import           Graphics.EasyPlot


main :: IO ()
main = do
  cli <- parseCommandLine
  -- print cli
  case cli of
    CMDTestPlot  -> void testPlot
    CMDProcess m -> do
      putStrLn $ "--> reading result blob from: " ++ show m
    CMDQuery{..} -> case cInputs of
      [db] | takeExtension db == ".sqlite3"
        -> runDB $ fromString db
      inputs
        -> mapM (runManifest cNodes) (zip inputs [0 :: Int ..]) >>= runPlot
    CMDTestPipe r -> testPipe r

-- for testing the EasyPlot module
testPlot :: IO Bool
testPlot =
  plot' [Debug, Interactive] _term3 plotData
  where
    _term1 = mkTerminal (PNGCairo "test.png")
    _term2 = Terminal X11 "test plot" (Just (1024, 768))
    _term3 = Terminal Qt "test plot" (Just (1024, 768))
    _term4 = mkTerminal Dumb
    plotData =
        [ Data2D [Title "Graph 1", Color Red] [] [(x, x ** 3) | x <- [-4,-3.9 :: Double .. 4]]
        , Function2D [Title "Function 2", Color Blue] [] (\x -> negate $ x ** 2)
        , Gnuplot2D [Title "Function Expr", Color Green] [] "x*x"
        , File2D [Title "from file", Color Yellow] [] "_plot1.dat" Nothing
        ]

testPipe :: Bool -> IO ()
testPipe readMode
  | readMode = do
    inp <- B.getContents
    putStrLn $ "--> does stdin match test buffer: " ++ show (inp == testBuffer)
  | otherwise = B.putStr testBuffer
  where
    testBuffer = B.pack $ take 2048 $ cycle [0 .. 255]

runManifest :: [String] -> (FilePath, Int) -> IO (Graph2D Double Double)
runManifest targetNodes (logManifest, ix) = do
  rls <- withTimingInfo "quick-query" $
    -- runOnRun (LoadLogObjectsWith selectMempoolTxs) logManifest [] -- ["node-2", "node-12", "node-22"]
    runOnRun LoadHeapData logManifest [targetNode]
    -- runOnRun LoadTimestamps logManifest [] -- ["node-2", "node-12", "node-22"]

  {-
  let
    perSlot :: RunLogs [ObjectsInSlot]
    perSlot = bySlotDomain `fmap` rls

    red = reduceRunLogs (TxsInMempoolPerSlot <-> changes)) perSlot
  -}

  {-
  now <- getCurrentTime
  let
    res = reduceRunLogs Silence{threshold = 1.2, startTime = now} rls
  mapM_ print (rlLogs res)
  -}

  let
    target =  snd $ hlLogs $ rlHostLogs rls Map.! fromString targetNode

    bySlotValue :: [BySlot Word64]
    bySlotValue = bySlot (either (const Nothing) Just) (fromLeft undefined) False target

    res    = reduce ResourceMeasurePerSlot bySlotValue
    bumps  = reduce changes res
    gibibytes = map (second (\w -> fromIntegral w / 1073741824)) (toDouble bumps)

    plotData = Data2D [Title targetTitle, Color targetColor, Style Steps] [] gibibytes
  putStrLn $ "--> target node: " ++ targetNode
  mapM_ print bumps
  pure plotData

  where
    targetNode  = fromMaybe "node-10" $ listToMaybe targetNodes
    targetColor = cycle [Red, Blue, Green, Yellow, Magenta, Cyan] !! ix
    targetTitle = case splitDirectories logManifest of
      "run" : runId : _ -> runId            -- workbench use
      _                 -> logManifest      -- any other manual use

runPlot :: Plot a => a -> IO ()
runPlot plotData =
   void $ plot' [preamble] term plotData
  where
    term = Terminal Qt "Heap size bumps" (Just (1440, 960))
    preamble = Preamble
      [ "set xlabel \"Slot\""
      , "set ylabel \"Heap size (GiB)\""
      , "set format y \"%'.2f\""
      , "set ytics nomirror"
      ]

runOnRun :: forall l. LoadFromDB l => l -> FilePath -> [String] -> IO (RunLogs [LoadResult l])
runOnRun loadFromDB logManifest onlyHosts =
  runExceptT go >>= either error pure
  where
    restrictRunLogs rl@RunLogs{rlHostLogs}
      | null onlyHosts  = rl
      | otherwise       =
        let onlyKeys = Set.fromList $ map fromString onlyHosts
        in rl{ rlHostLogs = Map.restrictKeys rlHostLogs onlyKeys }

    go :: ExceptT String IO (RunLogs [LoadResult l])
    go = do
      rl <- restrictRunLogs <$> readRunLogsBare (JsonInputFile logManifest)
      runLiftLogObjectsDB loadFromDB rl

-- sample case:
-- we want to know the txns in mempool for each slot

runDB :: ConnectionString -> IO ()
runDB dbName = do
  (summary, result) <-
    withTimingInfo "withDb/selectMempoolTxs" $
      withDb dbName $
        (,) <$> getSummary <*> run (sqlOrdered selectMempoolTxs)

  let
    logObjects = map (sqlToLogObject summary) result

    bySlotOjbs :: [BySlot LogObject]
    bySlotOjbs = bySlotLogObjects logObjects

    res1   = take 1600 $ reduce TxsInMempoolPerSlot bySlotOjbs
    res2   = reduce changes res1

  let
    term    = Terminal X11 "Txns in Mempool" (Just (1280, 960))
    points1 = toDouble res1
    points2 = toDouble res2

  void $ plot' [] term (Data2D [Title "txn count per slot", Color Red, Style Linespoints] [] points1)
  void $ plot' [] term (Data2D [Title "txn count changes", Color Blue, Style Steps] [] points2)

selectMempoolTxs :: [SQLSelect6Cols]
selectMempoolTxs =
  [ sqlGetSlot
  , sqlGetTxns `sqlAppend` "WHERE cons='LOMempoolTxs'"
  ]


toDouble :: [(SlotNo, a)] -> [(Double, a)]
toDouble = map (first (fromIntegral . unSlotNo))

{-
  This should eventually be part of a QuickQuery typeclass. This class is defined by:
  - a query + (possibly parametrizable) filter, making use of the LoadFromDB typeclass
  - a (possibly parametrizable) reducer, making use of the Reducer typeclass
  - meaningful stdout output
  - optionally: file output, like e.g. CSV
  - optionally: a plot / plots
-}

data LoadHeapData = LoadHeapData


-- TODO: use timestamp to infer slot numbers during startup
instance LoadFromDB LoadHeapData where
  type instance LoadResult LoadHeapData = Either Word64 SlotNo

  loadQuery _ = Just "SELECT at, slot, null as heap FROM slot UNION SELECT at, null, heap FROM resource ORDER BY at ASC"

  loadConvert _ _ = \case
    [_, slot_, heap_] ->
      let
        slot :: SMaybe SlotNo
        slot = fromSqlData slot_
        heap :: SMaybe Word64
        heap = fromSqlData heap_
      in smaybe (Left $ unsafeFromSJust heap) Right slot
    _ -> error "loadConvert(LoadHeapData): expected 3 result columns"


data LoadTimestamps = LoadTimestamps

instance LoadFromDB LoadTimestamps where
  type instance LoadResult LoadTimestamps = (UTCTime, SMaybe SlotNo)

  loadQuery _ = Just $ "SELECT at, slot FROM slot" <> from "resource" <> from "txns" <> from "event" <> " ORDER BY at ASC"
    where from t = " UNION SELECT at, null FROM " <> t

  loadConvert _ _ = \case
    [at, slot] -> (fromSqlData at, fromSqlData slot)
    _ -> error "loadConvert(LoadTimestamps): expected 2 result columns"


--
-- command line parsing
--

data ProcessMode
    = PassThru                                        -- ^ read result blob from stdin
    | WriteBlob FilePath                              -- ^ read result blob from stdin and also write to file (to retain a local copy when blob is piped from remote)
    | ReadBlob FilePath                               -- ^ read result blob from file

instance Show ProcessMode where
  show PassThru       = "<stdin>"
  show (WriteBlob f)  = "<stdin> (will dump to: " ++ f ++ ")"
  show (ReadBlob f)   = f

data Command
    = CMDTestPlot
    | CMDQuery
      { cQuery        :: ()                           -- ^ the query to run
      , cInputs       :: [FilePath]                   -- ^ log manifests of all runs to query, or a single .sqlite3 DB
      , cNodes        :: [String]                     -- ^ hosts to query (e.g. ["node-10", "node-12"]; empty: all hosts
      , cDumpResult   :: Bool                         -- ^ dump result blob to stdout only; don't process
      }
    | CMDProcess
      { cProcessMode  :: ProcessMode
      }
    | CMDTestPipe
      { cReadMode     :: Bool
      }
    deriving Show

parseCommandLine :: IO Command
parseCommandLine
  = post <$> Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parserCommandLine Opt.fullDesc

    -- post processing: if run paths were given, extend to log manifest path accoring to workbench default
    post  = \case
      cmd@CMDQuery{cInputs = ps} -> cmd {cInputs = map extend ps}
      other                      -> other
      where
        extend f = f

parserCommandLine :: Parser Command
parserCommandLine = subparser $ mconcat
  [ parserOp "qq"       "run a quick query"                 parserQuickQuery
  , parserOp "proc"     "process a query result"            (CMDProcess <$> parserProcessMode)
  , parserOp "testpipe" "test stdout/stdin piping a buffer" parserTestPipe
  , parserOp "testplot" "test plotting with dummy data"     (pure CMDTestPlot)
  ]
  where
    parserTestPipe = CMDTestPipe <$>
      (flag' True (short 'r' <> help "read from stdin") <|> flag' False (short 'w' <> help "write to stdout"))

    parserProcessMode =
          WriteBlob <$> strOption (short 'w' <> metavar "FILE" <> help "read from stdin and also dump to file")
      <|> ReadBlob  <$> strOption (short 'r' <> metavar "FILE" <> help "read from file")
      <|> pure PassThru

parserQuickQuery :: Parser Command
parserQuickQuery =
  CMDQuery
    <$> pure ()
    <*> parseRuns
    <*> pure []
    <*> switch (short 'd' <> long "dump-only" <> help "dump result blob to stdout only; don't process")
  where
    {-
    parseScriptName =
          strArgument (help "name of a known script"        <> metavar "NAME")
      <|> strArgument (help "custom serialized script file" <> metavar "FILE" <> completer (bashCompleter "file"))
    parseParamPath =
      strOption $ long "param" <> metavar "JSON" <> completer (bashCompleter "file")
        <> help "protocol parameter file; default: data/protocol-parameters-v10.json"
    parseBudgetHint =
      option auto $ long "hint" <> metavar "BUDGET"
        <> help "Which budget does the script target? <Mem|Steps>"
    -}
    parseRuns =
      option readCommaSepList $ long "run" <> short 'r' <> metavar "run(s)"
        <> help "comma-separated list of run dirs or log manifest JSONs, or a single SQLite DB"

readCommaSepList :: Opt.ReadM [String]
readCommaSepList = Opt.maybeReader
  ((\inp -> if null inp then Nothing else Just inp) . filter (not . null) . splitOn ",")

parserOp :: String -> String -> Parser a -> Mod CommandFields a
parserOp c descr p = command c $ info (p <**> helper) $ progDesc descr
