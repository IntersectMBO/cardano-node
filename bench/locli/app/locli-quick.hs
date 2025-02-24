{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-redundant-constraints -Wno-unused-top-binds #-}

import           Cardano.Api (ExceptT, SlotNo (..), runExceptT)

import           Cardano.Analysis.API.Ground (JsonInputFile (..))
import           Cardano.Analysis.Reducer
import           Cardano.Analysis.Reducer.Util
import           Cardano.Unlog.BackendDB
import           Cardano.Unlog.BackendFile (readRunLogsBare)
import           Cardano.Unlog.LogObject (LogObject (..), RunLogs (..), rlLogs)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util hiding (toDouble)

import           Prelude hiding (log, seq)

import           Data.Bifunctor (first)
import           Data.Either
import           Data.Function (on)
import           Data.List (find, isSuffixOf)
import           Data.Map.Strict as Map (restrictKeys)
import           Data.Reducer
import qualified Data.Set as Set (fromList, null)
import           Data.Word
import           System.Environment (getArgs)

import           Database.Sqlite.Easy hiding (Text)
import           Graphics.EasyPlot


main :: IO ()
main = do
  getArgs >>= \case
    db : _
      | ".sqlite3"  `isSuffixOf` db -> runDB $ fromString db
      | ".json"     `isSuffixOf` db -> runManifest db
      | db == "testplot"            -> void testPlot
    _ -> putStrLn "please specify DB file or log manifest, or 'testplot'"

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

runManifest :: FilePath -> IO ()
runManifest logManifest = do
  rls <- withTimingInfo "quick-query" $
    -- runOnRun (LoadLogObjectsWith selectMempoolTxs) logManifest [] -- ["node-2", "node-12", "node-22"]
    -- runOnRun LoadHeapData logManifest [] -- ["node-2", "node-12", "node-22"]
    runOnRun LoadTimestamps logManifest [] -- ["node-2", "node-12", "node-22"]

  {-
  let
    perSlot :: RunLogs [ObjectsInSlot]
    perSlot = bySlotDomain `fmap` rls

    red    = reduceRunLogs (TxsInMempoolPerSlot <-> Changes ((/=) `on` snd)) perSlot
  -}

  {-
  let
    perSlot :: RunLogs [BySlot Word64]
    perSlot = bySlot
                (either (const Nothing) Just)
                (fromLeft undefined)
                False
              `fmap` rls
  -}

  now <- getCurrentTime
  let
    res = reduceRunLogs Silence{threshold = 1.2, startTime = now} rls

  mapM_ print (rlLogs res)

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
    toDouble :: SlotNo -> Double
    toDouble = fromIntegral . unSlotNo

    term    = Terminal X11 "Txns in Mempool" (Just (1024, 768))
    points1 = map (first toDouble) res1
    points2 = map (first toDouble) res2

  void $ plot' [] term (Data2D [Title "txn count per slot", Color Red, Style Linespoints] [] points1)
  void $ plot' [] term (Data2D [Title "txn count changes", Color Blue, Style Steps] [] points2)

selectMempoolTxs :: [SQLSelect6Cols]
selectMempoolTxs =
  [ sqlGetSlot
  , sqlGetTxns `sqlAppend` "WHERE cons='LOMempoolTxs'"
  ]


{-
  This should eventually be part of a QuickQuery typeclass. This class is defined by:
  - a query + (possibly parametrizable) filter, making use of the LoadFromDB typeclass
  - a (possibly parametrizable) reducer, making use of the Reducer typeclass
  - meaningful stdout output
  - optionally: file output, like e.g. CSV
  - optionally: a plot / plots
-}

data LoadHeapData = LoadHeapData

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
