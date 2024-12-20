{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Api (ExceptT, SlotNo (..))

import           Cardano.Analysis.API.Ground (JsonInputFile (..))
import           Cardano.Unlog.BackendDB
import           Cardano.Unlog.BackendFile (readRunLogsBare)
import           Cardano.Unlog.LogObject (LOBody (..), LogObject (..), rlCastWith, rlLogs)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util

import           Prelude hiding (log)

import           Data.Bifunctor (second)
import           Data.List.Split (chop)
import           Data.Maybe
import           System.Environment (getArgs)

import           Database.Sqlite.Easy hiding (Text)
import           Graphics.EasyPlot


main :: IO ()
main = do
  getArgs >>= \case
    []     -> testPlot >> putStrLn "please specify DB file"
    db : _ -> runDB $ fromString db

testPlot :: IO Bool
testPlot =
  plot' [Debug] _term2 plotData
  where
    _term1 = Terminal PNGCairo "test.png" Nothing
    _term2 = Terminal X11 "test plot" (Just (1024, 768))
    _term3 = Terminal Dumb "" Nothing
    plotData =
        [ Data2D [Title "Graph 1", Color Red] [] [(x, x ** 3) | x <- [-4,-3.9 :: Double .. 4]]
        , Function2D [Title "Function 2", Color Blue] [] (\x -> negate $ x ** 2)
        , Gnuplot2D [Title "Function Expr", Color Green] [] "x*x"
        , File2D [Title "from file", Color Yellow] [] "_plot1.dat" Nothing
        ]


_heapPlots :: [[LogObject]] -> IO ()
_heapPlots inputStreams = do
    mapM_ print bySlotStreams
    pure ()
  where
    bySlotStreams :: [[(SlotNo, [LogObject])]]
    bySlotStreams = [ map (second safeLast) (bySlotDomain s) | s <- inputStreams ]


_runOnRun :: FilePath -> ExceptT String IO [[LogObject]]
_runOnRun logManifest = do
  rl <- readRunLogsBare (JsonInputFile logManifest)
  loaded <- runLiftLogObjectsDB (LoadLogObjectsWith query) (rlCastWith (const []) rl)

  let
    _loStreams :: [[LogObject]]
    _loStreams = snd <$> rlLogs loaded

  pure _loStreams
  where
    query =
      [ sqlGetSlot
      , sqlGetResource
      ]

-- sample case:
-- we want to know the txns in mempool for each slot

runDB :: ConnectionString -> IO ()
runDB dbName = do
  (summary, res2) <-
    withTimingInfo "withDb/selectMempoolTxs" $
      withDb dbName $
        (,) <$> getSummary <*> run selectMempoolTxs

  let logObjects = map (sqlToLogObject summary) res2

  -- TODO: needs a reducer
  mapM_ (print . second safeLast) (bySlotDomain logObjects)

safeLast :: [a] -> [a]
safeLast [] = []
safeLast xs = [last xs]

bySlotDomain :: [LogObject] -> [(SlotNo, [LogObject])]
bySlotDomain logObjs =
  case dropWhile (isNothing . newSlot) logObjs of
    [] -> []
    xs -> chop go xs
  where
    newSlot LogObject{loBody} = case loBody of { LOTraceStartLeadershipCheck s _ _ -> Just s; _ -> Nothing }

    go (lo:los) = let (inSlot, rest) = span (isNothing . newSlot) los in ((fromJust $ newSlot lo, inSlot), rest)
    go []       = error "bySlotDomain/chop: empty list"

selectMempoolTxs :: SQL
selectMempoolTxs = sqlOrdered
  [ sqlGetSlot
  , sqlGetTxns `sqlAppend` "WHERE cons='LOMempoolTxs'"
  ]
