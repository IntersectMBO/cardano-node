{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}


import           Cardano.Api (BlockNo (..), MonadIO, SlotNo (..))

import           Cardano.Analysis.API.Ground (Hash (..), Host (..), TId (..))
import           Cardano.Logging.Resources.Types (ResourceStats, Resources (..))
import           Cardano.Unlog.LogObject (LOAnyType (..), LOBody (..), LogObject (..), fromTextRef)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util (smaybe)

import           Prelude hiding (log)

import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad
import           Data.Aeson as Aeson
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Coerce
import           Data.Data
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashMap.Strict as HMS
import           Data.List.Split (chop)
import qualified Data.Map.Lazy as ML
import           Data.Maybe
import qualified Data.Text as TS (Text, empty, intercalate, pack, splitOn, unpack)
import qualified Data.Text.Lazy as TL (Text, fromStrict, pack)
import qualified Data.Text.Short as ShortText (ShortText, empty, fromText, pack, toText)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Data.Typeable
import           Data.Word
import qualified GHC.Stats as RTS
import           System.Directory (removeFile)
import           System.Environment (getArgs)

import           Database.Sqlite.Easy hiding (Text)


dbName :: ConnectionString
dbName  = "test_logobject.sqlite3"

dbPath :: FilePath
dbPath = TS.unpack $ unConnectionString dbName

main :: IO ()
main = do
  print selectAll
  getArgs >>= \case
    []      -> tryRead >> putStrLn "(to recreate DB, provide log name as arg)"
    log : _ -> do
      removeFile dbPath `catch` \SomeException{} -> pure ()
      ls <- BSL.lines <$> BSL.readFile log

      _ <-
        timed "withDb/createDbCounting" $ withDb dbName $ do
          let
            alterFunc :: Maybe Int -> Maybe Int
            alterFunc = maybe (Just 1) (Just . succ)

            go acc line = case eitherDecode line of
              Right logObject@LogObject{loNS, loKind} -> do

                mapM_ runSqlRunnable (logObjectToSql logObject)

                let name = fromTextRef loNS <> ":" <> fromTextRef loKind
                pure $ ML.alter alterFunc name acc
              Left err -> uncurry runWith (errorToSql err $ BSL.unpack line) >> pure acc

          mapM_ run createTables

          tracefreqs <- transaction $ foldM go (ML.empty :: TraceFreqs) ls

          transaction $ mapM_ runSqlRunnable (traceFreqsToSql tracefreqs)

          [[tMin, tMax]] <- run "SELECT MIN(at), MAX(at) FROM (SELECT at FROM resource union SELECT at FROM slot union SELECT at FROM txns union SELECT at FROM event)"
          let
            dbSummary = SummaryDB
              { sdbName     = "test"
              , sdbLines    = sum tracefreqs
              , sdbFirstAt  = fromSqlData tMin
              , sdbLastAt   = fromSqlData tMax
              }
          runSqlRunnable $ summaryToSql dbSummary

      pure ()


dbSummaryDum :: SummaryDB
dbSummaryDum = SummaryDB "tryRead" 0 undefined undefined

tryRead :: IO ()
tryRead = do
  res <- take 84 . drop 4096 <$> timed "withDb/selectAll" (withDb dbName (run selectAll))
  mapM_ print res
  mapM_ (\r -> print r >> print (sqlToLogObject dbSummaryDum r)) res

timed :: MonadIO m => String -> m a -> m a
timed name action = do
  before <- liftIO getPOSIXTime
  result <- action
  after  <- liftIO getPOSIXTime
  heap   <- liftIO $ RTS.gcdetails_mem_in_use_bytes . RTS.gc <$> RTS.getRTSStats

  let
    seconds   :: Int
    seconds   = floor $ after - before
    mibibytes = heap `div` 1024 `div` 1024

  liftIO $ putStrLn $ "<<timed>> time: " ++ show seconds ++ "s; heap: " ++ show mibibytes ++ "MiB; (" ++ name ++ ")"
  pure result


createTables :: [SQL]
createTables =
  [ "CREATE TABLE event      (at REAL NOT NULL, cons TEXT NOT NULL, slot INTEGER, block INTEGER, hash TEXT)"
  , "CREATE TABLE resource   (at REAL NOT NULL, centi_cpu INTEGER, rss INTEGER, heap INTEGER, alloc INTEGER, as_blob BLOB)"
  , "CREATE TABLE slot       (at REAL NOT NULL, slot INTEGER, utxo_size INTEGER, chain_dens REAL)"
  , "CREATE TABLE txns       (at REAL NOT NULL, cons TEXT NOT NULL, count INTEGER, rejected INTEGER, tid TEXT)"
  , "CREATE TABLE tracefreq  (msg TEXT NOT NULL, count INTEGER NOT NULL)"
  , "CREATE TABLE summary    (name TEXT NOT NULL, lines INTEGER NOT NULL, first_at REAL NOT NULL, last_at REAL NOT NULL)"
  , "CREATE TABLE error      (msg TEXT NOT NULL, input TEXT)"
  ]

selectAll :: SQL
selectAll =
  "SELECT at, cons, slot as arg1, block as arg2, null as arg3, hash as arg4 FROM event \
  \UNION \
  \SELECT at, cons, count as arg1, rejected as arg2, null as arg3, tid as arg4 FROM txns \
  \UNION \
  \SELECT at, 'LOResources' as cons, null, null, null, as_blob FROM resource \
  \UNION \
  \SELECT at, 'LOTraceStartLeadershipCheck' as cons, slot, utxo_size, chain_dens, null FROM slot \
  \ORDER BY at"


bySlotDomain :: [LogObject] -> [(SlotNo, [LogObject])]
bySlotDomain logObjs =
  case dropWhile (isNothing . newSlot) logObjs of
    [] -> []
    xs -> chop go xs
  where
    newSlot LogObject{loBody} = case loBody of { LOTraceStartLeadershipCheck s _ _ -> Just s; _ -> Nothing }

    go ~(lo:los) = let (inSlot, rest) = span (isNothing . newSlot) los in ((fromJust $ newSlot lo, inSlot), rest)


{-
createIndex :: SQL
createIndex =
  "CREATE INDEX idx_lo_body ON lo_body(at)"
-}

{-
mempool saturation example, needs reducer:
select at, null as slot, count from txns where cons='LOMempoolTxs' union select at, slot, null as count from slot order by at;
-}

{-
TODO: gather summary data:

grep '^{' f | head -n1: 1st timestamp
grep -c '^{' f: no. of trace msgs
tail -n1 f: last timestamp
-}
