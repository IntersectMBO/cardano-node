
module Cardano.Unlog.BackendDB
       ( prepareDB
       , prepareDBFromLines
       , HostSummary(..)
       , readRunLogsBare
       , runLiftLogObjectsDB
       , LoadFromDB(..)
       , LoadLogObjects(..)
       , LoadSummaryOnly(..)

       -- specific SQLite queries or statements
       , getSummary
       , getLastUTxOSize
       , getTraceFreqs
       , selectAll
       , sqlGetEvent
       , sqlGetTxns
       , sqlGetResource
       , sqlGetSlot
       , sqlOrdered
       ) where

import           Cardano.Analysis.API.Ground (Host (..), JsonInputFile (..), LogObjectSource (..))
import           Cardano.Prelude (ExceptT (..), throwError, (<|>))
import           Cardano.Unlog.LogObject (HostLogs (..), LogObject (..), RunLogs (..), fromTextRef)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util (sequenceConcurrentlyChunksOf, withTimingInfo)

import           Prelude hiding (log)

import           Control.Exception (SomeException (..), catch)
import           Control.Monad
import           Data.Aeson as Aeson (FromJSON (..), decode, eitherDecode, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Kind (Type)
import           Data.List (sort)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text.Short as ShortText (unpack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601
import           Data.Word (Word64)
import           GHC.Conc (numCapabilities)
import           System.Directory (removeFile)
import           System.IO (stdin)

import           Database.Sqlite.Easy hiding (Text)


class LoadFromDB l where
  type family LoadResult l :: Type

  loadQuery       :: l -> Maybe SQL

  loadConvert     :: l -> SummaryDB -> [SQLData] -> LoadResult l

  loadTraceFreqs  :: l -> Bool
  loadTraceFreqs  = const False

  loadTimingInfo  :: l -> Bool
  loadTimingInfo  = const False

data LoadLogObjects =
    LoadLogObjectsAll
  | LoadLogObjectsWith [SQLSelect6Cols]
  | LoadTraceFreqsOnly
  deriving (Eq, Show)

data LoadSummaryOnly =
    LoadSummaryOnly

instance LoadFromDB LoadLogObjects where
  type instance LoadResult LoadLogObjects = LogObject

  loadTraceFreqs = \case {LoadLogObjectsWith{} -> False; _ -> True}
  loadQuery = \case
    LoadLogObjectsAll          -> Just selectAll
    LoadLogObjectsWith selects -> Just $ sqlOrdered selects
    LoadTraceFreqsOnly         -> Nothing
  loadConvert    = const sqlToLogObject
  loadTimingInfo = (== LoadLogObjectsAll)

instance LoadFromDB LoadSummaryOnly where
  type instance LoadResult LoadSummaryOnly = SummaryDB

  loadQuery = const $ Just "SELECT null"
  loadConvert _ summary _ = summary


-- | The run's log manifest, as written by `wb analyse prepare-db`: per host, its DB and raw log files.
readRunLogsBare :: JsonInputFile a -> ExceptT String IO (RunLogs ())
readRunLogsBare rlf =
  ExceptT $ Aeson.eitherDecode <$> BSL.readFile (unJsonInputFile rlf)

runLiftLogObjectsDB :: LoadFromDB l => l -> RunLogs a -> ExceptT String IO (RunLogs [LoadResult l])
runLiftLogObjectsDB loadMode RunLogs{rlHostLogs, ..} = liftIO $ do
  hostLogs' <- Map.fromList
    <$> sequenceConcurrentlyChunksOf numCapabilities loadActions
  pure $ RunLogs{rlHostLogs = hostLogs', ..}
  where
    loadActions = map load (Map.toList rlHostLogs)
    timingInfo h
      | loadTimingInfo loadMode = withTimingInfo ("loadHostLogsDB/" ++ ShortText.unpack h)
      | otherwise               = id

    load (host@(Host h), hl) = timingInfo h $
      (,) host <$> loadHostLogsDB loadMode hl

-- If the logs have been split up into multiple files, e.g. by a log rotator,
-- this assumes sorting log files by *name* results in chronological order
-- of all *trace messages* contained in them.
prepareDB :: String -> [FilePath] -> FilePath -> ExceptT String IO ()
prepareDB machName (sort -> logFiles) outFile = liftIO $ do

  removeFile outFile `catch` \SomeException{} -> pure ()

  withTimingInfo ("prepareDB/" ++ machName) $ withDb dbName $ do
    mapM_ run createSchema

    tracefreqs <- foldM prepareFile (ML.empty :: TraceFreqs) logFiles

    (tMin, tMax) <- liftIO $ tMinMax logFiles
    writeSummary machName tracefreqs tMin tMax
  where
    dbName = fromString outFile

prepareFile :: TraceFreqs -> FilePath -> SQLite TraceFreqs
prepareFile tracefreqs log = do
  ls <- BSL.lines <$> liftIO (BSL.readFile log)
  transaction $ foldM go tracefreqs ls
  where
    alterFunc :: Maybe Int -> Maybe Int
    alterFunc = maybe (Just 1) (Just . succ)

    -- Strict in the map and its counts: a lazy accumulator would keep every
    -- line's decoded object alive until the end of the file.
    go !acc line = do
      decoded <- ingestLine line
      pure $! case decoded of
        Nothing                -> acc
        Just LogObject{loNS}   -> Map.alter alterFunc (fromTextRef loNS) acc

-- | Decode one log line and write what's persistable; a line that doesn't decode
--   becomes an error row. The decoded line, for counting.
ingestLine :: BSL.ByteString -> SQLite (Maybe LogObject)
ingestLine line = case Aeson.eitherDecode line of
  Right logObject -> do
    forM_ (logObjectToSql logObject)
        runSqlRunnable
    pure $ Just logObject

  Left err -> runSqlRunnable (errorToSql err $ BSL.unpack line) >> pure Nothing

-- | The tables 'prepareDB' fills from what it counted while decoding.
writeSummary :: String -> TraceFreqs -> UTCTime -> UTCTime -> SQLite ()
writeSummary machName tracefreqs tMin tMax = do
  transaction $ mapM_ runSqlRunnable (traceFreqsToSql tracefreqs)
  now <- liftIO getCurrentTime
  let
    dbSummary = SummaryDB
      { sdbName     = fromString machName
      , sdbLines    = sum tracefreqs
      , sdbFirstAt  = tMin
      , sdbLastAt   = tMax
      , sdbCreated  = now
      }
  void $ runSqlRunnable $ summaryToSql dbSummary

-- | What 'prepareDB' counts while decoding every line of a host's logs, computed
--   beforehand by whatever provides the lines (bench-parquet host-summary).
data HostSummary
  = HostSummary
    { hsTraceFreqs :: !TraceFreqs  -- ^ JSON lines per namespace
    , hsLines      :: !Int         -- ^ JSON lines
    , hsFirstAt    :: !UTCTime     -- ^ the first JSON line, in log order
    , hsLastAt     :: !UTCTime     -- ^ the last JSON line, in log order
    }

instance FromJSON HostSummary where
  parseJSON = withObject "HostSummary" $ \o ->
    HostSummary
      <$> o .: "tracefreqs"
      <*> o .: "lines"
      <*> o .: "firstAt"
      <*> o .: "lastAt"

-- | Like 'prepareDB', for a host's log lines on stdin, usually only those of the
--   interpreted namespaces ('logObjectStreamInterpreterKeys'), in log order. What
--   'prepareDB' counts over all lines comes precomputed in the 'HostSummary'.
prepareDBFromLines :: String -> HostSummary -> FilePath -> ExceptT String IO ()
prepareDBFromLines machName HostSummary{..} outFile = do
  when (sum hsTraceFreqs /= hsLines) $
    throwError $ "host summary: the counts per namespace add up to " ++ show (sum hsTraceFreqs)
                 ++ ", expected its line count " ++ show hsLines
  liftIO $ do
    removeFile outFile `catch` \SomeException{} -> pure ()

    withTimingInfo ("prepareDBFromLines/" ++ machName) $ withDb (fromString outFile) $ do
      mapM_ run createSchema

      ls <- BSL.lines <$> liftIO (BSL.hGetContents stdin)
      transaction $ mapM_ ingestLine ls

      writeSummary machName hsTraceFreqs hsFirstAt hsLastAt

tMinMax :: [FilePath] -> IO (UTCTime, UTCTime)
tMinMax [] = fail "tMinMax: empty list of log files"
tMinMax [log] = do
  ls2 <- BSL.lines <$> BSL.readFile log
  let
    loMin :: LogObject
    loMax :: UTCTime
    loMin = head $ mapMaybe Aeson.decode ls2
    loMax = fromJust $ (loAt <$> Aeson.decode (last ls2)) <|> parseTimeStampFromPartial (last ls2)
  pure (loAt loMin, loMax)
tMinMax logs = do
  (tMin, _   ) <- tMinMax [head logs]
  (_   , tMax) <- tMinMax [last logs]
  pure (tMin, tMax)

-- extracts timestamp from the last log line even if incomplete (due to node shutdown) - sample data:
-- {"at":"2026-03-25T16:19:47.482228445Z","ns":"...",...
parseTimeStampFromPartial :: BSL.ByteString -> Maybe UTCTime
parseTimeStampFromPartial partial =
  formatParseM iso8601Format $ BSL.unpack ts
  where ts = BSL.takeWhile (/= '"') . BSL.drop 7 $ partial


-- selects the entire LogObject stream, containing all objects relevant for standard analysis
selectAll :: SQL
selectAll = sqlOrdered
  [ sqlGetEvent
  , sqlGetTxns
  , sqlGetResource
  , sqlGetSlot
  , sqlGetLMetrics
  ]

sqlGetEvent, sqlGetTxns, sqlGetResource, sqlGetSlot, sqlGetLMetrics :: SQLSelect6Cols
sqlGetEvent    = mkSQLSelectFrom "event"          Nothing                              (Just "slot")  (Just "block")     Nothing             (Just "hash")
sqlGetTxns     = mkSQLSelectFrom "txns"           Nothing                              (Just "count") (Just "rejected")  Nothing             (Just "tid")
sqlGetResource = mkSQLSelectFrom "resource"       (Just "LOResources")                 Nothing        Nothing            Nothing             (Just "as_blob")
sqlGetSlot     = mkSQLSelectFrom "slot"           (Just "LOTraceStartLeadershipCheck") (Just "slot")  (Just "utxo_size") (Just "chain_dens") Nothing
sqlGetLMetrics = mkSQLSelectFrom "ledgermetrics"  (Just "LOLedgerMetrics")             (Just "slot")  (Just "utxo_size") (Just "chain_dens") Nothing

getSummary :: SQLite SummaryDB
getSummary =
  fromSqlDataWithArgs . head
    <$> run "SELECT * FROM summary"

-- | The UTxO size of the host's last LedgerMetrics, if any.
getLastUTxOSize :: SQLite (Maybe Word64)
getLastUTxOSize =
  fmap fromSqlData . (listToMaybe <=< listToMaybe)
    <$> run "SELECT utxo_size FROM ledgermetrics ORDER BY at DESC LIMIT 1"

getTraceFreqs :: SQLite TraceFreqs
getTraceFreqs =
  ML.fromList . map fromSqlDataPair
    <$> run "SELECT * FROM tracefreq"

loadHostLogsDB :: LoadFromDB l => l -> HostLogs a -> IO (HostLogs [LoadResult l])
loadHostLogsDB loadMode HostLogs{..} =
  case fst hlLogs of
    log@(LogObjectSourceSQLite dbFile) ->
      withDb (fromString dbFile) $ do

        -- mini-migration for backwards compatibility with DBs based on the old TraceStartLeadershipCheckPlus
        _ <- run createLMetricsIfNotExists

        summary@SummaryDB{..} <- getSummary
        lastUTxOSize          <- getLastUTxOSize
        traceFreqs            <- if loadTraceFreqs loadMode
                                   then getTraceFreqs
                                   else pure hlRawTraceFreqs
        rows                  <- maybe (pure []) run (loadQuery loadMode)

        -- Count while the rows are at hand; forced, so the count doesn't keep them alive.
        let !loadedCount      = length rows
            rows'             = map (loadConvert loadMode summary) rows

        pure $ HostLogs
          { hlRawTraceFreqs   = traceFreqs
          , hlRawFirstAt      = Just sdbFirstAt
          , hlRawLastAt       = Just sdbLastAt
          , hlRawLastUTxOSize = lastUTxOSize
          , hlLoadedCount     = loadedCount <$ loadQuery loadMode  -- Nothing if the load mode runs no query
          , hlRawLines        = sdbLines
          , hlLogs            = (log, rows')
          , ..
          }
    other -> error $ "loadHostLogsDB: expected SQLite DB file, got " ++ show other
