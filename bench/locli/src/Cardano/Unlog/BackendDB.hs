
module Cardano.Unlog.BackendDB
       ( prepareDB
       , runLiftLogObjectsDB

       -- specific SQLite queries or statements
       , getSummary
       , getTraceFreqs
       , sqlGetEvent
       , sqlGetTxns
       , sqlGetResource
       , sqlGetSlot
       , sqlOrdered
       ) where

import           Cardano.Analysis.API.Ground (Host (..), LogObjectSource (..))
import           Cardano.Prelude (ExceptT, Text)
import           Cardano.Unlog.LogObject (HostLogs (..), LogObject (..), RunLogs (..), fromTextRef)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util (sequenceConcurrentlyChunksOf, withTimingInfo)

import           Prelude hiding (log)

import           Control.Exception (SomeException (..), catch)
import           Control.Monad
import           Data.Aeson as Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List (sort)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text.Short as ShortText (unpack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           GHC.Conc (numCapabilities)
import           System.Directory (removeFile)

import           Database.Sqlite.Easy hiding (Text)


runLiftLogObjectsDB :: RunLogs () -> ExceptT Text IO (RunLogs [LogObject])
runLiftLogObjectsDB RunLogs{rlHostLogs, ..} = liftIO $ do
  hostLogs' <- Map.fromList
    <$> sequenceConcurrentlyChunksOf numCapabilities loadActions
  pure $ RunLogs{ rlHostLogs = hostLogs', ..}
  where
    loadActions = map load (Map.toList rlHostLogs)

    load (host@(Host h), hl) =
      withTimingInfo ("loadHostLogsDB/" ++ ShortText.unpack h) $
        (,) host <$> loadHostLogsDB hl

-- If the logs have been split up into multiple files, e.g. by a log rotator,
-- this assumes sorting log files by *name* results in chronological order
-- of all *trace messages* contained in them.
prepareDB :: String -> [FilePath] -> FilePath -> ExceptT Text IO ()
prepareDB machName (sort -> logFiles) outFile = liftIO $ do

  removeFile outFile `catch` \SomeException{} -> pure ()

  withTimingInfo ("prepareDB/" ++ machName) $ withDb dbName $ do
    mapM_ run createSchema

    tracefreqs <- foldM prepareFile (ML.empty :: TraceFreqs) logFiles

    transaction $ mapM_ runSqlRunnable (traceFreqsToSql tracefreqs)

    (tMin, tMax) <- liftIO $ tMinMax logFiles
    now          <- liftIO getCurrentTime
    let
      dbSummary = SummaryDB
        { sdbName     = fromString machName
        , sdbLines    = sum tracefreqs
        , sdbFirstAt  = tMin
        , sdbLastAt   = tMax
        , sdbCreated  = now
        }
    void $ runSqlRunnable $ summaryToSql dbSummary
  where
    dbName = fromString outFile

prepareFile :: TraceFreqs -> FilePath -> SQLite TraceFreqs
prepareFile tracefreqs log = do
  ls <- BSL.lines <$> liftIO (BSL.readFile log)
  transaction $ foldM go tracefreqs ls
  where
    alterFunc :: Maybe Int -> Maybe Int
    alterFunc = maybe (Just 1) (Just . succ)

    go acc line = case Aeson.eitherDecode line of
      Right logObject@LogObject{loNS, loKind} -> do
        forM_ (logObjectToSql logObject)
            runSqlRunnable

        let name = fromTextRef loNS <> ":" <> fromTextRef loKind
        pure $ ML.alter alterFunc name acc

      Left err -> runSqlRunnable (errorToSql err $ BSL.unpack line) >> pure acc

tMinMax :: [FilePath] -> IO (UTCTime, UTCTime)
tMinMax [] = fail "tMinMax: empty list of log files"
tMinMax [log] = do
  ls2 <- BSL.lines <$> BSL.readFile log
  let
    loMin, loMax :: LogObject
    loMin = head $ mapMaybe Aeson.decode ls2
    loMax = fromJust (Aeson.decode $ last ls2)
  pure (loAt loMin, loAt loMax)
tMinMax logs = do
  (tMin, _   ) <- tMinMax [head logs]
  (_   , tMax) <- tMinMax [last logs]
  pure (tMin, tMax)


-- selects the entire LogObject stream, containing all objects relevant for standard analysis
selectAll :: SQL
selectAll = sqlOrdered
  [ sqlGetEvent
  , sqlGetTxns
  , sqlGetResource
  , sqlGetSlot
  ]

sqlGetEvent, sqlGetTxns, sqlGetResource, sqlGetSlot :: SQLSelect6Cols
sqlGetEvent    = mkSQLSelectFrom "event"    Nothing                              (Just "slot")  (Just "block")     Nothing             (Just "hash")
sqlGetTxns     = mkSQLSelectFrom "txns"     Nothing                              (Just "count") (Just "rejected")  Nothing             (Just "tid")
sqlGetResource = mkSQLSelectFrom "resource" (Just "LOResources")                 Nothing        Nothing            Nothing             (Just "as_blob")
sqlGetSlot     = mkSQLSelectFrom "slot"     (Just "LOTraceStartLeadershipCheck") (Just "slot")  (Just "utxo_size") (Just "chain_dens") Nothing

getSummary :: SQLite SummaryDB
getSummary =
  fromSqlDataWithArgs . head
    <$> run "SELECT * FROM summary"

getTraceFreqs :: SQLite TraceFreqs
getTraceFreqs =
  ML.fromList . map fromSqlDataPair
    <$> run "SELECT * FROM tracefreq"

loadHostLogsDB :: HostLogs a -> IO (HostLogs [LogObject])
loadHostLogsDB hl =
  case fst $ hlLogs hl of
    log@(LogObjectSourceSQLite dbFile) ->
      withDb (fromString dbFile) $ do
        summary@SummaryDB{..} <- getSummary
        traceFreqs            <- getTraceFreqs
        rows                  <- run selectAll

        pure $ hl
          { hlRawTraceFreqs = traceFreqs
          , hlRawFirstAt    = Just sdbFirstAt
          , hlRawLastAt     = Just sdbLastAt
          , hlRawLines      = sdbLines
          , hlLogs          = (log, map (sqlToLogObject summary) rows)
          }
    other -> error $ "loadHostLogsDB: expected SQLite DB file, got " ++ show other
