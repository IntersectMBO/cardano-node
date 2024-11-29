
module Cardano.Unlog.BackendDB
       ( prepareDB
       , runLiftLogObjectsDB
       ) where

import           Cardano.Analysis.API.Ground (JsonLogfile (..))
import           Cardano.Prelude (ExceptT, Text)
import           Cardano.Unlog.LogObject (HostLogs (..), LogObject (..), RunLogs (..), fromTextRef)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util (withTimingInfo)

import           Prelude hiding (log)

import           Control.Exception (SomeException (..), catch)
import           Control.Monad
import           Data.Aeson as Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List (sort)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           System.Directory (removeFile)

import           Database.Sqlite.Easy hiding (Text)



runLiftLogObjectsDB :: RunLogs () -> ExceptT Text IO (RunLogs [LogObject])
runLiftLogObjectsDB _rl@RunLogs{..} = liftIO $ do
  print dbNames
  pure $
    let hostLogs' = (`setLogs` []) `fmap` rlHostLogs
    in RunLogs{rlHostLogs = hostLogs', ..}
  where
    dbNames :: [ConnectionString]
    dbNames = map (fromString . unJsonLogfile . fst . hlLogs) (Map.elems rlHostLogs)

    setLogs :: HostLogs a -> b -> HostLogs b
    setLogs hl x = hl { hlLogs = (fst $ hlLogs hl, x) }


prepareDB :: String -> [FilePath] -> FilePath -> ExceptT Text IO ()
prepareDB machName (sort -> logFiles) outFile = liftIO $ do

  removeFile outFile `catch` \SomeException{} -> pure ()

  withTimingInfo ("prepareDB/" ++ machName) $ withDb dbName $ do
    mapM_ run createSchema

    tracefreqs <- foldM prepareFile (ML.empty :: TraceFreqs) logFiles

    transaction $ mapM_ runSqlRunnable (traceFreqsToSql tracefreqs)

    (tMin, tMax) <- liftIO $ tMinMax logFiles
    now          <- liftIO $ getCurrentTime
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
    loMin = head [ lo | Just lo <- map Aeson.decode ls2 ]
    loMax = fromJust (Aeson.decode $ last ls2)
  pure (loAt loMin, loAt loMax)
tMinMax logs = do
  (tMin, _   ) <- tMinMax [head logs]
  (_   , tMax) <- tMinMax [last logs]
  pure (tMin, tMax)
