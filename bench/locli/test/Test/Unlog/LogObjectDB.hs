{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Unlog.LogObjectDB where

import           Cardano.Prelude

import           Cardano.Analysis.API.Ground (LogObjectSource (..))
import           Cardano.Unlog.BackendDB (LoadLogObjects (..), getLastUTxOSize, prepareDB, runLiftLogObjectsDB, selectAll)
import           Cardano.Unlog.LogObject
import           Cardano.Unlog.LogObjectDB
import           Cardano.Unlog.Resources (Resources (..))
import           Cardano.Util (zeroUTCTime)

import           Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime)
import qualified Data.Aeson as AE
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AE (parseEither)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set (difference, empty, fromList, notMember)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Short as ST
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Database.Sqlite.Easy

import           Hedgehog


-- This property ensures there are converter implementations
-- for all LOBody constructors. These converters are used
-- to reliably reconstruct a LOBody value from a database result row.

prop_LOBody_converter_for_each_constructor = property $
  allLOBodyConstructors `Set.difference` knownLOBodyConstructors
  ===
  Set.empty

-- This property round-trips a fixture log stream through an in-memory
-- SQLite DB (the exact same schema & marshalling `locli` uses for real
-- runs) and checks every LogObject's LOBody survives unchanged.
--
-- `loNS` and `loTid` are deliberately not compared: the DB schema does not
-- (and is not meant to) preserve them for most LOBody constructors -- see
-- `sqlToLogObject`. `LOAny`/`LOGeneratorSummary`/`LOTxsAcked` are never
-- written to the DB at all (`logObjectToSql` returns Nothing for them), so
-- they're excluded from the comparison the same way `logObjectToSql`
-- itself excludes them, rather than via a second, separately-maintained
-- list of exclusions.
--
-- To extend coverage: add lines to
-- test/data/logobject-roundtrip-fixture.jsonl, one representative raw
-- node trace line per LOBody shape you want exercised.

fixtureFile :: FilePath
fixtureFile = "test/data/logobject-roundtrip-fixture.jsonl"

-- | The fixture's log objects, decoded line by line as `prepare-db` does. Lines of namespaces
--   without an interpreter (`LOAny`) are dropped, as no analysis sees them; a line that doesn't
--   decode fails the test.
readFixture :: IO [LogObject]
readFixture = do
  ls <- filter (not . BS.null) . BS.lines <$> BS.readFile fixtureFile
  pure . flip mapMaybe ls $ \l -> case AE.eitherDecodeStrict l of
    Left err -> panic $ "fixture line doesn't decode: " <> show err <> "\n" <> decodeUtf8 l
    Right LogObject{loBody = LOAny{}} -> Nothing
    Right lo -> Just lo

-- `sqlToLogObject` only ever reads `sdbName` out of this; the rest are
-- unused placeholders for this test.
fixtureSummary :: SummaryDB
fixtureSummary = SummaryDB
  { sdbName     = "test-host"
  , sdbLines    = 0
  , sdbFirstAt  = zeroUTCTime
  , sdbLastAt   = zeroUTCTime
  , sdbCreated  = zeroUTCTime
  }

-- Comfortably larger than any observed rounding error in the UTCTime <->
-- Double (POSIX seconds) conversion `AsSQLData UTCTime` relies on, and far
-- below the ~0.1ms timestamp granularity that actually matters for
-- benchmarking analysis.
maxClockDrift :: NominalDiffTime
maxClockDrift = 0.00001 -- 10 microseconds

prop_LogObject_sqlite_roundtrip = withTests 1 $ property $ do
  logObjects <- evalIO readFixture
  let persistable = filter (isJust . logObjectToSql) logObjects

  readBack <- evalIO $ withDb ":memory:" $ do
    mapM_ run createSchema
    mapM_ (mapM_ runSqlRunnable . logObjectToSql) logObjects
    rows <- run selectAll
    pure $ map (sqlToLogObject fixtureSummary) rows

  length readBack === length persistable

  for_ (zip persistable readBack) $ \(orig, back) -> do
    loBody back === loBody orig
    assert $ abs (diffUTCTime (loAt back) (loAt orig)) <= maxClockDrift

-- The final UTxO size comes from the DB: the UTxO size of the latest LedgerMetrics by
-- timestamp, not by insertion order. Earlier LedgerMetrics with other UTxO sizes,
-- written first and last, must not be picked.
prop_last_utxo_size_from_db = withTests 1 $ property $ do
  logObjects <- evalIO readFixture
  let metrics = [ (loAt, lo, slot, utxo, density)
                | lo@LogObject{loAt, loBody = LOLedgerMetrics slot utxo density} <- logObjects ]
  (latestAt, latest, slot, latestUtxo, density) <-
    maybe failure pure . lastMay $ sortBy (comparing (\(at, _, _, _, _) -> at)) metrics
  let earlier h = latest { loAt   = addUTCTime (- (3600 * fromIntegral h)) latestAt
                         , loBody = LOLedgerMetrics slot (latestUtxo + h) density }

  (noMetrics, lastUtxo) <- evalIO $ withDb ":memory:" $ do
    mapM_ run createSchema
    none <- getLastUTxOSize
    mapM_ (mapM_ runSqlRunnable . logObjectToSql) (earlier 1 : logObjects <> [earlier 2])
    (none,) <$> getLastUTxOSize

  noMetrics === Nothing
  lastUtxo  === Just latestUtxo

-- Loading a host's DB (as `unlog-db` does) provides what the summary would otherwise
-- take from a pass over all of the host's log objects: their count, and the last UTxO size.
prop_loadHostLogsDB_counts_and_last_utxo = withTests 1 $ property $ do
  logObjects <- evalIO readFixture
  let persistable = filter (isJust . logObjectToSql) logObjects
      lastUtxo    = lastMay [ u | LogObject{loBody = LOLedgerMetrics{loUtxoSize = u}} <- logObjects ]

  hl <- evalEither =<< evalIO (withSystemTempDirectory "locli-test-db" $ \dir -> runExceptT $ do
    let db = dir </> "node-1.sqlite3"
    prepareDB "node-1" [fixtureFile] db
    rl <- runLiftLogObjectsDB LoadLogObjectsAll RunLogs
            { rlHostLogs = Map.singleton "node-1" (emptyHostLogs (LogObjectSourceSQLite db))
            , rlFilterDate = zeroUTCTime }
    maybe (throwError "no host") pure . head . Map.elems $ rlHostLogs rl)

  hlLoadedCount hl     === Just (length persistable)
  hlLoadedCount hl     === Just (length (snd (hlLogs hl)))
  hlRawLastUTxOSize hl === lastUtxo
  assert $ isJust lastUtxo
 where
   emptyHostLogs src = HostLogs
     { hlRawLogfiles = [], hlRawLines = 0, hlRawTraceFreqs = mempty, hlLogs = (src, ())
     , hlProfile = [], hlRawFirstAt = Nothing, hlRawLastAt = Nothing
     , hlRawLastUTxOSize = Nothing, hlLoadedCount = Nothing }

-- Log manifests as written before these fields existed (and by the file backend) read
-- them as not available.
prop_hostLogs_without_db_fields_readable = withTests 1 $ property $ do
  let hl = HostLogs { hlRawLogfiles = ["node-1.json"], hlRawLines = 1, hlRawTraceFreqs = mempty
                    , hlLogs = (LogObjectSourceOther "node-1.json", ()), hlProfile = []
                    , hlRawFirstAt = Nothing, hlRawLastAt = Nothing
                    , hlRawLastUTxOSize = Just 1, hlLoadedCount = Just 1 }
  obj <- case AE.toJSON hl of
    AE.Object o -> pure o
    _           -> failure
  back :: HostLogs () <- evalEither . AE.parseEither AE.parseJSON . AE.Object $
    foldr KeyMap.delete obj ["hlRawLastUTxOSize", "hlLoadedCount"]
  hlRawLastUTxOSize back === Nothing
  hlLoadedCount back     === Nothing

-- A `Resources` sample with only the fields the observability spec requires (e.g. from a
-- runtime without GC) parses; the absent ones read as 0, also after a round-trip through the
-- DB. Without `CentiCpu`, it doesn't parse.
prop_resources_optional_fields = withTests 1 $ property $ do
  let line fields = "{\"at\":\"2026-11-10T10:00:00Z\",\"ns\":\"Resources\",\"data\":{\"kind\":\"Resources\"" <> fields <> "},\"sev\":\"Info\",\"thread\":\"1\",\"host\":\"node-x\"}"
  lo <- evalEither (AE.eitherDecodeStrict (line ",\"CentiCpu\":7,\"RSS\":1000,\"Threads\":4") :: Either [Char] LogObject)
  case loBody lo of
    LOResources r -> do
      (rCentiCpu r, rRSS r, rThreads r) === (7, 1000, 4)
      [rCentiGC r, rCentiMut r, rGcsMajor r, rGcsMinor r, rAlloc r, rLive r, rHeap r, rCentiBlkIO r, rNetRd r, rNetWr r, rFsRd r, rFsWr r]
        === replicate 12 0
    other -> annotateShow other >> failure
  back <- evalIO $ withDb ":memory:" $ do
    mapM_ run createSchema
    mapM_ runSqlRunnable (logObjectToSql lo)
    map (sqlToLogObject fixtureSummary) <$> run selectAll
  map loBody back === [loBody lo]
  assert . isLeft $ (AE.eitherDecodeStrict (line ",\"RSS\":1000,\"Threads\":4") :: Either [Char] LogObject)

-- This property ensures every namespace `locli` interprets is backed by a
-- real log line in the fixture, so an interpreter can't be added or renamed
-- without being verified against actual node output: namespaces and payload
-- shapes drift across node versions without notice.

-- The fixture holds both edges of one ChainSync server update (rising edge with
-- "risingEdge": true, falling edge without the field): only one is an announcement.
prop_chainSync_update_announced_once = withTests 1 $ property $ do
  logObjects <- evalIO readFixture
  length [ () | LogObject{loBody = LOChainSyncServerSendHeader{}} <- logObjects ] === 1

prop_interpreters_covered_by_fixture = withTests 1 $ property $ do
  fixtureLines <- evalIO $ BS.lines <$> BS.readFile fixtureFile
  namespaces   <- forM fixtureLines $ \l ->
    evalEither $ AE.eitherDecodeStrict l >>= AE.parseEither (AE.withObject "LogObject" (AE..: "ns"))
  let covered = Set.fromList (namespaces :: [Text])
  filter ((`Set.notMember` covered) . ST.toText) logObjectStreamInterpreterKeys
    === []

tests :: IO Bool
tests =
  checkSequential $$discover
