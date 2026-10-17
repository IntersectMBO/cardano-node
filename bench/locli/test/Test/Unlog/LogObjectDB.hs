{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Unlog.LogObjectDB where

import           Cardano.Prelude

import           Cardano.Unlog.BackendDB (selectAll)
import           Cardano.Unlog.BackendFile (readLogObjectStream)
import           Cardano.Unlog.LogObject
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util (zeroUTCTime)

import           Data.Time.Clock (NominalDiffTime, diffUTCTime)
import qualified Data.Set as Set (difference, empty)

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
  logObjects <- evalIO $ readLogObjectStream fixtureFile False Nothing
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

tests :: IO Bool
tests =
  checkSequential $$discover
