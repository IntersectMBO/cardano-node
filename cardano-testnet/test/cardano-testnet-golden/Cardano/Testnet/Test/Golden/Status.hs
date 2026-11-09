{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use camelCase" -}

module Cardano.Testnet.Test.Golden.Status
  ( golden_StatusJson
  , golden_StatusTable
  , golden_StatusCommand
  ) where

import           Cardano.Api (AnyCardanoEra (..), BlockHeader, BlockNo (..), CardanoEra (..),
                   EpochNo (..), Hash, SlotNo (..), deserialiseFromRawBytesHex)

import           Cardano.Testnet.Test.Golden.Util (execDetailCardanoTestnet)

import           Prelude

import           Data.Aeson (Value (..), eitherDecodeStrict)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           System.Exit (ExitCode (..))
import           System.FilePath (isPathSeparator, (</>))

import           Testnet.Manifest (ManifestNodeRole (..))
import           Testnet.Start.Types (defaultTestnetDir)
import           Testnet.Status.Run (renderStatusReportHumanReadable)
import           Testnet.Status.Types

import           Hedgehog (Property, (===))
import           Hedgehog.Extras.Test.Base (assertWith, leftFail, moduleWorkspace, propertyOnce)
import qualified Hedgehog.Extras.Test.File as H
import           Hedgehog.Extras.Test.Golden (diffVsGoldenFile)
import qualified Hedgehog.Extras.Test.Process as H

-- | A fixed sample report with one node in each state, so that every node
-- field, with and without a value, is pinned.
sampleReport :: StatusReport
sampleReport = StatusReport
  { reportOutputDir          = "testnet"
  , reportCheckedAt          = UTCTime (fromGregorian 2026 1 1) 60
  , reportStatus             = NetworkDegraded
  , reportBestTip            = Just freshTip
  , reportNodes              =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just True) NodeOk (Just freshTip) Nothing
          "logs/node1/stdout.log" "logs/node1/stderr.log" (Just 0) (Just sampleMempool)
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just True) NodeStalled (Just oldTip) Nothing
          "logs/node2/stdout.log" "logs/node2/stderr.log" (Just 36) (Just emptyMempool)
      , NodeProbeResult "node3" RoleRelay Nothing Nothing NodeUnreachable Nothing
          (Just "no answer within 5s")
          "logs/node3/stdout.log" "logs/node3/stderr.log" Nothing Nothing
      , NodeProbeResult "node4" RoleRelay (Just 12348) (Just False) NodeDown Nothing
          (Just "Network.Socket.connect: <socket: 23>: does not exist (Connection refused)")
          "logs/node4/stdout.log" "logs/node4/stderr.log" Nothing Nothing
      ]
  , reportProblemExplanation = Nothing
  , reportChain              = Just (ChainInfo (AnyCardanoEra ConwayEra) (EpochNo 2) 234 266)
  }

-- | The mempools of the sample nodes that answered: one with a transaction,
-- and an empty one.
sampleMempool, emptyMempool :: MempoolInfo
sampleMempool = MempoolInfo 1 239 129024
emptyMempool = MempoolInfo 0 0 129024

-- | The tips of the sample reports: a fresh one and an old one.
freshTip, oldTip :: TipInfo
freshTip = TipInfo (SlotNo 1234) (BlockNo 56)
  (sampleHash "cc061706c16ad2b35450f1528f5652ff172f9b0acabcdc29284e2363d3bebb2a") 0.5
oldTip = TipInfo (SlotNo 400) (BlockNo 20)
  (sampleHash "5b1e29bbc6e3e4d1f4f4f6bb2dbd3c1b2a7c68b7a8a4b3a1c4e0d2f9e8a7b6c5") 83.4

-- | Fixed sample reports for the other network states, for the text goldens.
sampleRunningReport :: StatusReport
sampleRunningReport = sampleReport
  { reportStatus = NetworkRunning
  , reportNodes =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just True) NodeOk (Just freshTip) Nothing
          "logs/node1/stdout.log" "logs/node1/stderr.log" (Just 0) (Just sampleMempool)
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just True) NodeOk (Just freshTip) Nothing
          "logs/node2/stdout.log" "logs/node2/stderr.log" (Just 0) (Just sampleMempool)
      ]
  }

sampleStalledReport :: StatusReport
sampleStalledReport = sampleReport
  { reportStatus = NetworkStalled
  , reportBestTip = Just oldTip
  , reportChain = Just (ChainInfo (AnyCardanoEra ConwayEra) (EpochNo 0) 400 100)
  , reportNodes =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just True) NodeStalled (Just oldTip) Nothing
          "logs/node1/stdout.log" "logs/node1/stderr.log" (Just 0) (Just emptyMempool)
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just False) NodeDown Nothing
          (Just "Network.Socket.connect: <socket: 23>: does not exist (Connection refused)")
          "logs/node2/stdout.log" "logs/node2/stderr.log" Nothing Nothing
      ]
  }

sampleStalledNoBlocksReport :: StatusReport
sampleStalledNoBlocksReport = sampleReport
  { reportStatus = NetworkStalled
  , reportBestTip = Nothing
  , reportChain = Nothing
  , reportNodes =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just True) NodeStalled Nothing Nothing
          "logs/node1/stdout.log" "logs/node1/stderr.log" Nothing (Just emptyMempool)
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just True) NodeStalled Nothing Nothing
          "logs/node2/stdout.log" "logs/node2/stderr.log" Nothing (Just emptyMempool)
      ]
  }

sampleStoppedReport :: StatusReport
sampleStoppedReport = sampleReport
  { reportStatus = NetworkStopped
  , reportBestTip = Nothing
  , reportChain = Nothing
  , reportNodes =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just False) NodeDown Nothing
          (Just "Network.Socket.connect: <socket: 23>: does not exist (Connection refused)")
          "logs/node1/stdout.log" "logs/node1/stderr.log" Nothing Nothing
      , NodeProbeResult "node2" RoleRelay Nothing Nothing NodeUnreachable Nothing
          (Just "no answer within 5s")
          "logs/node2/stdout.log" "logs/node2/stderr.log" Nothing Nothing
      ]
  }

-- | A fixed sample report for a directory without a testnet: it has no nodes
-- and no tip, so the empty and null forms of the output are pinned.
sampleNoTestnetReport :: StatusReport
sampleNoTestnetReport = StatusReport
  { reportOutputDir          = "testnet"
  , reportCheckedAt          = UTCTime (fromGregorian 2026 1 1) 60
  , reportStatus             = NoTestnet
  , reportBestTip            = Nothing
  , reportNodes              = []
  , reportProblemExplanation = Just DirMissing
  , reportChain              = Nothing
  }

-- | Parse a fixture hash, failing loudly on a typo in the literal.
sampleHash :: ByteString -> Hash BlockHeader
sampleHash h =
  either (\e -> error $ "sampleReport: bad fixture hash: " <> show e) id $
    deserialiseFromRawBytesHex h

goldenFile :: FilePath -> FilePath
goldenFile name = "cardano-testnet/test/cardano-testnet-golden/files/golden" </> name

-- | The golden files are also checked against
-- @cardano-testnet/schemas/status.schema.json@ (@make testnet-status-validate@),
-- so together they check that the output matches the schema.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_StatusJson/"'@
golden_StatusJson :: Property
golden_StatusJson = propertyOnce $ do
  base <- H.getProjectBase
  diffVsGoldenFile (encode sampleReport) $ base </> goldenFile "status.json"
  diffVsGoldenFile (encode sampleNoTestnetReport) $ base </> goldenFile "status_no_testnet.json"
  where
    encode = Text.unpack . Text.decodeUtf8 . LBS.toStrict . encodePretty

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_StatusTable/"'@
golden_StatusTable :: Property
golden_StatusTable = propertyOnce $ do
  base <- H.getProjectBase
  diffVsGoldenFile (render sampleReport) $
    base </> goldenFile "status.txt"
  diffVsGoldenFile (render sampleNoTestnetReport) $
    base </> goldenFile "status_no_testnet.txt"
  diffVsGoldenFile
    (render sampleNoTestnetReport{reportProblemExplanation = Just NotReady}) $
    base </> goldenFile "status_no_testnet_not_ready.txt"
  diffVsGoldenFile
    (render sampleNoTestnetReport{reportProblemExplanation = Just NotATestnetDir}) $
    base </> goldenFile "status_no_testnet_not_a_testnet_dir.txt"
  diffVsGoldenFile (render sampleRunningReport) $
    base </> goldenFile "status_running.txt"
  diffVsGoldenFile (render sampleStalledReport) $
    base </> goldenFile "status_stalled.txt"
  diffVsGoldenFile (render sampleStalledNoBlocksReport) $
    base </> goldenFile "status_stalled_no_blocks.txt"
  diffVsGoldenFile (render sampleStoppedReport) $
    base </> goldenFile "status_stopped.txt"

  -- With the default directory "./testnet", each log path has one kind of
  -- separator, the one of the platform. The comparisons above cannot tell,
  -- because 'render' turns every separator into '/'.
  assertWith (show $ renderStatusReportHumanReadable sampleStoppedReport{reportOutputDir = defaultTestnetDir}) $
    isInfixOf $ "(logs: " <> ("testnet" </> "logs" </> "node1" </> "stdout.log")
                  <> ", " <> ("testnet" </> "logs" </> "node1" </> "stderr.log") <> ")"
  where
    -- The golden files have '/' in the log paths, so on Windows every path
    -- separator is turned into '/' before the comparison.
    render :: StatusReport -> String
    render = map (\c -> if isPathSeparator c then '/' else c) . show . renderStatusReportHumanReadable

-- | The real command, without a testnet: the exit code, stdout and stderr for
-- a missing directory, as a table and as JSON, and for a broken manifest. The
-- JSON is the golden one, apart from the time of the check.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-golden --test-options '-p "/golden_StatusCommand/"'@
golden_StatusCommand :: Property
golden_StatusCommand = propertyOnce . moduleWorkspace "status-command" $ \dir -> do
  base <- H.getProjectBase
  let missing = dir </> "missing"

  (code, out, err) <- execDetailCardanoTestnet ["status", "--testnet-dir", missing]
  code === ExitFailure 5
  lines (filter (/= '\r') out) ===
    [ "Network: NO-TESTNET - no manifest.json in " <> missing
    , "No testnet (The directory"
    , missing
    , "does not exist.)"
    ]
  err === ""

  (jsonCode, jsonOut, jsonErr) <- execDetailCardanoTestnet ["status", "--testnet-dir", missing, "--json"]
  jsonCode === ExitFailure 5
  json <- leftFail (eitherDecodeStrict (Text.encodeUtf8 $ Text.pack jsonOut) :: Either String Value)
  golden <- H.readJsonFileOk $ base </> goldenFile "status_no_testnet.json"
  withoutCheckedAt json === withoutCheckedAt golden
  jsonErr === ""

  H.writeFile (dir </> "manifest.json") "{"
  (brokenCode, brokenOut, brokenErr) <- execDetailCardanoTestnet ["status", "--testnet-dir", dir]
  brokenCode === ExitFailure 1
  brokenOut === ""
  assertWith brokenErr ("manifest.json" `isInfixOf`)
  where
    withoutCheckedAt :: Value -> Value
    withoutCheckedAt (Object o) = Object (KeyMap.delete "checkedAt" o)
    withoutCheckedAt v = v
