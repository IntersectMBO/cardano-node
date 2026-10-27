{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use camelCase" -}

module Cardano.Testnet.Test.Golden.Status
  ( golden_StatusJson
  , golden_StatusTable
  , golden_StatusCommand
  ) where

import           Cardano.Api (BlockHeader, BlockNo (..), Hash, SlotNo (..),
                   deserialiseFromRawBytesHex)

import           Cardano.Testnet.Test.Golden.Util (execDetailCardanoTestnet)

import           Prelude

import           Data.Aeson (Value (..), eitherDecodeFileStrict, eitherDecodeStrict)
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
import           System.FilePath ((</>))

import           Testnet.Manifest (ManifestNodeRole (..))
import           Testnet.Status.Run (renderStatusReportHumanReadable)
import           Testnet.Status.Types

import           Hedgehog (Property, evalIO, (===))
import           Hedgehog.Extras.Test.Base (assertWith, leftFail, leftFailM, moduleWorkspace,
                   propertyOnce)
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
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just True) NodeStalled (Just oldTip) Nothing
      , NodeProbeResult "node3" RoleRelay Nothing Nothing NodeUnreachable Nothing
          (Just "no answer within 5s")
      , NodeProbeResult "node4" RoleRelay (Just 12348) (Just False) NodeDown Nothing
          (Just "Network.Socket.connect: <socket: 23>: does not exist (Connection refused)")
      ]
  , reportProblemExplanation = Nothing
  }

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
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just True) NodeOk (Just freshTip) Nothing
      ]
  }

sampleStalledReport :: StatusReport
sampleStalledReport = sampleReport
  { reportStatus = NetworkStalled
  , reportBestTip = Just oldTip
  , reportNodes =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just True) NodeStalled (Just oldTip) Nothing
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just False) NodeDown Nothing
          (Just "Network.Socket.connect: <socket: 23>: does not exist (Connection refused)")
      ]
  }

sampleStalledNoBlocksReport :: StatusReport
sampleStalledNoBlocksReport = sampleReport
  { reportStatus = NetworkStalled
  , reportBestTip = Nothing
  , reportNodes =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just True) NodeStalled Nothing Nothing
      , NodeProbeResult "node2" RoleRelay (Just 12346) (Just True) NodeStalled Nothing Nothing
      ]
  }

sampleStoppedReport :: StatusReport
sampleStoppedReport = sampleReport
  { reportStatus = NetworkStopped
  , reportBestTip = Nothing
  , reportNodes =
      [ NodeProbeResult "node1" RoleSpo (Just 12345) (Just False) NodeDown Nothing
          (Just "Network.Socket.connect: <socket: 23>: does not exist (Connection refused)")
      , NodeProbeResult "node2" RoleRelay Nothing Nothing NodeUnreachable Nothing
          (Just "no answer within 5s")
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
  diffVsGoldenFile (renderStatusReportHumanReadable sampleReport) $
    base </> goldenFile "status.txt"
  diffVsGoldenFile (renderStatusReportHumanReadable sampleNoTestnetReport) $
    base </> goldenFile "status_no_testnet.txt"
  diffVsGoldenFile
    (renderStatusReportHumanReadable sampleNoTestnetReport{reportProblemExplanation = Just NotReady}) $
    base </> goldenFile "status_no_testnet_not_ready.txt"
  diffVsGoldenFile
    (renderStatusReportHumanReadable sampleNoTestnetReport{reportProblemExplanation = Just NotATestnetDir}) $
    base </> goldenFile "status_no_testnet_not_a_testnet_dir.txt"
  diffVsGoldenFile (renderStatusReportHumanReadable sampleRunningReport) $
    base </> goldenFile "status_running.txt"
  diffVsGoldenFile (renderStatusReportHumanReadable sampleStalledReport) $
    base </> goldenFile "status_stalled.txt"
  diffVsGoldenFile (renderStatusReportHumanReadable sampleStalledNoBlocksReport) $
    base </> goldenFile "status_stalled_no_blocks.txt"
  diffVsGoldenFile (renderStatusReportHumanReadable sampleStoppedReport) $
    base </> goldenFile "status_stopped.txt"

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
    , "No testnet (The directory " <> missing <> " does not exist.)"
    ]
  err === ""

  (jsonCode, jsonOut, jsonErr) <- execDetailCardanoTestnet ["status", "--testnet-dir", missing, "--json"]
  jsonCode === ExitFailure 5
  json <- leftFail (eitherDecodeStrict (Text.encodeUtf8 $ Text.pack jsonOut) :: Either String Value)
  golden <- leftFailM . evalIO $
    (eitherDecodeFileStrict (base </> goldenFile "status_no_testnet.json") :: IO (Either String Value))
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
