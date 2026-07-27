{-# LANGUAGE TypeApplications #-}

-- | End-to-end regression test for the downstream @db-synthesizer@: build a
-- Cardano 'ProtocolInfo' and block forgers from a node config file plus a
-- (bulk) forging-credentials fixture, synthesize a ChainDB, immutalise it, and
-- analyse it — checking the block count is preserved end to end.
--
-- This is the config/credential-driven pipeline that used to live in
-- @ouroboros-consensus@'s @tools-test@ and moved downstream with the eject. It
-- deliberately uses real bulk credentials (a proper KES validity window) and the
-- original forge limits, exercising the path the standalone tool takes — not the
-- short-lived testlib credentials the in-repo synthesis-only test now uses.
module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Node.Tools.DBSynthesizer (initializeProtocol)
import           Cardano.Node.Types (KESSource, ProtocolFilepaths (..))
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import           Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import           Cardano.Tools.DBSynthesizer.Types
import           Ouroboros.Consensus.Block (WithOrigin (Origin))
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Fixtures live next to this test; paths are relative to the @cardano-node@
-- package directory (cabal's working directory when running the test suite).
fixtureDir, nodeConfig, bulkCreds, chainDB :: FilePath
fixtureDir = "cardano-node/test/db-synthesizer/disk/config"
nodeConfig = fixtureDir <> "/config.json"
bulkCreds = fixtureDir <> "/bulk-creds-k2.json"
chainDB = "cardano-node/test/db-synthesizer/disk/chaindb"

-- | Forging credentials for the test: only the bulk-credentials file, mirroring
-- how the tool is typically driven. Bulk creds carry a real KES validity window,
-- so the original (larger) forge limits below stay well within it.
testProtocolFiles :: ProtocolFilepaths
testProtocolFiles =
  ProtocolFilepaths
    { byronCertFile = Nothing
    , byronKeyFile = Nothing
    , shelleyKESSource = Nothing :: Maybe KESSource
    , shelleyVRFFile = Nothing
    , shelleyCertFile = Nothing
    , shelleyBulkCredsFile = Just bulkCreds
    }

testSynthOptionsCreate :: DBSynthesizerOptions
testSynthOptionsCreate =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitEpoch 1
    , synthOpenMode = OpenCreateForce
    }

testSynthOptionsAppend :: DBSynthesizerOptions
testSynthOptionsAppend =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitSlot 8192
    , synthOpenMode = OpenAppend
    }

testImmutaliserConfig :: DBImmutaliser.Opts
testImmutaliserConfig =
  DBImmutaliser.Opts
    { DBImmutaliser.dbDirs =
        DBImmutaliser.DBDirs
          { DBImmutaliser.immDBDir = chainDB <> "/immutable"
          , DBImmutaliser.volDBDir = chainDB <> "/volatile"
          }
    , DBImmutaliser.configFile = nodeConfig
    , DBImmutaliser.verbose = False
    , DBImmutaliser.dotOut = Nothing
    , DBImmutaliser.dryRun = False
    }

testAnalyserConfig :: DBAnalyserConfig
testAnalyserConfig =
  DBAnalyserConfig
    { dbDir = chainDB
    , ldbBackend = V2InMem
    , verbose = False
    , selectDB = SelectImmutableDB Origin
    , validation = Just ValidateAllBlocks
    , analysis = CountBlocks
    , confLimit = Unlimited
    }

testBlockArgs :: Cardano.Args (CardanoBlock StandardCrypto)
testBlockArgs = Cardano.CardanoBlockArgs nodeConfig Nothing

-- | 1. synthesize a ChainDB from scratch (create) and count blocks forged.
--   2. append to it and count blocks forged.
--   3. copy the VolatileDB into the ImmutableDB.
--   4. analyse the ImmutableDB and confirm the total block count matches.
blockCountTest :: (String -> IO ()) -> Assertion
blockCountTest logStep = do
  logStep "building protocol from config + bulk credentials"
  (protocolInfo, mkForgers, epochSize) <- initializeProtocol nodeConfig testProtocolFiles

  logStep "running synthesis - create"
  resultCreate <-
    DBSynthesizer.synthesize genTxs testSynthOptionsCreate epochSize chainDB (protocolInfo, mkForgers)
  let blockCountCreate = resultForged resultCreate
  blockCountCreate > 0 @? "no blocks have been forged during create step"

  logStep "running synthesis - append"
  resultAppend <-
    DBSynthesizer.synthesize genTxs testSynthOptionsAppend epochSize chainDB (protocolInfo, mkForgers)
  let blockCountAppend = resultForged resultAppend
  blockCountAppend > 0 @? "no blocks have been forged during append step"

  logStep "copy volatile to immutable DB"
  DBImmutaliser.run testImmutaliserConfig

  logStep "running analysis"
  resultAnalysis <- DBAnalyser.analyse testAnalyserConfig testBlockArgs

  let blockCount = blockCountCreate + blockCountAppend
  resultAnalysis == Just (ResultCountBlock blockCount)
    @? "wrong number of blocks encountered during analysis \
       \ (counted: "
      ++ show resultAnalysis
      ++ "; expected: "
      ++ show blockCount
      ++ ")"
 where
  genTxs _ _ _ _ = pure []

tests :: TestTree
tests =
  testGroup
    "db-synthesizer"
    [ testCaseSteps "synthesize (bulk creds) -> immutalise -> analyse: blockCount\n" blockCountTest
    ]

main :: IO ()
main = do
  cryptoInit
  defaultMain tests
