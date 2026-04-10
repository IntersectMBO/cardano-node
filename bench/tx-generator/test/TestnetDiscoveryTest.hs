{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module TestnetDiscoveryTest
  ( testnetDiscoveryTests
  ) where

import           Prelude

import           Data.Aeson (Value, (.=), encode, fromJSON, object, toJSON)
import           Data.Aeson.Types (Result (..))
import           Data.ByteString.Lazy as LBS (writeFile)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultPortFile,
                   defaultSocketPath, defaultUtxoSKeyPath)
import           Cardano.TxGenerator.Setup.TestnetDiscovery (FillDefaults (..), ForceInfra (..),
                   TestnetConfig (..), TestnetMergeFlags (..), discoverTestnetConfig)


testnetDiscoveryTests :: TestTree
testnetDiscoveryTests = testGroup "TestnetDiscovery"
  [ testCase "round-trip: discoverTestnetConfig result survives JSON serialization" roundTripTest
  ]


-- | Create a mock testnet directory with the minimum files needed by discoverTestnetConfig,
-- then verify that the discovered NixServiceOptions survives a JSON round-trip.
roundTripTest :: Assertion
roundTripTest = withSystemTempDirectory "mock-testnet" $ \tmpDir -> do
  setupMockTestnetDir tmpDir

  let tc = TestnetConfig
        { tcDir = tmpDir
        , tcMergeFlags = TestnetMergeFlags FillDefaults NoForceInfra
        }
  opts <- discoverTestnetConfig tc (object mempty)

  case fromJSON (toJSON opts) of
    Error err ->
      assertFailure $ "JSON round-trip deserialization failed: " ++ err
    Success opts' ->
      opts @?= opts'


-- | Set up a minimal mock testnet directory with all files that discoverTestnetConfig expects.
setupMockTestnetDir :: FilePath -> IO ()
setupMockTestnetDir dir = do
  mapM_ (setupNodeDir dir) [1..3]

  let socketPath = dir </> defaultSocketPath 1
  createDirectoryIfMissing True (takeDirectory socketPath)
  Prelude.writeFile socketPath ""

  let sigKeyPath = dir </> defaultUtxoSKeyPath 1
  createDirectoryIfMissing True (takeDirectory sigKeyPath)
  Prelude.writeFile sigKeyPath "{}"

  let configPath = dir </> defaultConfigFile
  LBS.writeFile configPath $ encode minimalTestnetConfig


-- | Create a node data directory with a port file.
setupNodeDir :: FilePath -> Int -> IO ()
setupNodeDir dir idx = do
  let portPath = dir </> defaultPortFile idx
  createDirectoryIfMissing True (takeDirectory portPath)
  Prelude.writeFile portPath (show (30000 + idx))


-- | Minimal configuration.yaml that will parse and allow era detection.
minimalTestnetConfig :: Value
minimalTestnetConfig = object
  [ "Protocol"                       .= ("Cardano" :: String)
  , "LastKnownBlockVersion-Major"    .= (9 :: Int)
  , "LastKnownBlockVersion-Minor"    .= (0 :: Int)
  , "LastKnownBlockVersion-Alt"      .= (0 :: Int)
  , "ByronGenesisFile"               .= ("byron-genesis.json" :: String)
  , "ShelleyGenesisFile"             .= ("shelley-genesis.json" :: String)
  , "AlonzoGenesisFile"              .= ("alonzo-genesis.json" :: String)
  , "ConwayGenesisFile"              .= ("conway-genesis.json" :: String)
  , "DijkstraGenesisFile"            .= ("dijkstra-genesis.json" :: String)
  , "RequiresNetworkMagic"           .= ("RequiresMagic" :: String)
  , "ExperimentalHardForksEnabled"   .= True
  , "ExperimentalProtocolsEnabled"   .= True
  , "TestShelleyHardForkAtEpoch"     .= (0 :: Int)
  , "TestAllegraHardForkAtEpoch"     .= (0 :: Int)
  , "TestMaryHardForkAtEpoch"        .= (0 :: Int)
  , "TestAlonzoHardForkAtEpoch"      .= (0 :: Int)
  , "TestBabbageHardForkAtEpoch"     .= (0 :: Int)
  , "TestConwayHardForkAtEpoch"      .= (0 :: Int)
  ]
