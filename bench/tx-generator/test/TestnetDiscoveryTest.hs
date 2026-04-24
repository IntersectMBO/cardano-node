{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module TestnetDiscoveryTest
  ( testnetDiscoveryTests
  ) where

import           Prelude

import           Data.Aeson (Value (..), (.=), encode, fromJSON, object, toJSON)
import           Data.Aeson.Types (Result (..))
import           Data.ByteString.Lazy as LBS (writeFile)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Cardano.Api (AnyCardanoEra (..), CardanoEra (..))
import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultPortFile,
                   defaultSocketPath, defaultUtxoSKeyPath)
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..))
import           Cardano.TxGenerator.Setup.TestnetDiscovery (TestnetConfig (..),
                   discoverTestnetConfig)


testnetDiscoveryTests :: TestTree
testnetDiscoveryTests = testGroup "TestnetDiscovery"
  [ testCase "round-trip: JSON serialization" roundTripTest
  ]


-- | A complete user config that provides all required non-infra fields.
completeUserConfig :: Value
completeUserConfig = object
  [ "debugMode"      .= False
  , "era"            .= AnyCardanoEra ConwayEra
  , "tps"            .= (10 :: Int)
  , "tx_count"       .= (100 :: Int)
  , "inputs_per_tx"  .= (2 :: Int)
  , "outputs_per_tx" .= (2 :: Int)
  , "tx_fee"         .= (212345 :: Int)
  , "min_utxo_value" .= (1000000 :: Int)
  , "add_tx_size"    .= (39 :: Int)
  , "init_cooldown"  .= (50 :: Double)
  ]


-- | Verify that the discovered NixServiceOptions survives a JSON round-trip.
roundTripTest :: Assertion
roundTripTest = withMockTestnet $ \tmpDir -> do
  opts <- discover tmpDir completeUserConfig
  let json = toJSON opts
  case fromJSON json of
    Error err -> assertFailure $ "JSON round-trip failed: " ++ err ++ "\nJSON: " ++ show json
    Success opts' -> opts @?= opts'


-- Helpers

withMockTestnet :: (FilePath -> IO a) -> IO a
withMockTestnet action = withSystemTempDirectory "mock-testnet" $ \dir -> do
  setupMockTestnetDir dir
  action dir

discover :: FilePath -> Value -> IO NixServiceOptions
discover dir = discoverTestnetConfig TestnetConfig { tcDir = dir }


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


-- | Minimal configuration.yaml that will allow node-config parsing.
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
