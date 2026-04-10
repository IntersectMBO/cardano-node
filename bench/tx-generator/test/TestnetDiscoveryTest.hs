{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module TestnetDiscoveryTest
  ( testnetDiscoveryTests
  ) where

import           Prelude

import           Control.Exception (evaluate, try, SomeException)
import           Data.Aeson (Value, (.=), encode, fromJSON, object, toJSON)
import           Data.Aeson.Types (Result (..))
import           Data.ByteString.Lazy as LBS (writeFile)
import           Data.List (isSuffixOf)
import           Data.List.NonEmpty (NonEmpty (..))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultPortFile,
                   defaultSocketPath, defaultUtxoSKeyPath)
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..), NodeDescription (..))
import           Cardano.TxGenerator.Setup.TestnetDiscovery (FillDefaults (..), ForceInfra (..),
                   TestnetConfig (..), TestnetMergeFlags (..), discoverTestnetConfig)
import Cardano.Node.Configuration.NodeAddress (unFile)


testnetDiscoveryTests :: TestTree
testnetDiscoveryTests = testGroup "TestnetDiscovery"
  [ testCase "round-trip: JSON serialization" roundTripTest
  , testCase "default flags: correct paths from testnet dir" defaultFlagsPathsTest
  , testCase "default flags: user overrides are picked up" userOverridesTest
  , testCase "NoFillDefaults: missing tx params fails" noFillDefaultsFailsTest
  , testCase "ForceInfra: user infra overrides are ignored" forceInfraTest
  ]


-- | Verify that the discovered NixServiceOptions survives a JSON round-trip.
roundTripTest :: Assertion
roundTripTest = withMockTestnet $ \tmpDir -> do
  opts <- discover tmpDir FillDefaults NoForceInfra (object mempty)
  case fromJSON (toJSON opts) of
    Error err -> assertFailure $ "JSON round-trip failed: " ++ err
    Success opts' -> opts @?= opts'


-- | Default flags with empty user config produces NixServiceOptions with correct paths.
defaultFlagsPathsTest :: Assertion
defaultFlagsPathsTest = withMockTestnet $ \tmpDir -> do
  opts <- discover tmpDir FillDefaults NoForceInfra (object mempty)

  -- Socket path uses Testnet.Paths convention
  assertBool "socket path" $
    defaultSocketPath 1 `isSuffixOf` _nix_localNodeSocketPath opts

  -- Signing key path uses Testnet.Paths convention
  assertBool "signing key path" $
    defaultUtxoSKeyPath 1 `isSuffixOf` unFile (_nix_sigKey opts)

  -- Config file path uses Testnet.Paths convention
  case _nix_nodeConfigFile opts of
    Nothing -> assertFailure "nodeConfigFile should be set"
    Just cfg -> assertBool "config file path" $ defaultConfigFile `isSuffixOf` cfg

  -- Three nodes discovered
  let (n1 :| ns) = _nix_targetNodes opts
  length (n1 : ns) @?= 3

  -- Node names follow convention
  ndName n1 @?= "node1"

  -- Default tx params are filled in
  _nix_tps opts @?= 10
  _nix_tx_count opts @?= 100


-- | Default flags with user overrides: user values are picked up.
userOverridesTest :: Assertion
userOverridesTest = withMockTestnet $ \tmpDir -> do
  let userConfig = object [ "tps" .= (50 :: Int), "tx_count" .= (200 :: Int) ]
  opts <- discover tmpDir FillDefaults NoForceInfra userConfig

  _nix_tps opts @?= 50
  _nix_tx_count opts @?= 200

  -- infra still comes from testnet
  assertBool "socket path still from testnet" $
    defaultSocketPath 1 `isSuffixOf` _nix_localNodeSocketPath opts


-- | NoFillDefaults with empty user config: fails because required tx params are missing.
noFillDefaultsFailsTest :: Assertion
noFillDefaultsFailsTest = withMockTestnet $ \tmpDir -> do
  result <- try (evaluate =<< discover tmpDir NoFillDefaults NoForceInfra (object mempty))
              :: IO (Either SomeException NixServiceOptions)
  case result of
    Left _  -> pure ()  -- expected: die throws an exception
    Right _ -> assertFailure "Expected failure with NoFillDefaults and no user config"


-- | ForceInfra: user-provided infra values are overridden by testnet discovery.
forceInfraTest :: Assertion
forceInfraTest = withMockTestnet $ \tmpDir -> do
  let userConfig = object
        [ "localNodeSocketPath" .= ("/user/custom/socket" :: String)
        , "tps" .= (50 :: Int)
        ]
  opts <- discover tmpDir FillDefaults ForceInfra userConfig

  -- Infra forced back to testnet value
  assertBool "socket path forced to testnet" $
    defaultSocketPath 1 `isSuffixOf` _nix_localNodeSocketPath opts

  -- Non-infra user override is still picked up
  _nix_tps opts @?= 50


-- Helpers

withMockTestnet :: (FilePath -> IO ()) -> Assertion
withMockTestnet action = withSystemTempDirectory "mock-testnet" $ \dir -> do
  setupMockTestnetDir dir
  action dir

discover :: FilePath -> FillDefaults -> ForceInfra -> Value -> IO NixServiceOptions
discover dir fd fi = discoverTestnetConfig
    TestnetConfig { tcDir = dir, tcMergeFlags = TestnetMergeFlags fd fi }


-- | Set up a minimal mock testnet directory.
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


setupNodeDir :: FilePath -> Int -> IO ()
setupNodeDir dir idx = do
  let portPath = dir </> defaultPortFile idx
  createDirectoryIfMissing True (takeDirectory portPath)
  Prelude.writeFile portPath (show (30000 + idx))


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
