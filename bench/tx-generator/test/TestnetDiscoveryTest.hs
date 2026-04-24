{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module TestnetDiscoveryTest
  ( testnetDiscoveryTests
  ) where

import           Prelude

import           Control.Exception (bracket, evaluate, try, SomeException)
import           Data.Aeson (FromJSON, Value (..), (.=), (.:), encode, fromJSON, object, toJSON,
                   withObject)
import           Data.Aeson.KeyMap qualified as KM (member)
import           Data.Aeson.Key (Key, fromString)
import           Data.Aeson.Types (Result (..), parseMaybe)
import           Data.ByteString.Lazy as LBS (writeFile)
import           Data.Either (isLeft)
import           Data.List (isSuffixOf)
import           Data.Monoid.Extra (mwhen)
import           GHC.IO.Handle (hDuplicate, hDuplicateTo)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))
import           System.IO (IOMode (..), hClose, openFile, stderr)
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck hiding (Success, expectFailure)

import           Cardano.Api (AnyCardanoEra (..), CardanoEra (..))
import           Cardano.Node.Testnet.Paths (defaultConfigFile, defaultPortFile,
                   defaultSocketPath, defaultUtxoSKeyPath)
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..))
import           Cardano.TxGenerator.Setup.TestnetDiscovery (TestnetConfig (..),
                   discoverTestnetConfig)
import           Cardano.Node.Configuration.NodeAddress (unFile)
import           Data.Maybe (fromMaybe)


testnetDiscoveryTests :: TestTree
testnetDiscoveryTests = testGroup "TestnetDiscovery"
  [ testCase "round-trip: JSON serialization" roundTripTest
  , testProperty "infra fields always override user config" prop_infraOverride
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


-- Property test --

-- | Non-infra required field names.
requiredFieldNames :: [String]
requiredFieldNames = [ "debugMode", "era", "inputs_per_tx", "outputs_per_tx"
                     , "tx_fee", "min_utxo_value", "add_tx_size", "init_cooldown"
                     , "tps", "tx_count" ]

-- | Required keys for NixServiceOptions parsing to succeed (non-infra only).
requiredKeys :: [Key]
requiredKeys = map fromString requiredFieldNames


-- | Generate a user config JSON with a biased random subset of required fields,
-- and optionally with user-provided infra fields that should be overridden.
genUserConfig :: Gen Value
genUserConfig = do
  tpsVal       <- choose (1 :: Int, 1000)
  txCountVal   <- choose (1 :: Int, 10000)
  socketVal    <- ("/test/socket/" ++) . show <$> choose (1 :: Int, 100)
  keepaliveVal <- oneof [pure Nothing, Just <$> choose (1 :: Integer, 120)]
  includeInfra <- arbitrary

  included <- frequency
    [ (70, pure requiredFieldNames)
    , (30, randomSubset requiredFieldNames)
    ]

  let has :: String -> Bool
      has n = n `elem` included

  let allFields :: [(String, Value)]
      allFields =
        [ ("debugMode",           toJSON False)
        , ("era",                 toJSON (AnyCardanoEra ConwayEra))
        , ("tps",                 toJSON tpsVal)
        , ("tx_count",            toJSON txCountVal)
        , ("inputs_per_tx",       toJSON (2 :: Int))
        , ("outputs_per_tx",      toJSON (2 :: Int))
        , ("tx_fee",              toJSON (212345 :: Int))
        , ("min_utxo_value",      toJSON (1000000 :: Int))
        , ("add_tx_size",         toJSON (39 :: Int))
        , ("init_cooldown",       toJSON (50 :: Double))
        ]

  pure $ object
    $  [ fromString k .= v | (k, v) <- allFields, has k ]
    ++ [ "keepalive" .= v | Just v <- [keepaliveVal] ]
    ++ [ "localNodeSocketPath" .= socketVal | includeInfra ]


-- | Pick a uniformly random number of elements from a list.
randomSubset :: [a] -> Gen [a]
randomSubset xs = do
  n <- choose (0, length xs)
  take n <$> shuffle xs


-- | Predict whether 'discoverTestnetConfig' will fail: all required
-- non-infra fields must be present in the user config.
expectFailure :: Value -> Bool
expectFailure (Object obj) = not $ all (`KM.member` obj) requiredKeys
expectFailure _            = True


-- | Extract a field from a JSON 'Value', returning 'Nothing' if absent.
jsonField :: FromJSON a => Key -> Value -> Maybe a
jsonField k = parseMaybe (withObject "config" (.: k))


-- | Property: infrastructure fields always come from the testnet directory
-- regardless of what the user supplies; non-infra fields come from the user
-- config; missing required non-infra fields cause failure.
prop_infraOverride :: Property
prop_infraOverride =
  forAll genUserConfig $ \userConfig ->
  let fails = expectFailure userConfig
      hasUserSocket = case jsonField "localNodeSocketPath" userConfig :: Maybe String of
                        Just _ -> True
                        Nothing -> False
  in
  cover 30 (not fails)                        "success" $
  cover 5  fails                              "failure (missing required)" $
  cover 5  (hasUserSocket && not fails)       "user provides infra (should be overridden)" $
  ioProperty $ withMockTestnet $ \tmpDir -> do
    let tryDiscover :: IO (Either SomeException NixServiceOptions)
        tryDiscover = try (evaluate =<< discover tmpDir userConfig)
    result <- if fails then withSilentStderr tryDiscover else tryDiscover

    pure $ conjoin
      [
        counterexample
          ("outcome: expected " ++ (if fails then "failure" else "success")
           ++ ", got " ++ either (\e -> "failure (" ++ show e ++ ")") (const "success") result)
        $ property (isLeft result == fails)

      , case result of
          Left _ -> property fails
          Right opts ->
            let expectedTps :: Double
                expectedTps = case jsonField "tps" userConfig :: Maybe Int of
                  Just v  -> fromIntegral v
                  Nothing -> error "unreachable: expectFailure guards this"

                expectedTxCount :: Int
                expectedTxCount = case jsonField "tx_count" userConfig :: Maybe Int of
                  Just v  -> v
                  Nothing -> error "unreachable: expectFailure guards this"

                expectedKeepalive :: Maybe Integer
                expectedKeepalive = jsonField "keepalive" userConfig

            in conjoin
              [ assertSuffix "sigKey from discovery:"
                  (defaultUtxoSKeyPath 1)
                  (unFile (_nix_sigKey opts))
              , assertSuffix "socket path from discovery:"
                  (defaultSocketPath 1)
                  (_nix_localNodeSocketPath opts)
              , assertSuffix "nodeConfigFile from discovery:"
                  defaultConfigFile
                  (fromMaybe "" (_nix_nodeConfigFile opts))
              , _nix_tps opts === expectedTps
              , _nix_tx_count opts === expectedTxCount
              , _nix_keepalive opts === expectedKeepalive
              ]
      ]
  where
    assertSuffix :: String -> String -> String -> Property
    assertSuffix preface expectedSuffix actual =
      if expectedSuffix `isSuffixOf` actual
        then property True
        else counterexample (munless (null preface) (preface ++ "\n") ++
              "expected string with suffix: " ++ show expectedSuffix ++ "\n but got: " ++ show actual) $ property False
      where
        munless :: Monoid m => Bool -> m -> m
        munless b = mwhen (not b)


-- Helpers

-- | Run an IO action with stderr silenced.
withSilentStderr :: IO a -> IO a
withSilentStderr action = bracket acquire release (const action)
  where
    acquire = do
      saved <- hDuplicate stderr
      devNull <- openFile "/dev/null" WriteMode
      hDuplicateTo devNull stderr
      pure (saved, devNull)
    release (saved, devNull) = do
      hDuplicateTo saved stderr
      hClose saved
      hClose devNull

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
