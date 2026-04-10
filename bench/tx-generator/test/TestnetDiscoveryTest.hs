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
import           Data.Monoid.Extra (mwhen)
import           Data.Either (isLeft)
import           Data.List (isSuffixOf)
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
import           Cardano.TxGenerator.Setup.TestnetDiscovery (FillDefaults (..), ForceInfra (..),
                   TestnetConfig (..), TestnetMergeFlags (..), discoverTestnetConfig)
import           Cardano.Node.Configuration.NodeAddress (unFile)


testnetDiscoveryTests :: TestTree
testnetDiscoveryTests = testGroup "TestnetDiscovery"
  [ testCase "round-trip: JSON serialization" roundTripTest
  , testProperty "merge flags produce predictable results" prop_mergeFlags
  ]


-- | Verify that the discovered NixServiceOptions survives a JSON round-trip.
roundTripTest :: Assertion
roundTripTest = withMockTestnet $ \tmpDir -> do
  opts <- discover tmpDir FillDefaults NoForceInfra (object mempty)
  let json = toJSON opts
  case fromJSON json of
    Error err -> assertFailure $ "JSON round-trip failed: " ++ err ++ "\nJSON: " ++ show json
    Success opts' -> opts @?= opts'


-- Property test --

-- | Non-probe required field names.
baseFieldNames :: [String]
baseFieldNames = [ "debugMode", "era", "inputs_per_tx", "outputs_per_tx"
                 , "tx_fee", "min_utxo_value", "add_tx_size", "init_cooldown" ]

-- | Required probe fields (non-infra, checked in the property).
requiredProbeNames :: [String]
requiredProbeNames = ["tps", "tx_count"]

-- | Infra probe field (discovery always provides it, but user may override).
infraProbeNames :: [String]
infraProbeNames = ["localNodeSocketPath"]

-- | All probe field names.
probeFieldNames :: [String]
probeFieldNames = requiredProbeNames ++ infraProbeNames

-- | All field names subject to the biased selection (keepalive is handled separately).
allFieldNames :: [String]
allFieldNames = baseFieldNames ++ probeFieldNames

-- | Non-infra keys required for 'NixServiceOptions' parsing to succeed.
requiredKeys :: [Key]
requiredKeys = map fromString (baseFieldNames ++ requiredProbeNames)


-- | Generate a user config JSON with biased field selection:
--
-- * 25% base fields only
-- * 25% probe fields only
-- * 30% everything
-- * 20% a random subset (count uniformly chosen from @[0..all]@)
genUserConfig :: Gen Value
genUserConfig = do
  tpsVal       <- choose (1 :: Int, 1000)
  txCountVal   <- choose (1 :: Int, 10000)
  socketVal    <- ("/test/socket/" ++) . show <$> choose (1 :: Int, 100)
  keepaliveVal <- oneof [pure Nothing, Just <$> choose (1 :: Integer, 120)]

  included <- frequency
    [ (25, pure baseFieldNames)
    , (25, pure probeFieldNames)
    , (30, pure allFieldNames)
    , (20, randomSubset allFieldNames)
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
        , ("localNodeSocketPath", toJSON socketVal)
        ]

  pure $ object
    $  [ fromString k .= v | (k, v) <- allFields, has k ]
    ++ [ "keepalive" .= v | Just v <- [keepaliveVal] ]


-- | Pick a uniformly random number of elements from a list.
randomSubset :: [a] -> Gen [a]
randomSubset xs = do
  n <- choose (0, length xs)
  take n <$> shuffle xs


-- | Predict whether 'discoverTestnetConfig' will fail, by inspecting the
-- actual JSON content.
expectFailure :: FillDefaults -> Value -> Bool
expectFailure NoFillDefaults (Object obj) = not $ all (`KM.member` obj) requiredKeys
expectFailure NoFillDefaults _            = True
expectFailure FillDefaults   _            = False


-- | Extract a field from a JSON 'Value', returning 'Nothing' if absent.
jsonField :: FromJSON a => Key -> Value -> Maybe a
jsonField k = parseMaybe (withObject "config" (.: k))


-- | Property: the merge logic in 'discoverTestnetConfig' produces predictable
-- results depending on 'FillDefaults', 'ForceInfra', and which fields the user
-- provides.
--
-- Three independently generated variables: 'FillDefaults', 'ForceInfra', and
-- a biased random user config JSON.
prop_mergeFlags :: Property
prop_mergeFlags =
  forAll arbitraryBoundedEnum $ \fd ->
  forAll arbitraryBoundedEnum $ \fi ->
  forAll genUserConfig $ \userConfig ->
  let fails = expectFailure fd userConfig
  in
  cover 5 (fd == FillDefaults   && fi == NoForceInfra)                "FillDefaults + NoForceInfra (success)" $
  cover 5 (fd == FillDefaults   && fi == ForceInfra)                  "FillDefaults + ForceInfra (success)" $
  cover 1 (fd == NoFillDefaults && fi == NoForceInfra && not fails) "NoFillDefaults + NoForceInfra (success)" $
  cover 1 (fd == NoFillDefaults && fi == ForceInfra && not fails)   "NoFillDefaults + ForceInfra (success)" $
  cover 5 (fails && fi == NoForceInfra)                             "NoFillDefaults + NoForceInfra (failure)" $
  cover 5 (fails && fi == ForceInfra)                               "NoFillDefaults + ForceInfra (failure)" $
  ioProperty $ withMockTestnet $ \tmpDir -> do
    let tryDiscover :: IO (Either SomeException NixServiceOptions)
        tryDiscover = try (evaluate =<< discover tmpDir fd fi userConfig)
    result <- if fails then withSilentStderr tryDiscover else tryDiscover

    pure $ conjoin
      [
        -- Check 1: outcome matches prediction from expectFailure
        counterexample
          ("outcome: expected " ++ (if fails then "failure" else "success")
           ++ ", got " ++ either (\e -> "failure (" ++ show e ++ ")") (const "success") result)
        $ property (isLeft result == fails)

        -- Check 2: in the success case, verify the merged values
      , case result of
          Left _ -> property fails
          Right opts ->
            let expectedTps :: Double
                expectedTps = case (fd, jsonField "tps" userConfig :: Maybe Int) of
                  (_,              Just v)  -> fromIntegral v
                  (FillDefaults,   Nothing) -> 10
                  (NoFillDefaults, Nothing) -> error "unreachable: expectFailure guards this"

                expectedTxCount :: Int
                expectedTxCount = case (fd, jsonField "tx_count" userConfig :: Maybe Int) of
                  (_,              Just v)  -> v
                  (FillDefaults,   Nothing) -> 100
                  (NoFillDefaults, Nothing) -> error "unreachable: expectFailure guards this"

                sigKeyOk :: Bool
                sigKeyOk = defaultUtxoSKeyPath 1 `isSuffixOf` unFile (_nix_sigKey opts)

                socketPathAssertion :: Property
                socketPathAssertion = case (fi, jsonField "localNodeSocketPath" userConfig :: Maybe String) of
                  (NoForceInfra, Just v) -> counterexample "wrong socket path: " $ _nix_localNodeSocketPath opts === v
                  (_,            _)      -> assertSuffix "socket path unexpected:" (defaultSocketPath 1) (_nix_localNodeSocketPath opts)

                -- keepalive is optional, non-infra, non-required:
                -- ForceInfra should never affect it.
                expectedKeepalive :: Maybe Integer
                expectedKeepalive = case (fd, jsonField "keepalive" userConfig :: Maybe Integer) of
                  (_,              Just v)  -> Just v
                  (FillDefaults,   Nothing) -> Just 30
                  (NoFillDefaults, Nothing) -> Nothing

            in conjoin
              [ counterexample ("tps: expected " ++ show expectedTps ++ ", got " ++ show (_nix_tps opts))
                  $ _nix_tps opts === expectedTps
              , counterexample ("tx_count: expected " ++ show expectedTxCount ++ ", got " ++ show (_nix_tx_count opts))
                  $ _nix_tx_count opts === expectedTxCount
              , counterexample ("sigKey should end with testnet path, got: " ++ unFile (_nix_sigKey opts))
                  $ property sigKeyOk
              , socketPathAssertion
              , counterexample ("keepalive: expected " ++ show expectedKeepalive ++ ", got " ++ show (_nix_keepalive opts))
                  $ _nix_keepalive opts === expectedKeepalive
              ]
      ]
  where
    -- Asserts that the specified expected value is a suffix of
    -- the actual value, providing better error messages for path checks.
    assertSuffix :: String -> String -> String -> Property
    assertSuffix preface expectedSuffix actual =
      if expectedSuffix `isSuffixOf` actual
        then property True
        else counterexample ((munless (null preface) preface ++ "\n") ++
              "expected string with suffix: " ++ show expectedSuffix ++ "\n but got: " ++ show actual) $ property False
      where
        munless :: Monoid m => Bool -> m -> m
        munless b = mwhen (not b)


-- Helpers

-- | Run an IO action with stderr silenced. Useful for expected-failure tests
-- where 'die' prints an error message before throwing.
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

discover :: FilePath -> FillDefaults -> ForceInfra -> Value -> IO NixServiceOptions
discover dir fd fi = discoverTestnetConfig
    TestnetConfig { tcDir = dir, tcMergeFlags = TestnetMergeFlags fd fi }


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
