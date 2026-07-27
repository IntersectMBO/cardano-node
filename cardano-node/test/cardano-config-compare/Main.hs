{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the dual-parse (node vs @cardano-config@) comparison:
-- 'deprecatedFlagWarnings' and 'compareConfigurations' run end-to-end on a
-- fixture resolved by both the POM parser and cardano-config + the adapter.
module Main (main) where

import           Data.List (isInfixOf, isPrefixOf)
import           Data.Monoid (Last (..))

import qualified Cardano.Configuration as Cfg
import           Cardano.Node.Configuration.CardanoConfigAdapter
                   (cardanoConfigToNodeConfiguration)
import           Cardano.Node.Configuration.CardanoConfigCompare
                   (compareConfigurations, deprecatedFlagWarnings)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), defaultPartialNodeConfiguration,
                   makeNodeConfiguration, parseNodeConfigurationFP)
import           Cardano.Node.Types (ConfigYamlFilePath (..))

import           Test.Tasty
import           Test.Tasty.HUnit

-- | The db-synthesizer fixture configuration (test cwd is the repo root).
configPath :: FilePath
configPath = "cardano-node/test/db-synthesizer/disk/config/config.json"

-- | Documented, expected divergences on this fixture — none: the node and
-- cardano-config are expected to agree on everything.
allowedResidualLabels :: [String]
allowedResidualLabels = []

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "cardano-config dual-parse comparison"
  [ testCase "deprecated CLI aliases yield migration guidance" testDeprecatedAliases
  , testCase "removed mempool flags yield removal guidance" testRemovedMempoolFlags
  , testCase "no guidance for accepted / unrelated flags" testNoFalsePositives
  , testCase "compareConfigurations runs on the fixture (divergences ⊆ residuals)"
      testFixtureComparison
  ]

testDeprecatedAliases :: Assertion
testDeprecatedAliases = do
  let warnings =
        deprecatedFlagWarnings
          ["--delegation-certificate", "x", "--signing-key", "y", "--non-producing-node"]
      suggests new = any (new `isInfixOf`) warnings
  assertBool "suggests --byron-delegation-certificate" (suggests "--byron-delegation-certificate")
  assertBool "suggests --byron-signing-key"            (suggests "--byron-signing-key")
  assertBool "suggests --start-as-non-producing-node"  (suggests "--start-as-non-producing-node")
  length warnings @?= 3

testRemovedMempoolFlags :: Assertion
testRemovedMempoolFlags = do
  let warnings = deprecatedFlagWarnings ["--mempool-capacity-override", "100"]
  length warnings @?= 1
  assertBool "says no longer supported"
    (any ("no longer supported" `isInfixOf`) warnings)
  assertBool "points to MempoolCapacityBytesOverride in the config file"
    (any ("MempoolCapacityBytesOverride" `isInfixOf`) warnings)

testNoFalsePositives :: Assertion
testNoFalsePositives =
  deprecatedFlagWarnings ["--config", "c.json", "--topology", "t.json", "--database-path", "db"]
    @?= []

testFixtureComparison :: Assertion
testFixtureComparison = do
  -- cardano-config side: resolve from the file and adapt to a NodeConfiguration.
  resolved <- Cfg.resolveConfigurationFromFile configPath
  (cfgNc, _warns) <- either (assertFailure . (("cardano-config resolve failed: " <>) . show)) pure resolved
  adapted <- either (assertFailure . ("adapter failed: " <>)) pure
               (cardanoConfigToNodeConfiguration cfgNc)

  -- Node side: parse the same file with POM, mirroring the CLI-only fields
  -- (topology / database / protocol files / socket) from the adapter output.
  fileYaml <- parseNodeConfigurationFP (Just (ConfigYamlFilePath configPath))
  let withCli =
        (defaultPartialNodeConfiguration <> fileYaml)
          { pncConfigFile   = Last (Just (ConfigYamlFilePath configPath))
          , pncTopologyFile = Last (Just (ncTopologyFile adapted))
          , pncDatabaseFile = Last (Just (ncDatabaseFile adapted))
          , pncProtocolFiles = Last (Just (ncProtocolFiles adapted))
          , pncSocketConfig = Last (Just (ncSocketConfig adapted))
          }
  pomNc <- either (assertFailure . ("POM makeNodeConfiguration failed: " <>)) pure
             (makeNodeConfiguration withCli)

  let divergences = compareConfigurations pomNc adapted
      isAllowed d = any (`isPrefixOf` d) allowedResidualLabels
      unexpected = filter (not . isAllowed) divergences

  -- Print what the comparison reports, so the run is legible even when it passes.
  putStrLn $ "  compareConfigurations reported " <> show (length divergences)
    <> " divergence(s) on the fixture:"
  mapM_ (putStrLn . ("    - " <>)) divergences

  assertBool
    ("divergences outside the documented residual set: " <> show unexpected)
    (null unexpected)
