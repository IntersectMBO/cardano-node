{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the two configuration dialects the node understands
-- ('Cardano.Node.Configuration.CardanoConfigResolve'):
--
--   * a legacy (pre-@cardano-config@) configuration is read by both parsers, and
--     the divergences between them are exactly the documented residual set; and
--   * a @cardano-config@ envelope configuration is read by @cardano-config@ only
--     — the node's own POM parser genuinely cannot resolve it, which is why the
--     dual parse is skipped for it — and resolving it yields the same
--     configuration as the legacy form it was migrated from.
--
-- Plus 'deprecatedFlagWarnings', the operator-facing guidance emitted when
-- @cardano-config@'s CLI parser rejects the node's argv.
--
-- The two fixture configurations are the same configuration in the two dialects:
-- @config-envelope.json@ is @config.json@ put through @cardano-node migrate@.
-- They sit in the same directory, so both resolve the same (relative) genesis
-- file paths and can be compared field by field.
module Main (main) where

import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad (filterM)
import           Data.List (isInfixOf, isPrefixOf)
import           Data.Monoid (Last (..))

import qualified Cardano.Configuration as Cfg
import           Cardano.Node.Configuration.CardanoConfigAdapter
                   (cardanoConfigToNodeConfiguration)
import           Cardano.Node.Configuration.CardanoConfigCompare
                   (compareConfigurations, deprecatedFlagWarnings)
import           Cardano.Node.Configuration.CardanoConfigResolve
                   (ConfigurationDialect (..), classifyConfigurationFile)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), defaultPartialNodeConfiguration,
                   makeNodeConfiguration, parseNodeConfigurationFP)
import           Cardano.Node.Types (ConfigYamlFilePath (..))

import           System.Directory (doesFileExist)
import           System.FilePath ((</>))

import           Test.Tasty
import           Test.Tasty.HUnit

-- | Locate the fixture configuration directory. Depending on the runner the
-- working directory is either the package directory or the repository root, so
-- try both rather than assume one.
fixtureDir :: IO FilePath
fixtureDir = do
  found <- filterM (doesFileExist . (</> "config.json")) candidates
  case found of
    dir : _ -> pure dir
    [] ->
      assertFailure $
        "could not find the fixture configuration directory; looked in " <> show candidates
 where
  candidates =
    [ "test/cardano-config-compare/config"
    , "cardano-node/test/cardano-config-compare/config"
    ]

-- | The fixture's legacy (flat, pre-cardano-config) configuration file.
legacyConfigPath :: IO FilePath
legacyConfigPath = (</> "config.json") <$> fixtureDir

-- | The same configuration in the cardano-config envelope (the output of
-- @cardano-node migrate@ on 'legacyConfigPath').
envelopeConfigPath :: IO FilePath
envelopeConfigPath = (</> "config-envelope.json") <$> fixtureDir

-- | Labels ('compareConfigurations' prefixes each divergence with one) that are
-- documented, expected divergences on this fixture. They fall into three kinds,
-- and none is an adapter defect — the adapter faithfully reflects what
-- cardano-config resolved; the check exists precisely to surface these:
--
--   (1) adapter gaps — a field the adapter cannot populate from cardano-config
--       (see 'Cardano.Node.Configuration.CardanoConfigAdapter.adapterGaps');
--   (2) representation differences — same meaning, different shape;
--   (3) parser default mismatches — a field the fixture does not set (or sets
--       under a key one parser ignores), for which the two parsers fall back to
--       different defaults.
--
-- The fixture comparison must not diverge on anything OUTSIDE this set, so a
-- regression that changes a currently-agreeing field (or an adapter change that
-- breaks a mapped one) is still caught.
allowedResidualLabels :: [String]
allowedResidualLabels =
  [ -- (1) adapter gap: Byron supported-protocol-version is hard-coded 1/0/0 by
    -- the adapter, whereas the fixture sets LastKnownBlockVersion-Major = 3.
    "Byron protocol config"
    -- (1) adapter gap: the CheckpointsFile/CheckpointsFileHash keys have no
    -- cardano-config counterpart. The fixture sets neither (so this label does
    -- not appear on it), but a configuration that does — mainnet's, for one —
    -- diverges here.
  , "Checkpoints protocol config"
    -- (2) representation: "MempoolCapacityBytesOverride: NoOverride" is an
    -- explicit no-override value for POM but simply absent for cardano-config.
  , "MaybeMempoolCapacityOverride"
    -- (3) default mismatch: the fixture leaves these unset; POM defaults to
    -- Nothing (no limit) while cardano-config supplies its own default.
  , "MaxConcurrencyBulkSync"
  , "MaxConcurrencyDeadline"
    -- (3) default mismatch: cardano-config ships its own storage defaults where
    -- the node uses its bare LedgerDB defaults.
  , "LedgerDbConfig"
    -- (3) key/default mismatch: the fixture sets TargetNumberOf{Root,Known}Peers
    -- (= 100), which POM reads as the deadline targets, but cardano-config falls
    -- back to its own deadline defaults.
  , "DeadlineTargetOfRootPeers"
  , "DeadlineTargetOfKnownPeers"
    -- (3) default mismatch: the fixture leaves this unset; the two parsers
    -- default it differently.
  , "TxSubmissionInitDelay"
  ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "cardano-config configuration dialects"
  [ testGroup "deprecated CLI flag guidance"
      [ testCase "deprecated CLI aliases yield migration guidance" testDeprecatedAliases
      , testCase "removed mempool flags yield removal guidance" testRemovedMempoolFlags
      , testCase "no guidance for accepted / unrelated flags" testNoFalsePositives
      ]
  , testGroup "legacy dialect (both parsers)"
      [ testCase "the fixture is classified as legacy" testLegacyClassification
      , testCase "the two parsers diverge only on the documented residuals"
          testLegacyDualParse
      ]
  , testGroup "cardano-config envelope dialect (cardano-config only)"
      [ testCase "the migrated fixture is classified as an envelope"
          testEnvelopeClassification
      , testCase "the node's own parser cannot resolve an envelope" testEnvelopeDefeatsPom
      , testCase "cardano-config resolves an envelope to the same configuration"
          testEnvelopeResolvesToSameConfiguration
      ]
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

testLegacyClassification :: Assertion
testLegacyClassification =
  legacyConfigPath >>= classifyConfigurationFile >>= (@?= LegacyDialect)

-- | Resolve the fixture both ways and check that the divergences stay inside the
-- documented residual set.
testLegacyDualParse :: Assertion
testLegacyDualParse = do
  configPath <- legacyConfigPath
  adapted <- resolveWithCardanoConfig configPath
  pomNc <- resolveWithPom configPath adapted

  let divergences = compareConfigurations pomNc adapted
      isAllowed d = any (`isPrefixOf` d) allowedResidualLabels
      unexpected = filter (not . isAllowed) divergences

  -- Print what the comparison reports on this real config, so the run is legible
  -- even when it passes.
  putStrLn $ "  compareConfigurations reported " <> show (length divergences)
    <> " divergence(s) on the fixture:"
  mapM_ (putStrLn . ("    - " <>)) divergences

  assertBool
    ("divergences outside the documented residual set: " <> show unexpected)
    (null unexpected)

testEnvelopeClassification :: Assertion
testEnvelopeClassification =
  envelopeConfigPath >>= classifyConfigurationFile >>= (@?= CardanoConfigDialect)

-- | The reason the node does not run the dual parse on an envelope: POM cannot
-- read it. Every setting lives nested under @Configuration@, so POM sees a
-- document with none of the settings it requires. It may fail either while
-- decoding the file (its decoder throws on a missing required key) or in
-- 'makeNodeConfiguration'; only that it fails matters here.
testEnvelopeDefeatsPom :: Assertion
testEnvelopeDefeatsPom = do
  envelope <- envelopeConfigPath
  outcome <- try $ do
    filePartial <- parseNodeConfigurationFP (Just (ConfigYamlFilePath envelope))
    evaluate (makeNodeConfiguration (defaultPartialNodeConfiguration <> filePartial))
  case outcome of
    Left (_ :: SomeException) -> pure ()
    Right (Left _) -> pure ()
    Right (Right _) ->
      assertFailure
        "the node's own parser resolved an envelope configuration; the dual-parse\
        \ dispatch in CardanoConfigResolve assumes it cannot"

-- | The envelope is a reshaping, not a change of meaning: resolving it must give
-- exactly what resolving the legacy form it was migrated from gives.
testEnvelopeResolvesToSameConfiguration :: Assertion
testEnvelopeResolvesToSameConfiguration = do
  fromLegacy <- resolveWithCardanoConfig =<< legacyConfigPath
  fromEnvelope <- resolveWithCardanoConfig =<< envelopeConfigPath
  let divergences = compareConfigurations fromLegacy fromEnvelope
  assertBool
    ("the envelope resolved differently from the legacy form it was migrated from: "
       <> show divergences)
    (null divergences)

-- | Resolve a configuration file with cardano-config (file only, no CLI layer)
-- and adapt it to the node's own 'NodeConfiguration'.
resolveWithCardanoConfig :: FilePath -> IO NodeConfiguration
resolveWithCardanoConfig fp = do
  resolved <- Cfg.resolveConfigurationFromFile fp
  (cfgNc, _warns) <-
    either (assertFailure . ("cardano-config resolve failed: " <>) . show) pure resolved
  either (assertFailure . ("adapter failed: " <>)) pure
    (cardanoConfigToNodeConfiguration cfgNc)

-- | Resolve a configuration file with the node's own POM parser.
--
-- The CLI-only fields (topology / database / protocol files / socket) are not in
-- the file; mirror them from the given cardano-config result so the comparison
-- isolates the file-parse and adapter-gap differences rather than CLI-supplied
-- noise.
resolveWithPom :: FilePath -> NodeConfiguration -> IO NodeConfiguration
resolveWithPom fp adapted = do
  fileYaml <- parseNodeConfigurationFP (Just (ConfigYamlFilePath fp))
  -- 'PartialNodeConfiguration' is a Semigroup but not a Monoid, so there is no
  -- empty value to start from: merge defaults with the file layer (mirroring
  -- 'buildNodeConfiguration'), then override the CLI-only fields on top.
  let withCli =
        (defaultPartialNodeConfiguration <> fileYaml)
          { pncConfigFile = Last (Just (ConfigYamlFilePath fp))
          , pncTopologyFile = Last (Just (ncTopologyFile adapted))
          , pncDatabaseFile = Last (Just (ncDatabaseFile adapted))
          , pncProtocolFiles = Last (Just (ncProtocolFiles adapted))
          , pncSocketConfig = Last (Just (ncSocketConfig adapted))
          }
  either (assertFailure . ("POM makeNodeConfiguration failed: " <>)) pure
    (makeNodeConfiguration withCli)
