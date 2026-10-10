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
-- @config-envelope.json@ is @config.json@ put through @cardano-node config migrate@.
-- They sit in the same directory, so both resolve the same (relative) genesis
-- file paths and can be compared field by field.
module Main (main) where

import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad (filterM)
import           Data.List (isInfixOf, isPrefixOf)

import qualified Cardano.Configuration as Cfg
import           Cardano.Node.Configuration.CardanoConfigAdapter
                   (cardanoConfigToNodeConfiguration)
import           Cardano.Node.Configuration.CardanoConfigCompare
                   (compareConfigurations, deprecatedFlagWarnings)
import           Cardano.Node.Configuration.CardanoConfigResolve
                   (ConfigurationDialect (..), cardanoConfigCliLayer,
                   classifyConfigurationFile, nodeCliLayer)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   defaultPartialNodeConfiguration, makeNodeConfiguration,
                   parseNodeConfigurationFP)
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Types (ConfigYamlFilePath (..))

import           Data.Monoid (Last (..))

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
-- @cardano-node config migrate@ on 'legacyConfigPath').
envelopeConfigPath :: IO FilePath
envelopeConfigPath = (</> "config-envelope.json") <$> fixtureDir

-- | Labels ('compareConfigurations' prefixes each divergence with one) that are
-- documented, expected divergences on this fixture. They fall into three kinds,
-- and none is an adapter defect — the adapter faithfully reflects what
-- cardano-config resolved; the check exists precisely to surface these:
--
--   (1) adapter gaps — a field the adapter cannot populate from cardano-config
--       (see 'Cardano.Node.Configuration.CardanoConfigAdapter.adapterGaps'); no
--       gap reaches this fixture, since the one the Byron configuration still
--       has (the supported-protocol-version trio) is left out of the comparison;
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
  [ -- (2)/(3): cardano-config ships a QueryBatchSize where the node has none,
    -- and it states the snapshot interval as the explicit 86400 slots where the
    -- node leaves it at DefaultSnapshotInterval, which is the same 40*k slots
    -- named rather than spelled out. The rest of the snapshot policy agrees.
    "LedgerDbConfig"
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
      , testCase "an absent --port means the same ephemeral port to both parsers"
          testFlaglessPortAgrees
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
  pomNc <- resolveWithPom configPath

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

-- | An absent @--port@ must mean the same thing to both parsers.
--
-- Both spell the flag with @value 0@ ("use an ephemeral port"), so a flagless
-- command line resolves to port 0 on either side. The distinction matters: an
-- UNSET port is not the same as 0, because 'gatherConfiguredSockets' hands the
-- port straight to @getaddrinfo@, which fails when neither a host address nor a
-- service is given. Pinning this here keeps the agreement from being an accident
-- of the two parsers being given the same command line.
testFlaglessPortAgrees :: Assertion
testFlaglessPortAgrees = do
  configPath <- legacyConfigPath
  adapted <- resolveWithCardanoConfig configPath
  pomNc <- resolveWithPom configPath
  let port = getLast . ncNodePortNumber . ncSocketConfig
  port pomNc @?= Just 0
  port adapted @?= port pomNc

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
  (fileCfg, _warns) <- Cfg.parseConfigurationFiles fp
  -- The same empty command line the POM side is given (see 'resolveWithPom'),
  -- read by cardano-config's own parser. Both parsers must see the same two
  -- inputs, or the diff reports the inputs rather than the parsers.
  cli <- either (assertFailure . ("cardano-config CLI layer failed: " <>)) pure
           (cardanoConfigCliLayer fp [])
  (cfgNc, _checkWarns) <-
    either (assertFailure . ("cardano-config resolve failed: " <>) . show) pure
      (Cfg.resolveConfiguration cli fileCfg)
  either (assertFailure . ("adapter failed: " <>)) pure
    (cardanoConfigToNodeConfiguration cfgNc)

-- | Resolve a configuration file with the node's own POM parser.
--
-- The CLI-only fields (topology / database / protocol files / socket) are not in
-- the file; mirror them from the given cardano-config result so the comparison
-- isolates the file-parse and adapter-gap differences rather than CLI-supplied
-- noise.
resolveWithPom :: FilePath -> IO NodeConfiguration
resolveWithPom fp = do
  fileYaml <- parseNodeConfigurationFP (Just (ConfigYamlFilePath fp))
  -- The three layers a node assembles, in the node's own order: defaults, then
  -- the file, then the command line. The command line here is the one a node
  -- started with nothing but @--config@ has, which is not the same as no layer
  -- at all (see 'nodeCliLayer').
  let fileLayer = defaultPartialNodeConfiguration <> fileYaml
      (mCliLayer, cliReport) = nodeCliLayer fp []
  assertBool ("the node's CLI layer could not be built: " <> show cliReport) (null cliReport)
  either (assertFailure . ("POM makeNodeConfiguration failed: " <>)) pure
    (makeNodeConfiguration (maybe fileLayer (fileLayer <>) mCliLayer))
