{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Turning a node configuration file into the node's 'NodeConfiguration',
-- with whichever of the two parsers the file is written for.
--
-- The node understands two configuration dialects while the migration to the
-- shared @cardano-config@ package is in progress:
--
--   [Legacy] the flat, pre-@cardano-config@ configuration the node has always
--     read with its own POM parser. Both parsers can read it (@cardano-config@
--     migrates it on the fly, see 'Cardano.Configuration.File.Migrate.migrate'),
--     so it is resolved with /both/ and the results are compared: the POM result
--     is what the node runs on, and every divergence is reported as a non-fatal
--     warning so the two can be reconciled before POM is dropped.
--
--   [Envelope] the @cardano-config@ @{ $schema, Version, Configuration }@
--     envelope. The POM parser cannot read it at all (every setting lives nested
--     under @Configuration@, so POM sees an empty document and fails on the first
--     required field), so it is not run: @cardano-config@ alone resolves the
--     configuration and the adapter maps it to the node's 'NodeConfiguration'.
--
-- The dialect is decided by 'classifyConfigurationFile', on the file itself.
--
-- Note that the legacy path pays for the second parse at every startup:
-- @cardano-config@ reads and decodes the era genesis files as part of resolving,
-- so the genesis files are parsed twice. That is the price of the cross-check and
-- it goes away with the POM parser.
module Cardano.Node.Configuration.CardanoConfigResolve
  ( -- * Resolving
    ResolvedNodeConfiguration (..)
  , CrossCheck (..)
  , buildNodeConfiguration
  , NodeConfigurationError (..)

    -- * Dialects
  , ConfigurationDialect (..)
  , classifyConfigurationFile
  ) where

import           Cardano.Logging.Types (TraceConfig)
import qualified Cardano.Configuration as Cfg
import qualified Cardano.Configuration.CliArgs as CliArgs
import           Cardano.Node.Configuration.CardanoConfigAdapter
                   (cardanoConfigToNodeConfiguration)
import           Cardano.Node.Configuration.CardanoConfigCompare
                   (compareConfigurations, deprecatedFlagWarnings)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), defaultPartialNodeConfiguration,
                   makeNodeConfiguration, parseNodeConfigurationFP)
import           Cardano.Node.Types (ConfigYamlFilePath (..))

import           Control.Exception (Exception (..))
import qualified Control.Exception as Exception
import           Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.List (isPrefixOf)
import           Data.Monoid (Last (..))
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Opt
import           System.Environment (getArgs)

-- | The configuration dialect a node configuration file is written in.
data ConfigurationDialect
  = -- | A flat, pre-@cardano-config@ configuration: readable by both parsers.
    LegacyDialect
  | -- | A @cardano-config@ envelope: readable by @cardano-config@ only.
    CardanoConfigDialect
  deriving (Eq, Show)

-- | Whether to cross-check a legacy configuration against @cardano-config@'s
-- parser. Worth it at startup; not worth re-reading every genesis file for on a
-- configuration reload, which only wants the new values.
data CrossCheck
  = CrossCheckWithCardanoConfig
  | SkipCrossCheck
  deriving (Eq, Show)

-- | The node's resolved configuration together with everything the caller needs
-- to know about how it was obtained.
data ResolvedNodeConfiguration = ResolvedNodeConfiguration
  { rncConfiguration :: !NodeConfiguration
  , rncTraceConfig :: !(Maybe TraceConfig)
  -- ^ The tracing configuration, when it was resolved alongside the rest of the
  -- configuration (envelope dialect: the tracing settings live nested under
  -- @Configuration.HermodTracing@, where @trace-dispatcher@'s own file parser
  -- would not find them). 'Nothing' for the legacy dialect, where
  -- @trace-dispatcher@ reads the configuration file itself, as it always has.
  , rncReport :: ![String]
  -- ^ Non-fatal lines to trace at startup: the parser divergences found by the
  -- dual parse, and any warning raised while resolving.
  }

-- | The configuration file could not be turned into a 'NodeConfiguration'.
newtype NodeConfigurationError = NodeConfigurationError String

instance Show NodeConfigurationError where
  show (NodeConfigurationError err) = "Error in creating the NodeConfiguration: " <> err

instance Exception NodeConfigurationError

-- | Read the node configuration from the file named by the given
-- 'PartialNodeConfiguration', using the parser its dialect calls for (see the
-- module header).
buildNodeConfiguration ::
  -- | Whether to cross-check a legacy configuration against @cardano-config@.
  CrossCheck ->
  -- | The command-line configuration layer (also naming the configuration file).
  PartialNodeConfiguration ->
  IO ResolvedNodeConfiguration
buildNodeConfiguration crossCheck partialConf = do
  classifyConfigurationFile configFp >>= \case
    LegacyDialect -> buildFromLegacy crossCheck partialConf configFp
    CardanoConfigDialect -> buildFromCardanoConfig configFp
 where
  -- An absent @--config@ falls back to the node's default path, exactly as
  -- 'parseNodeConfigurationFP' does.
  -- 'Last' is right-biased, so the command-line layer goes on the RIGHT of the
  -- default to win.
  configFp =
    unConfigPath $
      case getLast (pncConfigFile defaultPartialNodeConfiguration <> pncConfigFile partialConf) of
        Just fp -> fp
        -- Unreachable: 'defaultPartialNodeConfiguration' always names a file.
        Nothing -> ConfigYamlFilePath "configuration/cardano/mainnet-config.json"

-- | Decide which dialect a configuration file is written in.
--
-- The marker is the envelope's @Configuration@ key: that is exactly what
-- @cardano-config@ splits the document on
-- ('Cardano.Configuration.File.Merge.splitEnvelope'), and exactly what makes the
-- document unreadable to POM (which expects the settings at the top level). A
-- file that cannot even be decoded as YAML is reported as legacy so that the
-- POM path raises the syntax error, as it always did.
classifyConfigurationFile :: FilePath -> IO ConfigurationDialect
classifyConfigurationFile fp =
  Yaml.decodeFileEither fp >>= \case
    Right (Object o) | KeyMap.member "Configuration" o -> pure CardanoConfigDialect
    _ -> pure LegacyDialect

-- | Resolve a legacy configuration with the node's own POM parser — the result
-- the node runs on — and cross-check it against @cardano-config@'s.
buildFromLegacy ::
  CrossCheck -> PartialNodeConfiguration -> FilePath -> IO ResolvedNodeConfiguration
buildFromLegacy crossCheck partialConf configFp = do
  configYamlPc <- parseNodeConfigurationFP (Just (ConfigYamlFilePath configFp))
  nc <-
    either (Exception.throwIO . NodeConfigurationError) pure $
      makeNodeConfiguration (defaultPartialNodeConfiguration <> configYamlPc <> partialConf)
  report <- case crossCheck of
    SkipCrossCheck -> pure []
    CrossCheckWithCardanoConfig -> crossCheckWithCardanoConfig configFp nc
  pure
    ResolvedNodeConfiguration
      { rncConfiguration = nc
      , rncTraceConfig = Nothing
      , rncReport = report
      }

-- | Resolve an envelope configuration with @cardano-config@ alone and map it to
-- the node's 'NodeConfiguration'. Unlike the cross-check on the legacy path,
-- every failure here is fatal: there is no second parser to fall back to.
buildFromCardanoConfig :: FilePath -> IO ResolvedNodeConfiguration
buildFromCardanoConfig configFp = do
  cliArgs <-
    cardanoConfigCliArgs configFp
      >>= either (Exception.throwIO . NodeConfigurationError) pure
  (fileCfg, fileWarnings) <- Cfg.parseConfigurationFiles configFp
  (cfgNc, checkWarnings) <-
    either (Exception.throwIO . NodeConfigurationError . show) pure $
      Cfg.resolveConfiguration cliArgs fileCfg
  nc <-
    either (Exception.throwIO . NodeConfigurationError) pure $
      cardanoConfigToNodeConfiguration cfgNc
  pure
    ResolvedNodeConfiguration
      { rncConfiguration = nc
      , rncTraceConfig = Just (Cfg.tracingConfiguration cfgNc)
      , rncReport =
          "cardano-config: resolved the configuration (the node's own parser was not run: this"
            <> " is a cardano-config envelope configuration, which it cannot read)"
            : map (("cardano-config: " <>) . Cfg.renderConfigWarning)
                  (fileWarnings <> checkWarnings)
      }

-- | Resolve the same legacy configuration with @cardano-config@ and diff it
-- against the POM result, returning the lines to report.
--
-- To keep the comparison fair, @cardano-config@ resolves from the SAME two
-- inputs the node used: it parses the node's own command line with its own CLI
-- parser and combines that with the configuration file, so both sides are
-- @file + CLI@. Every failure here is only ever reported, never fatal — the node
-- runs on the POM result either way.
crossCheckWithCardanoConfig :: FilePath -> NodeConfiguration -> IO [String]
crossCheckWithCardanoConfig configFp nc = do
  (cliArgs, cliReport) <-
    cardanoConfigCliArgs configFp >>= \case
      Right cli -> pure (cli, [])
      Left err ->
        -- A parse failure means the operator used a node flag cardano-config
        -- does not model (in practice, a deprecated one — 'deprecatedFlagWarnings'
        -- names it). Fall back to a file-only resolution so the rest is still
        -- checked, and say so.
        pure
          ( Cfg.defaultCliArgs configFp
          , [ "cardano-config: could not parse the node command line; comparing the"
                <> " configuration file only. " <> err
            ]
          )
  result <- Exception.try $ do
    (fileCfg, fileWarnings) <- Cfg.parseConfigurationFiles configFp
    resolved <- Exception.evaluate (Cfg.resolveConfiguration cliArgs fileCfg)
    pure (resolved, fileWarnings)
  pure . (cliReport <>) $ case result of
    Left (e :: Exception.SomeException) ->
      ["cardano-config: failed to parse the node configuration (ignored): " <> show e]
    Right (Left err, _) ->
      ["cardano-config: failed to resolve the node configuration (ignored): " <> show err]
    Right (Right (cfgNc, checkWarnings), fileWarnings) ->
      map (("cardano-config: " <>) . Cfg.renderConfigWarning) (fileWarnings <> checkWarnings)
        <> case cardanoConfigToNodeConfiguration cfgNc of
          Left adaptErr ->
            ["cardano-config: could not adapt to the node configuration (ignored): " <> adaptErr]
          Right adaptedNc ->
            case compareConfigurations nc adaptedNc of
              [] ->
                [ "cardano-config: the resolved configuration (file + CLI) agrees with the"
                    <> " node's own parser."
                ]
              divergences ->
                ( "cardano-config: WARNING - the resolved configuration (file + CLI) diverges"
                    <> " from the node's own parser:"
                )
                  : map ("  - " <>) divergences

-- | Re-parse the node's own command line with @cardano-config@'s CLI parser, so
-- both parsers resolve from the same @file + CLI@ inputs.
--
-- The node is invoked as @cardano-node run \<flags…\>@ (the RTS has already
-- stripped its own arguments), so the leading non-flag subcommand token is
-- dropped to get the flag list; @cardano-config@'s 'Cfg.parseCliArgs' is a flat
-- parser with no @run@ subcommand, matching that stripped list. On failure the
-- returned message leads with actionable guidance for each offending flag.
cardanoConfigCliArgs :: FilePath -> IO (Either String Cfg.CliArgs)
cardanoConfigCliArgs configFp = do
  argv <- getArgs
  let flags = dropWhile (not . ("-" `isPrefixOf`)) argv
  pure $ case Opt.execParserPure Opt.defaultPrefs (Opt.info Cfg.parseCliArgs mempty) flags of
    Opt.Success cli -> Right (onConfigFile cli)
    Opt.CompletionInvoked _ -> Right (Cfg.defaultCliArgs configFp)
    Opt.Failure failure ->
      let (msg, _exitCode) = Opt.renderFailure failure "cardano-node"
       in Left (unlines (deprecatedFlagWarnings flags <> [msg]))
 where
  -- The file being resolved is already settled (it is what was classified); pin
  -- it rather than take cardano-config's own @--config@ default, so a caller that
  -- named the file some other way than on the command line still resolves the
  -- genesis paths (which hang off the configuration file's directory) correctly.
  onConfigFile cli = cli{CliArgs.configFilePath = configFp}
