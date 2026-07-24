{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Cardano.Configuration as Cfg
import qualified Cardano.Configuration.CliArgs as CliArgs
import qualified Cardano.Configuration.Commands as Cmds
import qualified Cardano.Crypto.Init as Crypto
import           Cardano.Git.Rev (gitRev)
import           Cardano.Node.Configuration.CardanoConfigAdapter
                   (cardanoConfigToNodeConfiguration)
import           Cardano.Node.Configuration.CardanoConfigCompare
                   (compareConfigurations)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), defaultPartialNodeConfiguration,
                   makeNodeConfiguration, parseNodeConfigurationFP)
import           Cardano.Node.Handlers.TopLevel
import           Cardano.Node.Parsers (nodeCLIParser)
import           Cardano.Node.Run (runNode)
import           Cardano.Node.Tracing.Documentation (TraceDocumentationCmd (..),
                   parseTraceDocumentationCmd, runTraceDocumentationCmd)
import           Cardano.Node.Types (ConfigYamlFilePath (..))

import           Data.Monoid (Last (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import           Options.Applicative
import qualified Options.Applicative as Opt
import           System.Exit (exitFailure)
import           System.Info (arch, compilerName, compilerVersion, os)
import           System.IO (hPutStrLn, stderr)

import           Paths_cardano_node (version)

main :: IO ()
main = do
  Crypto.cryptoInit

  toplevelExceptionHandler $ do
    cmd <- Opt.customExecParser p opts

    case cmd of
      RunCmd args -> do
        warnIfSet args pncMaybeMempoolCapacityOverride "mempool-capacity-override" "MempoolCapacityBytesOverride"
        runNode args
      TraceDocumentation tdc -> runTraceDocumentationCmd tdc
      VersionCmd  -> runVersionCommand
      ConfigCmd act -> act

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      warnIfSet :: PartialNodeConfiguration -> (PartialNodeConfiguration -> Last a) -> String -> String -> IO ()
      warnIfSet args f name key =
          maybe
            (pure ())
            (\_ -> hPutStrLn stderr $ "WARNING: Option --" ++ name ++ " was set via CLI flags.\
            \ This CLI flag will be removed in upcoming node releases.\
            \ Please, set this configuration option in the configuration file instead with key " ++ key ++ ".")
        $ getLast
        $ f args

      opts :: Opt.ParserInfo Command
      opts =
        Opt.info (fmap RunCmd nodeCLIParser
                  <|> fmap TraceDocumentation parseTraceDocumentationCmd
                  <|> parseVersionCmd
                  <|> fmap ConfigCmd configSubcommands
                  <**> helper)

          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )


data Command = RunCmd PartialNodeConfiguration
             | TraceDocumentation TraceDocumentationCmd
             | VersionCmd
             | ConfigCmd (IO ())

-- Yes! A --version flag or version command. Either guess is right!
parseVersionCmd :: Parser Command
parseVersionCmd =
      Opt.subparser
        (mconcat
         [ Opt.commandGroup "Miscellaneous commands"
         , Opt.metavar "version"
         , Opt.hidden
         , command'
           "version"
           "Show the cardano-node version"
           (pure VersionCmd)
         ]
        )
  <|> Opt.flag' VersionCmd
        (  Opt.long "version"
        <> Opt.help "Show the cardano-node version"
        <> Opt.hidden
        )

runVersionCommand :: IO ()
runVersionCommand =
    Text.putStrLn $ mconcat
      [ "cardano-node ", renderVersion version
      , " - ", Text.pack os, "-", Text.pack arch
      , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
      , "\ngit rev ", $(gitRev)
      ]
  where
    renderVersion = Text.pack . showVersion


command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [ progDesc descr ])
    , metavar c
    ]

-- cardano-config subcommands --------------------------------------------------

-- | The @migrate@, @schema@ and @resolve@ subcommands, spliced from the shared
-- @cardano-config:commands@ sublibrary. @migrate@ and @schema@ are
-- cardano-config's own commands, unchanged; @resolve@ is a node-specific variant
-- (see 'resolveDualCommand') that additionally cross-checks the node's own parser
-- against cardano-config's.
configSubcommands :: Parser (IO ())
configSubcommands =
  Opt.hsubparser
    ( Opt.commandGroup "Configuration commands:"
        <> Cmds.migrateCommand
        <> Cmds.schemaCommand
        <> resolveDualCommand
    )

-- | A node-specific @resolve@: resolve the configuration with cardano-config
-- (printing the result as YAML, exactly like cardano-config's own @resolve@),
-- then re-resolve the same configuration with the node's own POM parser and
-- report any discrepancies between the two. Exits non-zero when they disagree,
-- so it doubles as a CI parity check while the node still has two parsers.
resolveDualCommand :: Mod CommandFields (IO ())
resolveDualCommand =
  command "resolve"
    ( info
        (runDualResolve <$> Cmds.resolveOptionsParser)
        ( progDesc
            ( "Resolve a cardano-node configuration (defaults + file + CLI) with both the "
                <> "node and cardano-config parsers, print the result as YAML, and report any "
                <> "discrepancies between the two parsers (exit non-zero if they disagree)."
            )
        )
    )

runDualResolve :: Cmds.ResolveOptions -> IO ()
runDualResolve resolveOpts@(Cmds.ResolveOptions cli _geneses) = do
  -- Print the resolved configuration using cardano-config's own renderer (which
  -- honours --with-geneses); this also terminates via 'die' if resolution fails.
  Cmds.runResolveCommand resolveOpts
  -- Cross-check: resolve the same inputs with the node's POM parser and diff.
  discrepancies <- resolveDiscrepancies cli
  case discrepancies of
    [] ->
      putStrLn "resolve: the node and cardano-config parsers agree on the resolved configuration."
    ds -> do
      hPutStrLn stderr $
        "resolve: " <> show (length ds)
          <> " discrepancy(ies) between the node and cardano-config parsers:"
      mapM_ (hPutStrLn stderr . ("  - " <>)) ds
      exitFailure

-- | Resolve the configuration file (+ CLI) both ways and return the divergences.
-- The node (POM) side takes its CLI-supplied, file-absent fields (topology /
-- database / protocol files / socket) from the shared cardano-config resolution,
-- so the diff reflects how the two parsers read the configuration FILE (plus the
-- documented adapter gaps) rather than an independent — and necessarily
-- asymmetric — CLI reverse-mapping.
resolveDiscrepancies :: Cfg.CliArgs -> IO [String]
resolveDiscrepancies cli = do
  (fileCfg, _warns) <- Cfg.parseConfigurationFiles configFp
  case Cfg.resolveConfiguration cli fileCfg of
    Left err -> pure ["cardano-config failed to resolve the configuration: " <> show err]
    Right (cfgNc, _) ->
      case cardanoConfigToNodeConfiguration cfgNc of
        Left adaptErr -> pure ["cardano-config configuration could not be adapted: " <> adaptErr]
        Right adaptedNc -> do
          filePartial <- parseNodeConfigurationFP (Just (ConfigYamlFilePath configFp))
          let withCli =
                (defaultPartialNodeConfiguration <> filePartial)
                  { pncConfigFile    = Last (Just (ConfigYamlFilePath configFp))
                  , pncTopologyFile  = Last (Just (ncTopologyFile adaptedNc))
                  , pncDatabaseFile  = Last (Just (ncDatabaseFile adaptedNc))
                  , pncProtocolFiles = Last (Just (ncProtocolFiles adaptedNc))
                  , pncSocketConfig  = Last (Just (ncSocketConfig adaptedNc))
                  }
          case makeNodeConfiguration withCli of
            Left err -> pure ["node parser (makeNodeConfiguration) failed: " <> err]
            Right pomNc -> pure (compareConfigurations pomNc adaptedNc)
 where
  configFp = CliArgs.configFilePath cli
