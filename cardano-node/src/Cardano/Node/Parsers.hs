{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | The node's @run@ command line.
--
-- The individual option parsers come from @cardano-config@
-- ('Cardano.Configuration.CliArgs'), which is the one definition of the node's
-- flags, their metavars and their help text; this module assembles them into a
-- 'PartialNodeConfiguration' and converts cardano-config's types to the node's
-- (see 'Cardano.Node.Configuration.CardanoConfigAdapter').
--
-- What is still defined here is what cardano-config does not carry:
--
--   * the deprecated mempool capacity flags, which cardano-config dropped by
--     design (the setting belongs in the configuration file);
--   * the three deprecated aliases, @--delegation-certificate@,
--     @--signing-key@ and @--non-producing-node@, which cardano-config knows
--     only by their current spellings.
module Cardano.Node.Parsers
  ( nodeCLIParser
  , parseConfigFile
  , parserHelpHeader
  , parserHelpOptions
  , renderHelpDoc
  ) where

import qualified Cardano.Configuration.CliArgs as Cfg
import           Cardano.Configuration.CliArgs (parseConfigFile)
import           Cardano.Node.Configuration.CardanoConfigAdapter
                   (credentialsToProtocolFilepaths, fromCfgDbPaths, fromCfgGrpcEndpoint,
                   fromCfgTracerConnection, toNodeShutdownOn)
import           Cardano.Node.Configuration.NodeAddress (File (..),
                   NodeHostIPv4Address (NodeHostIPv4Address),
                   NodeHostIPv6Address (NodeHostIPv6Address))
import           Cardano.Node.Configuration.POM (PartialNodeConfiguration (..), lastOption)
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Handlers.Shutdown (ShutdownConfig (..), ShutdownOn (..))
import           Cardano.Node.Types (ConfigYamlFilePath (..), ProtocolFilepaths (..),
                   TopologyFile (..))
import           Cardano.Rpc.Server.Config (PartialRpcConfig, RpcConfigF (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytesOverride (..))

import           Data.Maybe (fromMaybe)
import           Data.Monoid (Last (..))
import           Data.Word (Word32)
import           Options.Applicative hiding (str, switch)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as OptI
import qualified Prettyprinter.Internal as PP

nodeCLIParser  :: Parser PartialNodeConfiguration
nodeCLIParser = subparser
                (  commandGroup "Run the node"
                <> metavar "run"
                <> command "run"
                     (info (nodeRunParser <**> helper)
                           (progDesc "Run the node." ))
                )

nodeRunParser :: Parser PartialNodeConfiguration
nodeRunParser = do
  -- Filepaths
  topFp <- lastOption Cfg.parseTopologyFile
  dbFp <- Last . fmap fromCfgDbPaths <$> Cfg.parseNodeDatabasePaths
  validate <- lastOption Cfg.parseValidateDB
  socketFp <- lastOption (File <$> Cfg.parseSocketPath)
  traceForwardSocket <- lastOption (fromCfgTracerConnection <$> Cfg.parseTracerSocketMode)
  nodeConfigFp <- lastOption Cfg.parseConfigFile

  -- Protocol files
  credentials <- Cfg.parseCredentials
  deprecatedByronCertFile <- optional parseByronDelegationCertDeprecated
  deprecatedByronKeyFile <- optional parseByronSigningKeyDeprecated
  startAsNonProducingNode <- (\depr new -> Last depr <> Last new)
                         <$> parseStartAsNonProducingNodeDeprecated
                         <*> Cfg.parseStartAsNonProducingNode

  -- Node Address
  nIPv4Address <- lastOption (NodeHostIPv4Address <$> Cfg.parseHostIPv4Addr)
  nIPv6Address <- lastOption (NodeHostIPv6Address <$> Cfg.parseHostIPv6Addr)
  nPortNumber  <- lastOption Cfg.parsePort

  -- Shutdown
  shutdownIPC <- lastOption Cfg.parseShutdownIPC
  shutdownOnLimit <- lastOption (toNodeShutdownOn <$> Cfg.parseShutdownOn)

  -- Hidden options (to be removed eventually)
  maybeMempoolCapacityOverride <- lastOption parseMempoolCapacityOverride

  -- gRPC
  rpcIsEnabled <- lastOption Cfg.parseEnableGrpc
  rpcEndpoint <- lastOption (fromCfgGrpcEndpoint <$> Cfg.parseGrpcEndpoint)

  pure $ PartialNodeConfiguration
           { pncSocketConfig =
               Last . Just $ SocketConfig
                 nIPv4Address
                 nIPv6Address
                 nPortNumber
                 socketFp
           , pncConfigFile   = ConfigYamlFilePath <$> nodeConfigFp
           , pncTopologyFile = TopologyFile <$> topFp
           , pncDatabaseFile = dbFp
           , pncDiffusionMode = mempty
           , pncExperimentalProtocolsEnabled = mempty
           , pncProtocolFiles =
               Last . Just $
                 withDeprecatedByronCredentials
                   deprecatedByronCertFile
                   deprecatedByronKeyFile
                   (credentialsToProtocolFilepaths credentials)
           , pncValidateDB = validate
           , -- An absent @--shutdown-on-*@ means 'NoShutdown' rather than
             -- "unset", which is what the node has always resolved it to.
             -- cardano-config's parser has no such fallback, so it is applied
             -- here.
             pncShutdownConfig =
               Last . Just $
                 ShutdownConfig
                   (getLast shutdownIPC)
                   (Just (fromMaybe NoShutdown (getLast shutdownOnLimit)))
           , pncStartAsNonProducingNode = startAsNonProducingNode
           , pncProtocolConfig = mempty
           , pncMaxConcurrencyBulkSync = mempty
           , pncMaxConcurrencyDeadline = mempty
           , pncTraceForwardSocket = traceForwardSocket
           , pncMaybeMempoolCapacityOverride = maybeMempoolCapacityOverride
           , pncLedgerDbConfig = mempty
           , pncProtocolIdleTimeout = mempty
           , pncTimeWaitTimeout = mempty
           , pncEgressPollInterval = mempty
           , pncChainSyncIdleTimeout = mempty
           , pncMempoolTimeoutSoft = mempty
           , pncMempoolTimeoutHard = mempty
           , pncMempoolTimeoutCapacity = mempty
           , pncAcceptedConnectionsLimit = mempty
           , pncDeadlineTargetOfRootPeers = mempty
           , pncDeadlineTargetOfKnownPeers = mempty
           , pncDeadlineTargetOfEstablishedPeers = mempty
           , pncDeadlineTargetOfActivePeers = mempty
           , pncDeadlineTargetOfKnownBigLedgerPeers = mempty
           , pncDeadlineTargetOfEstablishedBigLedgerPeers = mempty
           , pncDeadlineTargetOfActiveBigLedgerPeers = mempty
           , pncSyncTargetOfRootPeers = mempty
           , pncSyncTargetOfKnownPeers = mempty
           , pncSyncTargetOfEstablishedPeers = mempty
           , pncSyncTargetOfActivePeers = mempty
           , pncSyncTargetOfKnownBigLedgerPeers = mempty
           , pncSyncTargetOfEstablishedBigLedgerPeers = mempty
           , pncSyncTargetOfActiveBigLedgerPeers = mempty
           , pncMinBigLedgerPeersForTrustedState = mempty
           , pncConsensusMode = mempty
           , pncPeerSharing = mempty
           , pncGenesisConfigFlags = mempty
           , pncResponderCoreAffinityPolicy = mempty
           , pncRpcConfig =
               (mempty :: PartialRpcConfig)
                 { isEnabled = rpcIsEnabled
                 , rpcEndpoint
                 }
           , pncTxSubmissionLogicVersion = mempty
           , pncTxSubmissionInitDelay = mempty
           }

-- | Fill the Byron credentials in from the deprecated aliases where
-- cardano-config's parser, which knows only the current spellings, left a hole.
withDeprecatedByronCredentials ::
  Maybe FilePath -> Maybe FilePath -> ProtocolFilepaths -> ProtocolFilepaths
withDeprecatedByronCredentials cert key files =
  files
    { byronCertFile = byronCertFile files <|> cert
    , byronKeyFile = byronKeyFile files <|> key
    }

-- | Deprecated alias of @--byron-delegation-certificate@.
parseByronDelegationCertDeprecated :: Parser FilePath
parseByronDelegationCertDeprecated =
  strOption (long "delegation-certificate" <> Opt.internal)

-- | Deprecated alias of @--byron-signing-key@.
parseByronSigningKeyDeprecated :: Parser FilePath
parseByronSigningKeyDeprecated =
  strOption (long "signing-key" <> Opt.internal)

-- | Deprecated alias of @--start-as-non-producing-node@.
parseStartAsNonProducingNodeDeprecated :: Parser (Maybe Bool)
parseStartAsNonProducingNodeDeprecated =
  flag Nothing (Just True) $ mconcat
    [ long "non-producing-node"
    , help $ mconcat
        [ "DEPRECATED, use --start-as-non-producing-node instead. "
        , "This option will be removed in one of the future versions of cardano-node."
        ]
    , hidden
    ]

-- | Deprecated, and not carried by cardano-config: the mempool capacity is a
-- configuration file setting (@MempoolCapacityBytesOverride@).
parseMempoolCapacityOverride :: Parser MempoolCapacityBytesOverride
parseMempoolCapacityOverride = parseOverride <|> parseNoOverride
  where
    parseOverride :: Parser MempoolCapacityBytesOverride
    parseOverride =
      MempoolCapacityBytesOverride . ByteSize32 <$>
        Opt.option (auto @Word32)
          (  long "mempool-capacity-override"
          <> metavar "BYTES"
          <> help "[DEPRECATED: Set it in config file with key MempoolCapacityBytesOverride] The number of bytes"
          )
    parseNoOverride :: Parser MempoolCapacityBytesOverride
    parseNoOverride =
      flag' NoMempoolCapacityBytesOverride
        (  long "no-mempool-capacity-override"
        <> help "[DEPRECATED: Set it in config file] Don't override mempool capacity"
        )

-- | Produce just the brief help header for a given CLI option parser,
--   without the options.
parserHelpHeader :: String -> Opt.Parser a -> OptI.Doc
parserHelpHeader = flip (OptI.parserUsage (Opt.prefs mempty))

-- | Produce just the options help for a given CLI option parser,
--   without the header.
parserHelpOptions :: Opt.Parser a -> OptI.Doc
parserHelpOptions = fromMaybe mempty . OptI.unChunk . OptI.fullDesc (Opt.prefs mempty)

-- | Render the help pretty document.
renderHelpDoc :: Int -> OptI.Doc -> String
renderHelpDoc cols =
  (`PP.renderShowS` "") . OptI.layoutPretty (OptI.LayoutOptions (OptI.AvailablePerLine cols 1.0))
