{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Parsers
  ( nodeCLIParser
  , parseConfigFile
  , parserHelpHeader
  , parserHelpOptions
  , renderHelpDoc
  , parseHostPort
  ) where

import           Cardano.Logging.Types
import qualified Cardano.Logging.Types as Net
import           Cardano.Node.Configuration.NodeAddress (
                   NodeHostIPv4Address (NodeHostIPv4Address), File (..),
                   NodeHostIPv6Address (NodeHostIPv6Address), PortNumber, SocketPath)
import           Cardano.Node.Configuration.POM (PartialNodeConfiguration (..), lastOption)
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Types
import           Cardano.Prelude (ConvertText (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node

import           Data.Foldable
import           Data.Char (isDigit)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Last (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word16, Word32)
import           Options.Applicative hiding (str, switch)
-- Don't use switch.  It will not allow to set an option in a configuration
-- file.  See `parseStartAsNonProducingNode` and `parseValidateDB`.
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as OptI
import qualified Prettyprinter.Internal as PP
import           System.Posix.Types (Fd (..))
import           Text.Read (readMaybe)

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
  topFp <- lastOption parseTopologyFile
  dbFp <- lastOption parseNodeDatabasePaths
  validate <- lastOption parseValidateDB
  socketFp <- lastOption $ parseSocketPath "Path to a cardano-node socket"
  traceForwardSocket <- lastOption parseTracerSocketMode
  nodeConfigFp <- lastOption parseConfigFile

  -- Protocol files
  byronCertFile   <- optional parseByronDelegationCert
  byronKeyFile    <- optional parseByronSigningKey
  shelleyKESFile  <- optional parseKesKeyFilePath
  shelleyVRFFile  <- optional parseVrfKeyFilePath
  shelleyCertFile <- optional parseOperationalCertFilePath
  shelleyBulkCredsFile <- optional parseBulkCredsFilePath
  startAsNonProducingNode <- (\depr new -> Last depr <> Last new)
                         <$> parseStartAsNonProducingNodeDeprecated
                         <*> parseStartAsNonProducingNode

  -- Node Address
  nIPv4Address <- lastOption parseHostIPv4Addr
  nIPv6Address <- lastOption parseHostIPv6Addr
  nPortNumber  <- lastOption parsePort

  -- Shutdown
  shutdownIPC <- lastOption parseShutdownIPC
  shutdownOnLimit <- lastOption parseShutdownOn

  -- Hidden options (to be removed eventually)
  maybeMempoolCapacityOverride <- lastOption parseMempoolCapacityOverride

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
           , pncProtocolFiles = Last $ Just ProtocolFilepaths
             { byronCertFile
             , byronKeyFile
             , shelleyKESFile
             , shelleyVRFFile
             , shelleyCertFile
             , shelleyBulkCredsFile
             }
           , pncValidateDB = validate
           , pncShutdownConfig =
               Last . Just $ ShutdownConfig (getLast shutdownIPC) (getLast shutdownOnLimit)
           , pncStartAsNonProducingNode = startAsNonProducingNode
           , pncProtocolConfig = mempty
           , pncMaxConcurrencyBulkSync = mempty
           , pncMaxConcurrencyDeadline = mempty
           , pncLoggingSwitch = mempty
           , pncLogMetrics = mempty
           , pncTraceConfig = mempty
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
           }

parseSocketPath :: Text -> Parser SocketPath
parseSocketPath helpMessage =
  fmap File $ strOption $ mconcat
    [ long "socket-path"
    , help (toS helpMessage)
    , completer (bashCompleter "file")
    , metavar "FILEPATH"
    ]


-- leave hostname untouched, non-empty
-- 0 <= port <= 65535
parseNodeAddress :: Opt.ReadM Net.HowToConnect
parseNodeAddress = Opt.eitherReader parseHostPort

parseHostPort :: String -> Either String Net.HowToConnect
parseHostPort str
  | (portRev, ':' : hostRev) <- break (== ':') (reverse str)
  = if
    | null hostRev        -> Left "parseHostPort: Empty host."
    | null portRev        -> Left "parseHostPort: Empty port."
    | all isDigit portRev
    , Just port <- readMaybe @Word16 (reverse portRev) -> if
      | 0 <= port, port <= 65535 -> Right (Net.RemoteSocket (Text.pack (reverse hostRev)) port)
      | otherwise -> Left ("parseHostPort: Numeric port '" ++ show port ++ "' out of range: 0 - 65535)")
    | otherwise -> Left "parseHostPort: Non-numeric port."
  | otherwise
  = Left "parseHostPort: No colon found."

parseTracerSocketMode :: Parser (Net.HowToConnect, ForwarderMode)
parseTracerSocketMode =
  asum
    [ fmap (, Responder) $ option parseNodeAddress $ mconcat
      [ long "tracer-socket-network-accept"
      , help "Accept incoming cardano-tracer connection on HOST:PORT"
      , metavar "HOST:PORT"
      ]
    , fmap (, Initiator) $ option parseNodeAddress $ mconcat
      [ long "tracer-socket-network-connect"
      , help "Connect to cardano-tracer listening on HOST:PORT"
      , metavar "HOST:PORT"
      ]
    , fmap (\host -> (Net.LocalPipe host, Responder)) $ strOption $ mconcat
      [ long "tracer-socket-path-accept"
      , help "Accept incoming cardano-tracer connection at local socket"
      , completer (bashCompleter "file")
      , metavar "FILEPATH"
      ]
    , fmap (\host -> (Net.LocalPipe host, Initiator)) $ strOption $ mconcat
      [ long "tracer-socket-path-connect"
      , help "Connect to cardano-tracer listening on a local socket"
      , completer (bashCompleter "file")
      , metavar "FILEPATH"
      ]
    ]

parseHostIPv4Addr :: Parser NodeHostIPv4Address
parseHostIPv4Addr =
    Opt.option (eitherReader parseNodeHostIPv4Address) (
          long "host-addr"
       <> metavar "IPV4"
       <> help "An optional IPv4 address"
    )

parseHostIPv6Addr :: Parser NodeHostIPv6Address
parseHostIPv6Addr =
    Opt.option (eitherReader parseNodeHostIPv6Address) (
          long "host-ipv6-addr"
       <> metavar "IPV6"
       <> help "An optional IPv6 address"
    )

parseNodeHostIPv4Address :: String -> Either String NodeHostIPv4Address
parseNodeHostIPv4Address str =
  maybe
    (Left $
      "Failed to parse IPv4 address: " ++ str ++
      ". If you want to specify an IPv6 address, use --host-ipv6-addr option.")
    (Right . NodeHostIPv4Address)
    (readMaybe str)

parseNodeHostIPv6Address :: String -> Either String NodeHostIPv6Address
parseNodeHostIPv6Address str =
  maybe
    (Left $
      "Failed to parse IPv6 address: " ++ str ++
      ". If you want to specify an IPv4 address, use --host-addr option.")
    (Right . NodeHostIPv6Address)
    (readMaybe str)

parsePort :: Parser PortNumber
parsePort =
    Opt.option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
       <> value 0 -- Use an ephemeral port
    )

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
    <> metavar "NODE-CONFIGURATION"
    <> help "Configuration file for the cardano-node"
    <> completer (bashCompleter "file")
    )

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

parseNodeDatabasePaths :: Parser NodeDatabasePaths
parseNodeDatabasePaths = parseDbPath <|> parseMultipleDbPaths

parseDbPath :: Parser NodeDatabasePaths
parseDbPath =
    fmap OnePathForAllDbs $
        strOption $
            mconcat
                [ long "database-path"
                , metavar "FILEPATH"
                , help "Directory where the state is stored."
                , completer (bashCompleter "file")
                ]

parseMultipleDbPaths :: Parser NodeDatabasePaths
parseMultipleDbPaths = MultipleDbPaths <$> parseImmutableDbPath <*> parseVolatileDbPath

parseVolatileDbPath :: Parser FilePath
parseVolatileDbPath = strOption $
  mconcat
    [ long "volatile-database-path"
    , metavar "FILEPATH"
    , help "Directory where the state is stored."
    , completer (bashCompleter "file")
    ]

parseImmutableDbPath :: Parser FilePath
parseImmutableDbPath = strOption $
  mconcat
    [ long "immutable-database-path"
    , metavar "FILEPATH"
    , help "Directory where the state is stored."
    , completer (bashCompleter "file")
    ]


-- | This parser will always override configuration option, even if the
-- `--validate-db` is not present.  This is fine for `--validate-db` switch,
-- but might not be for something else.  See `parseStartAsNonProducingNode` for
-- an alternative solution.
parseValidateDB :: Parser Bool
parseValidateDB =
    Opt.switch (
         long "validate-db"
      <> help "Validate all on-disk database files"
    )

parseShutdownIPC :: Parser Fd
parseShutdownIPC =
  Opt.option (Fd <$> auto) (
           long "shutdown-ipc"
        <> metavar "FD"
        <> help "Shut down the process when this inherited FD reaches EOF"
        <> hidden
      )

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
         <> completer (bashCompleter "file")
    )

parseByronDelegationCert :: Parser FilePath
parseByronDelegationCert =
  strOption ( long "byron-delegation-certificate"
    <> metavar "FILEPATH"
    <> help "Path to the delegation certificate."
    <> completer (bashCompleter "file")
    )
  <|>
  strOption
    ( long "delegation-certificate"
    <> Opt.internal
    )

parseByronSigningKey :: Parser FilePath
parseByronSigningKey =
  strOption ( long "byron-signing-key"
            <> metavar "FILEPATH"
            <> help "Path to the Byron signing key."
            <> completer (bashCompleter "file")
            )
  <|>
  strOption ( long "signing-key"
            <> Opt.internal
            )

parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
  strOption
    ( long "shelley-operational-certificate"
        <> metavar "FILEPATH"
        <> help "Path to the delegation certificate."
        <> completer (bashCompleter "file")
    )

parseBulkCredsFilePath :: Parser FilePath
parseBulkCredsFilePath =
  strOption
    ( long "bulk-credentials-file"
        <> metavar "FILEPATH"
        <> help "Path to the bulk pool credentials file."
        <> completer (bashCompleter "file")
    )

--TODO: pass the current KES evolution, not the KES_0
parseKesKeyFilePath :: Parser FilePath
parseKesKeyFilePath =
  strOption
    ( long "shelley-kes-key"
        <> metavar "FILEPATH"
        <> help "Path to the KES signing key."
        <> completer (bashCompleter "file")
    )

parseVrfKeyFilePath :: Parser FilePath
parseVrfKeyFilePath =
  strOption
    ( long "shelley-vrf-key"
        <> metavar "FILEPATH"
        <> help "Path to the VRF signing key."
        <> completer (bashCompleter "file")
    )

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

-- | A parser which returns `Nothing` or `Just True`; the default value is set
-- in `defaultPartialNodeConfiguration`.  This allows to set this option either
-- in the configuration file or as command line flag.
parseStartAsNonProducingNode :: Parser (Maybe Bool)
parseStartAsNonProducingNode =
  flag Nothing (Just True) $ mconcat
    [ long "start-as-non-producing-node"
    , help $ mconcat
        [ "Start the node as a non block producing node even if "
        , "credentials are specified."
        ]
    ]


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
