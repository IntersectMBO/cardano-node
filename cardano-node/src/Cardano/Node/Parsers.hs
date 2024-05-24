{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Parsers
  ( nodeCLIParser
  , parseConfigFile
  , parserHelpHeader
  , parserHelpOptions
  , renderHelpDoc
  ) where

import           Cardano.Logging.Types
import           Cardano.Node.Configuration.LedgerDB
import           Cardano.Node.Configuration.NodeAddress (File (..),
                   NodeHostIPv4Address (NodeHostIPv4Address),
                   NodeHostIPv6Address (NodeHostIPv6Address), PortNumber, SocketPath)
import           Cardano.Node.Configuration.POM (PartialNodeConfiguration (..), lastOption)
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Types
import           Cardano.Prelude (ConvertText (..))
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytes (..),
                   MempoolCapacityBytesOverride (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots (NumOfDiskSnapshots (..),
                   SnapshotInterval (..))
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args (FlushFrequency (..),
                   QueryBatchSize (..))

import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Last (..))
import           Data.Text (Text)
import           Data.Time.Clock (secondsToDiffTime)
import           Data.Word (Word32)
import           Options.Applicative hiding (str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as OptI
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
  dbFp <- lastOption parseDbPath
  socketFp <- lastOption $ parseSocketPath "Path to a cardano-node socket"
  traceForwardSocket <- lastOption parseTracerSocketMode

  -- Protocol files
  byronCertFile   <- optional parseByronDelegationCert
  byronKeyFile    <- optional parseByronSigningKey
  shelleyKESFile  <- optional parseKesKeyFilePath
  shelleyVRFFile  <- optional parseVrfKeyFilePath
  shelleyCertFile <- optional parseOperationalCertFilePath
  shelleyBulkCredsFile <- optional parseBulkCredsFilePath
  startAsNonProducingNode <- lastOption parseStartAsNonProducingNode

  -- Node Address
  nIPv4Address <- lastOption parseHostIPv4Addr
  nIPv6Address <- lastOption parseHostIPv6Addr
  nPortNumber  <- lastOption parsePort

  -- NodeConfiguration filepath
  nodeConfigFp <- lastOption parseConfigFile

  validate <- lastOption parseValidateDB
  shutdownIPC <- lastOption parseShutdownIPC
  shutdownOnLimit <- lastOption parseShutdownOn

  maybeMempoolCapacityOverride <- lastOption parseMempoolCapacityOverride

  -- LedgerDB configuration
  numOfDiskSnapshots <- lastOption parseNumOfDiskSnapshots
  snapshotInterval   <- lastOption parseSnapshotInterval
  ledgerDBBackend    <- lastOption parseLedgerDBBackend
  pncFlushFrequency  <- lastOption parseFlushFrequency
  pncQueryBatchSize  <- lastOption parseQueryBatchSize

  -- Storing to SSD configuration
  ssdDatabaseDir    <- lastOption parseSsdDatabaseDir
  ssdSnapshotState  <- lastOption parseSsdSnapshotState
  ssdSnapshotTables <- lastOption parseSsdSnapshotTables

  pure $ PartialNodeConfiguration
           { pncSocketConfig =
               Last . Just $ SocketConfig
                 nIPv4Address
                 nIPv6Address
                 nPortNumber
                 socketFp
           , pncConfigFile   = ConfigYamlFilePath <$> nodeConfigFp
           , pncTopologyFile = TopologyFile <$> topFp
           , pncDatabaseFile = DbFile <$> dbFp
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
           , pncNumOfDiskSnapshots = numOfDiskSnapshots
           , pncSnapshotInterval = snapshotInterval
           , pncLedgerDBBackend = ledgerDBBackend
           , pncFlushFrequency
           , pncQueryBatchSize
           , pncProtocolIdleTimeout = mempty
           , pncTimeWaitTimeout = mempty
           , pncChainSyncIdleTimeout = mempty
           , pncAcceptedConnectionsLimit = mempty
           , pncTargetNumberOfRootPeers = mempty
           , pncTargetNumberOfKnownPeers = mempty
           , pncTargetNumberOfEstablishedPeers = mempty
           , pncTargetNumberOfActivePeers = mempty
           , pncTargetNumberOfKnownBigLedgerPeers = mempty
           , pncTargetNumberOfEstablishedBigLedgerPeers = mempty
           , pncTargetNumberOfActiveBigLedgerPeers = mempty
           , pncEnableP2P = mempty
           , pncPeerSharing = mempty
           , pncSsdDatabaseDir = ssdDatabaseDir
           , pncSsdSnapshotState = ssdSnapshotState
           , pncSsdSnapshotTables = ssdSnapshotTables
           }

parseSocketPath :: Text -> Parser SocketPath
parseSocketPath helpMessage =
  fmap File $ strOption $ mconcat
    [ long "socket-path"
    , help (toS helpMessage)
    , completer (bashCompleter "file")
    , metavar "FILEPATH"
    ]

parseTracerSocketMode :: Parser (SocketPath, ForwarderMode)
parseTracerSocketMode =
  asum
    [ fmap ((, Responder) . File) $ strOption $ mconcat
      [ long "tracer-socket-path-accept"
      , help "Accept incoming cardano-tracer connection at local socket"
      , completer (bashCompleter "file")
      , metavar "FILEPATH"
      ]
    , fmap ((, Initiator) . File) $ strOption $ mconcat
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
      MempoolCapacityBytesOverride . MempoolCapacityBytes <$>
      Opt.option (auto @Word32)
        (  long "mempool-capacity-override"
        <> metavar "BYTES"
        <> help "The number of bytes"
        )
    parseNoOverride :: Parser MempoolCapacityBytesOverride
    parseNoOverride =
      flag' NoMempoolCapacityBytesOverride
        (  long "no-mempool-capacity-override"
        <> help "Don't override the mempool capacity"
        )

parseLedgerDBBackend :: Parser LedgerDbSelectorFlag
parseLedgerDBBackend = parseV1InMemory <|> parseV2InMemory <|> parseLMDB <*> optional parseMapSize
  where
    parseV1InMemory :: Parser LedgerDbSelectorFlag
    parseV1InMemory =
      flag' V1InMemory (  long "v1-in-memory-ledger-db-backend"
                     <> help "Use the V1 InMemory ledger DB backend. \
                             \ Incompatible with `--lmdb-ledger-db-backend`. \
                             \ The node uses the in-memory backend by default \
                             \ if no ``--*-db-backend`` flags are set."
                     )

    parseV2InMemory :: Parser LedgerDbSelectorFlag
    parseV2InMemory =
      flag' V2InMemory (  long "v2-in-memory-ledger-db-backend"
                     <> help "Use the V2 InMemory ledger DB backend. \
                             \ Incompatible with `--lmdb-ledger-db-backend`. \
                             \ The node uses the in-memory backend by default \
                             \ if no ``--*-db-backend`` flags are set."
                     )

    parseLMDB :: Parser (Maybe Gigabytes -> LedgerDbSelectorFlag)
    parseLMDB =
      flag' V1LMDB (  long "v1-lmdb-ledger-db-backend"
                 <> help "Use the LMDB ledger DB backend. By default, the \
                         \ mapsize (maximum database size) of the backend \
                         \ is set to 16 Gigabytes. Warning: if the database \
                         \ size exceeds the given mapsize, the node will \
                         \ abort. Therefore, the mapsize should be set to a \
                         \ value high enough to guarantee that the maximum \
                         \ database size will not be reached during the \
                         \ expected node uptime. \
                         \ Incompatible with `--in-memory-ledger-db-backend`."
                )

    parseMapSize :: Parser Gigabytes
    parseMapSize =
      option auto (
           long "lmdb-mapsize"
        <> metavar "NR_GIGABYTES"
        <> help "The maximum database size defined in number of Gigabytes."
      )

parseFlushFrequency :: Parser FlushFrequency
parseFlushFrequency = RequestedFlushFrequency <$>
  option auto (
      long "flush-frequency"
    <> metavar "WORD"
    <> help "Flush parts of the ledger state to disk after WORD blocks have \
            \moved into the immutable part of the chain. This should be at \
            \least 0."
    )

parseQueryBatchSize :: Parser QueryBatchSize
parseQueryBatchSize = RequestedQueryBatchSize <$>
  option auto (
       long "query-batch-size"
    <> metavar "WORD"
    <> help "When reading large amounts of ledger state data from disk for a \
            \ledger state query, perform reads in batches of WORD size. This \
            \should be at least 1."
    )

parseDbPath :: Parser FilePath
parseDbPath =
  strOption
    ( long "database-path"
    <> metavar "FILEPATH"
    <> help "Directory where the state is stored."
    <> completer (bashCompleter "file")
    )

parseValidateDB :: Parser Bool
parseValidateDB =
    switch (
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

parseStartAsNonProducingNode :: Parser Bool
parseStartAsNonProducingNode =
  switch $ mconcat
    [ long "non-producing-node"
    , help $ mconcat
        [ "Start the node as a non block producing node even if "
        , "credentials are specified."
        ]
    ]

parseNumOfDiskSnapshots :: Parser NumOfDiskSnapshots
parseNumOfDiskSnapshots = fmap RequestedNumOfDiskSnapshots parseNum
  where
  parseNum = Opt.option auto
    ( long "num-of-disk-snapshots"
        <> metavar "NUMOFDISKSNAPSHOTS"
        <> help "Number of ledger snapshots stored on disk."
    )

-- TODO revisit because it sucks
parseSnapshotInterval :: Parser SnapshotInterval
parseSnapshotInterval = fmap (RequestedSnapshotInterval . secondsToDiffTime) parseDifftime
  where
  parseDifftime = Opt.option auto
    ( long "snapshot-interval"
        <> metavar "SNAPSHOTINTERVAL"
        <> help "Snapshot Interval (in seconds)"
    )

parseSsdDatabaseDir :: Parser FilePath
parseSsdDatabaseDir =
  strOption
    ( long "ssd-database-dir"
      <> metavar "FILEPATH"
      <> help "Directory where the LMDB is stored."
      <> completer (bashCompleter "file")
    )

parseSsdSnapshotState :: Parser Bool
parseSsdSnapshotState =
  switch (
       long "ssd-snapshot-state"
    <> help "Store serialization of the ledger state in the SSD dir."
  )

parseSsdSnapshotTables :: Parser Bool
parseSsdSnapshotTables =
  switch (
       long "ssd-snapshot-tables"
    <> help "Store the copied LMDB tables in the SSD dir."
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
  (`OptI.renderShowS` "") . OptI.layoutPretty (OptI.LayoutOptions (OptI.AvailablePerLine cols 1.0))
