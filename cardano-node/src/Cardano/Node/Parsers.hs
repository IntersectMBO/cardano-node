{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Parsers
  ( nodeCLIParser
  , parserHelpHeader
  , parserHelpOptions
  , renderHelpDoc
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Data.Maybe (fromMaybe)
import           Network.Socket (PortNumber)
import           Options.Applicative hiding (str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as OptI
import           System.Posix.Types (Fd (..))

import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo (..))

import           Cardano.Node.Types

nodeCLIParser  :: Parser NodeCLI
nodeCLIParser = subparser
                (  commandGroup "Run the node"
                <> metavar "run"
                <> command "run"
                     (info (nodeRunParser <**> helper)
                           (progDesc "Run the node." ))
                )

nodeRunParser :: Parser NodeCLI
nodeRunParser = do
  -- Filepaths
  topFp <- parseTopologyFile
  dbFp <- parseDbPath
  socketFp <-   optional $ parseSocketPath "Path to a cardano-node socket"

  -- Protocol files
  byronCertFile   <- optional parseDelegationCert
  byronKeyFile    <- optional parseSigningKey
  shelleyKESFile  <- optional parseKesKeyFilePath
  shelleyVRFFile  <- optional parseVrfKeyFilePath
  shelleyCertFile <- optional parseOperationalCertFilePath

  -- Node Address
  nAddress <- optional parseNodeAddress

  -- NodeConfiguration filepath
  nodeConfigFp <- parseConfigFile

  validate <- parseValidateDB
  shutdownIPC <- parseShutdownIPC

  shutdownOnSlotSynced <- parseShutdownOnSlotSynced

  pure NodeCLI
    { nodeAddr = nAddress
    , configFile   = ConfigYamlFilePath nodeConfigFp
    , topologyFile = TopologyFile topFp
    , databaseFile = DbFile dbFp
    , socketFile   = socketFp
    , protocolFiles = ProtocolFilepaths
      { byronCertFile
      , byronKeyFile
      , shelleyKESFile
      , shelleyVRFFile
      , shelleyCertFile
      }
    , validateDB = validate
    , shutdownIPC
    , shutdownOnSlotSynced
    }

parseSocketPath :: Text -> Parser SocketPath
parseSocketPath helpMessage =
  SocketPath <$> strOption
    ( long "socket-path"
        <> (help $ toS helpMessage)
        <> completer (bashCompleter "file")
        <> metavar "FILEPATH"
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseHostAddr :: Parser NodeHostAddress
parseHostAddr =
    option (eitherReader parseNodeHostAddress) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "Optionally limit node to one ipv6 or ipv4 address"
       <> value (NodeHostAddress Nothing)
    )

parseNodeHostAddress :: String -> Either String NodeHostAddress
parseNodeHostAddress str =
   maybe (Left $ "Failed to parse: " ++ str) (Right . NodeHostAddress . Just) $ readMaybe str

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
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

parseShutdownIPC :: Parser (Maybe Fd)
parseShutdownIPC =
    optional $ option (Fd <$> auto) (
         long "shutdown-ipc"
      <> metavar "FD"
      <> help "Shut down the process when this inherited FD reaches EOF"
      <> hidden
    )

parseShutdownOnSlotSynced :: Parser MaxSlotNo
parseShutdownOnSlotSynced =
    fmap (fromMaybe NoMaxSlotNo) $
    optional $ option (MaxSlotNo . SlotNo <$> auto) (
         long "shutdown-on-slot-synced"
      <> metavar "SLOT"
      <> help "Shut down the process after ChainDB is synced up to the specified slot"
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

parseDelegationCert :: Parser FilePath
parseDelegationCert =
  strOption
    ( long "delegation-certificate"
        <> metavar "FILEPATH"
        <> help "Path to the delegation certificate."
        <> completer (bashCompleter "file")
    )

parseSigningKey :: Parser FilePath
parseSigningKey =
  strOption
    ( long "signing-key"
        <> metavar "FILEPATH"
        <> help "Path to the signing key."
        <> completer (bashCompleter "file")
    )

parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
  strOption
    ( long "shelley-operational-certificate"
        <> metavar "FILEPATH"
        <> help "Path to the delegation certificate."
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


-- | Produce just the brief help header for a given CLI option parser,
--   without the options.
parserHelpHeader :: String -> Opt.Parser a -> OptI.Doc
parserHelpHeader execName = flip (OptI.parserUsage (Opt.prefs mempty)) execName

-- | Produce just the options help for a given CLI option parser,
--   without the header.
parserHelpOptions :: Opt.Parser a -> OptI.Doc
parserHelpOptions = fromMaybe mempty . OptI.unChunk . OptI.fullDesc (Opt.prefs mempty)

-- | Render the help pretty document.
renderHelpDoc :: Int -> OptI.Doc -> String
renderHelpDoc cols =
  (`OptI.displayS` "") . OptI.renderPretty 1.0 cols
