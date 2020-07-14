{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Parsers
  ( nodeCLIParser
  ) where

import           Cardano.Prelude hiding (option)

import           Options.Applicative
import           System.Posix.Types (Fd(..))

import           Ouroboros.Network.Block (MaxSlotNo(..), SlotNo(..))

import           Cardano.Node.Types
import           Cardano.Config.Types (DbFile(..), ProtocolFilepaths(..),
                   NodeProtocolMode(..), TopologyFile(..))
import           Cardano.Config.Parsers (parseConfigFile, parseDbPath,
                   parseNodeAddress, parseSocketPath)

nodeCLIParser  :: Parser NodeCLI
nodeCLIParser = nodeRealProtocolModeParser <|> nodeMockProtocolModeParser

nodeMockProtocolModeParser :: Parser NodeCLI
nodeMockProtocolModeParser = subparser
                           (  commandGroup "Execute node with a mock protocol."
                           <> metavar "run-mock"
                           <> command "run-mock"
                                (info (nodeMockParser <**> helper)
                                      (progDesc "Execute node with a mock protocol."))
                           )
nodeRealProtocolModeParser :: Parser NodeCLI
nodeRealProtocolModeParser = subparser
                           (  commandGroup "Execute node with a real protocol."
                           <> metavar "run"
                           <> command "run"
                                (info (nodeRealParser <**> helper)
                                      (progDesc "Execute node with a real protocol." ))
                           )

-- | The mock protocol parser.
nodeMockParser :: Parser NodeCLI
nodeMockParser = do
  -- Filepaths
  topFp <- parseTopologyFile
  dbFp <- parseDbPath
  socketFp <- optional $ parseSocketPath "Path to a cardano-node socket"

  -- NodeConfiguration filepath
  nodeConfigFp <- parseConfigFile

  -- Node Address
  nAddress <- optional parseNodeAddress

  validate <- parseValidateDB
  shutdownIPC <- parseShutdownIPC
  shutdownOnSlotSynced <- parseShutdownOnSlotSynced

  pure $ NodeCLI
           { nodeMode = MockProtocolMode
           , nodeAddr = nAddress
           , configFile   = ConfigYamlFilePath nodeConfigFp
           , topologyFile = TopologyFile topFp
           , databaseFile = DbFile dbFp
           , socketFile   = socketFp
           , protocolFiles = ProtocolFilepaths
             { byronCertFile = Nothing
             , byronKeyFile  = Nothing
             , shelleyKESFile  = Nothing
             , shelleyVRFFile  = Nothing
             , shelleyCertFile = Nothing
             }
           , validateDB = validate
           , shutdownIPC
           , shutdownOnSlotSynced
           }

-- | The real protocol parser.
nodeRealParser :: Parser NodeCLI
nodeRealParser = do
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
    { nodeMode = RealProtocolMode
    , nodeAddr = nAddress
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
