{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Parsers
  ( nodeCLIParser
  ) where

import           Cardano.Prelude hiding (option)

import           Options.Applicative
import           System.Posix.Types (Fd(..))

import           Cardano.Config.Byron.Parsers   as Byron
import           Cardano.Config.Shelley.Parsers as Shelley
import           Ouroboros.Network.Block (MaxSlotNo(..), SlotNo(..))

import           Cardano.Node.Types
import           Cardano.Config.Types (DbFile(..), ProtocolFilepaths(..),
                   NodeProtocolMode(..), TopologyFile(..))
import           Cardano.Config.Parsers

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
  byronCertFile   <- optional Byron.parseDelegationCert
  byronKeyFile    <- optional Byron.parseSigningKey
  shelleyKESFile  <- optional Shelley.parseKesKeyFilePath
  shelleyVRFFile  <- optional Shelley.parseVrfKeyFilePath
  shelleyCertFile <- optional Shelley.parseOperationalCertFilePath

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
    )
