{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Common.Parsers
  ( loggingParser
  , nodeCliParser
  , parseConfigFile
  , parseCoreNodeId
  , parseDbPath
  , parseFilePath
  , parseFlag
  , parseFlag'
  , parseGenesisFile
  , parseIntegral
  , parseIntegralWithDefault
  , parseLogOutputFile
  , parseNodeId
  , parseProtocol
  , parseSocketDir
  , parseTopologyInfo
  ) where


import           Prelude (String)

import           Cardano.Prelude hiding (option)

import           Network.Socket (PortNumber)
import           Options.Applicative

import           Cardano.Config.Logging (LoggingCLIArguments(..))
import           Ouroboros.Consensus.NodeId (NodeId(..), CoreNodeId(..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Protocol
import           Cardano.Config.Topology
import           Cardano.Config.Types

-- Common command line parsers

-- | The product parser for all the CLI arguments.
nodeCliParser :: Parser NodeCLI
nodeCliParser = do
  -- Filepaths
  topFp <- parseTopologyFile
  dbFp <- parseDbPath
  genFp <- parseGenesisPath
  delCertFp <- optional parseDelegationCert
  sKeyFp <- optional parseSigningKey
  socketFp <- parseSocketDir

  genHash <- parseGenesisHash

  -- Node Address
  nAddress <- parseNodeAddress

  -- NodeConfiguration filepath
  nodeConfigFp <- parseConfigFile

  validate <- parseValidateDB

  pure NodeCLI
    { mscFp = MiscellaneousFilepaths
      { topFile = TopologyFile topFp
      , dBFile = DbFile dbFp
      , genesisFile = GenesisFile genFp
      , delegCertFile = DelegationCertFile <$> delCertFp
      , signKeyFile = SigningKeyFile <$> sKeyFp
      , socketFile = SocketFile socketFp
      }
    , genesisHash = genHash
    , nodeAddr = nAddress
    , configFp = ConfigYamlFilePath nodeConfigFp
    , validateDB = validate
    }

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
    )


parseGenesisFile :: String -> Parser GenesisFile
parseGenesisFile opt =
  GenesisFile <$> parseFilePath opt "Genesis JSON file."

-- Common command line parsers

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption $ long optname <> metavar "FILEPATH" <> help desc

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseIntegralWithDefault :: Integral a => String -> String -> a -> Parser a
parseIntegralWithDefault optname desc def = option (fromInteger <$> auto)
 $ long optname <> metavar "INT" <> help desc <> value def

parseFlag :: String -> String -> Parser Bool
parseFlag = parseFlag' False True

parseFlag' :: a -> a -> String -> String -> Parser a
parseFlag' def active optname desc =
  flag def active $ long optname <> help desc

parseCoreNodeId :: Parser CoreNodeId
parseCoreNodeId =
    option (fmap CoreNodeId auto) (
            long "core-node-id"
         <> metavar "CORE-NODE-ID"
         <> help "The ID of the core node to which this client is connected."
    )

parseNodeId :: String -> Parser NodeId
parseNodeId desc =
    option (fmap (CoreId . CoreNodeId) auto) (
            long "node-id"
         <> metavar "NODE-ID"
         <> help desc
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseHostAddr :: Parser NodeHostAddress
parseHostAddr =
    option (NodeHostAddress . readMaybe <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "Optionally limit node to one ipv6 or ipv4 address"
       <> (value $ NodeHostAddress Nothing)
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

parseSocketDir :: Parser FilePath
parseSocketDir =
  strOption
    ( long "socket-dir"
        <> metavar "FILEPATH"
        <> help "Directory with local sockets:\
                \  ${dir}/node-{core,relay}-${node-id}.socket"
    )

parseValidateDB :: Parser Bool
parseValidateDB =
    switch (
         long "validate-db"
      <> help "Validate all on-disk database files"
    )

-- | Flag parser, that returns its argument on success.
flagParser :: a -> String -> String -> Parser a
flagParser val opt desc = flag' val $ long opt <> help desc

parseProtocol :: Parser Protocol
parseProtocol = asum
  [ flagParser ByronLegacy "byron-legacy"
    "Byron/Ouroboros Classic suite of algorithms"
  , flagParser BFT "bft"
    "BFT consensus"
  , flagParser Praos "praos"
    "Praos consensus"
  , flagParser MockPBFT "mock-pbft"
    "Permissive BFT consensus with a mock ledger"
  , flagParser RealPBFT "real-pbft"
    "Permissive BFT consensus with a real ledger"
  ]

parseTopologyInfo :: String -> Parser TopologyInfo
parseTopologyInfo desc = TopologyInfo <$> parseNodeId desc <*> parseTopologyFile

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )
parseLogOutputFile :: Parser FilePath
parseLogOutputFile =
  strOption
    ( long "log-output"
    <> metavar "FILEPATH"
    <> help "Logging output file"
    <> completer (bashCompleter "file")
    )

-- | A parser disables logging if --log-config is not supplied.
loggingParser :: Parser LoggingCLIArguments
loggingParser =
  fromMaybe muteLoggingCLIArguments
    <$> optional parseLoggingCLIArgumentsInternal
  where
    parseLoggingCLIArgumentsInternal :: Parser LoggingCLIArguments
    parseLoggingCLIArgumentsInternal =
      LoggingCLIArguments
        <$> (Just
             <$> strOption
              ( long "log-config"
                <> metavar "LOGCONFIG"
                <> help "Configuration file for logging"
                <> completer (bashCompleter "file")))
        <*> switch
         ( long "log-metrics"
           <> help "Log a number of metrics about this node")

    -- This is the value returned by the parser, when --log-config is omitted.
    muteLoggingCLIArguments :: LoggingCLIArguments
    muteLoggingCLIArguments =
      LoggingCLIArguments
      Nothing
      False
