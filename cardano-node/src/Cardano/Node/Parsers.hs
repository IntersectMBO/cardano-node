{-# LANGUAGE RankNTypes           #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Node.Parsers
  ( loggingParser
  , parseCoreNodeId
  , parseProtocol
  , parseProtocolAsCommand
  , parseTopologyInfo
  ) where


import           Prelude (String)

import           Cardano.Prelude hiding (option)

import           Options.Applicative

import           Cardano.Node.Features.Logging (LoggingCLIArguments(..))
import           Ouroboros.Consensus.NodeId (NodeId(..), CoreNodeId(..))

import           Cardano.Common.Protocol
import           Cardano.Node.Configuration.Topology

-- Common command line parsers

parseCoreNodeId :: Parser CoreNodeId
parseCoreNodeId =
    option (fmap CoreNodeId auto) (
            long "core-node-id"
         <> short 'n'
         <> metavar "CORE-NODE-ID"
         <> help "The ID of the core node to which this client is connected."
    )

parseNodeId :: String -> Parser NodeId
parseNodeId desc =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help desc
    )

parseProtocol :: Parser Protocol
parseProtocol = asum
  [ fl ByronLegacy "byron-legacy"
    "Use the Byron/Ouroboros Classic suite of algorithms"

  , fl BFT "bft"
    "Use the BFT consensus algorithm"

  , fl Praos "praos"
    "Use the Praos consensus algorithm"

  , fl MockPBFT "mock-pbft"
    "Use the Permissive BFT consensus algorithm using a mock ledger"

  , fl RealPBFT "real-pbft"
    "Use the Permissive BFT consensus algorithm using the real ledger"
  ]
  where
    fl :: forall a. a -> String -> String -> Parser a
    fl x l h = flag' x $ mconcat [long l, help h]

parseProtocolAsCommand :: Parser Protocol
parseProtocolAsCommand = subparser $ mconcat
  [ commandGroup "System version"
  , metavar "SYSTEMVER"
  , cmd "byron-legacy" "Byron Legacy mode" $ pure ByronLegacy
  , cmd "bft"          "BFT mode"          $ pure BFT
  , cmd "praos"        "Praos mode"        $ pure Praos
  , cmd "mock-pbft"    "Mock PBFT mode"    $ pure MockPBFT
  , cmd "real-pbft"    "Real PBFT mode"    $ pure RealPBFT
  ] where
  cmd :: forall a. String -> String -> Parser a -> Mod CommandFields a
  cmd c desc p = command c $ info (p <**> helper) $ mconcat [ progDesc desc ]

parseTopologyInfo :: String -> Parser TopologyInfo
parseTopologyInfo desc = TopologyInfo <$> parseNodeId desc <*> parseTopologyFile

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

-- | The parser for the logging specific arguments.
loggingParser :: Parser LoggingCLIArguments
loggingParser = LoggingCLIArguments
    <$> strOption
        ( long "log-config"
       <> metavar "LOGCONFIG"
       <> help "Configuration file for logging"
       <> completer (bashCompleter "file")
        )
