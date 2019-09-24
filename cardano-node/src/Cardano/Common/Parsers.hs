{-# LANGUAGE RankNTypes           #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Common.Parsers
  ( loggingParser
  , parseCoreNodeId
  , parseProtocol
  , parseProtocolActual
  , parseProtocolAsCommand
  , parseTopologyInfo
  ) where


import           Prelude (String)

import           Cardano.Prelude hiding (option)

import           Options.Applicative

import           Cardano.Config.Logging (LoggingCLIArguments(..))
import           Ouroboros.Consensus.NodeId (NodeId(..), CoreNodeId(..))

import qualified Cardano.BM.Data.Severity as Log
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

-- | Flag parser, that returns its argument on success.
fl :: a -> String -> String -> Parser a
fl val opt desc = flag' val $ mconcat [long opt, help desc]

parseProtocol :: Parser Protocol
parseProtocol = asum
  [ fl ByronLegacy "byron-legacy"
    "Byron/Ouroboros Classic suite of algorithms"

  , fl BFT "bft"
    "BFT consensus"

  , fl Praos "praos"
    "Praos consensus"

  , fl MockPBFT "mock-pbft"
    "Permissive BFT consensus with a mock ledger"

  , fl RealPBFT "real-pbft"
    "Permissive BFT consensus with a real ledger"
  ]

parseProtocolActual :: Parser Protocol
parseProtocolActual = asum
  [ fl ByronLegacy "byron-legacy"
    "Byron/Ouroboros Classic suite of algorithms"

  , fl RealPBFT "real-pbft"
    "Permissive BFT consensus with a real ledger"
  ]

parseProtocolAsCommand :: Parser Protocol
parseProtocolAsCommand = subparser $ mconcat
  [ commandGroup "System version"
  , metavar "SYSTEMVER"
  , cmd "byron-legacy" "Byron Legacy mode" $ pure ByronLegacy
  , cmd "bft"          "BFT mode"          $ pure BFT
  , cmd "praos"        "Praos mode"        $ pure Praos
  , cmd "mock-pbft"    "Mock PBFT mode"    $ pure MockPBFT
  , cmd "real-pbft"    "Real PBFT mode"    $ pure RealPBFT
  ]

  where
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
    <*> option auto
        ( long "log-min-severity"
       <> metavar "SEVERITY"
       <> help "Limit logging to items with severity at least this severity"
       <> value Log.Info
       <> showDefault
        )
