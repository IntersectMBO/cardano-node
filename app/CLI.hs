{-# LANGUAGE GADTs #-}

module CLI (
    -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , fromProtocol
    -- * CLI
  , NodeCLIArguments(..)
  , TopologyInfo(..)
  , Command(..)
  , nodeParser
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Data.Semigroup ((<>))
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.Ledger.Mock as Mock

import           Topology (NodeAddress (..), TopologyInfo (..))
import           TxSubmission (command', parseMockTx)

import           Cardano.Node.CLI

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data NodeCLIArguments = NodeCLIArguments {
    systemStart  :: !SystemStart
  , slotDuration :: !SlotLength
  , command      :: !Command
  }

data Command =
    SimpleNode  TopologyInfo NodeAddress Protocol
  | TxSubmitter TopologyInfo Mock.Tx     Protocol
  | TraceAcceptor

nodeParser :: Parser NodeCLIArguments
nodeParser = NodeCLIArguments
    <$> parseSystemStart
    <*> parseSlotDuration
    <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode <$> parseTopologyInfo <*> parseNodeAddress <*> parseProtocol
  , command' "submit" "Submit a transaction." $
      TxSubmitter <$> parseTopologyInfo <*> parseMockTx <*> parseProtocol
  , command' "trace-acceptor" "Spawn an acceptor." $
      pure TraceAcceptor
  ]

parseHostName :: Parser String
parseHostName =
    option str (
          long "host"
       <> metavar "HOST-NAME"
       <> help "The host name"
    )

parsePort :: Parser String
parsePort =
    option str (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostName <*> parsePort

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

parseTopologyInfo :: Parser TopologyInfo
parseTopologyInfo = TopologyInfo <$> parseNodeId <*> parseTopologyFile
