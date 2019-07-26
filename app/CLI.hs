{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}

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
import qualified Data.IP as IP
import           Options.Applicative
import           Network.Socket (PortNumber)

import           Cardano.Prelude hiding (option, (.))

import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.Ledger.Mock as Mock

import           Cardano.Shell.Constants.CLI
import           Cardano.Shell.Constants.PartialTypes (PartialGenesis (..), PartialStaticKeyMaterial (..))

import           Topology (NodeAddress (..), TopologyInfo (..))
import           TxSubmission (command', parseMockTx)

import           Cardano.Node.CLI

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data NodeCLIArguments = NodeCLIArguments {
    systemStart        :: !SystemStart
  , slotDuration       :: !SlotLength
  , genesisSpec        :: !(Last PartialGenesis)
  , keyMaterialSpec    :: !(Last PartialStaticKeyMaterial)
  , command            :: !Command
  }

data Command =
    SimpleNode  TopologyInfo NodeAddress Protocol ViewMode
  | TxSubmitter TopologyInfo Mock.Tx     Protocol
  | TraceAcceptor

nodeParser :: Parser NodeCLIArguments
nodeParser = NodeCLIArguments
    <$> parseSystemStart
    <*> parseSlotDuration
    <*> (Last . Just <$> configGenesisCLIParser)
    <*> (Last . Just <$> configStaticKeyMaterialCLIParser)
    <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode
        <$> parseTopologyInfo
        <*> parseNodeAddress
        <*> parseProtocol
        <*> parseViewMode
  , command' "submit" "Submit a transaction." $
      TxSubmitter <$> parseTopologyInfo <*> parseMockTx <*> parseProtocol
  , command' "trace-acceptor" "Spawn an acceptor." $
      pure TraceAcceptor
  ]

parseHostAddr :: Parser IP.IP
parseHostAddr =
    option (read <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "The ipv6 or ipv4 address"
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

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
