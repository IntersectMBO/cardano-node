{-# LANGUAGE GADTs #-}

module CLI (
    -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , fromProtocol
    -- * CLI
  , CLI(..)
  , TopologyInfo(..)
  , Command(..)
  , parseCLI
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Data.Foldable (asum)
import           Data.Semigroup ((<>))
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Util

import           Mock.TxSubmission (command', parseMockTx)
import           Topology (TopologyInfo (..), NodeAddress (..))

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  Untyped/typed protocol boundary
-------------------------------------------------------------------------------}

data Protocol =
    BFT
  | Praos
  | MockPBFT
  | RealPBFT

data SomeProtocol where
  SomeProtocol :: RunDemo blk => DemoProtocol blk -> SomeProtocol

fromProtocol :: Protocol -> IO SomeProtocol
fromProtocol BFT =
    case runDemo p of
      Dict -> return $ SomeProtocol p
  where
    p = DemoBFT defaultSecurityParam
fromProtocol Praos =
    case runDemo p of
      Dict -> return $ SomeProtocol p
  where
    p = DemoPraos defaultDemoPraosParams
fromProtocol MockPBFT =
    case runDemo p of
      Dict -> return $ SomeProtocol p
  where
    p = DemoMockPBFT defaultDemoPBftParams
fromProtocol RealPBFT =
    case runDemo p of
      Dict -> return $ SomeProtocol p
  where
    p = DemoRealPBFT defaultDemoPBftParams genesisConfig
    genesisConfig = Dummy.dummyConfig

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data CLI = CLI {
    systemStart  :: SystemStart
  , slotDuration :: SlotLength
  , command      :: Command
  }

data Command =
    SimpleNode  TopologyInfo NodeAddress Protocol
  | TxSubmitter TopologyInfo Mock.Tx

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseSystemStart
    <*> parseSlotDuration
    <*> parseCommand

parseSystemStart :: Parser SystemStart
parseSystemStart = option (SystemStart <$> auto) $ mconcat [
      long "system-start"
    , help "The start time of the system (e.g. \"2018-12-10 15:58:06\""
    ]

parseSlotDuration :: Parser SlotLength
parseSlotDuration = option (mkSlotLength <$> auto) $ mconcat [
      long "slot-duration"
    , value (mkSlotLength 5)
    , help "The slot duration (seconds)"
    ]
  where
    mkSlotLength :: Integer -> SlotLength
    mkSlotLength = slotLengthFromMillisec . (* 1000)

parseProtocol :: Parser Protocol
parseProtocol = asum [
      flag' BFT $ mconcat [
          long "bft"
        , help "Use the BFT consensus algorithm"
        ]
    , flag' Praos $ mconcat [
          long "praos"
        , help "Use the Praos consensus algorithm"
        ]
    , flag' MockPBFT $ mconcat [
          long "mock-pbft"
        , help "Use the Permissive BFT consensus algorithm using a mock ledger"
        ]
    , flag' RealPBFT $ mconcat [
          long "real-pbft"
        , help "Use the Permissive BFT consensus algorithm using the real ledger"
        ]
    ]

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode <$> parseTopologyInfo <*> parseNodeAddress <*> parseProtocol
  , command' "submit" "Submit a transaction." $
      TxSubmitter <$> parseTopologyInfo <*> parseMockTx
  ]

parseNodeId :: Parser NodeId
parseNodeId =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node"
    )

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
