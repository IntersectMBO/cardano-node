{-# LANGUAGE GADTs #-}

module Cardano.Node.CLI (
  -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , ViewMode(..)
  , fromProtocol
  -- * Parsers
  , parseSystemStart
  , parseSlotDuration
  , parseProtocol
  , parseNodeId
  , parseCoreNodeId
  , parseNumCoreNodes
  , parseViewMode
  -- * Generic
  , command'
  ) where

import           Prelude

import           Data.Foldable (asum)
import           Data.Semigroup ((<>))
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Util

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

-- Node can be run in two modes.
data ViewMode =
    LiveView    -- Live mode with TUI (based on 'ansi-terminal-game').
  | SimpleView  -- Simple mode, just output text.

{-------------------------------------------------------------------------------
  Command parsers
-------------------------------------------------------------------------------}

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

parseNodeId :: Parser NodeId
parseNodeId =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node"
    )

parseCoreNodeId :: Parser CoreNodeId
parseCoreNodeId =
    option (fmap CoreNodeId auto) (
            long "core-node-id"
         <> short 'n'
         <> metavar "CORE-NODE-ID"
         <> help "The ID of the core node to which this client is connected."
    )

parseNumCoreNodes :: Parser NumCoreNodes
parseNumCoreNodes =
    option (fmap NumCoreNodes auto) (
            long "num-core-nodes"
         <> short 'm'
         <> metavar "NUM-CORE-NODES"
         <> help "The number of core nodes"
    )

-- Optional flag for live view (with TUI graphics).
parseViewMode :: Parser ViewMode
parseViewMode =
    flag SimpleView LiveView $ mconcat
        [ long "live-view"
        , help "Live view with TUI."
        ]

{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]
