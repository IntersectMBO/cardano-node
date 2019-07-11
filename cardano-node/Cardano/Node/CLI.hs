{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

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
  , parseTestnetBalanceOptions
  , parseLovelace
  , parseLovelacePortion
  , parseFakeAvvmOptions
  , parseK
  , parseProtocolMagic
  , parseFilePath
  , parseIntegral
  , parseFlag
  , parseUTCTime
  -- * Generic
  , command'
  ) where

import           Prelude

import           Data.Foldable (asum)
import           Data.Semigroup ((<>))
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Util

import           Cardano.Binary (Annotated (..))
import           Cardano.Chain.Common
import           Cardano.Chain.Genesis
import           Cardano.Crypto.ProtocolMagic

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

parseTestnetBalanceOptions :: Parser TestnetBalanceOptions
parseTestnetBalanceOptions =
  TestnetBalanceOptions
  <$> parseIntegral        "n-poor-addresses"         "Number of poor nodes (with small balance)."
  <*> parseIntegral        "n-delegate-addresses"     "Number of delegate nodes (with huge balance)."
  <*> parseLovelace        "total-balance"            "Total balance owned by these nodes."
  <*> parseLovelacePortion "delegate-share"           "Portion of stake owned by all delegates together."
  <*> parseFlag            "use-hd-addresses"         "Whether generate plain addresses or with hd payload."

parseLovelace :: String -> String -> Parser Lovelace
parseLovelace optname desc =
  either (error . show) id . mkLovelace
  <$> parseIntegral optname desc

parseLovelacePortion :: String -> String -> Parser LovelacePortion
parseLovelacePortion optname desc =
  either (error . show) id . mkLovelacePortion
  <$> parseIntegral optname desc

parseFakeAvvmOptions :: Parser FakeAvvmOptions
parseFakeAvvmOptions =
  FakeAvvmOptions
  <$> parseIntegral        "avvm-entry-count"         "Number of AVVM addresses."
  <*> parseLovelace        "avvm-entry-balance"       "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
  <$> parseIntegral        "k"                        "The security parameter of the Ouroboros protocol."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated () . ProtocolMagicId
  <$> parseIntegral        "protocol-magic"           "The magic number unique to any instance of Cardano."

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
    strOption (
            long optname
         <> metavar "FILEPATH"
         <> help desc
    )

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc =
    option (fromInteger <$> auto) (
            long optname
         <> metavar "INT"
         <> help desc
    )

parseFlag :: String -> String -> Parser Bool
parseFlag optname desc =
    flag False True (
            long optname
         <> help desc
    )

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
    option (posixSecondsToUTCTime . fromInteger <$> auto) (
            long optname
         <> metavar "POSIXSECONDS"
         <> help desc
    )

{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]
