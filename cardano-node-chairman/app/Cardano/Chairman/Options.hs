{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Chairman.Options
  ( mkNodeClientProtocol
  , ChairmanArgs(..)
  , parseChairmanArgs
  , opts
  ) where

import           Cardano.Api.Protocol.Byron
import           Cardano.Api.Protocol.Cardano
import           Cardano.Api.Protocol.Shelley
import           Cardano.Api.Protocol.Types
import           Cardano.Api.Typed (NetworkMagic (..))
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types
import           Cardano.Prelude hiding (option)
import           Control.Monad.Class.MonadTime (DiffTime)
import           Options.Applicative
import           Ouroboros.Consensus.BlockchainTime (SlotLength, slotLengthFromSec)
import           Ouroboros.Consensus.Cardano (SecurityParam (..))
import           Ouroboros.Network.Block (BlockNo)

import qualified Options.Applicative as Opt

--TODO: replace this with the new stuff from Cardano.Api.Protocol
mkNodeClientProtocol :: Protocol -> SomeNodeClientProtocol
mkNodeClientProtocol protocol =
  case protocol of
    ByronProtocol ->
      mkSomeNodeClientProtocolByron
        (EpochSlots 21600)

    ShelleyProtocol ->
      mkSomeNodeClientProtocolShelley

    CardanoProtocol ->
      mkSomeNodeClientProtocolCardano
        (EpochSlots 21600)

data ChairmanArgs = ChairmanArgs
    -- | Stop the test after given number of seconds. The chairman will
    -- observe only for the given period of time, and check the consensus
    -- and progress conditions at the end.
    --
  { caRunningTime :: !DiffTime
    -- | Expect this amount of progress (chain growth) by the end of the test.
  , caMinProgress :: !(Maybe BlockNo)
  , caSocketPaths :: ![SocketPath]
  , caConfigYaml :: !ConfigYamlFilePath
  , caSlotLength :: !SlotLength
  , caSecurityParam :: !SecurityParam
  , caNetworkMagic :: !NetworkMagic
  }

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
    <> metavar "NODE-CONFIGURATION"
    <> help "Configuration file for the cardano-node"
    <> completer (bashCompleter "file")
    )

parseSocketPath :: Text -> Parser SocketPath
parseSocketPath helpMessage =
  SocketPath <$> strOption
    ( long "socket-path"
    <> help (toS helpMessage)
    <> completer (bashCompleter "file")
    <> metavar "FILEPATH"
    )

parseRunningTime :: Parser DiffTime
parseRunningTime =
  option ((fromIntegral :: Int -> DiffTime) <$> auto)
    (  long "timeout"
    <> short 't'
    <> metavar "SECONDS"
    <> help "Run the chairman for this length of time in seconds."
    )

parseSlotLength :: Parser SlotLength
parseSlotLength =
  option (slotLengthFromSec <$> Opt.auto)
    ( long "slot-length"
    <> metavar "INT"
    <> help "Slot length in seconds."
    )

parseSecurityParam :: Parser SecurityParam
parseSecurityParam =
  option (SecurityParam <$> Opt.auto)
    ( long "security-parameter"
    <> metavar "INT"
    <> help "Security parameter"
    )


parseTestnetMagic :: Parser NetworkMagic
parseTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "INT"
      <> Opt.help "The testnet network magic number"
      )

parseProgress :: Parser BlockNo
parseProgress =
  option ((fromIntegral :: Int -> BlockNo) <$> auto)
    (  long "require-progress"
    <> short 'p'
    <> metavar "INT"
    <> help "Require this much chain-growth progress, in blocks."
  )

parseChairmanArgs :: Parser ChairmanArgs
parseChairmanArgs =
  ChairmanArgs
  <$> parseRunningTime
  <*> optional parseProgress
  <*> some (parseSocketPath "Path to a cardano-node socket")
  <*> fmap ConfigYamlFilePath parseConfigFile
  <*> parseSlotLength
  <*> parseSecurityParam
  <*> parseTestnetMagic

opts :: ParserInfo ChairmanArgs
opts = info (parseChairmanArgs <**> helper)
  ( fullDesc
  <> progDesc "Chairman checks Cardano clusters for progress and consensus."
  <> header "Chairman sits in a room full of Shelley nodes, and checks \
            \if they are all behaving ...")
