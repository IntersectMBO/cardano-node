module Parsers.Cardano
  ( CardanoOptions(..)
  , cmdCardano
  , runCardanoOptions
  ) where

import           Prelude

import qualified Data.List as L
import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.Common.Parsers

import           Testnet
import           Testnet.Cardano
import           Testnet.Run (runTestnet)
import           Testnet.Util.Runtime (readNodeLoggingFormat)
import           Testnet.Utils

data CardanoOptions = CardanoOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: CardanoTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser CardanoTestnetOptions
optsTestnet = CardanoTestnetOptions
  <$> pNumBftAndSpoNodes
  <*> pCardanoEra
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (cardanoEpochLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "SECONDS"
      <>  OA.showDefault
      <>  OA.value (cardanoSlotLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (cardanoActiveSlotsCoeff defaultTestnetOptions)
      )
  <*> pMaxLovelaceSupply
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (cardanoEnableP2P defaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (cardanoNodeLoggingFormat defaultTestnetOptions)
      )

pNumBftAndSpoNodes :: Parser [TestnetNodeOptions]
pNumBftAndSpoNodes =
  (++)
    <$> OA.option
          ((`L.replicate` BftTestnetNodeOptions []) <$> auto)
          (   OA.long "num-bft-nodes"
          <>  OA.help "Number of BFT nodes"
          <>  OA.metavar "COUNT"
          <>  OA.showDefault
          <>  OA.value (cardanoNodes defaultTestnetOptions)
          )
    <*> OA.option
          ((`L.replicate` SpoTestnetNodeOptions) <$> auto)
          (   OA.long "num-pool-nodes"
          <>  OA.help "Number of pool nodes"
          <>  OA.metavar "COUNT"
          <>  OA.showDefault
          <>  OA.value (cardanoNodes defaultTestnetOptions)
          )

optsCardano :: Parser CardanoOptions
optsCardano = CardanoOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runCardanoOptions :: CardanoOptions -> IO ()
runCardanoOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.testnet (CardanoOnlyTestnetOptions $ testnetOptions options)

cmdCardano :: Mod CommandFields CardanoOptions
cmdCardano = command' "cardano" "Start a testnet in any era" optsCardano
