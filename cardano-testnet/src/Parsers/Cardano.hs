module Parsers.Cardano
  ( CardanoOptions(..)
  , cmdCardano
  ) where

import           Prelude

import           Cardano.CLI.Environment
import           Cardano.CLI.Legacy.Options
import qualified Data.List as L
import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.EraBased.Options.Common hiding (pNetworkId)

import           Testnet.Process.Cli
import           Testnet.Property.Utils
import           Testnet.Runtime (readNodeLoggingFormat)
import           Testnet.Start.Cardano

newtype CardanoOptions = CardanoOptions
  { testnetOptions :: CardanoTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: EnvCli -> Parser CardanoTestnetOptions
optsTestnet envCli = CardanoTestnetOptions
  <$> pNumBftAndSpoNodes
  <*> pLegacyCardanoEra envCli
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (cardanoEpochLength cardanoDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "SECONDS"
      <>  OA.showDefault
      <>  OA.value (cardanoSlotLength cardanoDefaultTestnetOptions)
      )
  <*> pNetworkId
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (cardanoActiveSlotsCoeff cardanoDefaultTestnetOptions)
      )
  <*> pMaxLovelaceSupply
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (cardanoEnableP2P cardanoDefaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (cardanoNodeLoggingFormat cardanoDefaultTestnetOptions)
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
          <>  OA.value (cardanoNodes cardanoDefaultTestnetOptions)
          )
    <*> OA.option
          ((`L.replicate` SpoTestnetNodeOptions) <$> auto)
          (   OA.long "num-pool-nodes"
          <>  OA.help "Number of pool nodes"
          <>  OA.metavar "COUNT"
          <>  OA.showDefault
          <>  OA.value (cardanoNodes cardanoDefaultTestnetOptions)
          )

optsCardano :: EnvCli -> Parser CardanoOptions
optsCardano envCli = CardanoOptions <$> optsTestnet envCli

cmdCardano :: EnvCli -> Mod CommandFields CardanoOptions
cmdCardano envCli = command' "cardano" "Start a testnet in any era" (optsCardano envCli)
