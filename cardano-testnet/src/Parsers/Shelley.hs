module Parsers.Shelley
  ( ShelleyOptions(..)
  , cmdShelley
  ) where

import           Prelude

import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.Common.Parsers hiding (pNetworkId)

import           Testnet.Process.Cli
import           Testnet.Property.Utils
import           Testnet.Start.Shelley


newtype ShelleyOptions = ShelleyOptions
  { testnetOptions :: ShelleyTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser ShelleyTestnetOptions
optsTestnet = ShelleyTestnetOptions
  <$> OA.option auto
      (   OA.long "num-praos-nodes"
      <>  OA.help "Number of PRAOS nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (shelleyNumPraosNodes shelleyDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (shelleyNumPoolNodes shelleyDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (shelleyActiveSlotsCoeff shelleyDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security param"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (shelleySecurityParam shelleyDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (shelleyEpochLength shelleyDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (shelleySlotLength shelleyDefaultTestnetOptions)
      )
  <*> pNetworkId
  <*> pMaxLovelaceSupply
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (shelleyEnableP2P shelleyDefaultTestnetOptions)
      )

optsShelley :: Parser ShelleyOptions
optsShelley = ShelleyOptions <$> optsTestnet


cmdShelley :: Mod CommandFields ShelleyOptions
cmdShelley = command' "shelley" "Start a Shelley testnet" optsShelley
