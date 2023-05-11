module Parsers.Shelley
  ( ShelleyOptions(..)
  , cmdShelley
  , runShelleyOptions
  ) where

import           Prelude

import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.Common.Parsers

import           Testnet
import           Testnet.Run (runTestnet)
import           Testnet.Shelley
import           Testnet.Utils


data ShelleyOptions = ShelleyOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: ShelleyTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser ShelleyTestnetOptions
optsTestnet = ShelleyTestnetOptions
  <$> OA.option auto
      (   OA.long "num-praos-nodes"
      <>  OA.help "Number of PRAOS nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (shelleyNumPraosNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (shelleyNumPoolNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (shelleyActiveSlotsCoeff defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security param"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (shelleySecurityParam defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (shelleyEpochLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (shelleySlotLength defaultTestnetOptions)
      )
  <*> pMaxLovelaceSupply
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (shelleyEnableP2P defaultTestnetOptions)
      )

optsShelley :: Parser ShelleyOptions
optsShelley = ShelleyOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runShelleyOptions :: ShelleyOptions -> IO ()
runShelleyOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.testnet (ShelleyOnlyTestnetOptions $ testnetOptions options)

cmdShelley :: Mod CommandFields ShelleyOptions
cmdShelley = command' "shelley" "Start a Shelley testnet" optsShelley
