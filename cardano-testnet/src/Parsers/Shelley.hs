module Parsers.Shelley
  ( ShelleyOptions(..)
  , cmdShelley
  , runShelleyOptions
  ) where


import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Run (runTestnet)
import           Testnet
import           Testnet.Shelley
import           Text.Show

import qualified Options.Applicative as OA

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
  <*> OA.option auto
      (   OA.long "max-lovelace-supply"
      <>  OA.help "Max lovelace supply"
      <>  OA.metavar "INTEGER"
      <>  OA.showDefault
      <>  OA.value (shelleyMaxLovelaceSupply defaultTestnetOptions)
      )
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

cmdShelley :: Mod CommandFields (IO ())
cmdShelley = command "shelley"  $ flip info idm $ runShelleyOptions <$> optsShelley
