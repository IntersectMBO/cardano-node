module Testnet.Commands.ByronShelley
  ( ByronShelleyOptions(..)
  , cmdByronShelley
  , runByronShelleyOptions
  ) where

import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.ByronShelley
import           Testnet.Run (runTestnet)
import           Text.Read (readEither)
import           Text.Show

import qualified Options.Applicative as OA

data ByronShelleyOptions = ByronShelleyOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-bft-nodes"
      <>  OA.help "Number of BFT nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numBftNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numPoolNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (activeSlotsCoeff defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (epochLength defaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readEither)
      (   OA.long "fork-point"
      <>  OA.help "Fork Point.  Valid values are: 'AtVersion <n>' and 'AtEpoch <n>'"
      <>  OA.metavar "FORKPOINT"
      <>  OA.showDefault
      <>  OA.value (forkPoint defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "initial supply"
      <>  OA.help "Initial supply"
      <>  OA.metavar "LOVELACE"
      <>  OA.showDefault
      <>  OA.value (initialSupply defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "max-supply"
      <>  OA.help "Max supply"
      <>  OA.metavar "LOVELACE"
      <>  OA.showDefault
      <>  OA.value (maxSupply defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (securityParam defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "n-poor-addresses"
      <>  OA.help "N poor addresses"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (nPoorAddresses defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "SECONDS"
      <>  OA.showDefault
      <>  OA.value (slotLength defaultTestnetOptions)
      )

optsByronShelley :: Parser ByronShelleyOptions
optsByronShelley = ByronShelleyOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runByronShelleyOptions :: ByronShelleyOptions -> IO ()
runByronShelleyOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.ByronShelley.testnet (testnetOptions options)

cmdByronShelley :: Mod CommandFields (IO ())
cmdByronShelley = command "byron-shelley"  $ flip info idm $ runByronShelleyOptions <$> optsByronShelley
