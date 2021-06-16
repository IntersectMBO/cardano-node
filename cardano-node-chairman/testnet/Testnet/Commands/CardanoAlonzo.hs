module Testnet.Commands.CardanoAlonzo
  ( CardanoAlonzoOptions(..)
  , cmdCardanoAlonzo
  , runCardanoAlonzoOptions
  ) where

import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.CardanoAlonzo
import           Testnet.Run (runTestnet)
import           Text.Read (readEither)
import           Text.Show

import qualified Options.Applicative as OA

data CardanoAlonzoOptions = CardanoAlonzoOptions
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

optsCardanoAlonzo :: Parser CardanoAlonzoOptions
optsCardanoAlonzo = CardanoAlonzoOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runCardanoAlonzoOptions :: CardanoAlonzoOptions -> IO ()
runCardanoAlonzoOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.CardanoAlonzo.testnet (testnetOptions options)

cmdCardanoAlonzo :: Mod CommandFields (IO ())
cmdCardanoAlonzo = command "cardano-alonzo"  $ flip info idm $ runCardanoAlonzoOptions <$> optsCardanoAlonzo
