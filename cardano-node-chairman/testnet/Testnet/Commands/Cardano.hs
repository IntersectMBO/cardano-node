{-# LANGUAGE TypeApplications #-}

module Testnet.Commands.Cardano
  ( CardanoOptions(..)
  , cmdCardano
  , runCardanoOptions
  ) where

import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           GHC.Enum
import           Options.Applicative
import qualified Options.Applicative as OA
import           System.IO (IO)
import           Text.Read
import           Text.Show

import           Testnet.Cardano
import           Testnet.Run (runTestnet)

data CardanoOptions = CardanoOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: CardanoTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser CardanoTestnetOptions
optsTestnet = CardanoTestnetOptions
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
  <*> OA.option (OA.eitherReader readEither)
      (   OA.long "era"
      <>  OA.help ("Era to upgrade to.  " <> show @[Era] [minBound .. maxBound])
      <>  OA.metavar "ERA"
      <>  OA.showDefault
      <>  OA.value (era defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (epochLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "SECONDS"
      <>  OA.showDefault
      <>  OA.value (slotLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (activeSlotsCoeff defaultTestnetOptions)
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
runCardanoOptions options =
  runTestnet $ Testnet.Cardano.testnet (testnetOptions options)

cmdCardano :: Mod CommandFields (IO ())
cmdCardano = command "cardano"  $ flip info idm $ runCardanoOptions <$> optsCardano
