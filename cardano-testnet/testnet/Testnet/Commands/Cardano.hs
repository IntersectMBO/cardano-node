{-# LANGUAGE TypeApplications #-}

module Testnet.Commands.Cardano
  ( CardanoOptions(..)
  , cmdCardano
  , runCardanoOptions
  ) where

import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.List (replicate)
import           Data.Maybe
import           Data.Semigroup
import           GHC.Enum
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Cardano
import           Testnet.Run (runTestnet)
import           Text.Read
import           Text.Show

import qualified Options.Applicative as OA

data CardanoOptions = CardanoOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option
      ((`replicate` defaultTestnetNodeOptions) <$> auto)
      (   OA.long "num-bft-nodes"
      <>  OA.help "Number of BFT nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (bftNodeOptions defaultTestnetOptions)
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
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (enableP2P defaultTestnetOptions)
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
  Testnet.Cardano.testnet (testnetOptions options)

cmdCardano :: Mod CommandFields (IO ())
cmdCardano = command "cardano"  $ flip info idm $ runCardanoOptions <$> optsCardano
