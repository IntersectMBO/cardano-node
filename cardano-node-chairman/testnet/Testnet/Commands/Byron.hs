module Testnet.Commands.Byron
  ( ByronOptions(..)
  , cmdByron
  , runByronOptions
  ) where

import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Byron
import           Testnet.Run (runTestnet)
import           Text.Show

import qualified Options.Applicative as OA

data ByronOptions = ByronOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsByron :: Parser ByronOptions
optsByron = ByronOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-bft-nodes"
      <>  OA.help "Number of BFT nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numBftNodes defaultTestnetOptions)
      )

runByronOptions :: ByronOptions -> IO ()
runByronOptions opts = runTestnet (maybeTestnetMagic opts) (Testnet.Byron.testnet (testnetOptions opts))

cmdByron :: Mod CommandFields (IO ())
cmdByron = command "byron" $ flip info idm $ runByronOptions <$> optsByron
