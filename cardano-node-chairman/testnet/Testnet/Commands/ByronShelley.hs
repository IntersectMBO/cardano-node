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
import           Text.Show

import qualified Options.Applicative as OA

data ByronShelleyOptions = ByronShelleyOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> optional
      ( OA.option auto
        (   long "active-slots-coeff"
        <>  help "Active slots co-efficient"
        <>  metavar "DOUBLE"
        )
      )
  <*> optional
      ( OA.option auto
        (   long "epoch-length"
        <>  help "Epoch length"
        <>  metavar "MILLISECONDS"
        )
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
