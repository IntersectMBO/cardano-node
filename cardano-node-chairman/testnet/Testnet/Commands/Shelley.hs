module Testnet.Commands.Shelley
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
import           Testnet.Shelley
import           Text.Show

import qualified Options.Applicative as OA

data ShelleyOptions = ShelleyOptions
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
  Testnet.Shelley.testnet (testnetOptions options)

cmdShelley :: Mod CommandFields (IO ())
cmdShelley = command "shelley"  $ flip info idm $ runShelleyOptions <$> optsShelley
