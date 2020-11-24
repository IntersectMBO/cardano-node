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

newtype ShelleyOptions = ShelleyOptions
  { testnetMagic :: Maybe Int
  } deriving (Eq, Show)

optsShelley :: Parser ShelleyOptions
optsShelley = ShelleyOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )

runShelleyOptions :: ShelleyOptions -> IO ()
runShelleyOptions (ShelleyOptions maybeTestnetMagic) = runTestnet maybeTestnetMagic Testnet.Shelley.testnet

cmdShelley :: Mod CommandFields (IO ())
cmdShelley = command "shelley"  $ flip info idm $ runShelleyOptions <$> optsShelley
