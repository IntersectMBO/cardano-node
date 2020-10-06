module Testnet.Commands.Shelley
  ( ShelleyOptions(..)
  , cmdShelley
  , runShelleyOptions
  ) where

import           Data.Function
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Run (runTestnet)
import           Testnet.Shelley

data ShelleyOptions = ShelleyOptions

optsShelley :: Parser ShelleyOptions
optsShelley = pure ShelleyOptions

runShelleyOptions :: ShelleyOptions -> IO ()
runShelleyOptions ShelleyOptions = runTestnet Testnet.Shelley.testnet

cmdShelley :: Mod CommandFields (IO ())
cmdShelley = command "shelley"  $ flip info idm $ runShelleyOptions <$> optsShelley
