module Testnet.Commands.ByronShelley
  ( ByronShelleyOptions(..)
  , cmdByronShelley
  , runByronShelleyOptions
  ) where

import           Data.Function
import           Options.Applicative
import           System.IO (IO)
import           Testnet.ByronShelley
import           Testnet.Run (runTestnet)

data ByronShelleyOptions = ByronShelleyOptions

optsByronShelley :: Parser ByronShelleyOptions
optsByronShelley = pure ByronShelleyOptions

runByronShelleyOptions :: ByronShelleyOptions -> IO ()
runByronShelleyOptions ByronShelleyOptions = runTestnet Testnet.ByronShelley.testnet

cmdByronShelley :: Mod CommandFields (IO ())
cmdByronShelley = command "byron-shelley"  $ flip info idm $ runByronShelleyOptions <$> optsByronShelley
