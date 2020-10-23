module Testnet.Commands.Byron
  ( ByronOptions(..)
  , cmdByron
  , runByronOptions
  ) where

import           Data.Function
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Byron
import           Testnet.Run (runTestnet)

data ByronOptions = ByronOptions

optsByron :: Parser ByronOptions
optsByron = pure ByronOptions

runByronOptions :: ByronOptions -> IO ()
runByronOptions ByronOptions = runTestnet Testnet.Byron.testnet

cmdByron :: Mod CommandFields (IO ())
cmdByron = command "byron"  $ flip info idm $ runByronOptions <$> optsByron
