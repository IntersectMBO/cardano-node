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

newtype ByronOptions = ByronOptions
  { testnetMagic :: Maybe Int
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

runByronOptions :: ByronOptions -> IO ()
runByronOptions (ByronOptions maybeTestnetMagic) = runTestnet maybeTestnetMagic Testnet.Byron.testnet

cmdByron :: Mod CommandFields (IO ())
cmdByron = command "byron"  $ flip info idm $ runByronOptions <$> optsByron
