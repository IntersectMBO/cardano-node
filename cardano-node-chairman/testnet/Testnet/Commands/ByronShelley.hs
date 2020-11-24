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

newtype ByronShelleyOptions = ByronShelleyOptions
  { testnetMagic :: Maybe Int
  } deriving (Eq, Show)

optsByronShelley :: Parser ByronShelleyOptions
optsByronShelley = ByronShelleyOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )

runByronShelleyOptions :: ByronShelleyOptions -> IO ()
runByronShelleyOptions (ByronShelleyOptions maybeTestnetMagic) = runTestnet maybeTestnetMagic Testnet.ByronShelley.testnet

cmdByronShelley :: Mod CommandFields (IO ())
cmdByronShelley = command "byron-shelley"  $ flip info idm $ runByronShelleyOptions <$> optsByronShelley
