{-# LANGUAGE TypeApplications #-}

module Testnet.Commands.Babbage
  ( BabbageOptions(..)
  , cmdBabbage
  , runBabbageOptions
  ) where

import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Babbage
import           Testnet.Run (runTestnet)
import           Text.Show

import qualified Options.Applicative as OA

data BabbageOptions = BabbageOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = pure TestnetOptions

optsBabbage :: Parser BabbageOptions
optsBabbage = BabbageOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runBabbageOptions :: BabbageOptions -> IO ()
runBabbageOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.Babbage.testnet (testnetOptions options)

cmdBabbage :: Mod CommandFields (IO ())
cmdBabbage = command "babbage"  $ flip info idm $ runBabbageOptions <$> optsBabbage
