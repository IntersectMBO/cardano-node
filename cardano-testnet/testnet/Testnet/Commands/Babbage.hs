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
import           Test.Runtime (readNodeLoggingFormat)
import           Testnet.Babbage
import           Testnet.Run (runTestnet)
import           Text.Show

import qualified Options.Applicative as OA

data BabbageOptions = BabbageOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-spo-nodes"
      <>  OA.help "Number of SPO nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numSpoNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (slotDuration defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (securityParam defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (totalBalance defaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (nodeLoggingFormat defaultTestnetOptions)
      )

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
