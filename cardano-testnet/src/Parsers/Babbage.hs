module Parsers.Babbage
  ( BabbageOptions(..)
  , cmdBabbage
  , runBabbageOptions
  ) where

import           Prelude

import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.Common.Parsers hiding (pNetworkId)

import           Testnet
import           Testnet.Options
import           Testnet.Run (runTestnet)
import           Testnet.Util.Cli
import           Testnet.Util.Runtime (readNodeLoggingFormat)

data BabbageOptions = BabbageOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: BabbageTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser BabbageTestnetOptions
optsTestnet = BabbageTestnetOptions
  <$> OA.option auto
      (   OA.long "num-spo-nodes"
      <>  OA.help "Number of SPO nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (babbageNumSpoNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (babbageSlotDuration defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (babbageSecurityParam defaultTestnetOptions)
      )
  <*> pNetworkId
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (babbageTotalBalance defaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (babbageNodeLoggingFormat defaultTestnetOptions)
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
runBabbageOptions options =
  runTestnet $ Testnet.testnet (BabbageOnlyTestnetOptions $ testnetOptions options)

cmdBabbage :: Mod CommandFields BabbageOptions
cmdBabbage = command' "babbage" "Start a babbage testnet " optsBabbage
