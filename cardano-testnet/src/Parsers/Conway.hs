module Parsers.Conway
  ( ConwayOptions(..)
  , cmdConway
  , runConwayOptions
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

newtype ConwayOptions = ConwayOptions
  { testnetOptions :: ConwayTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser ConwayTestnetOptions
optsTestnet = ConwayTestnetOptions
  <$> OA.option auto
      (   OA.long "num-spo-nodes"
      <>  OA.help "Number of SPO nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (conwayNumSpoNodes defaultConwayTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (conwaySlotDuration defaultConwayTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (conwaySecurityParam defaultConwayTestnetOptions)
      )
  <*> pNetworkId
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (conwayTotalBalance defaultConwayTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (conwayNodeLoggingFormat defaultConwayTestnetOptions)
      )

optsConway :: Parser ConwayOptions
optsConway = ConwayOptions <$> optsTestnet

runConwayOptions :: ConwayOptions -> IO ()
runConwayOptions options =
  runTestnet $ Testnet.testnet (ConwayOnlyTestnetOptions $ testnetOptions options)

cmdConway :: Mod CommandFields ConwayOptions
cmdConway = command' "conway" "Start a conway testnet " optsConway
