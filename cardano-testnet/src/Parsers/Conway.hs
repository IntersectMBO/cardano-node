module Parsers.Conway
  ( ConwayOptions(..)
  , cmdConway
  ) where

import           Prelude

import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.Common.Parsers hiding (pNetworkId)

import           Testnet.Process.Cli
import           Testnet.Runtime (readNodeLoggingFormat)
import           Testnet.Start.Conway

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
      <>  OA.value (conwayNumSpoNodes conwayDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (conwaySlotDuration conwayDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (conwaySecurityParam conwayDefaultTestnetOptions)
      )
  <*> pNetworkId
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (conwayTotalBalance conwayDefaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (conwayNodeLoggingFormat conwayDefaultTestnetOptions)
      )

optsConway :: Parser ConwayOptions
optsConway = ConwayOptions <$> optsTestnet

cmdConway :: Mod CommandFields ConwayOptions
cmdConway = command' "conway" "Start a conway testnet " optsConway
