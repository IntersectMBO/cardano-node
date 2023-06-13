module Parsers.Babbage
  ( BabbageOptions(..)
  , cmdBabbage
  ) where

import           Prelude

import           Options.Applicative
import qualified Options.Applicative as OA

import           Cardano.CLI.Common.Parsers hiding (pNetworkId)

import           Testnet.Process.Cli
import           Testnet.Runtime (readNodeLoggingFormat)
import           Testnet.Start.Babbage

newtype BabbageOptions = BabbageOptions
  { testnetOptions :: BabbageTestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser BabbageTestnetOptions
optsTestnet = BabbageTestnetOptions
  <$> OA.option auto
      (   OA.long "num-spo-nodes"
      <>  OA.help "Number of SPO nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (babbageNumSpoNodes babbageDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-duration"
      <>  OA.help "Slot duration"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (babbageSlotDuration babbageDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security parameter"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (babbageSecurityParam babbageDefaultTestnetOptions)
      )
  <*> pNetworkId
  <*> OA.option auto
      (   OA.long "total-balance"
      <>  OA.help "Total balance"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (babbageTotalBalance babbageDefaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (babbageNodeLoggingFormat babbageDefaultTestnetOptions)
      )

optsBabbage :: Parser BabbageOptions
optsBabbage = BabbageOptions <$> optsTestnet

cmdBabbage :: Mod CommandFields BabbageOptions
cmdBabbage = command' "babbage" "Start a babbage testnet " optsBabbage
