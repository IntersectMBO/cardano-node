{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Parsers.Cardano
  ( cmdCardano
  , cmdNodes
  ) where

import           Cardano.Api (AnyCardanoEra (..), EraInEon (..), ToCardanoEra (..), bounded)

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common hiding (pNetworkId)

import           Prelude

import           Data.Functor
import qualified Data.List as L
import           Data.Word (Word64)
import           Options.Applicative
import qualified Options.Applicative as OA

import           Testnet.Start.Cardano
import           Testnet.Types (readNodeLoggingFormat)


optsTestnet :: EnvCli -> Parser CardanoTestnetOptions
optsTestnet = pCardanoTestnetOptions pNumSpoNodes

optsNodes :: EnvCli -> Parser CardanoTestnetOptions
optsNodes = pCardanoTestnetOptions pPerNodeOptions

pCardanoTestnetOptions :: Parser [TestnetNodeOptions] -> EnvCli -> Parser CardanoTestnetOptions
pCardanoTestnetOptions pTestnetNodeOptions envCli = CardanoTestnetOptions
  <$> pTestnetNodeOptions
  <*> pCardanoEra
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length, in number of slots"
      <>  OA.metavar "SLOTS"
      <>  OA.showDefault
      <>  OA.value (cardanoEpochLength cardanoDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "SECONDS"
      <>  OA.showDefault
      <>  OA.value (cardanoSlotLength cardanoDefaultTestnetOptions)
      )
  <*> pNetworkId
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (cardanoActiveSlotsCoeff cardanoDefaultTestnetOptions)
      )
  <*> pMaxLovelaceSupply
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (cardanoEnableP2P cardanoDefaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (cardanoNodeLoggingFormat cardanoDefaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "num-dreps"
      <>  OA.help "Number of delegate representatives (DReps) to generate"
      <>  OA.metavar "NUMBER"
      <>  OA.showDefault
      <>  OA.value 3
      )
  <*> OA.flag False True
      (   OA.long "enable-new-epoch-state-logging"
      <>  OA.help "Enable new epoch state logging to logs/ledger-epoch-state.log"
      <>  OA.showDefault
      )
  where
    -- TODO replace era in 'CardanoTestnetOptions' 'AnyShelleyBasedEra' - we're not supporting
    -- byron testnets
    pCardanoEra :: Parser AnyCardanoEra
    pCardanoEra =
      pAnyShelleyBasedEra envCli <&>
        \case EraInEon eon -> AnyCardanoEra $ toCardanoEra eon

pNumSpoNodes :: Parser [TestnetNodeOptions]
pNumSpoNodes =
  OA.option
     ((`L.replicate` SpoTestnetNodeOptions []) <$> auto)
     (   OA.long "num-pool-nodes"
     <>  OA.help "Number of pool nodes. Note this uses a default node configuration for all nodes."
     <>  OA.metavar "COUNT"
     <>  OA.showDefault
     <>  OA.value (cardanoNodes cardanoDefaultTestnetOptions)
     )

pPerNodeOptions :: Parser [TestnetNodeOptions]
pPerNodeOptions =
  many pTestnetNodeOptions
  where
    pTestnetNodeOptions :: Parser TestnetNodeOptions
    pTestnetNodeOptions =
      PerNodeOption <$>
        OA.strOption
          (OA.long "single-node-options" <> OA.metavar "FILEPATH")

cmdCardano :: EnvCli -> Mod CommandFields CardanoTestnetOptions
cmdCardano envCli = command' "cardano" "Start a testnet in any era" (optsTestnet envCli)

cmdNodes :: EnvCli -> Mod CommandFields CardanoTestnetOptions
cmdNodes envCli = command' "nodes" "Start a testnet in any era, with node-specific options" (optsNodes envCli)

pNetworkId :: Parser Int
pNetworkId =
  OA.option (bounded "TESTNET_MAGIC") $ mconcat
    [ OA.long "testnet-magic"
    , OA.metavar "INT"
    , OA.help "Specify a testnet magic id."
    ]

pMaxLovelaceSupply :: Parser Word64
pMaxLovelaceSupply =
  option auto
      (   long "max-lovelace-supply"
      <>  help "Max lovelace supply that your testnet starts with."
      <>  metavar "WORD64"
      <>  showDefault
      <>  value 10_020_000_000
      )

