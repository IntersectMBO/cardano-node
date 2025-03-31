{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Cardano
  ( cmdCardano
  ) where

import           Cardano.Api (AnyShelleyBasedEra (AnyShelleyBasedEra), EraInEon (..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Common.Option hiding (pNetworkId)

import           Prelude

import           Control.Applicative
import           Data.Default.Class
import           Data.Functor
import qualified Data.List as L
import           Data.Word (Word64)
import           Options.Applicative
import qualified Options.Applicative as OA

import           Testnet.Start.Cardano
import           Testnet.Start.Types
import           Testnet.Types (readNodeLoggingFormat)

{- HLINT ignore "Use asum" -}

optsTestnet :: EnvCli -> Parser CardanoTestnetCliOptions
optsTestnet envCli = CardanoTestnetCliOptions
  <$> pCardanoTestnetCliOptions envCli
  <*> pGenesisOptions

pCardanoTestnetCliOptions :: EnvCli -> Parser CardanoTestnetOptions
pCardanoTestnetCliOptions envCli = CardanoTestnetOptions
  <$> pTestnetNodeOptions
  <*> pAnyShelleyBasedEra'
  <*> pMaxLovelaceSupply
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (cardanoNodeLoggingFormat def)
      )
  <*> OA.option auto
      (   OA.long "num-dreps"
      <>  OA.help "Number of delegate representatives (DReps) to generate. Ignored if a custom Conway genesis file is passed."
      <>  OA.metavar "NUMBER"
      <>  OA.showDefault
      <>  OA.value 3
      )
  <*> OA.flag False True
      (   OA.long "enable-new-epoch-state-logging"
      <>  OA.help "Enable new epoch state logging to logs/ledger-epoch-state.log"
      <>  OA.showDefault
      )
  <*> OA.flag GenerateAndRun OnlyGenerate
      (   OA.long "only-generate-config-files"
      <>  OA.help "Do not actually run anything, only generate config files and output them in 'output-dir'"
      <>  OA.showDefault
      )
  <*> optional (OA.strOption
      (   OA.long "output-dir"
      <>  OA.help "Directory where to store files, sockets, and so on. It is created if it doesn't exist. If unset, a temporary directory is used."
      <>  OA.metavar "DIRECTORY"
      ))
  where
    pAnyShelleyBasedEra' :: Parser AnyShelleyBasedEra
    pAnyShelleyBasedEra' =
      pAnyShelleyBasedEra envCli <&> (\(EraInEon x) -> AnyShelleyBasedEra x)

pTestnetNodeOptions :: Parser TestnetNodeOptions
pTestnetNodeOptions =
  asum' [
      AutomaticNodeOptions . (`L.replicate` defaultSpoOptions) <$>
        OA.option auto
        (   OA.long "num-pool-nodes"
        <>  OA.help "Number of pool nodes. Note this uses a default node configuration for all nodes."
        <>  OA.metavar "COUNT"
        <>  OA.showDefault
        <>  OA.value 1)
    , UserProvidedNodeOptions
        <$> strOption ( long "node-config"
                        <> metavar "FILEPATH"
                        <> help "Path to the node's configuration file (which is generated otherwise). If you use this option, you should also pass all the genesis files (files pointed to by the fields \"AlonzoGenesisFile\", \"ShelleyGenesisFile\", etc.).")
    ]
  where
    defaultSpoOptions = SpoNodeOptions []
    -- \| Because asum is not available GHC 8.10.7's base (4.14.3.0). This can be removed
    -- when oldest version of GHC we use is >= 9.0 (base >= 4.15)
    asum' :: (Foldable t, Alternative f) => t (f a) -> f a
    asum' = foldr (<|>) empty

pGenesisOptions :: Parser GenesisOptions
pGenesisOptions =
  GenesisOptions
    <$> pNetworkId
    <*> pEpochLength
    <*> pSlotLength
    <*> pActiveSlotCoeffs
  where
    pEpochLength =
      OA.option auto
        (   OA.long "epoch-length"
        -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
        <>  OA.help "Epoch length, in number of slots. Ignored if a custom Shelley genesis file is passed."
        <>  OA.metavar "SLOTS"
        <>  OA.showDefault
        <>  OA.value (genesisEpochLength def)
        )
    pSlotLength =
      OA.option auto
        (   OA.long "slot-length"
        -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
        <>  OA.help "Slot length. Ignored if a custom Shelley genesis file is passed."
        <>  OA.metavar "SECONDS"
        <>  OA.showDefault
        <>  OA.value (genesisSlotLength def)
        )
    pActiveSlotCoeffs =
      OA.option auto
        (   OA.long "active-slots-coeff"
        -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
        <>  OA.help "Active slots coefficient. Ignored if a custom Shelley genesis file is passed."
        <>  OA.metavar "DOUBLE"
        <>  OA.showDefault
        <>  OA.value (genesisActiveSlotsCoeff def)
        )

cmdCardano :: EnvCli -> Mod CommandFields CardanoTestnetCliOptions
cmdCardano envCli = command' "cardano" "Start a testnet in any era" (optsTestnet envCli)

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
      -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
      <>  help "Max lovelace supply that your testnet starts with. Ignored if a custom Shelley genesis file is passed."
      <>  metavar "WORD64"
      <>  showDefault
      <>  value (cardanoMaxSupply def)
      )
