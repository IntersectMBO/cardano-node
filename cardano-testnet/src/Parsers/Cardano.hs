module Parsers.Cardano
  ( cmdCardano
  ) where

import           Cardano.Api (EraInEon (..), bounded, AnyShelleyBasedEra (AnyShelleyBasedEra))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common hiding (pNetworkId)

import           Prelude

import           Data.Default.Class
import           Data.Functor
import qualified Data.List as L
import           Data.Word (Word64)
import           Options.Applicative
import qualified Options.Applicative as OA

import           Testnet.Start.Cardano
import           Testnet.Start.Types
import           Testnet.Types (readNodeLoggingFormat)


optsTestnet :: EnvCli -> Parser CardanoTestnetCliOptions
optsTestnet envCli = CardanoTestnetCliOptions
  <$> pCardanoTestnetCliOptions envCli
  <*> pGenesisOptions

pCardanoTestnetCliOptions :: EnvCli -> Parser CardanoTestnetOptions
pCardanoTestnetCliOptions envCli = CardanoTestnetOptions
  <$> pNumSpoNodes
  <*> pAnyShelleyBasedEra'
  <*> pMaxLovelaceSupply
  <*> OA.option auto
      (   OA.long "enable-p2p"
      <>  OA.help "Enable P2P"
      <>  OA.metavar "BOOL"
      <>  OA.showDefault
      <>  OA.value (cardanoEnableP2P def)
      )
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "nodeLoggingFormat"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefault
      <>  OA.value (cardanoNodeLoggingFormat def)
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
    pAnyShelleyBasedEra' :: Parser AnyShelleyBasedEra
    pAnyShelleyBasedEra' =
      pAnyShelleyBasedEra envCli <&> (\(EraInEon x) -> AnyShelleyBasedEra x)

pNumSpoNodes :: Parser [TestnetNodeOptions]
pNumSpoNodes =
  OA.option
     ((`L.replicate` SpoTestnetNodeOptions Nothing []) <$> auto)
     (   OA.long "num-pool-nodes"
     <>  OA.help "Number of pool nodes. Note this uses a default node configuration for all nodes."
     <>  OA.metavar "COUNT"
     <>  OA.showDefault
     <>  OA.value (cardanoNodes def)
     )

_pSpo :: Parser TestnetNodeOptions
_pSpo =
  SpoTestnetNodeOptions . Just
    <$> parseNodeConfigFile
    <*> pure [] -- TODO: Consider adding support for extra args

parseNodeConfigFile :: Parser NodeConfigurationYaml
parseNodeConfigFile = NodeConfigurationYaml <$>
  strOption
    (mconcat
       [ long "configuration-file"
       , metavar "NODE-CONFIGURATION"
       , help helpText
       , completer (bashCompleter "file")
       ]
    )
 where
   helpText = unwords
               [ "Configuration file for the cardano-node(s)."
               , "Specify a configuration file per node you want to have in the cluster."
               , "Or use num-pool-nodes to use cardano-testnet's default configuration."
               ]

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
        <>  OA.help "Epoch length, in number of slots"
        <>  OA.metavar "SLOTS"
        <>  OA.showDefault
        <>  OA.value (genesisEpochLength def)
        )
    pSlotLength =
      OA.option auto
        (   OA.long "slot-length"
        <>  OA.help "Slot length"
        <>  OA.metavar "SECONDS"
        <>  OA.showDefault
        <>  OA.value (genesisSlotLength def)
        )
    pActiveSlotCoeffs =
      OA.option auto
        (   OA.long "active-slots-coeff"
        <>  OA.help "Active slots co-efficient"
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
      <>  help "Max lovelace supply that your testnet starts with."
      <>  metavar "WORD64"
      <>  showDefault
      <>  value (cardanoMaxSupply def)
      )

