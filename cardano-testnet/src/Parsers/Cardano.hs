{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Cardano
  ( cmdCardano
  , cmdCreateEnv
  ) where

import           Cardano.Api (AnyShelleyBasedEra (..))
import           Cardano.Api.Pretty

import           Cardano.CLI.EraBased.Common.Option hiding (pNetworkId)
import           Cardano.Prelude (readMaybe)

import           Prelude

import           Control.Applicative (optional, (<|>))
import           Data.Default.Class (def)
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe
import           Data.Word (Word64)
import           Options.Applicative (CommandFields, Mod, Parser)
import qualified Options.Applicative as OA
import           Options.Applicative.Types (readerAsk)

import           Testnet.Defaults (defaultEra)
import           Testnet.Start.Cardano
import           Testnet.Start.Types
import           Testnet.Types (readNodeLoggingFormat)

optsTestnet :: Parser CardanoTestnetCliOptions
optsTestnet = CardanoTestnetCliOptions
  <$> pCardanoTestnetCliOptions
  <*> pGenesisOptions
  <*> pNodeEnvironment
  <*> pUpdateTimestamps

optsCreateTestnet :: Parser CardanoTestnetCreateEnvOptions
optsCreateTestnet = CardanoTestnetCreateEnvOptions
  <$> pCardanoTestnetCliOptions
  <*> pGenesisOptions
  <*> pEnvOutputDir
  <*> pCreateEnvOptions

-- We can't fill in the optional Genesis files at parse time, because we want to be in a monad
-- to properly parse JSON. We delegate this task to the caller.
pCreateEnvOptions :: Parser CreateEnvOptions
pCreateEnvOptions = CreateEnvOptions
  <$> pOnChainParams

pCardanoTestnetCliOptions :: Parser CardanoTestnetOptions
pCardanoTestnetCliOptions = CardanoTestnetOptions
  <$> pTestnetNodeOptions
  <*> pure (AnyShelleyBasedEra defaultEra)
  <*> pMaxLovelaceSupply
  <*> OA.option (OA.eitherReader readNodeLoggingFormat)
      (   OA.long "node-logging-format"
      <>  OA.help "Node logging format (json|text)"
      <>  OA.metavar "LOGGING_FORMAT"
      <>  OA.showDefaultWith prettyShow
      <>  OA.value (cardanoNodeLoggingFormat def)
      )
  <*> OA.option OA.auto
      (   OA.long "num-dreps"
      <>  OA.help "Number of delegate representatives (DReps) to generate. Ignored if a node environment is passed."
      <>  OA.metavar "NUMBER"
      <>  OA.showDefault
      <>  OA.value 3
      )
  <*> OA.switch
      (   OA.long "enable-new-epoch-state-logging"
      <>  OA.help "Enable new epoch state logging to logs/ledger-epoch-state.log"
      <>  OA.showDefault
      )
  <*> ( OA.flag' GenerateTemplateConfigForTxGenerator
          (   OA.long "generate-tx-generator-config"
          <>  OA.help "Generate a template configuration file for tx-generator."
          )
      <|> OA.flag' GenerateAndRunTxGenerator
          (   OA.long "run-tx-generator"
          <>  OA.help "Generate a configuration file for tx-generator and run it."
          )
      <|> pure NoTxGeneratorSupport
      )
  <*> (maybe NoUserProvidedEnv UserProvidedEnv <$> optional (OA.strOption
      (   OA.long "output-dir"
      <>  OA.help "Directory where to store files, sockets, and so on. It is created if it doesn't exist. If unset, a temporary directory is used."
      <>  OA.metavar "DIRECTORY"
      )))
  <*> OA.switch
      (   OA.long "enable-grpc"
      <>  OA.help "[EXPERIMENTAL] Enable gRPC endpoint on all of testnet nodes. The listening socket file will be the same directory as node's N2C socket."
      <>  OA.showDefault
      )

pTestnetNodeOptions :: Parser (NonEmpty NodeOption)
pTestnetNodeOptions =
  -- If `--num-pool-nodes N` is present, return N nodes with option `SpoNodeOptions []`.
  -- Otherwise, return `cardanoDefaultTestnetNodeOptions`
  fmap (maybe cardanoDefaultTestnetNodeOptions (\num -> defaultSpoOptions :| L.replicate (num - 1) defaultSpoOptions)) <$>
    optional $ OA.option ensureAtLeastOne
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes. Note this uses a default node configuration for all nodes."
      <>  OA.metavar "COUNT"
      )
  where
    defaultSpoOptions = SpoNodeOptions []

    ensureAtLeastOne :: OA.ReadM Int
    ensureAtLeastOne = readerAsk >>= \arg ->
      case readMaybe arg of
        Just n | n >= 1 -> pure n
        _ -> fail "Need at least one SPO node to produce blocks, but got none."

pNodeEnvironment :: Parser UserProvidedEnv
pNodeEnvironment = fmap (maybe NoUserProvidedEnv UserProvidedEnv) <$>
  optional $ OA.strOption
    (  OA.long "node-env"
    <> OA.metavar "FILEPATH"
    <> OA.help "Path to the node's environment (which is generated otherwise). You can generate a default environment with the 'create-env' command, then modify it and pass it with this argument."
    )

pOnChainParams :: Parser TestnetOnChainParams
pOnChainParams = fmap (fromMaybe DefaultParams) <$> optional $
  pCustomParamsFile <|> pMainnetParams

pCustomParamsFile :: Parser TestnetOnChainParams
pCustomParamsFile = OnChainParamsFile <$> OA.strOption
  (  OA.long "params-file"
  <> OA.help "File containing custom on-chain parameters in Blockfrost format:\nhttps://docs.blockfrost.io/#tag/cardano--epochs/GET/epochs/latest/parameters"
  <> OA.metavar "FILEPATH"
  )

pMainnetParams :: Parser TestnetOnChainParams
pMainnetParams = OA.flag' OnChainParamsMainnet
  (  OA.long "params-mainnet"
  <> OA.help "Use mainnet on-chain parameters"
  )

pUpdateTimestamps :: Parser UpdateTimestamps
pUpdateTimestamps = OA.flag DontUpdateTimestamps UpdateTimestamps
  (  OA.long "update-time"
  <> OA.help "Update the time stamps in genesis files to current date"
  <> OA.showDefault
  )

pEnvOutputDir :: Parser FilePath
pEnvOutputDir = OA.strOption
  (   OA.long "output"
  <>  OA.help "Directory where to create the sandbox environment."
  <>  OA.metavar "DIRECTORY"
  )

pGenesisOptions :: Parser GenesisOptions
pGenesisOptions =
  GenesisOptions
    <$> pNetworkId
    <*> pEpochLength
    <*> pSlotLength
    <*> pActiveSlotCoeffs
  where
    pEpochLength =
      OA.option OA.auto
        (   OA.long "epoch-length"
        <>  OA.help "Epoch length, in number of slots. Ignored if a node environment is passed."
        <>  OA.metavar "SLOTS"
        <>  OA.showDefault
        <>  OA.value (genesisEpochLength def)
        )
    pSlotLength =
      OA.option OA.auto
        (   OA.long "slot-length"
        <>  OA.help "Slot length. Ignored if a node environment is passed."
        <>  OA.metavar "SECONDS"
        <>  OA.showDefault
        <>  OA.value (genesisSlotLength def)
        )
    pActiveSlotCoeffs =
      OA.option OA.auto
        (   OA.long "active-slots-coeff"
        <>  OA.help "Active slots coefficient. Ignored if a node environment is passed."
        <>  OA.metavar "DOUBLE"
        <>  OA.showDefault
        <>  OA.value (genesisActiveSlotsCoeff def)
        )

cmdCardano :: Mod CommandFields CardanoTestnetCliOptions
cmdCardano = command' "cardano" "Start a testnet and keep it running until stopped" optsTestnet

cmdCreateEnv :: Mod CommandFields CardanoTestnetCreateEnvOptions
cmdCreateEnv = command' "create-env" "Create a sandbox for Cardano testnet" optsCreateTestnet

pNetworkId :: Parser Int
pNetworkId =
  OA.option (bounded "TESTNET_MAGIC") $ mconcat
    [ OA.long "testnet-magic"
    , OA.metavar "INT"
    , OA.help "Specify a testnet magic id."
    , OA.showDefault
    , OA.value defaultTestnetMagic
    ]

pMaxLovelaceSupply :: Parser Word64
pMaxLovelaceSupply =
  OA.option OA.auto
      (   OA.long "max-lovelace-supply"
      <>  OA.help "Max lovelace supply that your testnet starts with. Ignored if a node environment is passed."
      <>  OA.metavar "WORD64"
      <>  OA.showDefault
      <>  OA.value (cardanoMaxSupply def)
      )
