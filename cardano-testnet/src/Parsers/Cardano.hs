{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Cardano
  ( cmdCardano
  , cmdCreateEnv
  ) where

import           Cardano.Api ( AnyShelleyBasedEra (AnyShelleyBasedEra), EraInEon (..), Eon(..)
                             , forEraInEonMaybe, convert, ShelleyBasedEra(..), AnyCardanoEra(..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Common.Option hiding (pNetworkId)

import           Prelude

import           Control.Applicative
import           Data.Default.Class
import           Data.Functor
import qualified Data.List as L
import           Data.Maybe
import           Data.Typeable
import           Data.Word (Word64)
import           Options.Applicative (CommandFields, Mod, Parser)
import qualified Options.Applicative as OA

import           Testnet.Start.Cardano
import           Testnet.Start.Types
import           Testnet.Types (readNodeLoggingFormat)

optsTestnet :: EnvCli -> Parser CardanoTestnetCliOptions
optsTestnet envCli = CardanoTestnetCliOptions
  <$> pCardanoTestnetCliOptions envCli
  <*> pGenesisOptions
  <*> pNodeEnvironment
  <*> pUpdateTimestamps

optsCreateTestnet :: EnvCli -> Parser CardanoTestnetCreateEnvOptions
optsCreateTestnet envCli = CardanoTestnetCreateEnvOptions
  <$> pCardanoTestnetCliOptions envCli
  <*> pGenesisOptions
  <*> pEnvOutputDir
  <*> pCreateEnvOptions

-- We can't fill in the optional Genesis files at parse time, because we want to be in a monad
-- to properly parse JSON. We delegate this task to the caller.
pCreateEnvOptions :: Parser CreateEnvOptions
pCreateEnvOptions = CreateEnvOptions
  <$> pOnChainParams
  <*> pTopologyType

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
  <*> OA.option OA.auto
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
  <*> (maybe NoUserProvidedEnv UserProvidedEnv <$> optional (OA.strOption
      (   OA.long "output-dir"
      <>  OA.help "Directory where to store files, sockets, and so on. It is created if it doesn't exist. If unset, a temporary directory is used."
      <>  OA.metavar "DIRECTORY"
      )))
  where
    pAnyShelleyBasedEra' :: Parser AnyShelleyBasedEra
    pAnyShelleyBasedEra' =
      pAnyShelleyBasedEra envCli <&> (\(EraInEon x) -> AnyShelleyBasedEra x)

pAnyShelleyBasedEra :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pAnyShelleyBasedEra envCli =
  asum $
    mconcat
      [
        [ OA.flag' (EraInEon ShelleyBasedEraShelley) $
            mconcat [OA.long "shelley-era", OA.help $ "Specify the Shelley era" <> deprecationText]
        , OA.flag' (EraInEon ShelleyBasedEraAllegra) $
            mconcat [OA.long "allegra-era", OA.help $ "Specify the Allegra era" <> deprecationText]
        , OA.flag' (EraInEon ShelleyBasedEraMary) $
            mconcat [OA.long "mary-era", OA.help $ "Specify the Mary era" <> deprecationText]
        , OA.flag' (EraInEon ShelleyBasedEraAlonzo) $
            mconcat [OA.long "alonzo-era", OA.help $ "Specify the Alonzo era" <> deprecationText]
        , OA.flag' (EraInEon ShelleyBasedEraBabbage) $
            mconcat [OA.long "babbage-era", OA.help $ "Specify the Babbage era (default)" <> deprecationText]
        , fmap (EraInEon . convert) $ pConwayEra envCli
        ]
      , maybeToList $ pure <$> envCliAnyEon envCli
      , pure $ pure $ EraInEon ShelleyBasedEraConway
      ]
 where
  deprecationText :: String
  deprecationText = " - DEPRECATED - will be removed in the future"

  envCliAnyEon :: Typeable eon => Eon eon => EnvCli -> Maybe (EraInEon eon)
  envCliAnyEon envCli' = do
    AnyCardanoEra era <- envCliAnyCardanoEra envCli'
    forEraInEonMaybe era EraInEon

pTestnetNodeOptions :: Parser [NodeOption]
pTestnetNodeOptions =
  -- If `--num-pool-nodes N` is present, return N nodes with option `SpoNodeOptions []`.
  -- Otherwise, return `cardanoDefaultTestnetNodeOptions`
  fmap (maybe cardanoDefaultTestnetNodeOptions (`L.replicate` defaultSpoOptions)) <$>
    optional $ OA.option OA.auto
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes. Note this uses a default node configuration for all nodes."
      <>  OA.metavar "COUNT"
      )
  where
    defaultSpoOptions = SpoNodeOptions []

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

pTopologyType :: Parser TopologyType
pTopologyType = OA.flag DirectTopology P2PTopology
  (  OA.long "p2p-topology"
  <> OA.help "Use P2P topology files instead of \"direct\" topology files"
  <> OA.showDefault
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
        -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
        <>  OA.help "Epoch length, in number of slots. Ignored if a custom Shelley genesis file is passed."
        <>  OA.metavar "SLOTS"
        <>  OA.showDefault
        <>  OA.value (genesisEpochLength def)
        )
    pSlotLength =
      OA.option OA.auto
        (   OA.long "slot-length"
        -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
        <>  OA.help "Slot length. Ignored if a custom Shelley genesis file is passed."
        <>  OA.metavar "SECONDS"
        <>  OA.showDefault
        <>  OA.value (genesisSlotLength def)
        )
    pActiveSlotCoeffs =
      OA.option OA.auto
        (   OA.long "active-slots-coeff"
        -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
        <>  OA.help "Active slots coefficient. Ignored if a custom Shelley genesis file is passed."
        <>  OA.metavar "DOUBLE"
        <>  OA.showDefault
        <>  OA.value (genesisActiveSlotsCoeff def)
        )

cmdCardano :: EnvCli -> Mod CommandFields CardanoTestnetCliOptions
cmdCardano envCli = command' "cardano" "Start a testnet in any era" (optsTestnet envCli)

cmdCreateEnv :: EnvCli -> Mod CommandFields CardanoTestnetCreateEnvOptions
cmdCreateEnv envCli = command' "create-env" "Create a sandbox for Cardano testnet" (optsCreateTestnet envCli)

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
      -- TODO Check that this flag is not used when a custom Shelley genesis file is passed
      <>  OA.help "Max lovelace supply that your testnet starts with. Ignored if a custom Shelley genesis file is passed."
      <>  OA.metavar "WORD64"
      <>  OA.showDefault
      <>  OA.value (cardanoMaxSupply def)
      )
