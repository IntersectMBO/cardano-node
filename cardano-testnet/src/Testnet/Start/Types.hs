{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetCliOptions(..)
  , CardanoTestnetCreateEnvOptions (..)
  , CardanoTestnetOptions(..)
  , InputNodeConfigFile(..)
  , NodeId(..)
  , NumDReps(..)
  , NumPools(..)
  , NumRelays(..)
  , TxGeneratorSupport(..)
  , cardanoNumPools
  , cardanoNumRelays

  , anyEraToString
  , anyShelleyBasedEraToString
  , defaultTestnetMagic
  , eraToString

  , CreateEnvOptions(..)
  , UpdateTimestamps(..)
  , TestnetOnChainParams(..)
  , mainnetParamsRequest
  , NodeOption(..)
  , isRelayNodeOptions
  , cardanoDefaultTestnetNodeOptions
  , GenesisOptions(..)
  , UserProvidedData(..)
  , UserProvidedEnv(..)
  , UserProvidedGeneses(..)

  , NodeLoggingFormat(..)
  , Conf(..)
  , GenesisHashesPolicy (..)
  , NodeConfiguration
  , NodeConfigurationYaml
  , mkConf
  , mkConfigAbs
  ) where

import           Cardano.Api hiding (cardanoEra)

import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis)

import           Prelude

import           Control.Exception (throw)
import           Control.Monad (unless)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (parseFail)
import           Data.Char (toLower)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as Text
import           Data.Word
import           GHC.Stack
import qualified Network.HTTP.Simple as HTTP
import           System.Directory (createDirectory, doesDirectoryExist, makeAbsolute)
import           System.FilePath (addTrailingPathSeparator)

import           Testnet.Filepath

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H

-- | The default value for the --testnet-magic option for `cardano-testnet`
defaultTestnetMagic :: Int
defaultTestnetMagic = 42

-- | Command line options for the @cardano-testnet@ executable. They are used
-- in the parser, and then get split into 'CardanoTestnetOptions' and
-- 'GenesisOptions'. If 'cliNodeEnvironment' is provided, don't create a
-- sandbox environment, use the one at the given path.
data CardanoTestnetCliOptions = CardanoTestnetCliOptions
  { cliTestnetOptions :: CardanoTestnetOptions
  , cliGenesisOptions :: GenesisOptions
  , cliNodeEnvironment :: UserProvidedEnv
  , cliUpdateTimestamps :: UpdateTimestamps
  } deriving (Eq, Show)

instance Default CardanoTestnetCliOptions where
  def = CardanoTestnetCliOptions
    { cliTestnetOptions = def
    , cliGenesisOptions = def
    , cliNodeEnvironment = def
    , cliUpdateTimestamps = def
    }

data UserProvidedEnv
  = NoUserProvidedEnv
  | UserProvidedEnv FilePath
  deriving (Eq, Show)

instance Default UserProvidedEnv where
  def = NoUserProvidedEnv

data UpdateTimestamps = UpdateTimestamps | DontUpdateTimestamps
  deriving (Eq, Show)

instance Default UpdateTimestamps where
  def = DontUpdateTimestamps

newtype CreateEnvOptions = CreateEnvOptions
  { ceoOnChainParams :: TestnetOnChainParams
  } deriving (Eq, Show)

instance Default CreateEnvOptions where
  def = CreateEnvOptions { ceoOnChainParams = def }

data TestnetOnChainParams
  = DefaultParams
  | OnChainParamsFile FilePath
  -- ^ A file path to a JSON file containing on-chain params, formatted as:
  -- https://docs.blockfrost.io/#tag/cardano--epochs/GET/epochs/latest
  | OnChainParamsMainnet
  deriving (Eq, Show)

instance Default TestnetOnChainParams where
  def = DefaultParams

data UserProvidedGeneses = UserProvidedGeneses
  { upgShelleyGenesis :: UserProvidedData ShelleyGenesis
  , upgAlonzoGenesis :: UserProvidedData AlonzoGenesis
  , upgConwayGenesis :: UserProvidedData ConwayGenesis
  } deriving (Eq, Show)

instance Default UserProvidedGeneses where
  def = UserProvidedGeneses
    def
    def
    def

-- | An HTTP request to get a file containing up-to-date mainnet on-chain parameters.
-- The file should be formatted with Blockfrost format:
-- https://docs.blockfrost.io/#tag/cardano--epochs/GET/epochs/latest/parameters
mainnetParamsRequest :: HTTP.Request
mainnetParamsRequest = either throw id $ HTTP.parseRequest
  "https://raw.githubusercontent.com/input-output-hk/cardano-parameters/refs/heads/main/mainnet/parameters.json"

-- | An abstract node id, used as placeholder in topology files
-- when the actual ports/addresses aren't known yet (i.e. before runtime)
newtype NodeId = NodeId Int
  deriving (Eq, Ord, Show)

instance ToJSON NodeId where
  toJSON (NodeId i) = Aeson.String $ Text.pack $ "node_" ++ show i

instance FromJSON NodeId where
  parseJSON = Aeson.withText "NodeId" $ \t -> case Text.breakOn "_" t of
    ("node", textId) -> case Aeson.eitherDecodeStrictText (Text.drop 1 textId) of
      Right i -> pure $ NodeId i
      Left _ -> parseFail $ "Incorrect format for NodeId: " ++ show t
    _ -> parseFail $ "Incorrect format for NodeId: " ++ show t

data CardanoTestnetCreateEnvOptions = CardanoTestnetCreateEnvOptions
  { createEnvTestnetOptions :: CardanoTestnetOptions
  , createEnvGenesisOptions :: GenesisOptions
  , createEnvOutputDir :: FilePath
  , createEnvCreateEnvOptions :: CreateEnvOptions
  } deriving (Eq, Show)

data TxGeneratorSupport
  = NoTxGeneratorSupport
  | GenerateTemplateConfigForTxGenerator
  deriving (Eq, Show)

-- | Options which, contrary to 'GenesisOptions' are not implemented
-- by tuning the genesis files.
data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | Options controlling how many nodes to create and of which type.
    cardanoNodes :: NonEmpty NodeOption
  , cardanoNodeEra :: AnyShelleyBasedEra -- ^ The era to start at
  , cardanoMaxSupply :: Word64 -- ^ The amount of Lovelace you are starting your testnet with (forwarded to shelley genesis)
                               -- TODO move me to GenesisOptions when https://github.com/IntersectMBO/cardano-cli/pull/874 makes it to cardano-node
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  , cardanoNumDReps :: NumDReps -- ^ The number of DReps to generate at creation
  , cardanoEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  , txGeneratorSupport :: TxGeneratorSupport -- ^ Options regarding support for the tx-generator on the testnet (config generation, execution, etc.)
  , cardanoOutputDir :: UserProvidedEnv -- ^ The output directory where to store files, sockets, and so on. If unset, a temporary directory is used.
  } deriving (Eq, Show)

-- | Path to the configuration file of the node, specified by the user
newtype InputNodeConfigFile = InputNodeConfigFile FilePath
  deriving (Eq, Show)

cardanoNumPools :: CardanoTestnetOptions -> NumPools
cardanoNumPools CardanoTestnetOptions{cardanoNodes} =
  NumPools $ length $ NEL.filter isSpoNodeOptions cardanoNodes

cardanoNumRelays :: CardanoTestnetOptions -> NumRelays
cardanoNumRelays CardanoTestnetOptions{cardanoNodes} =
  NumRelays $ length $ NEL.filter isRelayNodeOptions cardanoNodes

-- | Number of stake pool nodes
newtype NumPools = NumPools Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

-- | Number of relay nodes
newtype NumRelays = NumRelays Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

-- | Number of Delegate Represenatives
newtype NumDReps = NumDReps Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

instance Default CardanoTestnetOptions where
  def = CardanoTestnetOptions
    { cardanoNodes = cardanoDefaultTestnetNodeOptions
    , cardanoNodeEra = AnyShelleyBasedEra ShelleyBasedEraConway
    , cardanoMaxSupply = 100_000_020_000_000 -- 100 000 billions Lovelace, so 100 millions ADA. This amount should be bigger than the 'byronTotalBalance' in Testnet.Start.Byron
    , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
    , cardanoNumDReps = 3
    , cardanoEnableNewEpochStateLogging = True
    , txGeneratorSupport = NoTxGeneratorSupport
    , cardanoOutputDir = def
    }

-- | Options that are implemented by writing fields in the Shelley genesis file.
data GenesisOptions = GenesisOptions
  { genesisTestnetMagic :: Int -- TODO Use the NetworkMagic type from API
  , genesisEpochLength :: Int -- ^ An epoch's duration, in number of slots
  , genesisSlotLength :: Double -- ^ Slot length, in seconds
  , genesisActiveSlotsCoeff :: Double
  } deriving (Eq, Show)

instance Default GenesisOptions where
  def = GenesisOptions
    { genesisTestnetMagic = defaultTestnetMagic
    , genesisEpochLength = 500
    , genesisSlotLength = 0.1
    , genesisActiveSlotsCoeff = 0.05
    }

-- | Whether a node should be an SPO or just a relay.
-- The '@String' arguments will be appended to the default options when starting the node.
data NodeOption
  = SpoNodeOptions [String]
  | RelayNodeOptions [String]
  deriving (Eq, Show)

-- | Type used to track whether the user is providing its data (node configuration file path, genesis file, etc.)
-- or whether it needs to be programmatically generated by @cardanoTestnet@ and friends.
data UserProvidedData a =
    UserProvidedData a
  | NoUserProvidedData
  deriving (Eq,Show)

instance Default (UserProvidedData a) where
  def = NoUserProvidedData

isSpoNodeOptions :: NodeOption -> Bool
isSpoNodeOptions SpoNodeOptions{} = True
isSpoNodeOptions RelayNodeOptions{} = False

isRelayNodeOptions :: NodeOption -> Bool
isRelayNodeOptions SpoNodeOptions{} = False
isRelayNodeOptions RelayNodeOptions{} = True

cardanoDefaultTestnetNodeOptions :: NonEmpty NodeOption
cardanoDefaultTestnetNodeOptions =
  SpoNodeOptions [] :| [ RelayNodeOptions []
                       , RelayNodeOptions []
                       ]

data NodeLoggingFormat = NodeLoggingFormatAsJson | NodeLoggingFormatAsText deriving (Eq, Show)

data NodeConfiguration

type NodeConfigurationYaml = File NodeConfiguration InOut

data GenesisHashesPolicy = WithHashes | WithoutHashes
  deriving (Eq, Show)

data Conf = Conf
  { genesisHashesPolicy :: GenesisHashesPolicy
  , tempAbsPath :: TmpAbsolutePath
  , updateTimestamps :: UpdateTimestamps
  } deriving (Eq, Show)

-- |  Same as mkConfig except that it renders the path
-- when failing in a property test.
mkConf :: (HasCallStack, MonadTest m) => FilePath -> m Conf
mkConf tempAbsPath' = withFrozenCallStack $ do
  H.note_ tempAbsPath'
  pure $ mkConfig tempAbsPath'

-- | Create a 'Conf' from a temporary absolute path, with Genesis Hashes enabled
-- and updating time stamps disabled.
mkConfig :: FilePath -> Conf
mkConfig tempAbsPath' =
  Conf
    { genesisHashesPolicy = WithHashes
    , tempAbsPath = TmpAbsolutePath (addTrailingPathSeparator tempAbsPath')
    , updateTimestamps = DontUpdateTimestamps
    }

-- | Create a 'Conf' from an absolute path, with Genesis Hashes enabled
-- and updating time stamps disabled.
mkConfigAbs :: FilePath -> IO Conf
mkConfigAbs userOutputDir = do
  absUserOutputDir <-  makeAbsolute userOutputDir
  dirExists <- doesDirectoryExist absUserOutputDir
  let conf = mkConfig absUserOutputDir
  unless dirExists $
    createDirectory absUserOutputDir
  pure conf

-- | @anyEraToString (AnyCardanoEra ByronEra)@ returns @"byron"@
anyEraToString :: AnyCardanoEra -> String
anyEraToString (AnyCardanoEra e) = eraToString e

-- | @eraToString ByronEra@ returns @"byron"@
eraToString :: Pretty (eon era) => eon era -> String
eraToString = map toLower . docToString . pretty

-- | @anyShelleyBasedEraToString $ AnyShelleyBasedEra ShelleyBasedEraConway@ returns @"conway"@
anyShelleyBasedEraToString :: AnyShelleyBasedEra -> String
anyShelleyBasedEraToString (AnyShelleyBasedEra sbe) = eraToString sbe
