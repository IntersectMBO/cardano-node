{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetCliOptions(..)
  , NoUserProvidedEnvOptions(..)
  , StartFromEnvOptions(..)
  , CardanoTestnetCreateEnvOptions (..)
  , TestnetCreationOptions(..)
  , TestnetRuntimeOptions(..)
  , TestnetEnvOptions(..)
  , InputNodeConfigFile(..)
  , NodeId(..)
  , NumDReps(..)
  , NumPools(..)
  , NumRelays(..)
  , RpcSupport(..)
  , creationNumPools
  , creationNumRelays

  , anyEraToString
  , anyShelleyBasedEraToString
  , defaultTestnetMagic
  , eraToString

  , UpdateTimestamps(..)
  , TestnetOnChainParams(..)
  , mainnetParamsRequest
  , TestnetNodesWithOptions(..)
  , NodeWithOptions(..)
  , cardanoDefaultTestnetNodesWithOptions
  , GenesisOptions(..)
  , UserProvidedData(..)
  , UserProvidedGeneses(..)
  , PraosCredentialsSource(..)

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

-- | Command line options for the @cardano-testnet cardano@ command.
-- Either create a new testnet environment or use a pre-existing one
-- (created by @create-env@).
data CardanoTestnetCliOptions
  = NoUserProvidedEnv NoUserProvidedEnvOptions
  | StartFromEnv StartFromEnvOptions
  deriving (Eq, Show)

-- | Options for @cardano-testnet cardano@ when no user-provided environment
-- is given: create a new environment and then start the testnet.
data NoUserProvidedEnvOptions = NoUserProvidedEnvOptions
  { noEnvCreationOptions :: TestnetCreationOptions
  , noEnvOutputDir :: Maybe FilePath -- ^ @--output-dir@, uses a temporary directory if absent
  , noEnvRuntimeOptions :: TestnetRuntimeOptions
  } deriving (Eq, Show)

-- | Options for @cardano-testnet cardano --node-env@ when starting a testnet
-- from a pre-existing environment (created by @create-env@).
data StartFromEnvOptions = StartFromEnvOptions
  { fromEnvOptions :: TestnetEnvOptions
  , fromEnvRuntimeOptions :: TestnetRuntimeOptions
  } deriving (Eq, Show)

data UpdateTimestamps = UpdateTimestamps | DontUpdateTimestamps
  deriving (Eq, Show)

instance Default UpdateTimestamps where
  def = DontUpdateTimestamps

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

data PraosCredentialsSource = UseKesKeyFile | UseKesSocket
  deriving (Eq, Show)

instance Default PraosCredentialsSource where
  def = UseKesKeyFile

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

-- | Command line options for the @cardano-testnet create-env@ subcommand.
-- Creates a sandbox environment without starting nodes.
data CardanoTestnetCreateEnvOptions = CardanoTestnetCreateEnvOptions
  { createEnvCreationOptions :: TestnetCreationOptions
  , createEnvOutputDir :: FilePath -- ^ Required @--output@ directory
  } deriving (Eq, Show)

data RpcSupport
  = RpcDisabled
  | RpcEnabled
  deriving (Eq, Show)

-- | Options for creating a testnet environment (genesis files, topology, ports).
-- Used by both the @cardano@ and @create-env@ subcommands, and by
-- 'Testnet.Start.Cardano.createAndRunTestnet' in tests.
data TestnetCreationOptions = TestnetCreationOptions
  { -- | Options controlling how many nodes to create and of which type.
    creationNodes :: TestnetNodesWithOptions
  , creationEra :: AnyShelleyBasedEra -- ^ The era to start at
  , creationMaxSupply :: Word64 -- ^ The amount of Lovelace you are starting your testnet with (forwarded to shelley genesis)
                                -- TODO move me to GenesisOptions when https://github.com/IntersectMBO/cardano-cli/pull/874 makes it to cardano-node
  , creationNumDReps :: NumDReps -- ^ The number of DReps to generate at creation
  , creationGenesisOptions :: GenesisOptions
  , creationOnChainParams :: TestnetOnChainParams
  } deriving (Eq, Show)

instance Default TestnetCreationOptions where
  def = TestnetCreationOptions
    { creationNodes = cardanoDefaultTestnetNodesWithOptions
    , creationEra = AnyShelleyBasedEra ShelleyBasedEraConway
    , creationMaxSupply = 100_000_020_000_000
    , creationNumDReps = 3
    , creationGenesisOptions = def
    , creationOnChainParams = def
    }

-- | Options for running testnet nodes (after the environment is created).
-- These are independent of how the environment was created (from scratch
-- or from an existing @--node-env@ path).
data TestnetRuntimeOptions = TestnetRuntimeOptions
  { runtimeEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  , runtimeEnableRpc :: RpcSupport -- ^ Whether to enable gRPC endpoints in all testnet nodes
  , runtimeKESSource :: PraosCredentialsSource
  } deriving (Eq, Show)

instance Default TestnetRuntimeOptions where
  def = TestnetRuntimeOptions
    { runtimeEnableNewEpochStateLogging = True
    , runtimeEnableRpc = RpcDisabled
    , runtimeKESSource = def
    }

-- | Options specific to the @--node-env@ path: the environment directory
-- and whether to update genesis timestamps before starting.
data TestnetEnvOptions = TestnetEnvOptions
  { envPath :: FilePath -- ^ Path to the pre-existing environment
  , envUpdateTimestamps :: UpdateTimestamps
  } deriving (Eq, Show)

-- | Path to the configuration file of the node, specified by the user
newtype InputNodeConfigFile = InputNodeConfigFile FilePath
  deriving (Eq, Show)

creationNumPools :: TestnetCreationOptions -> NumPools
creationNumPools TestnetCreationOptions{creationNodes} =
  NumPools $ NEL.length $ optSpoNodes creationNodes

creationNumRelays :: TestnetCreationOptions -> NumRelays
creationNumRelays TestnetCreationOptions{creationNodes} =
  NumRelays $ length $ optRelayNodes creationNodes

-- | Number of stake pool nodes
newtype NumPools = NumPools Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

-- | Number of relay nodes
newtype NumRelays = NumRelays Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

-- | Number of Delegate Represenatives
newtype NumDReps = NumDReps Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

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

-- | Configuration specific to each node
data NodeWithOptions = NodeWithOptions
  { nodeBin :: Maybe FilePath
    -- ^ Path to the @cardano-node@ binary to use for running this node.
    -- This allows testing different node versions side by side in the same testnet
    -- (e.g. to verify compatibility across upgrades).
    -- 'Nothing' uses the default resolution: the @CARDANO_NODE@ environment variable
    -- if set, otherwise the binary path from cabal's @plan.json@.
  , nodeExtraCliArgs :: [String] -- ^ Extra CLI arguments passed to @cardano-node run@
  } deriving (Eq, Show)

-- | Specifies the nodes to create for the testnet, split by role (SPO and relay).
-- SPO nodes participate in block production. Relay nodes only forward blocks.
data TestnetNodesWithOptions = TestnetNodesWithOptions
  { optSpoNodes :: NonEmpty NodeWithOptions -- ^ SPO (stake pool operator) nodes. Must have at least one.
  , optRelayNodes :: [NodeWithOptions] -- ^ Relay (non-producing) nodes
  } deriving (Eq, Show)

-- | Type used to track whether the user is providing its data (node configuration file path, genesis file, etc.)
-- or whether it needs to be programmatically generated by @cardanoTestnet@ and friends.
data UserProvidedData a =
    UserProvidedData a
  | NoUserProvidedData
  deriving (Eq,Show)

instance Default (UserProvidedData a) where
  def = NoUserProvidedData

cardanoDefaultTestnetNodesWithOptions :: TestnetNodesWithOptions
cardanoDefaultTestnetNodesWithOptions = TestnetNodesWithOptions
  { optSpoNodes = NodeWithOptions Nothing [] :| []
  , optRelayNodes = [ NodeWithOptions Nothing []
                    , NodeWithOptions Nothing []
                    ]
  }

data NodeLoggingFormat
  = NodeLoggingFormatAsJson
  | NodeLoggingFormatAsText
  deriving (Eq, Show)

instance Pretty NodeLoggingFormat where
  pretty = \case
    NodeLoggingFormatAsJson -> "json"
    NodeLoggingFormatAsText -> "text"

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
