{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetCliOptions(..)
  , CardanoTestnetCreateEnvOptions (..)
  , CardanoTestnetOptions(..)
  , InputNodeConfigFile(..)
  , NumDReps(..)
  , NumPools(..)
  , NumRelays(..)
  , cardanoNumPools
  , cardanoNumRelays

  , anyEraToString
  , anyShelleyBasedEraToString
  , eraToString

  , NodeOption(..)
  , isRelayNodeOptions
  , cardanoDefaultTestnetNodeOptions
  , GenesisOptions(..)
  , UserProvidedData(..)
  , UserProvidedEnv(..)

  , NodeLoggingFormat(..)
  , Conf(..)
  , GenesisHashesPolicy (..)
  , NodeConfiguration
  , NodeConfigurationYaml
  , mkConf
  ) where

import           Cardano.Api hiding (cardanoEra)

import           Prelude

import           Data.Char (toLower)
import           Data.Default.Class
import           Data.Word
import           GHC.Stack
import           System.FilePath (addTrailingPathSeparator)

import           Testnet.Filepath

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H

-- | Command line options for the @cardano-testnet@ executable. They are used
-- in the parser, and then get split into 'CardanoTestnetOptions' and
-- 'GenesisOptions'. If 'cliNodeEnvironment' is provided, don't create a
-- sandbox environment, use the one at the given path.
data CardanoTestnetCliOptions = CardanoTestnetCliOptions
  { cliTestnetOptions :: CardanoTestnetOptions
  , cliGenesisOptions :: GenesisOptions
  , cliNodeEnvironment :: UserProvidedEnv
  } deriving (Eq, Show)

instance Default CardanoTestnetCliOptions where
  def = CardanoTestnetCliOptions
    { cliTestnetOptions = def
    , cliGenesisOptions = def
    , cliNodeEnvironment = def
    }

data UserProvidedEnv
  = NoUserProvidedEnv
  | UserProvidedEnv FilePath
  deriving (Eq, Show)

instance Default UserProvidedEnv where
  def = NoUserProvidedEnv

data CardanoTestnetCreateEnvOptions = CardanoTestnetCreateEnvOptions
  { createEnvTestnetOptions :: CardanoTestnetOptions
  , createEnvGenesisOptions :: GenesisOptions
  , createEnvOutputDir :: FilePath
  } deriving (Eq, Show)

-- | Options which, contrary to 'GenesisOptions' are not implemented
-- by tuning the genesis files.
data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | Options controlling how many nodes to create and of which type.
    cardanoNodes :: [NodeOption]
  , cardanoNodeEra :: AnyShelleyBasedEra -- ^ The era to start at
  , cardanoMaxSupply :: Word64 -- ^ The amount of Lovelace you are starting your testnet with (forwarded to shelley genesis)
                               -- TODO move me to GenesisOptions when https://github.com/IntersectMBO/cardano-cli/pull/874 makes it to cardano-node
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  , cardanoNumDReps :: NumDReps -- ^ The number of DReps to generate at creation
  , cardanoEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  , cardanoOutputDir :: UserProvidedEnv -- ^ The output directory where to store files, sockets, and so on. If unset, a temporary directory is used.
  , cardanoEnableRpc :: Bool
  -- ^ True to enable gRPC endpoints in all testnet nodes
  } deriving (Eq, Show)

-- | Path to the configuration file of the node, specified by the user
newtype InputNodeConfigFile = InputNodeConfigFile FilePath
  deriving (Eq, Show)

cardanoNumPools :: CardanoTestnetOptions -> NumPools
cardanoNumPools CardanoTestnetOptions{cardanoNodes} =
  NumPools $ length $ filter isSpoNodeOptions cardanoNodes

cardanoNumRelays :: CardanoTestnetOptions -> NumRelays
cardanoNumRelays CardanoTestnetOptions{cardanoNodes} =
  NumRelays $ length $ filter isRelayNodeOptions cardanoNodes

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
    , cardanoOutputDir = def
    , cardanoEnableRpc = False
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
    { genesisTestnetMagic = 42
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

isSpoNodeOptions :: NodeOption -> Bool
isSpoNodeOptions SpoNodeOptions{} = True
isSpoNodeOptions RelayNodeOptions{} = False

isRelayNodeOptions :: NodeOption -> Bool
isRelayNodeOptions SpoNodeOptions{} = False
isRelayNodeOptions RelayNodeOptions{} = True

cardanoDefaultTestnetNodeOptions :: [NodeOption]
cardanoDefaultTestnetNodeOptions =
  [ SpoNodeOptions []
  , RelayNodeOptions []
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
  } deriving (Eq, Show)

-- | Create a 'Conf' from a temporary absolute path, with Genesis Hashes enabled.
-- Logs the argument in the test.
mkConf :: (HasCallStack, MonadTest m) => FilePath -> m Conf
mkConf tempAbsPath' = withFrozenCallStack $ do
  H.note_ tempAbsPath'
  pure $ Conf
    { genesisHashesPolicy = WithHashes
    , tempAbsPath = TmpAbsolutePath (addTrailingPathSeparator tempAbsPath')
    }

-- | @anyEraToString (AnyCardanoEra ByronEra)@ returns @"byron"@
anyEraToString :: AnyCardanoEra -> String
anyEraToString (AnyCardanoEra e) = eraToString e

-- | @eraToString ByronEra@ returns @"byron"@
eraToString :: Pretty (eon era) => eon era -> String
eraToString = map toLower . docToString . pretty

-- | @anyShelleyBasedEraToString $ AnyShelleyBasedEra ShelleyBasedEraConway@ returns @"conway"@
anyShelleyBasedEraToString :: AnyShelleyBasedEra -> String
anyShelleyBasedEraToString (AnyShelleyBasedEra sbe) = eraToString sbe
