{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetCliOptions(..)
  , CardanoTestnetOptions(..)
  , ConfigFilesBehaviour(..)
  , InputNodeConfigFile(..)
  , NumDReps(..)
  , NumPools(..)
  , NumRelays(..)
  , cardanoNumPools
  , cardanoNumRelays

  , anyEraToString
  , anyShelleyBasedEraToString
  , eraToString

  , TestnetNodeOptions(..)
  , AutomaticNodeOption(..)
  , isRelayNodeOptions
  , cardanoDefaultTestnetNodeOptions
  , GenesisOptions(..)
  , UserProvidedData(..)

  , NodeLoggingFormat(..)
  , Conf(..)
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
-- 'GenesisOptions'
data CardanoTestnetCliOptions = CardanoTestnetCliOptions
  { cliTestnetOptions :: CardanoTestnetOptions
  , cliGenesisOptions :: GenesisOptions
  } deriving (Eq, Show)

instance Default CardanoTestnetCliOptions where
  def = CardanoTestnetCliOptions
    { cliTestnetOptions = def
    , cliGenesisOptions = def
    }

-- | Options which, contrary to 'GenesisOptions' are not implemented
-- by tuning the genesis files.
data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | Options controlling how many nodes to create and whether to use user-provided
    -- configuration files, or to generate them automatically.
    cardanoNodes :: TestnetNodeOptions
  , cardanoNodeEra :: AnyShelleyBasedEra -- ^ The era to start at
  , cardanoMaxSupply :: Word64 -- ^ The amount of Lovelace you are starting your testnet with (forwarded to shelley genesis)
                               -- TODO move me to GenesisOptions when https://github.com/IntersectMBO/cardano-cli/pull/874 makes it to cardano-node
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  , cardanoNumDReps :: NumDReps -- ^ The number of DReps to generate at creation
  , cardanoEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  , cardanoConfigFilesBehaviour :: ConfigFilesBehaviour -- ^ Whether to continue tests after generating config files.
  , cardanoOutputDir :: Maybe FilePath -- ^ The output directory where to store files, sockets, and so on. If unset, a temporary directory is used.
  } deriving (Eq, Show)

-- | Path to the configuration file of the node, specified by the user
newtype InputNodeConfigFile = InputNodeConfigFile FilePath
  deriving (Eq, Show)

cardanoNumPools :: CardanoTestnetOptions -> NumPools
cardanoNumPools CardanoTestnetOptions{cardanoNodes} =
  NumPools $
    case cardanoNodes of
      UserProvidedNodeOptions _ -> 1
      AutomaticNodeOptions opts -> length $ filter isSpoNodeOptions opts

cardanoNumRelays :: CardanoTestnetOptions -> NumRelays
cardanoNumRelays CardanoTestnetOptions{cardanoNodes} =
  NumRelays $
    case cardanoNodes of
      UserProvidedNodeOptions _ -> 1
      AutomaticNodeOptions opts -> length $ filter isRelayNodeOptions opts

-- | Number of stake pool nodes
newtype NumPools = NumPools Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

-- | Number of relay nodes
newtype NumRelays = NumRelays Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

-- | Number of Delegate Represenatives
newtype NumDReps = NumDReps Int
  deriving (Show, Read, Eq, Enum, Ord, Num, Real, Integral) via Int

data ConfigFilesBehaviour = OnlyGenerate | GenerateAndRun
  deriving (Eq, Show)

instance Default CardanoTestnetOptions where
  def = CardanoTestnetOptions
    { cardanoNodes = cardanoDefaultTestnetNodeOptions
    , cardanoNodeEra = AnyShelleyBasedEra ShelleyBasedEraConway
    , cardanoMaxSupply = 100_000_020_000_000 -- 100 000 billions Lovelace, so 100 millions ADA. This amount should be bigger than the 'byronTotalBalance' in Testnet.Start.Byron
    , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
    , cardanoNumDReps = 3
    , cardanoEnableNewEpochStateLogging = True
    , cardanoConfigFilesBehaviour = GenerateAndRun
    , cardanoOutputDir = Nothing
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

data TestnetNodeOptions =
  UserProvidedNodeOptions FilePath
  -- ^ Value used when the user specifies the node configuration file. We start one single SPO node.
  | AutomaticNodeOptions [AutomaticNodeOption]
  -- ^ Value used when @cardano-testnet@ controls the node configuration files.
  -- We start a custom number of nodes.
  deriving (Eq, Show)

-- | Type used when the user doesn't specify the node configuration file. We start
-- a custom number of nodes. The '@String' arguments will be appended to the default
-- options when starting the node.
data AutomaticNodeOption =
    SpoNodeOptions [String]
  | RelayNodeOptions [String]
  deriving (Eq, Show)

-- | Type used to track whether the user is providing its data (node configuration file path, genesis file, etc.)
-- or whether it needs to be programmatically generated by @cardanoTestnet@ and friends.
data UserProvidedData a =
    UserProvidedData a
  | NoUserProvidedData

isSpoNodeOptions :: AutomaticNodeOption -> Bool
isSpoNodeOptions SpoNodeOptions{} = True
isSpoNodeOptions RelayNodeOptions{} = False

isRelayNodeOptions :: AutomaticNodeOption -> Bool
isRelayNodeOptions SpoNodeOptions{} = False
isRelayNodeOptions RelayNodeOptions{} = True

cardanoDefaultTestnetNodeOptions :: TestnetNodeOptions
cardanoDefaultTestnetNodeOptions =
  AutomaticNodeOptions [ SpoNodeOptions []
                       , RelayNodeOptions []
                       , RelayNodeOptions []
                       ]

data NodeLoggingFormat = NodeLoggingFormatAsJson | NodeLoggingFormatAsText deriving (Eq, Show)

data NodeConfiguration

type NodeConfigurationYaml = File NodeConfiguration InOut

newtype Conf = Conf
  { tempAbsPath :: TmpAbsolutePath
  } deriving (Eq, Show)

-- | Create a 'Conf' from a temporary absolute path. Logs the argument in the test.
mkConf :: (HasCallStack, MonadTest m) => FilePath -> m Conf
mkConf tempAbsPath' = withFrozenCallStack $ do
  H.note_ tempAbsPath'
  pure $ Conf
    { tempAbsPath = TmpAbsolutePath (addTrailingPathSeparator tempAbsPath')
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
