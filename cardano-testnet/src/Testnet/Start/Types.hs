{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetCliOptions(..)
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
data CardanoTestnetCliOptions = CardanoTestnetCliOptions {
    -- First options that are common to both the automatic and user-provided cases
    cardanoNodeLoggingFormat :: NodeLoggingFormat
  , cardanoEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  , cardanoOutputDir :: Maybe FilePath -- ^ The output directory where to store files, sockets, and so on. If unset, a temporary directory is used.   commonOptions :: CardanoTestnetCommonOptions
  -- Then onto options that depend on the scenario
  , nodeOptions :: Either AutomaticNodeOptions [InputNodeConfigFile]
  }

-- | The kind of options used when the user doesn't provide the
-- nodes' configuration files (nor the genesis files) and instead
-- relies on @cardano-testnet@ to generate them
data AutomaticNodeOptions = AutomaticNodeOptions
  { cardanoMaxSupply :: Word64 -- ^ The amount of Lovelace you are starting your testnet with (forward to shelley genesis)
  , cardanoNumDReps :: NumDReps -- ^ The number of DReps to generate at creation
  , cardanoNodeEra :: AnyShelleyBasedEra -- ^ The era to start at
  , individualNodeOptions :: AutomaticNodeOption -- ^ The number of nodes to create, and whether to create a SPO or relay node
  , genesisEpochLength :: Int -- ^ An epoch's duration, in number of slots
  , genesisSlotLength :: Double -- ^ Slot length, in seconds
  , genesisActiveSlotsCoeff :: Double
  , genesisTestnetMagic :: Int -- TODO Use the NetworkMagic type from API
  }
  deriving (Eq, Show)

data AutomaticNodeOption = AutomaticNodeOption
  { isSpoOrRelayNode :: Bool
  , enableP2P :: Bool
  }

instance Default CardanoTestnetCliOptions where
  def = undefined

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

instance Default CardanoTestnetOptions where
  def = undefined

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
