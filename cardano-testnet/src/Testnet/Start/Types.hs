{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetOptions(..)
  , cardanoDefaultTestnetOptions

  , anyEraToString
  , anyShelleyBasedEraToString
  , eraToString

  , TestnetNodeOptions(..)
  , extraSpoNodeCliArgs
  , cardanoDefaultTestnetNodeOptions

  , NodeLoggingFormat(..)
  , Conf(..)
  , NodeConfigurationYaml(..)
  , mkConf
  ) where

import           Cardano.Api hiding (cardanoEra)

import           Prelude

import           Data.Char (toLower)
import           Data.Word
import           GHC.Stack
import           System.FilePath (addTrailingPathSeparator)

import           Testnet.Filepath

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H


data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    cardanoNodes :: [TestnetNodeOptions]
  , cardanoNodeEra :: AnyShelleyBasedEra -- ^ The era to start at
  , cardanoEpochLength :: Int -- ^ An epoch's duration, in number of slots
  , cardanoSlotLength :: Double -- ^ Slot length, in seconds
  , cardanoTestnetMagic :: Int
  , cardanoActiveSlotsCoeff :: Double
  , cardanoMaxSupply :: Word64 -- ^ The amount of Lovelace you are starting your testnet with (forwarded to shelley genesis)
  , cardanoEnableP2P :: Bool
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  , cardanoNumDReps :: Int -- ^ The number of DReps to generate at creation
  , cardanoEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  } deriving (Eq, Show)

cardanoDefaultTestnetOptions :: CardanoTestnetOptions
cardanoDefaultTestnetOptions = CardanoTestnetOptions
  { cardanoNodes = cardanoDefaultTestnetNodeOptions
  , cardanoNodeEra = AnyShelleyBasedEra ShelleyBasedEraBabbage
  , cardanoEpochLength = 500
  , cardanoSlotLength = 0.1
  , cardanoTestnetMagic = 42
  , cardanoActiveSlotsCoeff = 0.05
  , cardanoMaxSupply = 100_000_020_000_000 -- 100 000 billions Lovelace, so 100 millions ADA. This amount should be bigger than the 'byronTotalBalance' in Testnet.Start.Byron
  , cardanoEnableP2P = False
  , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
  , cardanoNumDReps = 3
  , cardanoEnableNewEpochStateLogging = True
  }

-- | Specify a BFT node (Pre-Babbage era only) or an SPO (Shelley era onwards only)
data TestnetNodeOptions
  = SpoTestnetNodeOptions (Maybe NodeConfigurationYaml) [String]
    -- ^ These arguments will be appended to the default set of CLI options when
    -- starting the node.
  deriving (Eq, Show)

extraSpoNodeCliArgs :: TestnetNodeOptions -> [String]
extraSpoNodeCliArgs (SpoTestnetNodeOptions _ args) = args


cardanoDefaultTestnetNodeOptions :: [TestnetNodeOptions]
cardanoDefaultTestnetNodeOptions =
  [ SpoTestnetNodeOptions Nothing []
  , SpoTestnetNodeOptions Nothing []
  , SpoTestnetNodeOptions Nothing []
  ]

data NodeLoggingFormat = NodeLoggingFormatAsJson | NodeLoggingFormatAsText deriving (Eq, Show)


newtype NodeConfigurationYaml = NodeConfigurationYaml
  { unYamlFilePath :: FilePath
  } deriving (Eq, Show)

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
