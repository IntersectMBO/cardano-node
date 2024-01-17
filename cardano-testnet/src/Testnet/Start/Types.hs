{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetOptions(..)
  , cardanoDefaultTestnetOptions

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

import           Data.Word

import           Hedgehog.Extras.Test.Base (Integration)

import           Cardano.Api.Ledger (StandardCrypto)
import qualified Cardano.Ledger.Conway.Genesis as Ledger
import           Testnet.Filepath


{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    cardanoNodes :: [TestnetNodeOptions]
  , cardanoNodeEra :: AnyCardanoEra
  , cardanoEpochLength :: Int
  , cardanoSlotLength :: Double
  , cardanoTestnetMagic :: Int
  , cardanoActiveSlotsCoeff :: Double
  , cardanoMaxSupply :: Word64 -- ^ The amount of ADA you are starting your testnet with
  , cardanoEnableP2P :: Bool
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

cardanoDefaultTestnetOptions :: CardanoTestnetOptions
cardanoDefaultTestnetOptions = CardanoTestnetOptions
  { cardanoNodes = cardanoDefaultTestnetNodeOptions
  , cardanoNodeEra = AnyCardanoEra BabbageEra
  , cardanoEpochLength = 500
  , cardanoSlotLength = 0.1
  , cardanoTestnetMagic = 42
  , cardanoActiveSlotsCoeff = 0.1
  , cardanoMaxSupply = 10020000000
  , cardanoEnableP2P = False
  , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
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

data Conf = Conf
  { tempAbsPath :: TmpAbsolutePath
  , -- | Function to rewrite the Conway genesis file before creating the testnet
    rewriteConway :: Ledger.ConwayGenesis StandardCrypto -> Ledger.ConwayGenesis StandardCrypto
  }

instance Show Conf where
  -- We ignore rewriteConway
  show Conf { tempAbsPath } =
    unlines
      ["Conf{"
       , "  tempAbsPath=" ++ show tempAbsPath
       , "  rewriteConway=unshowable"
       , "}"
      ]

mkConf :: FilePath -> Integration Conf
mkConf tempAbsPath' =
  return $ Conf
    { tempAbsPath = TmpAbsolutePath tempAbsPath'
    , rewriteConway = id -- By default, don't change anything
    }


