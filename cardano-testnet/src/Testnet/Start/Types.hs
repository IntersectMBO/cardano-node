{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetOptions(..)
  , cardanoDefaultTestnetOptions

  , TestnetNodeOptions(..)
  , extraSpoNodeCliArgs
  , cardanoDefaultTestnetNodeOptions

  , NodeLoggingFormat(..)
  ) where

import           Cardano.Api hiding (cardanoEra)

import           Prelude

import           Data.Word



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
  , cardanoEpochLength = 1500
  , cardanoSlotLength = 0.2
  , cardanoTestnetMagic = 42
  , cardanoActiveSlotsCoeff = 0.2
  , cardanoMaxSupply = 10020000000
  , cardanoEnableP2P = False
  , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
  }

-- | Specify a BFT node (Pre-Babbage era only) or an SPO (Shelley era onwards only)
newtype TestnetNodeOptions
  = SpoTestnetNodeOptions [String]
    -- ^ These arguments will be appended to the default set of CLI options when
    -- starting the node.
  deriving (Eq, Show)

extraSpoNodeCliArgs :: TestnetNodeOptions -> [String]
extraSpoNodeCliArgs (SpoTestnetNodeOptions args) = args


cardanoDefaultTestnetNodeOptions :: [TestnetNodeOptions]
cardanoDefaultTestnetNodeOptions =
  [ SpoTestnetNodeOptions []
  , SpoTestnetNodeOptions []
  , SpoTestnetNodeOptions []
  ]

data NodeLoggingFormat = NodeLoggingFormatAsJson | NodeLoggingFormatAsText deriving (Eq, Show)
