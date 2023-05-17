{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Genesis
  ( ShelleyGenesis(..)
  , shelleyGenesisDefaults

  -- ** Configuration
  , ByronGenesisConfig
  , ShelleyGenesisConfig
  , AlonzoGenesisConfig
  , ConwayGenesisConfig

  , ShelleyConfig(..)
  , GenesisHashByron(..)
  , GenesisHashShelley(..)
  , GenesisHashAlonzo(..)
  , GenesisHashConway(..)

  -- ** Files
  , ByronGenesisFile
  , ShelleyGenesisFile
  , AlonzoGenesisFile
  , ConwayGenesisFile
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Time as Time
import           Lens.Micro

import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class

import           Cardano.Api.IO

import qualified Cardano.Chain.Genesis

import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.Core
import           Cardano.Ledger.Shelley.Genesis (NominalDiffTimeMicro, ShelleyGenesis (..),
                   emptyGenesisStaking)

import qualified Cardano.Ledger.Shelley.Genesis as Ledger

import qualified Ouroboros.Consensus.Shelley.Eras as Shelley

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(Ledger.ShelleyGenesis Shelley.StandardCrypto)
  , scGenesisHash :: !GenesisHashShelley
  }

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashConway = GenesisHashConway
  { unGenesisHashConway :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

type ByronGenesisConfig = Cardano.Chain.Genesis.Config
type ShelleyGenesisConfig = ShelleyConfig
type AlonzoGenesisConfig = AlonzoGenesis
type ConwayGenesisConfig = ConwayGenesis Shelley.StandardCrypto

type ByronGenesisFile = File ByronGenesisConfig
type ShelleyGenesisFile = File ShelleyGenesisConfig
type AlonzoGenesisFile = File AlonzoGenesisConfig
type ConwayGenesisFile = File ConwayGenesisConfig

-- | Some reasonable starting defaults for constructing a 'ShelleyGenesis'.
--
-- You must override at least the following fields for this to be useful:
--
-- * 'sgSystemStart' the time of the first block
-- * 'sgNetworkMagic' to a suitable testnet or mainnet network magic number.
-- * 'sgGenDelegs' to have some initial nodes
-- * 'sgInitialFunds' to have any money in the system
-- * 'sgMaxLovelaceSupply' must be at least the sum of the 'sgInitialFunds'
--   but more if you want to allow for rewards.
--
shelleyGenesisDefaults :: ShelleyGenesis StandardCrypto
shelleyGenesisDefaults =
  ShelleyGenesis
    {
      -- parameters for this specific chain
      sgSystemStart           = zeroTime
    , sgNetworkMagic          = 42
    , sgNetworkId             = Ledger.Testnet

      -- consensus protocol parameters
    , sgSlotLength            = 1.0 :: NominalDiffTimeMicro -- 1s slots
    , sgActiveSlotsCoeff      = fromMaybe
                                  (error "shelleyGenesisDefaults: impossible")
                                  (Ledger.boundRational (1/20))  -- 20s block times on average
    , sgSecurityParam         = k
    , sgEpochLength           = Ledger.EpochSize (k * 10 * 20) -- 10k/f
    , sgSlotsPerKESPeriod     = 60 * 60 * 36        -- 1.5 days with 1s slots
    , sgMaxKESEvolutions      = 60                  -- 90 days
    , sgUpdateQuorum          = 5                   -- assuming 7 genesis keys

    -- ledger protocol parameters
    , sgProtocolParams        =
        emptyPParams
        & ppDL         .~ maxBound
        & ppMaxBHSizeL .~ 1100                  -- TODO: compute from crypto
        & ppMaxBBSizeL .~ 64 * 1024             -- max 64kb blocks
        & ppMaxTxSizeL .~ 16 * 1024             -- max 16kb txs
        & ppEMaxL      .~ 18
        & ppMinFeeAL   .~ Coin 1                -- The linear factor for the minimum fee calculation
        & ppMinFeeBL   .~ Coin 0                -- The constant factor for the minimum fee calculation

      -- genesis keys and initial funds
    , sgGenDelegs             = Map.empty
    , sgStaking               = emptyGenesisStaking
    , sgInitialFunds          = ListMap.empty
    , sgMaxLovelaceSupply     = 0
    }
  where
    k = 2160
    zeroTime = Time.UTCTime (Time.fromGregorian 1970 1 1) 0 -- tradition

