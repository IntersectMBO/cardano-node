{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Shelley.Genesis
  ( ShelleyGenesis(..)
  , alonzoGenesisDefaults
  , shelleyGenesisDefaults
  ) where

import           Prelude

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio
import qualified Data.Time as Time
import           Numeric.Natural

import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Shelley.PParams as Ledger (PParams' (..), emptyPParams)
import           Cardano.Slotting.Slot (EpochSize (..))
import qualified Plutus.V1.Ledger.Api as Plutus

import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..), emptyGenesisStaking)

import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.Value

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
shelleyGenesisDefaults :: ShelleyGenesis crypto
shelleyGenesisDefaults =
  ShelleyGenesis
    {
      -- parameters for this specific chain
      sgSystemStart           = zeroTime
    , sgNetworkMagic          = 42
    , sgNetworkId             = Ledger.Testnet

      -- consensus protocol parameters
    , sgSlotLength            = 1.0 :: Time.NominalDiffTime -- 1s slots
    , sgActiveSlotsCoeff      = fromMaybe
                                  (error "shelleyGenesisDefaults: impossible")
                                  (Ledger.boundRational (1/20))  -- 20s block times on average
    , sgSecurityParam         = k
    , sgEpochLength           = EpochSize (k * 10 * 20) -- 10k/f
    , sgSlotsPerKESPeriod     = 60 * 60 * 36        -- 1.5 days with 1s slots
    , sgMaxKESEvolutions      = 60                  -- 90 days
    , sgUpdateQuorum          = 5                   -- assuming 7 genesis keys

    -- ledger protocol parameters
    , sgProtocolParams        =
        Ledger.emptyPParams
        { Ledger._d = maxBound
        , Ledger._maxBHSize = 1100                  -- TODO: compute from crypto
        , Ledger._maxBBSize = 64 * 1024             -- max 64kb blocks
        , Ledger._maxTxSize = 16 * 1024             -- max 16kb txs
        , Ledger._eMax      = 18
        , Ledger._minfeeA   = 1                     -- The linear factor for the minimum fee calculation
        , Ledger._minfeeB   = 0                     -- The constant factor for the minimum fee calculation
        }

      -- genesis keys and initial funds
    , sgGenDelegs             = Map.empty
    , sgStaking               = emptyGenesisStaking
    , sgInitialFunds          = Map.empty
    , sgMaxLovelaceSupply     = 0
    }
  where
    k = 2160
    zeroTime = Time.UTCTime (Time.fromGregorian 1970 1 1) 0 -- tradition

-- | Reasonable starting defaults for constructing an 'AlonzoGenesis'.
alonzoGenesisDefaults :: Alonzo.AlonzoGenesis
alonzoGenesisDefaults =
  let cModel = case Alonzo.CostModel <$> Plutus.defaultCostModelParams of
                 Just (Alonzo.CostModel m) ->
                   if Alonzo.validateCostModelParams m
                   then Map.singleton Alonzo.PlutusV1 (Alonzo.CostModel m)
                   else error "alonzoGenesisDefaults: defaultCostModel is invalid"
                 Nothing ->
                   error "alonzoGenesisDefaults: Could not extract cost model \
                         \params from defaultCostModel"
      --TODO: we need a better validation story. We also ought to wrap the
      -- genesis type in the API properly.
      prices' = case toAlonzoPrices alonzoGenesisDefaultExecutionPrices of
                  Nothing -> error "alonzoGenesisDefaults: invalid prices"
                  Just p  -> p
  in Alonzo.AlonzoGenesis
       { Alonzo.coinsPerUTxOWord     = toShelleyLovelace $ Lovelace 34482
       , Alonzo.costmdls             = cModel
       , Alonzo.prices               = prices'
       , Alonzo.maxTxExUnits         = toAlonzoExUnits alonzoGenesisDefaultMaxTxExecutionUnits
       , Alonzo.maxBlockExUnits      = toAlonzoExUnits alonzoGenesisDefaultMaxBlockExecutionUnits
       , Alonzo.maxValSize           = alonzoGenesisDefaultMaxValueSize
       , Alonzo.collateralPercentage = alonzoGenesisDefaultCollateralPercent
       , Alonzo.maxCollateralInputs  = alonzoGenesisDefaultMaxCollateralInputs
       }
 where
  alonzoGenesisDefaultExecutionPrices :: ExecutionUnitPrices
  alonzoGenesisDefaultExecutionPrices =
      ExecutionUnitPrices {
         priceExecutionSteps  = 1 % 10,
         priceExecutionMemory = 1 % 10
      }

  alonzoGenesisDefaultMaxTxExecutionUnits :: ExecutionUnits
  alonzoGenesisDefaultMaxTxExecutionUnits =
      ExecutionUnits {
        executionSteps  = 500_000_000_000,
        executionMemory = 500_000_000_000
      }

  alonzoGenesisDefaultMaxBlockExecutionUnits :: ExecutionUnits
  alonzoGenesisDefaultMaxBlockExecutionUnits =
      ExecutionUnits {
        executionSteps  = 500_000_000_000,
        executionMemory = 500_000_000_000
      }

  alonzoGenesisDefaultMaxValueSize :: Natural
  alonzoGenesisDefaultMaxValueSize = 4000

  alonzoGenesisDefaultCollateralPercent :: Natural
  alonzoGenesisDefaultCollateralPercent = 1

  alonzoGenesisDefaultMaxCollateralInputs :: Natural
  alonzoGenesisDefaultMaxCollateralInputs = 5
