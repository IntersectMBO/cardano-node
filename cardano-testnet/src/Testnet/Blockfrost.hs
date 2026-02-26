{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

module Testnet.Blockfrost
  ( BlockfrostParams
  , blockfrostToGenesis
  ) where

import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.PParams (CoinPerWord)
import           Cardano.Ledger.BaseTypes (EpochInterval, NonNegativeInterval, Nonce, ProtVer (..),
                   UnitInterval, Version)
import           Cardano.Ledger.Coin (Coin, CoinPerByte(..), compactCoinOrError)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import           Cardano.Ledger.Conway.PParams (DRepVotingThresholds (..),
                   PoolVotingThresholds (..), UpgradeConwayPParams (..))
import           Cardano.Ledger.Core (PParams (..))
import           Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis)
import           Cardano.Ledger.Plutus (CostModel, CostModels, ExUnits (..), Language (..),
                   Prices (..))
import qualified Cardano.Ledger.Plutus.CostModels as CostModels
import           Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import           Cardano.Ledger.Shelley.PParams (ShelleyPParams (..))

import           Control.Applicative ((<|>))
import           Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import           Data.Scientific (Scientific)
import           Data.Word (Word16, Word32)
import           Numeric.Natural (Natural)
import           Text.Read (readMaybe)

data BlockfrostParams = BlockfrostParams
  { -- Alonzo parameters
    bfgCoinsPerUTxOWord     :: CoinPerWord
  , bfgCollateralPercent    :: Natural
  , bfgMaxBlockExMem        :: Natural
  , bfgMaxBlockExSteps      :: Natural
  , bfgMaxCollateralInputs  :: Natural
  , bfgMaxTxExMem           :: Natural
  , bfgMaxTxExSteps         :: Natural
  , bfgMaxValueSize         :: Natural
  , bfgPriceMem             :: NonNegativeInterval
  , bfgPriceSteps           :: NonNegativeInterval
    -- PlutusV1 and PlutusV2
  , bfgAlonzoCostModels     :: CostModels

    -- Conway parameters
  , bfgCommitteeMaxTermLength     :: EpochInterval
  , bfgCommitteeMinSize           :: Word16
  , bfgDRepActivity               :: EpochInterval
  , bfgDRepDeposit                :: Coin
  , bfgDVTCommitteeNoConfidence   :: UnitInterval
  , bfgDVTCommitteeNormal         :: UnitInterval
  , bfgDVTHardForkInitiation      :: UnitInterval
  , bfgDVTMotionNoConfidence      :: UnitInterval
  , bfgDVTPPEconomicGroup         :: UnitInterval
  , bfgDVTPPGovGroup              :: UnitInterval
  , bfgDVTPPNetworkGroup          :: UnitInterval
  , bfgDVTPPTechnicalGroup        :: UnitInterval
  , bfgDVTTreasuryWithdrawal      :: UnitInterval
  , bfgDVTUpdateToConstitution    :: UnitInterval
  , bfgGovActionDeposit           :: Coin
  , bfgGovActionLifetime          :: EpochInterval
  , bfgMinFeeRevScriptCostPerByte :: NonNegativeInterval
  , bfgPVTCommitteeNoConfidence   :: UnitInterval
  , bfgPVTCommitteeNormal         :: UnitInterval
  , bfgPVTHardForkInitiation      :: UnitInterval
  , bfgPVTMotionNoConfidence      :: UnitInterval
  , bfgPVTPPSecurityGroup         :: UnitInterval
    -- PlutusV3
  , bfgConwayCostModel            :: CostModel

    -- Shelley parameters
  , bfgA0                     :: NonNegativeInterval
  , bfgDecentralisationParam  :: UnitInterval
  , bfgEMax                   :: EpochInterval
  , bfgExtraEntropy           :: Nonce
  , bfgKeyDeposit             :: Coin
  , bfgMaxBlockHeaderSize     :: Word16
  , bfgMaxBlockSize           :: Word32
  , bfgMaxTxSize              :: Word32
  , bfgMinFeeA                :: Coin
  , bfgMinFeeB                :: Coin
  , bfgMinPoolCost            :: Coin
  , bfgMinUTxO                :: Coin
  , bfgNOpt                   :: Word16
  , bfgPoolDeposit            :: Coin
  , bfgProtocolMajorVer       :: Version
  , bfgProtocolMinorVer       :: Natural
  , bfgRho                    :: UnitInterval
  , bfgTau                    :: UnitInterval
  } deriving (Eq, Show)

instance FromJSON BlockfrostParams where
  -- In this JSON instance, I don't use the applicative syntax to be independent from field order.
  -- The flag -Werror=missing-fields ensures that no field has been missed.
  parseJSON = withObject "BlockfrostParams" $ \o -> do

    -- Alonzo params
    bfgCoinsPerUTxOWord     <- o .:# "coins_per_utxo_word"
    bfgCollateralPercent    <- o .:# "collateral_percent"
    bfgMaxBlockExMem        <- o .:# "max_block_ex_mem"
    bfgMaxBlockExSteps      <- o .:# "max_block_ex_steps"
    bfgMaxCollateralInputs  <- o .:# "max_collateral_inputs"
    bfgMaxTxExMem           <- o .:# "max_tx_ex_mem"
    bfgMaxTxExSteps         <- o .:# "max_tx_ex_steps"
    bfgMaxValueSize         <- o .:# "max_val_size"
    bfgPriceMem             <- o .:# "price_mem"
    bfgPriceSteps           <- o .:# "price_step"

    -- Conway params
    bfgCommitteeMaxTermLength     <- o .:# "committee_max_term_length"
    bfgCommitteeMinSize           <- o .:# "committee_min_size"
    bfgDRepActivity               <- o .:# "drep_activity"
    bfgDRepDeposit                <- o .:# "drep_deposit"
    bfgDVTCommitteeNoConfidence   <- o .:# "dvt_committee_no_confidence"
    bfgDVTCommitteeNormal         <- o .:# "dvt_committee_normal"
    bfgDVTHardForkInitiation      <- o .:# "dvt_hard_fork_initiation"
    bfgDVTMotionNoConfidence      <- o .:# "dvt_motion_no_confidence"
    bfgDVTPPEconomicGroup         <- o .:# "dvt_p_p_economic_group"
    bfgDVTPPGovGroup              <- o .:# "dvt_p_p_gov_group"
    bfgDVTPPNetworkGroup          <- o .:# "dvt_p_p_network_group"
    bfgDVTPPTechnicalGroup        <- o .:# "dvt_p_p_technical_group"
    bfgDVTTreasuryWithdrawal      <- o .:# "dvt_treasury_withdrawal"
    bfgDVTUpdateToConstitution    <- o .:# "dvt_update_to_constitution"
    bfgGovActionDeposit           <- o .:# "gov_action_deposit"
    bfgGovActionLifetime          <- o .:# "gov_action_lifetime"
    bfgMinFeeRevScriptCostPerByte <- o .:# "min_fee_ref_script_cost_per_byte"
    bfgPVTCommitteeNoConfidence   <- o .:# "pvt_committee_no_confidence"
    bfgPVTCommitteeNormal         <- o .:# "pvt_committee_normal"
    bfgPVTHardForkInitiation      <- o .:# "pvt_hard_fork_initiation"
    bfgPVTMotionNoConfidence      <- o .:# "pvt_motion_no_confidence"
    bfgPVTPPSecurityGroup         <- o .:# "pvt_p_p_security_group"

    -- Shelley params
    bfgA0                     <- o .:# "a0"
    bfgDecentralisationParam  <- o .:# "decentralisation_param"
    bfgEMax                   <- o .:# "e_max"
    bfgExtraEntropy           <- o .:# "extra_entropy"
    bfgKeyDeposit             <- o .:# "key_deposit"
    bfgMaxBlockHeaderSize     <- o .:# "max_block_header_size"
    bfgMaxBlockSize           <- o .:# "max_block_size"
    bfgMaxTxSize              <- o .:# "max_tx_size"
    bfgMinFeeA                <- o .:# "min_fee_a"
    bfgMinFeeB                <- o .:# "min_fee_b"
    bfgMinPoolCost            <- o .:# "min_pool_cost"
    bfgMinUTxO                <- o .:# "min_utxo"
    bfgNOpt                   <- o .:# "n_opt"
    bfgPoolDeposit            <- o .:# "pool_deposit"
    bfgProtocolMajorVer       <- o .:# "protocol_major_ver"
    bfgProtocolMinorVer       <- o .:# "protocol_minor_ver"
    bfgRho                    <- o .:# "rho"
    bfgTau                    <- o .:# "tau"

    -- Cost models
    models' <- o .: "cost_models_raw"
    let models = CostModels.costModelsValid models'
        bfgAlonzoCostModels = CostModels.mkCostModels $
          Map.filterWithKey (\lang _ -> lang `elem` [PlutusV1, PlutusV2]) models
    bfgConwayCostModel <- case Map.lookup PlutusV3 models of
      Nothing -> Aeson.parseFail "Found no cost model for PlutusV3"
      Just model -> pure model

    pure $ BlockfrostParams {..}

    where
      -- Sometimes BlockFrost encodes integers as strings. If the default parsing fails,
      -- This parser tries to convert from String to Number, then tries again to parse the number.
      (.:#) :: FromJSON a => Aeson.Object -> Aeson.Key -> Aeson.Parser a
      o .:# k = (o .: k) <|> do
        (s :: String) <- o .: k
        case readMaybe s of
          Just (n :: Scientific) -> Aeson.parseJSON $ Aeson.Number n
          Nothing -> Aeson.parseFail $ "Bogus value at key " ++ show k ++ " is neither Number nor String"

-- Edit a set of Genesis files with data from Blockfrost parameters
blockfrostToGenesis :: ()
  => (ShelleyGenesis, AlonzoGenesis, ConwayGenesis, DijkstraGenesis)
  -> BlockfrostParams
  -> (ShelleyGenesis, AlonzoGenesis, ConwayGenesis, DijkstraGenesis)
blockfrostToGenesis (shelleyGenesis', alonzoGenesis', conwayGenesis', dijkstraGenesis') BlockfrostParams{..} =
  (shelleyGenesis, alonzoGenesis, conwayGenesis, dijkstraGenesis)
  where
    -- Alonzo params
    alonzoGenesis = alonzoGenesis'
      { agCoinsPerUTxOWord = bfgCoinsPerUTxOWord
      , agCollateralPercentage = bfgCollateralPercent
      , agMaxBlockExUnits = ExUnits
        { exUnitsMem = bfgMaxBlockExMem
        , exUnitsSteps = bfgMaxBlockExSteps
        }
      , agMaxCollateralInputs = bfgMaxCollateralInputs
      , agMaxTxExUnits = ExUnits
        { exUnitsMem = bfgMaxTxExMem
        , exUnitsSteps = bfgMaxTxExSteps
        }
      , agMaxValSize = bfgMaxValueSize
      , agPrices = Prices
        { prMem = bfgPriceMem
        , prSteps = bfgPriceSteps
        }
      , agPlutusV1CostModel = undefined -- TODO(10.7)
        -- CostModels.mkCostModels . Map.mapWithKey trimCostModelToInitial $ CostModels.costModelsValid bfgAlonzoCostModels
      }

    -- Conway Params
    conwayParams = UpgradeConwayPParams
      { ucppPoolVotingThresholds = PoolVotingThresholds
        { pvtMotionNoConfidence = bfgPVTMotionNoConfidence
        , pvtCommitteeNormal = bfgPVTCommitteeNormal
        , pvtCommitteeNoConfidence = bfgPVTCommitteeNoConfidence
        , pvtHardForkInitiation = bfgPVTHardForkInitiation
        , pvtPPSecurityGroup = bfgPVTPPSecurityGroup
        }
      , ucppDRepVotingThresholds = DRepVotingThresholds
        { dvtMotionNoConfidence = bfgDVTMotionNoConfidence
        , dvtCommitteeNormal = bfgDVTCommitteeNormal
        , dvtCommitteeNoConfidence = bfgDVTCommitteeNoConfidence
        , dvtUpdateToConstitution = bfgDVTUpdateToConstitution
        , dvtHardForkInitiation = bfgDVTHardForkInitiation
        , dvtPPNetworkGroup = bfgDVTPPNetworkGroup
        , dvtPPEconomicGroup = bfgDVTPPEconomicGroup
        , dvtPPTechnicalGroup = bfgDVTPPTechnicalGroup
        , dvtPPGovGroup = bfgDVTPPGovGroup
        , dvtTreasuryWithdrawal = bfgDVTTreasuryWithdrawal
        }
      , ucppCommitteeMinSize = bfgCommitteeMinSize
      , ucppCommitteeMaxTermLength = bfgCommitteeMaxTermLength
      , ucppGovActionLifetime = bfgGovActionLifetime
      , ucppGovActionDeposit = bfgGovActionDeposit
      , ucppDRepDeposit = bfgDRepDeposit
      , ucppDRepActivity = bfgDRepActivity
      , ucppMinFeeRefScriptCostPerByte = bfgMinFeeRevScriptCostPerByte
      , ucppPlutusV3CostModel = trimCostModelToInitial PlutusV3 bfgConwayCostModel
      }
    conwayGenesis = conwayGenesis'{cgUpgradePParams=conwayParams}

    -- Shelley params
    shelleyParams = PParams $ ShelleyPParams
      { sppTxFeePerByte = CoinPerByte . compactCoinOrError $ bfgMinFeeA
      , sppTxFeeFixed = compactCoinOrError $ bfgMinFeeB
      , sppMaxBBSize = bfgMaxBlockSize
      , sppMaxTxSize = bfgMaxTxSize
      , sppMaxBHSize = bfgMaxBlockHeaderSize
      , sppKeyDeposit = compactCoinOrError $ bfgKeyDeposit
      , sppPoolDeposit = compactCoinOrError $ bfgPoolDeposit
      , sppEMax = bfgEMax
      , sppNOpt = bfgNOpt
      , sppA0 = bfgA0
      , sppRho = bfgRho
      , sppTau = bfgTau
      , sppD = bfgDecentralisationParam
      , sppExtraEntropy = bfgExtraEntropy
      , sppProtocolVersion = ProtVer
        { pvMajor = bfgProtocolMajorVer
        , pvMinor = bfgProtocolMinorVer
        }
      , sppMinUTxOValue = compactCoinOrError $ bfgMinUTxO
      , sppMinPoolCost = compactCoinOrError $ bfgMinPoolCost
      }
    shelleyGenesis = shelleyGenesis'{sgProtocolParams=shelleyParams}

    -- TODO dijkstra: there are no dijkstra params on blockfrost
    dijkstraGenesis = dijkstraGenesis'

-- | Trims cost model to the initial number of parameters. The cost models in geneses can't
-- have more parameters than the initial number.
trimCostModelToInitial :: Language -> CostModel -> CostModel
trimCostModelToInitial lang cm = do
  let paramsCount = CostModels.costModelInitParamCount lang
  either (error . ("Testnet.Blockfrost: Cost model trimming failure: " <>) . show) id
    . CostModels.mkCostModel lang
    . take paramsCount
    $ CostModels.getCostModelParams cm
