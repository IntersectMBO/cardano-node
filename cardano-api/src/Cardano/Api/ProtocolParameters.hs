{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The various Cardano protocol parameters, including:
--
-- * the current values of updatable protocol parameters: 'ProtocolParameters'
-- * updates to protocol parameters: 'ProtocolParametersUpdate'
-- * update proposals that can be embedded in transactions: 'UpdateProposal'
-- * parameters fixed in the genesis file: 'GenesisParameters'
--
module Cardano.Api.ProtocolParameters (
    -- * The updatable protocol parameters
    ProtocolParameters(..),
    checkProtocolParameters,
    ProtocolParametersError(..),
    EpochNo,

    -- * Updates to the protocol parameters
    ProtocolParametersUpdate(..),

    -- * PraosNonce
    PraosNonce,
    makePraosNonce,

    -- * Execution units, prices and cost models,
    ExecutionUnits(..),
    ExecutionUnitPrices(..),
    CostModel(..),
    validateCostModel,

    -- * Update proposals to change the protocol parameters
    UpdateProposal(..),
    makeShelleyUpdateProposal,

    -- * Internal conversion functions
    toLedgerNonce,
    toLedgerUpdate,
    fromLedgerUpdate,
    toLedgerProposedPPUpdates,
    fromLedgerProposedPPUpdates,
    toLedgerPParams,
    fromLedgerPParams,
    fromShelleyPParams,
    toAlonzoPrices,
    fromAlonzoPrices,
    toAlonzoScriptLanguage,
    fromAlonzoScriptLanguage,
    toAlonzoCostModel,
    fromAlonzoCostModel,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Control.Monad
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?),
                   (.=))
import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.String (IsString)
import           Data.Text (Text)
import           GHC.Generics
import           Numeric.Natural

import           Cardano.Api.Json
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochNo)

import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger

import qualified Cardano.Ledger.Shelley.PParams as Ledger (ProposedPPUpdates (..), Update (..))
-- Some of the things from Cardano.Ledger.Shelley.PParams are generic across all
-- eras, and some are specific to the Shelley era (and other pre-Alonzo eras).
-- So we import in twice under different names.
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams, PParams' (..), PParamsUpdate)

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value


-- | The values of the set of /updatable/ protocol parameters. At any
-- particular point on the chain there is a current set of parameters in use.
--
-- These parameters can be updated (at epoch boundaries) via an
-- 'UpdateProposal', which contains a 'ProtocolParametersUpdate'.
--
-- The 'ProtocolParametersUpdate' is essentially a diff for the
-- 'ProtocolParameters'.
--
-- There are also parameters fixed in the Genesis file. See 'GenesisParameters'.
--
data ProtocolParameters =
     ProtocolParameters {

       -- | Protocol version, major and minor. Updating the major version is
       -- used to trigger hard forks.
       --                              (Major  , Minor  )
       protocolParamProtocolVersion :: (Natural, Natural),

       -- | The decentralization parameter. This is fraction of slots that
       -- belong to the BFT overlay schedule, rather than the Praos schedule.
       -- So 1 means fully centralised, while 0 means fully decentralised.
       --
       -- This is the \"d\" parameter from the design document.
       --
       protocolParamDecentralization :: Rational,

       -- | Extra entropy for the Praos per-epoch nonce.
       --
       -- This can be used to add extra entropy during the decentralisation
       -- process. If the extra entropy can be demonstrated to be generated
       -- randomly then this method can be used to show that the initial
       -- federated operators did not subtly bias the initial schedule so that
       -- they retain undue influence after decentralisation.
       --
       protocolParamExtraPraosEntropy :: Maybe PraosNonce,

       -- | The maximum permitted size of a block header.
       --
       -- This must be at least as big as the largest legitimate block headers
       -- but should not be too much larger, to help prevent DoS attacks.
       --
       -- Caution: setting this to be smaller than legitimate block headers is
       -- a sure way to brick the system!
       --
       protocolParamMaxBlockHeaderSize :: Natural,

       -- | The maximum permitted size of the block body (that is, the block
       -- payload, without the block header).
       --
       -- This should be picked with the Praos network delta security parameter
       -- in mind. Making this too large can severely weaken the Praos
       -- consensus properties.
       --
       -- Caution: setting this to be smaller than a transaction that can
       -- change the protocol parameters is a sure way to brick the system!
       --
       protocolParamMaxBlockBodySize :: Natural,

       -- | The maximum permitted size of a transaction.
       --
       -- Typically this should not be too high a fraction of the block size,
       -- otherwise wastage from block fragmentation becomes a problem, and
       -- the current implementation does not use any sophisticated box packing
       -- algorithm.
       --
       protocolParamMaxTxSize :: Natural,

       -- | The constant factor for the minimum fee calculation.
       --
       protocolParamTxFeeFixed :: Natural,

       -- | The linear factor for the minimum fee calculation.
       --
       protocolParamTxFeePerByte :: Natural,

       -- | The minimum permitted value for new UTxO entries, ie for
       -- transaction outputs.
       --
       protocolParamMinUTxOValue :: Maybe Lovelace,

       -- | The deposit required to register a stake address.
       --
       protocolParamStakeAddressDeposit :: Lovelace,

       -- | The deposit required to register a stake pool.
       --
       protocolParamStakePoolDeposit :: Lovelace,

       -- | The minimum value that stake pools are permitted to declare for
       -- their cost parameter.
       --
       protocolParamMinPoolCost :: Lovelace,

       -- | The maximum number of epochs into the future that stake pools
       -- are permitted to schedule a retirement.
       --
       protocolParamPoolRetireMaxEpoch :: EpochNo,

       -- | The equilibrium target number of stake pools.
       --
       -- This is the \"k\" incentives parameter from the design document.
       --
       protocolParamStakePoolTargetNum :: Natural,

       -- | The influence of the pledge in stake pool rewards.
       --
       -- This is the \"a_0\" incentives parameter from the design document.
       --
       protocolParamPoolPledgeInfluence :: Rational,

       -- | The monetary expansion rate. This determines the fraction of the
       -- reserves that are added to the fee pot each epoch.
       --
       -- This is the \"rho\" incentives parameter from the design document.
       --
       protocolParamMonetaryExpansion :: Rational,

       -- | The fraction of the fee pot each epoch that goes to the treasury.
       --
       -- This is the \"tau\" incentives parameter from the design document.
       --
       protocolParamTreasuryCut :: Rational,

       -- | Cost in ada per word of UTxO storage.
       --
       -- /Introduced in Alonzo/
       protocolParamUTxOCostPerWord :: Maybe Lovelace,

       -- | Cost models for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolParamCostModels :: Map AnyPlutusScriptVersion CostModel,

       -- | Price of execution units for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolParamPrices :: Maybe ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       --
       -- /Introduced in Alonzo/
       protocolParamMaxTxExUnits :: Maybe ExecutionUnits,

       -- | Max total script execution resources units allowed per block
       --
       -- /Introduced in Alonzo/
       protocolParamMaxBlockExUnits :: Maybe ExecutionUnits,

       -- | Max size of a Value in a tx output.
       --
       -- /Introduced in Alonzo/
       protocolParamMaxValueSize :: Maybe Natural,

       -- | The percentage of the script contribution to the txfee that must be
       -- provided as collateral inputs when including Plutus scripts.
       --
       -- /Introduced in Alonzo/
       protocolParamCollateralPercent :: Maybe Natural,

       -- | The maximum number of collateral inputs allowed in a transaction.
       --
       -- /Introduced in Alonzo/
       protocolParamMaxCollateralInputs :: Maybe Natural
    }
  deriving (Eq, Generic, Show)

instance FromJSON ProtocolParameters where
  parseJSON =
    withObject "ProtocolParameters" $ \o -> do
      v <- o .: "protocolVersion"
      ProtocolParameters
        <$> ((,) <$> v .: "major" <*> v .: "minor")
        <*> o .: "decentralization"
        <*> o .: "extraPraosEntropy"
        <*> o .: "maxBlockHeaderSize"
        <*> o .: "maxBlockBodySize"
        <*> o .: "maxTxSize"
        <*> o .: "txFeeFixed"
        <*> o .: "txFeePerByte"
        <*> o .: "minUTxOValue"
        <*> o .: "stakeAddressDeposit"
        <*> o .: "stakePoolDeposit"
        <*> o .: "minPoolCost"
        <*> o .: "poolRetireMaxEpoch"
        <*> o .: "stakePoolTargetNum"
        <*> o .: "poolPledgeInfluence"
        <*> o .: "monetaryExpansion"
        <*> o .: "treasuryCut"
        <*> o .:? "utxoCostPerWord"
        <*> o .:? "costModels" .!= Map.empty
        <*> o .:? "executionUnitPrices"
        <*> o .:? "maxTxExecutionUnits"
        <*> o .:? "maxBlockExecutionUnits"
        <*> o .:? "maxValueSize"
        <*> o .:? "collateralPercentage"
        <*> o .:? "maxCollateralInputs"

instance ToJSON ProtocolParameters where
  toJSON ProtocolParameters{..} =
    object
      [ "extraPraosEntropy"   .= protocolParamExtraPraosEntropy
      , "stakePoolTargetNum"  .= protocolParamStakePoolTargetNum
      , "minUTxOValue"        .= protocolParamMinUTxOValue
      , "poolRetireMaxEpoch"  .= protocolParamPoolRetireMaxEpoch
      , "decentralization"    .= toRationalJSON protocolParamDecentralization
      , "stakePoolDeposit"    .= protocolParamStakePoolDeposit
      , "maxBlockHeaderSize"  .= protocolParamMaxBlockHeaderSize
      , "maxBlockBodySize"    .= protocolParamMaxBlockBodySize
      , "maxTxSize"           .= protocolParamMaxTxSize
      , "treasuryCut"         .= toRationalJSON protocolParamTreasuryCut
      , "minPoolCost"         .= protocolParamMinPoolCost
      , "monetaryExpansion"   .= toRationalJSON protocolParamMonetaryExpansion
      , "stakeAddressDeposit" .= protocolParamStakeAddressDeposit
      , "poolPledgeInfluence" .= toRationalJSON protocolParamPoolPledgeInfluence
      , "protocolVersion"     .= let (major, minor) = protocolParamProtocolVersion
                                  in object ["major" .= major, "minor" .= minor]
      , "txFeeFixed"          .= protocolParamTxFeeFixed
      , "txFeePerByte"        .= protocolParamTxFeePerByte
      -- Alonzo era:
      , "utxoCostPerWord"        .= protocolParamUTxOCostPerWord
      , "costModels"             .= protocolParamCostModels
      , "executionUnitPrices"    .= protocolParamPrices
      , "maxTxExecutionUnits"    .= protocolParamMaxTxExUnits
      , "maxBlockExecutionUnits" .= protocolParamMaxBlockExUnits
      , "maxValueSize"           .= protocolParamMaxValueSize
      , "collateralPercentage"   .= protocolParamCollateralPercent
      , "maxCollateralInputs"    .= protocolParamMaxCollateralInputs
      ]


-- ----------------------------------------------------------------------------
-- Updates to the protocol parameters
--

-- | The representation of a change in the 'ProtocolParameters'.
--
data ProtocolParametersUpdate =
     ProtocolParametersUpdate {

       -- | Protocol version, major and minor. Updating the major version is
       -- used to trigger hard forks.
       --
       protocolUpdateProtocolVersion :: Maybe (Natural, Natural),

       -- | The decentralization parameter. This is fraction of slots that
       -- belong to the BFT overlay schedule, rather than the Praos schedule.
       -- So 1 means fully centralised, while 0 means fully decentralised.
       --
       -- This is the \"d\" parameter from the design document.
       --
       protocolUpdateDecentralization :: Maybe Rational,

       -- | Extra entropy for the Praos per-epoch nonce.
       --
       -- This can be used to add extra entropy during the decentralisation
       -- process. If the extra entropy can be demonstrated to be generated
       -- randomly then this method can be used to show that the initial
       -- federated operators did not subtly bias the initial schedule so that
       -- they retain undue influence after decentralisation.
       --
       protocolUpdateExtraPraosEntropy :: Maybe (Maybe PraosNonce),

       -- | The maximum permitted size of a block header.
       --
       -- This must be at least as big as the largest legitimate block headers
       -- but should not be too much larger, to help prevent DoS attacks.
       --
       -- Caution: setting this to be smaller than legitimate block headers is
       -- a sure way to brick the system!
       --
       protocolUpdateMaxBlockHeaderSize :: Maybe Natural,

       -- | The maximum permitted size of the block body (that is, the block
       -- payload, without the block header).
       --
       -- This should be picked with the Praos network delta security parameter
       -- in mind. Making this too large can severely weaken the Praos
       -- consensus properties.
       --
       -- Caution: setting this to be smaller than a transaction that can
       -- change the protocol parameters is a sure way to brick the system!
       --
       protocolUpdateMaxBlockBodySize :: Maybe Natural,

       -- | The maximum permitted size of a transaction.
       --
       -- Typically this should not be too high a fraction of the block size,
       -- otherwise wastage from block fragmentation becomes a problem, and
       -- the current implementation does not use any sophisticated box packing
       -- algorithm.
       --
       protocolUpdateMaxTxSize :: Maybe Natural,

       -- | The constant factor for the minimum fee calculation.
       --
       protocolUpdateTxFeeFixed :: Maybe Natural,

       -- | The linear factor for the minimum fee calculation.
       --
       protocolUpdateTxFeePerByte :: Maybe Natural,

       -- | The minimum permitted value for new UTxO entries, ie for
       -- transaction outputs.
       --
       protocolUpdateMinUTxOValue :: Maybe Lovelace,

       -- | The deposit required to register a stake address.
       --
       protocolUpdateStakeAddressDeposit :: Maybe Lovelace,

       -- | The deposit required to register a stake pool.
       --
       protocolUpdateStakePoolDeposit :: Maybe Lovelace,

       -- | The minimum value that stake pools are permitted to declare for
       -- their cost parameter.
       --
       protocolUpdateMinPoolCost :: Maybe Lovelace,

       -- | The maximum number of epochs into the future that stake pools
       -- are permitted to schedule a retirement.
       --
       protocolUpdatePoolRetireMaxEpoch :: Maybe EpochNo,

       -- | The equilibrium target number of stake pools.
       --
       -- This is the \"k\" incentives parameter from the design document.
       --
       protocolUpdateStakePoolTargetNum :: Maybe Natural,

       -- | The influence of the pledge in stake pool rewards.
       --
       -- This is the \"a_0\" incentives parameter from the design document.
       --
       protocolUpdatePoolPledgeInfluence :: Maybe Rational,

       -- | The monetary expansion rate. This determines the fraction of the
       -- reserves that are added to the fee pot each epoch.
       --
       -- This is the \"rho\" incentives parameter from the design document.
       --
       protocolUpdateMonetaryExpansion :: Maybe Rational,

       -- | The fraction of the fee pot each epoch that goes to the treasury.
       --
       -- This is the \"tau\" incentives parameter from the design document.
       --
       protocolUpdateTreasuryCut :: Maybe Rational,

       -- Introduced in Alonzo

       -- | Cost in ada per word of UTxO storage.
       --
       -- /Introduced in Alonzo/
       protocolUpdateUTxOCostPerWord :: Maybe Lovelace,

       -- | Cost models for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolUpdateCostModels :: Map AnyPlutusScriptVersion CostModel,

       -- | Price of execution units for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolUpdatePrices :: Maybe ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxTxExUnits :: Maybe ExecutionUnits,

       -- | Max total script execution resources units allowed per block
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxBlockExUnits :: Maybe ExecutionUnits,

       -- | Max size of a 'Value' in a tx output.
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxValueSize :: Maybe Natural,

       -- | The percentage of the script contribution to the txfee that must be
       -- provided as collateral inputs when including Plutus scripts.
       --
       -- /Introduced in Alonzo/
       protocolUpdateCollateralPercent :: Maybe Natural,

       -- | The maximum number of collateral inputs allowed in a transaction.
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxCollateralInputs :: Maybe Natural
    }
  deriving (Eq, Show)

instance Semigroup ProtocolParametersUpdate where
    ppu1 <> ppu2 =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = merge protocolUpdateProtocolVersion
      , protocolUpdateDecentralization    = merge protocolUpdateDecentralization
      , protocolUpdateExtraPraosEntropy   = merge protocolUpdateExtraPraosEntropy
      , protocolUpdateMaxBlockHeaderSize  = merge protocolUpdateMaxBlockHeaderSize
      , protocolUpdateMaxBlockBodySize    = merge protocolUpdateMaxBlockBodySize
      , protocolUpdateMaxTxSize           = merge protocolUpdateMaxTxSize
      , protocolUpdateTxFeeFixed          = merge protocolUpdateTxFeeFixed
      , protocolUpdateTxFeePerByte        = merge protocolUpdateTxFeePerByte
      , protocolUpdateMinUTxOValue        = merge protocolUpdateMinUTxOValue
      , protocolUpdateStakeAddressDeposit = merge protocolUpdateStakeAddressDeposit
      , protocolUpdateStakePoolDeposit    = merge protocolUpdateStakePoolDeposit
      , protocolUpdateMinPoolCost         = merge protocolUpdateMinPoolCost
      , protocolUpdatePoolRetireMaxEpoch  = merge protocolUpdatePoolRetireMaxEpoch
      , protocolUpdateStakePoolTargetNum  = merge protocolUpdateStakePoolTargetNum
      , protocolUpdatePoolPledgeInfluence = merge protocolUpdatePoolPledgeInfluence
      , protocolUpdateMonetaryExpansion   = merge protocolUpdateMonetaryExpansion
      , protocolUpdateTreasuryCut         = merge protocolUpdateTreasuryCut
      -- Introduced in Alonzo below.
      , protocolUpdateUTxOCostPerWord     = merge protocolUpdateUTxOCostPerWord
      , protocolUpdateCostModels          = mergeMap protocolUpdateCostModels
      , protocolUpdatePrices              = merge protocolUpdatePrices
      , protocolUpdateMaxTxExUnits        = merge protocolUpdateMaxTxExUnits
      , protocolUpdateMaxBlockExUnits     = merge protocolUpdateMaxBlockExUnits
      , protocolUpdateMaxValueSize        = merge protocolUpdateMaxValueSize
      , protocolUpdateCollateralPercent   = merge protocolUpdateCollateralPercent
      , protocolUpdateMaxCollateralInputs = merge protocolUpdateMaxCollateralInputs
      }
      where
        -- prefer the right hand side:
        merge :: (ProtocolParametersUpdate -> Maybe a) -> Maybe a
        merge f = f ppu2 `mplus` f ppu1

        -- prefer the right hand side:
        mergeMap :: Ord k => (ProtocolParametersUpdate -> Map k a) -> Map k a
        mergeMap f = f ppu2 `Map.union` f ppu1

instance Monoid ProtocolParametersUpdate where
    mempty =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = Nothing
      , protocolUpdateDecentralization    = Nothing
      , protocolUpdateExtraPraosEntropy   = Nothing
      , protocolUpdateMaxBlockHeaderSize  = Nothing
      , protocolUpdateMaxBlockBodySize    = Nothing
      , protocolUpdateMaxTxSize           = Nothing
      , protocolUpdateTxFeeFixed          = Nothing
      , protocolUpdateTxFeePerByte        = Nothing
      , protocolUpdateMinUTxOValue        = Nothing
      , protocolUpdateStakeAddressDeposit = Nothing
      , protocolUpdateStakePoolDeposit    = Nothing
      , protocolUpdateMinPoolCost         = Nothing
      , protocolUpdatePoolRetireMaxEpoch  = Nothing
      , protocolUpdateStakePoolTargetNum  = Nothing
      , protocolUpdatePoolPledgeInfluence = Nothing
      , protocolUpdateMonetaryExpansion   = Nothing
      , protocolUpdateTreasuryCut         = Nothing
      , protocolUpdateUTxOCostPerWord     = Nothing
      , protocolUpdateCostModels          = mempty
      , protocolUpdatePrices              = Nothing
      , protocolUpdateMaxTxExUnits        = Nothing
      , protocolUpdateMaxBlockExUnits     = Nothing
      , protocolUpdateMaxValueSize        = Nothing
      , protocolUpdateCollateralPercent   = Nothing
      , protocolUpdateMaxCollateralInputs = Nothing
      }

instance ToCBOR ProtocolParametersUpdate where
    toCBOR ProtocolParametersUpdate{..} =
        CBOR.encodeListLen 25
     <> toCBOR protocolUpdateProtocolVersion
     <> toCBOR protocolUpdateDecentralization
     <> toCBOR protocolUpdateExtraPraosEntropy
     <> toCBOR protocolUpdateMaxBlockHeaderSize
     <> toCBOR protocolUpdateMaxBlockBodySize
     <> toCBOR protocolUpdateMaxTxSize
     <> toCBOR protocolUpdateTxFeeFixed
     <> toCBOR protocolUpdateTxFeePerByte
     <> toCBOR protocolUpdateMinUTxOValue
     <> toCBOR protocolUpdateStakeAddressDeposit
     <> toCBOR protocolUpdateStakePoolDeposit
     <> toCBOR protocolUpdateMinPoolCost
     <> toCBOR protocolUpdatePoolRetireMaxEpoch
     <> toCBOR protocolUpdateStakePoolTargetNum
     <> toCBOR protocolUpdatePoolPledgeInfluence
     <> toCBOR protocolUpdateMonetaryExpansion
     <> toCBOR protocolUpdateTreasuryCut
     <> toCBOR protocolUpdateUTxOCostPerWord
     <> toCBOR protocolUpdateCostModels
     <> toCBOR protocolUpdatePrices
     <> toCBOR protocolUpdateMaxTxExUnits
     <> toCBOR protocolUpdateMaxBlockExUnits
     <> toCBOR protocolUpdateMaxValueSize
     <> toCBOR protocolUpdateCollateralPercent
     <> toCBOR protocolUpdateMaxCollateralInputs

instance FromCBOR ProtocolParametersUpdate where
    fromCBOR = do
      CBOR.enforceSize "ProtocolParametersUpdate" 25
      ProtocolParametersUpdate
        <$> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR


-- ----------------------------------------------------------------------------
-- Praos nonce
--

newtype PraosNonce = PraosNonce (Ledger.Hash StandardCrypto ByteString)
  deriving stock (Eq, Ord, Generic)
  deriving (Show, IsString)   via UsingRawBytesHex PraosNonce
  deriving (ToJSON, FromJSON) via UsingRawBytesHex PraosNonce
  deriving (ToCBOR, FromCBOR) via UsingRawBytes    PraosNonce

instance HasTypeProxy PraosNonce where
    data AsType PraosNonce = AsPraosNonce
    proxyToAsType _ = AsPraosNonce

instance SerialiseAsRawBytes PraosNonce where
    serialiseToRawBytes (PraosNonce h) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsPraosNonce bs =
      PraosNonce <$> Crypto.hashFromBytes bs


makePraosNonce :: ByteString -> PraosNonce
makePraosNonce = PraosNonce . Crypto.hashWith id

toLedgerNonce :: Maybe PraosNonce -> Ledger.Nonce
toLedgerNonce Nothing               = Ledger.NeutralNonce
toLedgerNonce (Just (PraosNonce h)) = Ledger.Nonce (Crypto.castHash h)

fromLedgerNonce :: Ledger.Nonce -> Maybe PraosNonce
fromLedgerNonce Ledger.NeutralNonce = Nothing
fromLedgerNonce (Ledger.Nonce h)    = Just (PraosNonce (Crypto.castHash h))


-- ----------------------------------------------------------------------------
-- Script execution unit prices and cost models
--

-- | The prices for 'ExecutionUnits' as a fraction of a 'Lovelace'.
--
-- These are used to determine the fee for the use of a script within a
-- transaction, based on the 'ExecutionUnits' needed by the use of the script.
--
data ExecutionUnitPrices =
     ExecutionUnitPrices {
       priceExecutionSteps  :: Rational,
       priceExecutionMemory :: Rational
     }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnitPrices where
  toCBOR ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
      CBOR.encodeListLen 2
   <> toCBOR priceExecutionSteps
   <> toCBOR priceExecutionMemory

instance FromCBOR ExecutionUnitPrices where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnitPrices" 2
    ExecutionUnitPrices
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnitPrices where
  toJSON ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    object [ "priceSteps"  .= toRationalJSON priceExecutionSteps
           , "priceMemory" .= toRationalJSON priceExecutionMemory
           ]

instance FromJSON ExecutionUnitPrices where
  parseJSON =
    withObject "ExecutionUnitPrices" $ \o ->
      ExecutionUnitPrices
        <$> o .: "priceSteps"
        <*> o .: "priceMemory"


toAlonzoPrices :: ExecutionUnitPrices -> Maybe Alonzo.Prices
toAlonzoPrices ExecutionUnitPrices {
                 priceExecutionSteps,
                 priceExecutionMemory
               } = do
  prSteps <- Ledger.boundRational priceExecutionSteps
  prMem   <- Ledger.boundRational priceExecutionMemory
  return Alonzo.Prices {
    Alonzo.prSteps,
    Alonzo.prMem
  }

fromAlonzoPrices :: Alonzo.Prices -> ExecutionUnitPrices
fromAlonzoPrices Alonzo.Prices{Alonzo.prSteps, Alonzo.prMem} =
  ExecutionUnitPrices {
    priceExecutionSteps  = Ledger.unboundRational prSteps,
    priceExecutionMemory = Ledger.unboundRational prMem
  }


-- ----------------------------------------------------------------------------
-- Script cost models
--

newtype CostModel = CostModel (Map Text Integer)
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (ToCBOR, FromCBOR)

validateCostModel :: PlutusScriptVersion lang
                  -> CostModel
                  -> Either InvalidCostModel ()
validateCostModel PlutusScriptV1 (CostModel m)
  | Alonzo.validateCostModelParams m = Right ()
  | otherwise                        = Left (InvalidCostModel (CostModel m))
validateCostModel PlutusScriptV2 (CostModel m)
  | Alonzo.validateCostModelParams m = Right ()
  | otherwise                        = Left (InvalidCostModel (CostModel m))

-- TODO alonzo: it'd be nice if the library told us what was wrong
newtype InvalidCostModel = InvalidCostModel CostModel
  deriving Show

instance Error InvalidCostModel where
  displayError (InvalidCostModel cm) =
    "Invalid cost model: " ++ show cm


toAlonzoCostModels
  :: Map AnyPlutusScriptVersion CostModel
  -> Map Alonzo.Language Alonzo.CostModel
toAlonzoCostModels =
    Map.fromList
  . map (bimap toAlonzoScriptLanguage toAlonzoCostModel)
  . Map.toList

fromAlonzoCostModels
  :: Map Alonzo.Language Alonzo.CostModel
  -> Map AnyPlutusScriptVersion CostModel
fromAlonzoCostModels =
    Map.fromList
  . map (bimap fromAlonzoScriptLanguage fromAlonzoCostModel)
  . Map.toList

toAlonzoScriptLanguage :: AnyPlutusScriptVersion -> Alonzo.Language
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Alonzo.PlutusV1
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Alonzo.PlutusV2

fromAlonzoScriptLanguage :: Alonzo.Language -> AnyPlutusScriptVersion
fromAlonzoScriptLanguage Alonzo.PlutusV1 = AnyPlutusScriptVersion PlutusScriptV1
fromAlonzoScriptLanguage Alonzo.PlutusV2 = AnyPlutusScriptVersion PlutusScriptV2

toAlonzoCostModel :: CostModel -> Alonzo.CostModel
toAlonzoCostModel (CostModel m) = Alonzo.CostModel m

fromAlonzoCostModel :: Alonzo.CostModel -> CostModel
fromAlonzoCostModel (Alonzo.CostModel m) = CostModel m


-- ----------------------------------------------------------------------------
-- Proposals embedded in transactions to update protocol parameters
--

data UpdateProposal =
     UpdateProposal
       !(Map (Hash GenesisKey) ProtocolParametersUpdate)
       !EpochNo
    deriving stock (Eq, Show)
    deriving anyclass SerialiseAsCBOR

instance HasTypeProxy UpdateProposal where
    data AsType UpdateProposal = AsUpdateProposal
    proxyToAsType _ = AsUpdateProposal

instance HasTextEnvelope UpdateProposal where
    textEnvelopeType _ = "UpdateProposalShelley"

instance ToCBOR UpdateProposal where
    toCBOR (UpdateProposal ppup epochno) =
        CBOR.encodeListLen 2
     <> toCBOR ppup
     <> toCBOR epochno

instance FromCBOR UpdateProposal where
    fromCBOR = do
      CBOR.enforceSize "ProtocolParametersUpdate" 2
      UpdateProposal
        <$> fromCBOR
        <*> fromCBOR

makeShelleyUpdateProposal :: ProtocolParametersUpdate
                          -> [Hash GenesisKey]
                          -> EpochNo
                          -> UpdateProposal
makeShelleyUpdateProposal params genesisKeyHashes =
    --TODO decide how to handle parameter validation
    --     for example we need to validate the Rational values can convert
    --     into the UnitInterval type ok.
    UpdateProposal (Map.fromList [ (kh, params) | kh <- genesisKeyHashes ])


-- ----------------------------------------------------------------------------
-- Conversion functions: updates to ledger types
--

toLedgerUpdate :: forall era ledgerera.
                  ShelleyLedgerEra era ~ ledgerera
               => Ledger.Crypto ledgerera ~ StandardCrypto
               => ShelleyBasedEra era
               -> UpdateProposal
               -> Ledger.Update ledgerera
toLedgerUpdate era (UpdateProposal ppup epochno) =
    Ledger.Update (toLedgerProposedPPUpdates era ppup) epochno


toLedgerProposedPPUpdates :: forall era ledgerera.
                             ShelleyLedgerEra era ~ ledgerera
                          => Ledger.Crypto ledgerera ~ StandardCrypto
                          => ShelleyBasedEra era
                          -> Map (Hash GenesisKey) ProtocolParametersUpdate
                          -> Ledger.ProposedPPUpdates ledgerera
toLedgerProposedPPUpdates era =
    Ledger.ProposedPPUpdates
  . Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh)
  . Map.map (toLedgerPParamsDelta era)


toLedgerPParamsDelta :: ShelleyBasedEra era
                     -> ProtocolParametersUpdate
                     -> Ledger.PParamsDelta (ShelleyLedgerEra era)
toLedgerPParamsDelta ShelleyBasedEraShelley = toShelleyPParamsUpdate
toLedgerPParamsDelta ShelleyBasedEraAllegra = toShelleyPParamsUpdate
toLedgerPParamsDelta ShelleyBasedEraMary    = toShelleyPParamsUpdate
toLedgerPParamsDelta ShelleyBasedEraAlonzo  = toAlonzoPParamsUpdate


--TODO: we should do validation somewhere, not just silently drop changes that
-- are not valid. Specifically, see Ledger.boundRational below.
toShelleyPParamsUpdate :: ProtocolParametersUpdate
                       -> Shelley.PParamsUpdate ledgerera
toShelleyPParamsUpdate
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraPraosEntropy
    , protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateMinUTxOValue
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    } =
    Shelley.PParams {
      Shelley._minfeeA     = noInlineMaybeToStrictMaybe protocolUpdateTxFeePerByte
    , Shelley._minfeeB     = noInlineMaybeToStrictMaybe protocolUpdateTxFeeFixed
    , Shelley._maxBBSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , Shelley._maxTxSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxTxSize
    , Shelley._maxBHSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , Shelley._keyDeposit  = toShelleyLovelace <$>
                               noInlineMaybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , Shelley._poolDeposit = toShelleyLovelace <$>
                               noInlineMaybeToStrictMaybe protocolUpdateStakePoolDeposit
    , Shelley._eMax        = noInlineMaybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , Shelley._nOpt        = noInlineMaybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , Shelley._a0          = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                              protocolUpdatePoolPledgeInfluence
    , Shelley._rho         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                                protocolUpdateMonetaryExpansion
    , Shelley._tau         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                                protocolUpdateTreasuryCut
    , Shelley._d           = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                                protocolUpdateDecentralization
    , Shelley._extraEntropy    = toLedgerNonce <$>
                                   noInlineMaybeToStrictMaybe protocolUpdateExtraPraosEntropy
    , Shelley._protocolVersion = uncurry Ledger.ProtVer <$>
                                   noInlineMaybeToStrictMaybe protocolUpdateProtocolVersion
    , Shelley._minUTxOValue    = toShelleyLovelace <$>
                                   noInlineMaybeToStrictMaybe protocolUpdateMinUTxOValue
    , Shelley._minPoolCost     = toShelleyLovelace <$>
                                   noInlineMaybeToStrictMaybe protocolUpdateMinPoolCost
    }


toAlonzoPParamsUpdate :: ProtocolParametersUpdate
                      -> Alonzo.PParamsUpdate ledgerera
toAlonzoPParamsUpdate
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraPraosEntropy
    , protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    , protocolUpdateUTxOCostPerWord
    , protocolUpdateCostModels
    , protocolUpdatePrices
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxCollateralInputs
    } =
    Alonzo.PParams {
      Alonzo._minfeeA     = noInlineMaybeToStrictMaybe protocolUpdateTxFeePerByte
    , Alonzo._minfeeB     = noInlineMaybeToStrictMaybe protocolUpdateTxFeeFixed
    , Alonzo._maxBBSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , Alonzo._maxTxSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxTxSize
    , Alonzo._maxBHSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , Alonzo._keyDeposit  = toShelleyLovelace <$>
                              noInlineMaybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , Alonzo._poolDeposit = toShelleyLovelace <$>
                              noInlineMaybeToStrictMaybe protocolUpdateStakePoolDeposit
    , Alonzo._eMax        = noInlineMaybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , Alonzo._nOpt        = noInlineMaybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , Alonzo._a0          = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                              protocolUpdatePoolPledgeInfluence
    , Alonzo._rho         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                               protocolUpdateMonetaryExpansion
    , Alonzo._tau         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                               protocolUpdateTreasuryCut
    , Alonzo._d           = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                               protocolUpdateDecentralization
    , Alonzo._extraEntropy    = toLedgerNonce <$>
                                  noInlineMaybeToStrictMaybe protocolUpdateExtraPraosEntropy
    , Alonzo._protocolVersion = uncurry Ledger.ProtVer <$>
                                  noInlineMaybeToStrictMaybe protocolUpdateProtocolVersion
    , Alonzo._minPoolCost     = toShelleyLovelace <$>
                                  noInlineMaybeToStrictMaybe protocolUpdateMinPoolCost
    , Alonzo._coinsPerUTxOWord  = toShelleyLovelace <$>
                                  noInlineMaybeToStrictMaybe protocolUpdateUTxOCostPerWord
    , Alonzo._costmdls        = if Map.null protocolUpdateCostModels
                                  then Ledger.SNothing
                                  else Ledger.SJust
                                         (toAlonzoCostModels protocolUpdateCostModels)
    , Alonzo._prices          = noInlineMaybeToStrictMaybe $
                                  toAlonzoPrices =<< protocolUpdatePrices
    , Alonzo._maxTxExUnits    = toAlonzoExUnits  <$>
                                  noInlineMaybeToStrictMaybe protocolUpdateMaxTxExUnits
    , Alonzo._maxBlockExUnits = toAlonzoExUnits  <$>
                                  noInlineMaybeToStrictMaybe protocolUpdateMaxBlockExUnits
    , Alonzo._maxValSize      = noInlineMaybeToStrictMaybe protocolUpdateMaxValueSize
    , Alonzo._collateralPercentage = noInlineMaybeToStrictMaybe protocolUpdateCollateralPercent
    , Alonzo._maxCollateralInputs  = noInlineMaybeToStrictMaybe protocolUpdateMaxCollateralInputs
    }


-- ----------------------------------------------------------------------------
-- Conversion functions: updates from ledger types
--

fromLedgerUpdate :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => Ledger.Crypto ledgerera ~ StandardCrypto
                 => ShelleyBasedEra era
                 -> Ledger.Update ledgerera
                 -> UpdateProposal
fromLedgerUpdate era (Ledger.Update ppup epochno) =
    UpdateProposal (fromLedgerProposedPPUpdates era ppup) epochno


fromLedgerProposedPPUpdates :: forall era ledgerera.
                               ShelleyLedgerEra era ~ ledgerera
                            => Ledger.Crypto ledgerera ~ StandardCrypto
                            => ShelleyBasedEra era
                            -> Ledger.ProposedPPUpdates ledgerera
                            -> Map (Hash GenesisKey) ProtocolParametersUpdate
fromLedgerProposedPPUpdates era =
    Map.map (fromLedgerPParamsDelta era)
  . Map.mapKeysMonotonic GenesisKeyHash
  . (\(Ledger.ProposedPPUpdates ppup) -> ppup)


fromLedgerPParamsDelta :: ShelleyBasedEra era
                       -> Ledger.PParamsDelta (ShelleyLedgerEra era)
                       -> ProtocolParametersUpdate
fromLedgerPParamsDelta ShelleyBasedEraShelley = fromShelleyPParamsUpdate
fromLedgerPParamsDelta ShelleyBasedEraAllegra = fromShelleyPParamsUpdate
fromLedgerPParamsDelta ShelleyBasedEraMary    = fromShelleyPParamsUpdate
fromLedgerPParamsDelta ShelleyBasedEraAlonzo  = fromAlonzoPParamsUpdate


fromShelleyPParamsUpdate :: Shelley.PParamsUpdate ledgerera
                         -> ProtocolParametersUpdate
fromShelleyPParamsUpdate
    Shelley.PParams {
      Shelley._minfeeA
    , Shelley._minfeeB
    , Shelley._maxBBSize
    , Shelley._maxTxSize
    , Shelley._maxBHSize
    , Shelley._keyDeposit
    , Shelley._poolDeposit
    , Shelley._eMax
    , Shelley._nOpt
    , Shelley._a0
    , Shelley._rho
    , Shelley._tau
    , Shelley._d
    , Shelley._extraEntropy
    , Shelley._protocolVersion
    , Shelley._minUTxOValue
    , Shelley._minPoolCost
    } =
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b)) <$>
                                          strictMaybeToMaybe _protocolVersion
    , protocolUpdateDecentralization    = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _d
    , protocolUpdateExtraPraosEntropy   = fromLedgerNonce <$>
                                            strictMaybeToMaybe _extraEntropy
    , protocolUpdateMaxBlockHeaderSize  = strictMaybeToMaybe _maxBHSize
    , protocolUpdateMaxBlockBodySize    = strictMaybeToMaybe _maxBBSize
    , protocolUpdateMaxTxSize           = strictMaybeToMaybe _maxTxSize
    , protocolUpdateTxFeeFixed          = strictMaybeToMaybe _minfeeB
    , protocolUpdateTxFeePerByte        = strictMaybeToMaybe _minfeeA
    , protocolUpdateMinUTxOValue        = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _minUTxOValue
    , protocolUpdateStakeAddressDeposit = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _keyDeposit
    , protocolUpdateStakePoolDeposit    = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _poolDeposit
    , protocolUpdateMinPoolCost         = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _minPoolCost
    , protocolUpdatePoolRetireMaxEpoch  = strictMaybeToMaybe _eMax
    , protocolUpdateStakePoolTargetNum  = strictMaybeToMaybe _nOpt
    , protocolUpdatePoolPledgeInfluence = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _a0
    , protocolUpdateMonetaryExpansion   = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _rho
    , protocolUpdateTreasuryCut         = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _tau
    , protocolUpdateUTxOCostPerWord     = Nothing
    , protocolUpdateCostModels          = mempty
    , protocolUpdatePrices              = Nothing
    , protocolUpdateMaxTxExUnits        = Nothing
    , protocolUpdateMaxBlockExUnits     = Nothing
    , protocolUpdateMaxValueSize        = Nothing
    , protocolUpdateCollateralPercent   = Nothing
    , protocolUpdateMaxCollateralInputs = Nothing
    }

fromAlonzoPParamsUpdate :: Alonzo.PParamsUpdate ledgerera
                        -> ProtocolParametersUpdate
fromAlonzoPParamsUpdate
    Alonzo.PParams {
      Alonzo._minfeeA
    , Alonzo._minfeeB
    , Alonzo._maxBBSize
    , Alonzo._maxTxSize
    , Alonzo._maxBHSize
    , Alonzo._keyDeposit
    , Alonzo._poolDeposit
    , Alonzo._eMax
    , Alonzo._nOpt
    , Alonzo._a0
    , Alonzo._rho
    , Alonzo._tau
    , Alonzo._d
    , Alonzo._extraEntropy
    , Alonzo._protocolVersion
    , Alonzo._minPoolCost
    , Alonzo._coinsPerUTxOWord
    , Alonzo._costmdls
    , Alonzo._prices
    , Alonzo._maxTxExUnits
    , Alonzo._maxBlockExUnits
    , Alonzo._maxValSize
    , Alonzo._collateralPercentage
    , Alonzo._maxCollateralInputs
    } =
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b)) <$>
                                          strictMaybeToMaybe _protocolVersion
    , protocolUpdateDecentralization    = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _d
    , protocolUpdateExtraPraosEntropy   = fromLedgerNonce <$>
                                            strictMaybeToMaybe _extraEntropy
    , protocolUpdateMaxBlockHeaderSize  = strictMaybeToMaybe _maxBHSize
    , protocolUpdateMaxBlockBodySize    = strictMaybeToMaybe _maxBBSize
    , protocolUpdateMaxTxSize           = strictMaybeToMaybe _maxTxSize
    , protocolUpdateTxFeeFixed          = strictMaybeToMaybe _minfeeB
    , protocolUpdateTxFeePerByte        = strictMaybeToMaybe _minfeeA
    , protocolUpdateMinUTxOValue        = Nothing
    , protocolUpdateStakeAddressDeposit = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _keyDeposit
    , protocolUpdateStakePoolDeposit    = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _poolDeposit
    , protocolUpdateMinPoolCost         = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _minPoolCost
    , protocolUpdatePoolRetireMaxEpoch  = strictMaybeToMaybe _eMax
    , protocolUpdateStakePoolTargetNum  = strictMaybeToMaybe _nOpt
    , protocolUpdatePoolPledgeInfluence = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _a0
    , protocolUpdateMonetaryExpansion   = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _rho
    , protocolUpdateTreasuryCut         = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _tau
    , protocolUpdateUTxOCostPerWord     = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _coinsPerUTxOWord
    , protocolUpdateCostModels          = maybe mempty fromAlonzoCostModels
                                               (strictMaybeToMaybe _costmdls)
    , protocolUpdatePrices              = fromAlonzoPrices <$>
                                            strictMaybeToMaybe _prices
    , protocolUpdateMaxTxExUnits        = fromAlonzoExUnits <$>
                                            strictMaybeToMaybe _maxTxExUnits
    , protocolUpdateMaxBlockExUnits     = fromAlonzoExUnits <$>
                                            strictMaybeToMaybe _maxBlockExUnits
    , protocolUpdateMaxValueSize        = strictMaybeToMaybe _maxValSize
    , protocolUpdateCollateralPercent   = strictMaybeToMaybe _collateralPercentage
    , protocolUpdateMaxCollateralInputs = strictMaybeToMaybe _maxCollateralInputs
    }


-- ----------------------------------------------------------------------------
-- Conversion functions: protocol parameters to ledger types
--

--TODO: this has to be a Maybe or Either for some of the parameter validation.
-- Both parameters that must be present or absent in specific eras,
-- and parameter values that need validation, such as the Rational values
toLedgerPParams
  :: ShelleyBasedEra era
  -> ProtocolParameters
  -> Ledger.PParams (ShelleyLedgerEra era)
toLedgerPParams ShelleyBasedEraShelley = toShelleyPParams
toLedgerPParams ShelleyBasedEraAllegra = toShelleyPParams
toLedgerPParams ShelleyBasedEraMary    = toShelleyPParams
toLedgerPParams ShelleyBasedEraAlonzo  = toAlonzoPParams

toShelleyPParams :: ProtocolParameters -> Shelley.PParams ledgerera
toShelleyPParams ProtocolParameters {
                   protocolParamProtocolVersion,
                   protocolParamDecentralization,
                   protocolParamExtraPraosEntropy,
                   protocolParamMaxBlockHeaderSize,
                   protocolParamMaxBlockBodySize,
                   protocolParamMaxTxSize,
                   protocolParamTxFeeFixed,
                   protocolParamTxFeePerByte,
                   protocolParamMinUTxOValue = Just minUTxOValue,
                   protocolParamStakeAddressDeposit,
                   protocolParamStakePoolDeposit,
                   protocolParamMinPoolCost,
                   protocolParamPoolRetireMaxEpoch,
                   protocolParamStakePoolTargetNum,
                   protocolParamPoolPledgeInfluence,
                   protocolParamMonetaryExpansion,
                   protocolParamTreasuryCut
                 } =
   Shelley.PParams
     { Shelley._protocolVersion
                             = let (maj, minor) = protocolParamProtocolVersion
                                in Ledger.ProtVer maj minor
     , Shelley._d            = fromMaybe
                                 (error "toAlonzoPParams: invalid Decentralization value")
                                 (Ledger.boundRational protocolParamDecentralization)
     , Shelley._extraEntropy = toLedgerNonce protocolParamExtraPraosEntropy
     , Shelley._maxBHSize    = protocolParamMaxBlockHeaderSize
     , Shelley._maxBBSize    = protocolParamMaxBlockBodySize
     , Shelley._maxTxSize    = protocolParamMaxTxSize
     , Shelley._minfeeB      = protocolParamTxFeeFixed
     , Shelley._minfeeA      = protocolParamTxFeePerByte
     , Shelley._minUTxOValue = toShelleyLovelace minUTxOValue
     , Shelley._keyDeposit   = toShelleyLovelace protocolParamStakeAddressDeposit
     , Shelley._poolDeposit  = toShelleyLovelace protocolParamStakePoolDeposit
     , Shelley._minPoolCost  = toShelleyLovelace protocolParamMinPoolCost
     , Shelley._eMax         = protocolParamPoolRetireMaxEpoch
     , Shelley._nOpt         = protocolParamStakePoolTargetNum
     , Shelley._a0           = fromMaybe
                                 (error "toAlonzoPParams: invalid PoolPledgeInfluence value")
                                 (Ledger.boundRational protocolParamPoolPledgeInfluence)
     , Shelley._rho          = fromMaybe
                                 (error "toAlonzoPParams: invalid MonetaryExpansion value")
                                 (Ledger.boundRational protocolParamMonetaryExpansion)
     , Shelley._tau          = fromMaybe
                                 (error "toAlonzoPParams: invalid TreasuryCut value")
                                 (Ledger.boundRational protocolParamTreasuryCut)
     }
toShelleyPParams ProtocolParameters { protocolParamMinUTxOValue = Nothing } =
  error "toShelleyPParams: must specify protocolParamMinUTxOValue"

toAlonzoPParams :: ProtocolParameters -> Alonzo.PParams ledgerera
toAlonzoPParams ProtocolParameters {
                   protocolParamProtocolVersion,
                   protocolParamDecentralization,
                   protocolParamExtraPraosEntropy,
                   protocolParamMaxBlockHeaderSize,
                   protocolParamMaxBlockBodySize,
                   protocolParamMaxTxSize,
                   protocolParamTxFeeFixed,
                   protocolParamTxFeePerByte,
                   protocolParamStakeAddressDeposit,
                   protocolParamStakePoolDeposit,
                   protocolParamMinPoolCost,
                   protocolParamPoolRetireMaxEpoch,
                   protocolParamStakePoolTargetNum,
                   protocolParamPoolPledgeInfluence,
                   protocolParamMonetaryExpansion,
                   protocolParamTreasuryCut,
                   protocolParamUTxOCostPerWord = Just utxoCostPerWord,
                   protocolParamCostModels,
                   protocolParamPrices          = Just prices,
                   protocolParamMaxTxExUnits    = Just maxTxExUnits,
                   protocolParamMaxBlockExUnits = Just maxBlockExUnits,
                   protocolParamMaxValueSize    = Just maxValueSize,
                   protocolParamCollateralPercent   = Just collateralPercentage,
                   protocolParamMaxCollateralInputs = Just maxCollateralInputs
                 } =
    Alonzo.PParams {
      Alonzo._protocolVersion
                           = let (maj, minor) = protocolParamProtocolVersion
                              in Ledger.ProtVer maj minor
    , Alonzo._d            = fromMaybe
                               (error "toAlonzoPParams: invalid Decentralization value")
                               (Ledger.boundRational protocolParamDecentralization)
    , Alonzo._extraEntropy = toLedgerNonce protocolParamExtraPraosEntropy
    , Alonzo._maxBHSize    = protocolParamMaxBlockHeaderSize
    , Alonzo._maxBBSize    = protocolParamMaxBlockBodySize
    , Alonzo._maxTxSize    = protocolParamMaxTxSize
    , Alonzo._minfeeB      = protocolParamTxFeeFixed
    , Alonzo._minfeeA      = protocolParamTxFeePerByte
    , Alonzo._keyDeposit   = toShelleyLovelace protocolParamStakeAddressDeposit
    , Alonzo._poolDeposit  = toShelleyLovelace protocolParamStakePoolDeposit
    , Alonzo._minPoolCost  = toShelleyLovelace protocolParamMinPoolCost
    , Alonzo._eMax         = protocolParamPoolRetireMaxEpoch
    , Alonzo._nOpt         = protocolParamStakePoolTargetNum
    , Alonzo._a0           = fromMaybe
                               (error "toAlonzoPParams: invalid PoolPledgeInfluence value")
                               (Ledger.boundRational protocolParamPoolPledgeInfluence)
    , Alonzo._rho          = fromMaybe
                               (error "toAlonzoPParams: invalid MonetaryExpansion value")
                               (Ledger.boundRational protocolParamMonetaryExpansion)
    , Alonzo._tau          = fromMaybe
                               (error "toAlonzoPParams: invalid TreasuryCut value")
                               (Ledger.boundRational protocolParamTreasuryCut)

      -- New params in Alonzo:
    , Alonzo._coinsPerUTxOWord  = toShelleyLovelace utxoCostPerWord
    , Alonzo._costmdls        = toAlonzoCostModels protocolParamCostModels
    , Alonzo._prices          = fromMaybe
                                  (error "toAlonzoPParams: invalid Price values")
                                  (toAlonzoPrices prices)
    , Alonzo._maxTxExUnits    = toAlonzoExUnits maxTxExUnits
    , Alonzo._maxBlockExUnits = toAlonzoExUnits maxBlockExUnits
    , Alonzo._maxValSize      = maxValueSize
    , Alonzo._collateralPercentage = collateralPercentage
    , Alonzo._maxCollateralInputs  = maxCollateralInputs
    }
toAlonzoPParams ProtocolParameters { protocolParamUTxOCostPerWord = Nothing } =
  error "toAlonzoPParams: must specify protocolParamUTxOCostPerWord"
toAlonzoPParams ProtocolParameters { protocolParamPrices          = Nothing } =
  error "toAlonzoPParams: must specify protocolParamPrices"
toAlonzoPParams ProtocolParameters { protocolParamMaxTxExUnits    = Nothing } =
  error "toAlonzoPParams: must specify protocolParamMaxTxExUnits"
toAlonzoPParams ProtocolParameters { protocolParamMaxBlockExUnits = Nothing } =
  error "toAlonzoPParams: must specify protocolParamMaxBlockExUnits"
toAlonzoPParams ProtocolParameters { protocolParamMaxValueSize    = Nothing } =
    error "toAlonzoPParams: must specify protocolParamMaxValueSize"
toAlonzoPParams ProtocolParameters { protocolParamCollateralPercent = Nothing } =
    error "toAlonzoPParams: must specify protocolParamCollateralPercent"
toAlonzoPParams ProtocolParameters { protocolParamMaxCollateralInputs = Nothing } =
    error "toAlonzoPParams: must specify protocolParamMaxCollateralInputs"


-- ----------------------------------------------------------------------------
-- Conversion functions: protocol parameters from ledger types
--

fromLedgerPParams
  :: ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> ProtocolParameters
fromLedgerPParams ShelleyBasedEraShelley = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraAllegra = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraMary    = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraAlonzo  = fromAlonzoPParams


fromShelleyPParams :: Shelley.PParams ledgerera
                   -> ProtocolParameters
fromShelleyPParams
    Shelley.PParams {
      Shelley._minfeeA
    , Shelley._minfeeB
    , Shelley._maxBBSize
    , Shelley._maxTxSize
    , Shelley._maxBHSize
    , Shelley._keyDeposit
    , Shelley._poolDeposit
    , Shelley._eMax
    , Shelley._nOpt
    , Shelley._a0
    , Shelley._rho
    , Shelley._tau
    , Shelley._d
    , Shelley._extraEntropy
    , Shelley._protocolVersion
    , Shelley._minUTxOValue
    , Shelley._minPoolCost
    } =
    ProtocolParameters {
      protocolParamProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = Ledger.unboundRational _d
    , protocolParamExtraPraosEntropy   = fromLedgerNonce _extraEntropy
    , protocolParamMaxBlockHeaderSize  = _maxBHSize
    , protocolParamMaxBlockBodySize    = _maxBBSize
    , protocolParamMaxTxSize           = _maxTxSize
    , protocolParamTxFeeFixed          = _minfeeB
    , protocolParamTxFeePerByte        = _minfeeA
    , protocolParamMinUTxOValue        = Just (fromShelleyLovelace _minUTxOValue)
    , protocolParamStakeAddressDeposit = fromShelleyLovelace _keyDeposit
    , protocolParamStakePoolDeposit    = fromShelleyLovelace _poolDeposit
    , protocolParamMinPoolCost         = fromShelleyLovelace _minPoolCost
    , protocolParamPoolRetireMaxEpoch  = _eMax
    , protocolParamStakePoolTargetNum  = _nOpt
    , protocolParamPoolPledgeInfluence = Ledger.unboundRational _a0
    , protocolParamMonetaryExpansion   = Ledger.unboundRational _rho
    , protocolParamTreasuryCut         = Ledger.unboundRational _tau
    , protocolParamUTxOCostPerWord     = Nothing
    , protocolParamCostModels          = Map.empty
    , protocolParamPrices              = Nothing
    , protocolParamMaxTxExUnits        = Nothing
    , protocolParamMaxBlockExUnits     = Nothing
    , protocolParamMaxValueSize        = Nothing
    , protocolParamCollateralPercent   = Nothing
    , protocolParamMaxCollateralInputs = Nothing
    }


fromAlonzoPParams :: Alonzo.PParams ledgerera -> ProtocolParameters
fromAlonzoPParams
    Alonzo.PParams {
      Alonzo._minfeeA
    , Alonzo._minfeeB
    , Alonzo._maxBBSize
    , Alonzo._maxTxSize
    , Alonzo._maxBHSize
    , Alonzo._keyDeposit
    , Alonzo._poolDeposit
    , Alonzo._eMax
    , Alonzo._nOpt
    , Alonzo._a0
    , Alonzo._rho
    , Alonzo._tau
    , Alonzo._d
    , Alonzo._extraEntropy
    , Alonzo._protocolVersion
    , Alonzo._minPoolCost
    , Alonzo._coinsPerUTxOWord
    , Alonzo._costmdls
    , Alonzo._prices
    , Alonzo._maxTxExUnits
    , Alonzo._maxBlockExUnits
    , Alonzo._maxValSize
    , Alonzo._collateralPercentage
    , Alonzo._maxCollateralInputs
    } =
    ProtocolParameters {
      protocolParamProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = Ledger.unboundRational _d
    , protocolParamExtraPraosEntropy   = fromLedgerNonce _extraEntropy
    , protocolParamMaxBlockHeaderSize  = _maxBHSize
    , protocolParamMaxBlockBodySize    = _maxBBSize
    , protocolParamMaxTxSize           = _maxTxSize
    , protocolParamTxFeeFixed          = _minfeeB
    , protocolParamTxFeePerByte        = _minfeeA
    , protocolParamMinUTxOValue        = Nothing
    , protocolParamStakeAddressDeposit = fromShelleyLovelace _keyDeposit
    , protocolParamStakePoolDeposit    = fromShelleyLovelace _poolDeposit
    , protocolParamMinPoolCost         = fromShelleyLovelace _minPoolCost
    , protocolParamPoolRetireMaxEpoch  = _eMax
    , protocolParamStakePoolTargetNum  = _nOpt
    , protocolParamPoolPledgeInfluence = Ledger.unboundRational _a0
    , protocolParamMonetaryExpansion   = Ledger.unboundRational _rho
    , protocolParamTreasuryCut         = Ledger.unboundRational _tau
    , protocolParamUTxOCostPerWord     = Just (fromShelleyLovelace _coinsPerUTxOWord)
    , protocolParamCostModels          = fromAlonzoCostModels _costmdls
    , protocolParamPrices              = Just (fromAlonzoPrices _prices)
    , protocolParamMaxTxExUnits        = Just (fromAlonzoExUnits _maxTxExUnits)
    , protocolParamMaxBlockExUnits     = Just (fromAlonzoExUnits _maxBlockExUnits)
    , protocolParamMaxValueSize        = Just _maxValSize
    , protocolParamCollateralPercent   = Just _collateralPercentage
    , protocolParamMaxCollateralInputs = Just _maxCollateralInputs
    }

data ProtocolParametersError =
    PParamsErrorMissingMinUTxoValue AnyCardanoEra
  | PParamsErrorMissingAlonzoProtocolParameter
  deriving Show

instance Error ProtocolParametersError where
  displayError (PParamsErrorMissingMinUTxoValue (AnyCardanoEra era)) =
   "The " <> show era <> " protocol parameters value is missing the following \
       \field: MinUTxoValue. Did you intend to use a " <> show era <> " protocol \
       \ parameters value?"
  displayError PParamsErrorMissingAlonzoProtocolParameter =
    "The Alonzo era protocol parameters in use is missing one or more of the \
    \following fields: UTxOCostPerWord, CostModels, Prices, MaxTxExUnits, \
    \MaxBlockExUnits, MaxValueSize, CollateralPercent, MaxCollateralInputs. Did \
    \you intend to use an Alonzo era protocol parameters value?"

checkProtocolParameters
  :: forall era. IsCardanoEra era
  => ShelleyBasedEra era
  -> ProtocolParameters
  -> Either ProtocolParametersError ()
checkProtocolParameters sbe ProtocolParameters{..} =
  case sbe of
    ShelleyBasedEraShelley -> checkMinUTxOVal
    ShelleyBasedEraAllegra -> checkMinUTxOVal
    ShelleyBasedEraMary -> checkMinUTxOVal
    ShelleyBasedEraAlonzo -> checkAlonzoParams
 where
   era :: CardanoEra era
   era = shelleyBasedToCardanoEra sbe

   costPerWord = isJust protocolParamUTxOCostPerWord
   cModel = not $ Map.null protocolParamCostModels
   prices = isJust protocolParamPrices
   maxTxUnits = isJust protocolParamMaxTxExUnits
   maxBlockExUnits = isJust protocolParamMaxBlockExUnits
   maxValueSize = isJust protocolParamMaxValueSize
   collateralPercent = isJust protocolParamCollateralPercent
   maxCollateralInputs = isJust protocolParamMaxCollateralInputs

   checkAlonzoParams :: Either ProtocolParametersError ()
   checkAlonzoParams =
     if all (== True) [ costPerWord
                      , cModel
                      , prices
                      , maxTxUnits
                      , maxBlockExUnits
                      , maxValueSize
                      , collateralPercent
                      , maxCollateralInputs
                      ]
     then return ()
     else Left PParamsErrorMissingAlonzoProtocolParameter

   checkMinUTxOVal :: Either ProtocolParametersError ()
   checkMinUTxOVal =
     if isJust protocolParamMinUTxOValue
     then return ()
     else Left . PParamsErrorMissingMinUTxoValue
               $ AnyCardanoEra era
