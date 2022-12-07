{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Redundant ==" -}

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
    fromAlonzoCostModels,

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
    toAlonzoCostModels,
    toAlonzoPParams,
    toBabbagePParams,

    -- * Data family instances
    AsType(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?),
                   (.=))
import           Data.Bifunctor (bimap, first)
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.String (IsString)
import           Data.Text (Text)
import           GHC.Generics
import           Numeric.Natural

import           Cardano.Api.Json (toRationalJSON)
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochNo)

import           Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD(..), BabbagePParamsUpdate, BabbagePParams)
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger

-- Some of the things from Cardano.Ledger.ShelleyPParams are generic across all
-- eras, and some are specific to the Shelley era (and other pre-Alonzo eras).
-- So we import in twice under different names.

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import           Cardano.Ledger.Alonzo.PParams (AlonzoPParams, AlonzoPParamsHKD (..),
                   AlonzoPParamsUpdate)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Shelley.API as Ledger hiding (PParams)
import           Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate, ShelleyPParamsHKD (..), ShelleyPParams)

import           Text.PrettyBy.Default (display)

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Shelley
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
       -- /Deprecated in Babbage/
       protocolParamDecentralization :: Maybe Rational,

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
       protocolParamMaxCollateralInputs :: Maybe Natural,

       -- | Cost in ada per byte of UTxO storage.
       --
       -- /Introduced in Babbage/
       protocolParamUTxOCostPerByte :: Maybe Lovelace

    }
  deriving (Eq, Generic, Show)

instance FromJSON ProtocolParameters where
  parseJSON =
    withObject "ProtocolParameters" $ \o -> do
      v <- o .: "protocolVersion"
      ProtocolParameters
        <$> ((,) <$> v .: "major" <*> v .: "minor")
        <*> o .:? "decentralization"
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
        <*> o .:? "utxoCostPerByte"

instance ToJSON ProtocolParameters where
  toJSON ProtocolParameters{..} =
    object
      [ "extraPraosEntropy"   .= protocolParamExtraPraosEntropy
      , "stakePoolTargetNum"  .= protocolParamStakePoolTargetNum
      , "minUTxOValue"        .= protocolParamMinUTxOValue
      , "poolRetireMaxEpoch"  .= protocolParamPoolRetireMaxEpoch
      , "decentralization"    .= (toRationalJSON <$> protocolParamDecentralization)
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
      -- Babbage era:
      , "utxoCostPerByte"        .= protocolParamUTxOCostPerByte
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

       -- Introduced in Alonzo,

       -- | Cost in ada per word of UTxO storage.
       --
       -- /Introduced in Alonzo, obsoleted in Babbage by 'protocolUpdateUTxOCostPerByte'/
       protocolUpdateUTxOCostPerWord :: Maybe Lovelace,

       -- Introduced in Alonzo,

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
       protocolUpdateMaxCollateralInputs :: Maybe Natural,

       -- | Cost in ada per byte of UTxO storage.
       --
       -- /Introduced in Babbage.  Supercedes 'protocolUpdateUTxOCostPerWord'/
       protocolUpdateUTxOCostPerByte :: Maybe Lovelace
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
      -- Introduced in Babbage below.
      , protocolUpdateUTxOCostPerByte     = merge protocolUpdateUTxOCostPerByte
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
      , protocolUpdateUTxOCostPerByte     = Nothing
      }

instance ToCBOR ProtocolParametersUpdate where
    toCBOR ProtocolParametersUpdate{..} =
        CBOR.encodeListLen 26
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
     <> toCBOR protocolUpdateUTxOCostPerByte

instance FromCBOR ProtocolParametersUpdate where
    fromCBOR = do
      CBOR.enforceSize "ProtocolParametersUpdate" 26
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

    eitherDeserialiseFromRawBytes AsPraosNonce bs =
      maybeToRight (SerialiseAsRawBytesError "Unable to deserialise PraosNonce") $
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
validateCostModel PlutusScriptV1 (CostModel m) =
    first (InvalidCostModel (CostModel m))
  $ Alonzo.assertWellFormedCostModelParams m
validateCostModel PlutusScriptV2 (CostModel m) =
    first (InvalidCostModel (CostModel m))
  $ Alonzo.assertWellFormedCostModelParams m

-- TODO alonzo: it'd be nice if the library told us what was wrong
data InvalidCostModel = InvalidCostModel CostModel Alonzo.CostModelApplyError
  deriving Show

instance Error InvalidCostModel where
  displayError (InvalidCostModel cm err) =
    "Invalid cost model: " ++ display err ++
    " Cost model: " ++ show cm


toAlonzoCostModels
  :: Map AnyPlutusScriptVersion CostModel
  -> Either String Alonzo.CostModels
toAlonzoCostModels m = do
  f <- mapM conv $ Map.toList m
  Right . Alonzo.CostModels $ Map.fromList f
 where
  conv :: (AnyPlutusScriptVersion, CostModel) -> Either String (Alonzo.Language, Alonzo.CostModel)
  conv (anySVer, cModel )= do
    -- TODO: Propagate InvalidCostModel further
    alonzoCostModel <- first displayError $ toAlonzoCostModel cModel (toAlonzoScriptLanguage anySVer)
    Right (toAlonzoScriptLanguage anySVer, alonzoCostModel)

fromAlonzoCostModels
  :: Alonzo.CostModels
  -> Map AnyPlutusScriptVersion CostModel
fromAlonzoCostModels (Alonzo.CostModels m)=
    Map.fromList
  . map (bimap fromAlonzoScriptLanguage fromAlonzoCostModel)
  $ Map.toList m

toAlonzoScriptLanguage :: AnyPlutusScriptVersion -> Alonzo.Language
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Alonzo.PlutusV1
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Alonzo.PlutusV2

fromAlonzoScriptLanguage :: Alonzo.Language -> AnyPlutusScriptVersion
fromAlonzoScriptLanguage Alonzo.PlutusV1 = AnyPlutusScriptVersion PlutusScriptV1
fromAlonzoScriptLanguage Alonzo.PlutusV2 = AnyPlutusScriptVersion PlutusScriptV2

toAlonzoCostModel :: CostModel -> Alonzo.Language -> Either InvalidCostModel Alonzo.CostModel
toAlonzoCostModel (CostModel m) l = first (InvalidCostModel (CostModel m)) $ Alonzo.mkCostModel l m

fromAlonzoCostModel :: Alonzo.CostModel -> CostModel
fromAlonzoCostModel m = CostModel $ Alonzo.getCostModelParams m


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
  . Map.map (toLedgerPParamsUpdate era)


toLedgerPParamsUpdate :: ShelleyBasedEra era
                     -> ProtocolParametersUpdate
                     -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
toLedgerPParamsUpdate ShelleyBasedEraShelley = toShelleyPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraAllegra = toShelleyPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraMary    = toShelleyPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraAlonzo  = toAlonzoPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraBabbage = toBabbagePParamsUpdate


--TODO: we should do validation somewhere, not just silently drop changes that
-- are not valid. Specifically, see Ledger.boundRational below.
toShelleyPParamsUpdate :: ProtocolParametersUpdate
                       -> ShelleyPParamsUpdate ledgerera
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
    ShelleyPParams {
      _minfeeA     = noInlineMaybeToStrictMaybe protocolUpdateTxFeePerByte
    , _minfeeB     = noInlineMaybeToStrictMaybe protocolUpdateTxFeeFixed
    , _maxBBSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , _maxTxSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxTxSize
    , _maxBHSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , _keyDeposit  = toShelleyLovelace <$>
                       noInlineMaybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , _poolDeposit = toShelleyLovelace <$>
                       noInlineMaybeToStrictMaybe protocolUpdateStakePoolDeposit
    , _eMax        = noInlineMaybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , _nOpt        = noInlineMaybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , _a0          = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                      protocolUpdatePoolPledgeInfluence
    , _rho         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                        protocolUpdateMonetaryExpansion
    , _tau         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                        protocolUpdateTreasuryCut
    , _d           = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                        protocolUpdateDecentralization
    , _extraEntropy    = toLedgerNonce <$>
                           noInlineMaybeToStrictMaybe protocolUpdateExtraPraosEntropy
    , _protocolVersion = uncurry Ledger.ProtVer <$>
                           noInlineMaybeToStrictMaybe protocolUpdateProtocolVersion
    , _minUTxOValue    = toShelleyLovelace <$>
                           noInlineMaybeToStrictMaybe protocolUpdateMinUTxOValue
    , _minPoolCost     = toShelleyLovelace <$>
                                   noInlineMaybeToStrictMaybe protocolUpdateMinPoolCost
    }


toAlonzoPParamsUpdate :: ProtocolParametersUpdate
                      -> AlonzoPParamsUpdate ledgerera
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
    AlonzoPParams {
      _minfeeA     = noInlineMaybeToStrictMaybe protocolUpdateTxFeePerByte
    , _minfeeB     = noInlineMaybeToStrictMaybe protocolUpdateTxFeeFixed
    , _maxBBSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , _maxTxSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxTxSize
    , _maxBHSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , _keyDeposit  = toShelleyLovelace <$>
                       noInlineMaybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , _poolDeposit = toShelleyLovelace <$>
                       noInlineMaybeToStrictMaybe protocolUpdateStakePoolDeposit
    , _eMax        = noInlineMaybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , _nOpt        = noInlineMaybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , _a0          = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                       protocolUpdatePoolPledgeInfluence
    , _rho         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                        protocolUpdateMonetaryExpansion
    , _tau         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                        protocolUpdateTreasuryCut
    , _d           = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                        protocolUpdateDecentralization
    , _extraEntropy    = toLedgerNonce <$>
                           noInlineMaybeToStrictMaybe protocolUpdateExtraPraosEntropy
    , _protocolVersion = uncurry Ledger.ProtVer <$>
                           noInlineMaybeToStrictMaybe protocolUpdateProtocolVersion
    , _minPoolCost     = toShelleyLovelace <$>
                           noInlineMaybeToStrictMaybe protocolUpdateMinPoolCost
    , _coinsPerUTxOWord  = toShelleyLovelace <$>
                           noInlineMaybeToStrictMaybe protocolUpdateUTxOCostPerWord
    , _costmdls        = if Map.null protocolUpdateCostModels
                           then Ledger.SNothing
                           else either (const Ledger.SNothing) Ledger.SJust
                                  (toAlonzoCostModels protocolUpdateCostModels)
    , _prices          = noInlineMaybeToStrictMaybe $
                           toAlonzoPrices =<< protocolUpdatePrices
    , _maxTxExUnits    = toAlonzoExUnits  <$>
                           noInlineMaybeToStrictMaybe protocolUpdateMaxTxExUnits
    , _maxBlockExUnits = toAlonzoExUnits  <$>
                           noInlineMaybeToStrictMaybe protocolUpdateMaxBlockExUnits
    , _maxValSize      = noInlineMaybeToStrictMaybe protocolUpdateMaxValueSize
    , _collateralPercentage = noInlineMaybeToStrictMaybe protocolUpdateCollateralPercent
    , _maxCollateralInputs  = noInlineMaybeToStrictMaybe protocolUpdateMaxCollateralInputs
    }

-- Decentralization and extra entropy are deprecated in Babbage
toBabbagePParamsUpdate :: ProtocolParametersUpdate
                       -> BabbagePParamsUpdate ledgerera
toBabbagePParamsUpdate
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
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
    , protocolUpdateCostModels
    , protocolUpdatePrices
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxCollateralInputs
    , protocolUpdateUTxOCostPerByte
    } =
    BabbagePParams {
      _minfeeA     = noInlineMaybeToStrictMaybe protocolUpdateTxFeePerByte
    , _minfeeB     = noInlineMaybeToStrictMaybe protocolUpdateTxFeeFixed
    , _maxBBSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , _maxTxSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxTxSize
    , _maxBHSize   = noInlineMaybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , _keyDeposit  = toShelleyLovelace <$>
                      noInlineMaybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , _poolDeposit = toShelleyLovelace <$>
                      noInlineMaybeToStrictMaybe protocolUpdateStakePoolDeposit
    , _eMax        = noInlineMaybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , _nOpt        = noInlineMaybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , _a0          = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                      protocolUpdatePoolPledgeInfluence
    , _rho         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                       protocolUpdateMonetaryExpansion
    , _tau         = noInlineMaybeToStrictMaybe $ Ledger.boundRational =<<
                       protocolUpdateTreasuryCut
    , _protocolVersion = uncurry Ledger.ProtVer <$>
                          noInlineMaybeToStrictMaybe protocolUpdateProtocolVersion
    , _minPoolCost     = toShelleyLovelace <$>
                           noInlineMaybeToStrictMaybe protocolUpdateMinPoolCost
    , _costmdls        = if Map.null protocolUpdateCostModels
                          then Ledger.SNothing
                          else either (const Ledger.SNothing) Ledger.SJust
                                 (toAlonzoCostModels protocolUpdateCostModels)
    , _prices          = noInlineMaybeToStrictMaybe $
                          toAlonzoPrices =<< protocolUpdatePrices
    , _maxTxExUnits    = toAlonzoExUnits  <$>
                          noInlineMaybeToStrictMaybe protocolUpdateMaxTxExUnits
    , _maxBlockExUnits = toAlonzoExUnits  <$>
                          noInlineMaybeToStrictMaybe protocolUpdateMaxBlockExUnits
    , _maxValSize      = noInlineMaybeToStrictMaybe protocolUpdateMaxValueSize
    , _collateralPercentage = noInlineMaybeToStrictMaybe protocolUpdateCollateralPercent
    , _maxCollateralInputs  = noInlineMaybeToStrictMaybe protocolUpdateMaxCollateralInputs
    , _coinsPerUTxOByte = toShelleyLovelace <$>
                                    noInlineMaybeToStrictMaybe protocolUpdateUTxOCostPerByte
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
    Map.map (fromLedgerPParamsUpdate era)
  . Map.mapKeysMonotonic GenesisKeyHash
  . (\(Ledger.ProposedPPUpdates ppup) -> ppup)


fromLedgerPParamsUpdate :: ShelleyBasedEra era
                       -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
                       -> ProtocolParametersUpdate
fromLedgerPParamsUpdate ShelleyBasedEraShelley = fromShelleyPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraAllegra = fromShelleyPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraMary    = fromShelleyPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraAlonzo  = fromAlonzoPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraBabbage = fromBabbagePParamsUpdate


fromShelleyPParamsUpdate :: ShelleyPParamsUpdate ledgerera
                         -> ProtocolParametersUpdate
fromShelleyPParamsUpdate
    ShelleyPParams {
      _minfeeA
    , _minfeeB
    , _maxBBSize
    , _maxTxSize
    , _maxBHSize
    , _keyDeposit
    , _poolDeposit
    , _eMax
    , _nOpt
    , _a0
    , _rho
    , _tau
    , _d
    , _extraEntropy
    , _protocolVersion
    , _minUTxOValue
    , _minPoolCost
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
    , protocolUpdateUTxOCostPerByte     = Nothing
    }

fromAlonzoPParamsUpdate :: AlonzoPParamsUpdate ledgerera
                        -> ProtocolParametersUpdate
fromAlonzoPParamsUpdate
    AlonzoPParams {
      _minfeeA
    , _minfeeB
    , _maxBBSize
    , _maxTxSize
    , _maxBHSize
    , _keyDeposit
    , _poolDeposit
    , _eMax
    , _nOpt
    , _a0
    , _rho
    , _tau
    , _d
    , _extraEntropy
    , _protocolVersion
    , _minPoolCost
    , _coinsPerUTxOWord
    , _costmdls
    , _prices
    , _maxTxExUnits
    , _maxBlockExUnits
    , _maxValSize
    , _collateralPercentage
    , _maxCollateralInputs
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
    , protocolUpdateUTxOCostPerByte     = Nothing
    }


fromBabbagePParamsUpdate :: BabbagePParamsUpdate ledgerera
                         -> ProtocolParametersUpdate
fromBabbagePParamsUpdate
    BabbagePParams {
      _minfeeA
    , _minfeeB
    , _maxBBSize
    , _maxTxSize
    , _maxBHSize
    , _keyDeposit
    , _poolDeposit
    , _eMax
    , _nOpt
    , _a0
    , _rho
    , _tau
    , _protocolVersion
    , _minPoolCost
    , _coinsPerUTxOByte
    , _costmdls
    , _prices
    , _maxTxExUnits
    , _maxBlockExUnits
    , _maxValSize
    , _collateralPercentage
    , _maxCollateralInputs
    } =
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b)) <$>
                                          strictMaybeToMaybe _protocolVersion
    , protocolUpdateDecentralization    = Nothing
    , protocolUpdateExtraPraosEntropy   = Nothing
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
    , protocolUpdateUTxOCostPerWord     = Nothing
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
    , protocolUpdateUTxOCostPerByte     = fromShelleyLovelace <$>
                                            strictMaybeToMaybe _coinsPerUTxOByte
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
toLedgerPParams ShelleyBasedEraBabbage = toBabbagePParams

toShelleyPParams :: ProtocolParameters -> ShelleyPParams ledgerera
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
   ShelleyPParams
     { _protocolVersion
                             = let (maj, minor) = protocolParamProtocolVersion
                                in Ledger.ProtVer maj minor
     , _d            = case protocolParamDecentralization of
                                 -- The decentralization parameter is deprecated in Babbage
                                 -- so we default to 0 if no dentralization parameter is found
                                 -- in the api's 'ProtocolParameter' type. If we don't do this
                                 -- we won't be able to construct an Alonzo tx using the Babbage
                                 -- era's protocol parameter because our only other option is to
                                 -- error.
                                 Nothing -> minBound
                                 Just pDecentral ->
                                   fromMaybe
                                     (error "toShelleyPParams: invalid Decentralization value")
                                     (Ledger.boundRational pDecentral)
     , _extraEntropy = toLedgerNonce protocolParamExtraPraosEntropy
     , _maxBHSize    = protocolParamMaxBlockHeaderSize
     , _maxBBSize    = protocolParamMaxBlockBodySize
     , _maxTxSize    = protocolParamMaxTxSize
     , _minfeeB      = protocolParamTxFeeFixed
     , _minfeeA      = protocolParamTxFeePerByte
     , _minUTxOValue = toShelleyLovelace minUTxOValue
     , _keyDeposit   = toShelleyLovelace protocolParamStakeAddressDeposit
     , _poolDeposit  = toShelleyLovelace protocolParamStakePoolDeposit
     , _minPoolCost  = toShelleyLovelace protocolParamMinPoolCost
     , _eMax         = protocolParamPoolRetireMaxEpoch
     , _nOpt         = protocolParamStakePoolTargetNum
     , _a0           = fromMaybe
                                 (error "toShelleyPParams: invalid PoolPledgeInfluence value")
                                 (Ledger.boundRational protocolParamPoolPledgeInfluence)
     , _rho          = fromMaybe
                                 (error "toShelleyPParams: invalid MonetaryExpansion value")
                                 (Ledger.boundRational protocolParamMonetaryExpansion)
     , _tau          = fromMaybe
                                 (error "toShelleyPParams: invalid TreasuryCut value")
                                 (Ledger.boundRational protocolParamTreasuryCut)
     }
toShelleyPParams ProtocolParameters { protocolParamMinUTxOValue = Nothing } =
  error "toShelleyPParams: must specify protocolParamMinUTxOValue"

toAlonzoPParams :: ProtocolParameters -> AlonzoPParams ledgerera
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
                   protocolParamUTxOCostPerWord,
                   protocolParamCostModels,
                   protocolParamPrices          = Just prices,
                   protocolParamMaxTxExUnits    = Just maxTxExUnits,
                   protocolParamMaxBlockExUnits = Just maxBlockExUnits,
                   protocolParamMaxValueSize    = Just maxValueSize,
                   protocolParamCollateralPercent   = Just collateralPercentage,
                   protocolParamMaxCollateralInputs = Just maxCollateralInputs,
                   protocolParamUTxOCostPerByte
                 } =
    let !coinsPerUTxOWord = fromMaybe
          (error "toAlonzoPParams: must specify protocolParamUTxOCostPerWord or protocolParamUTxOCostPerByte") $
            protocolParamUTxOCostPerWord <|> ((* 8) <$> protocolParamUTxOCostPerByte)
    in
    AlonzoPParams {
      _protocolVersion
                           = let (maj, minor) = protocolParamProtocolVersion
                              in Ledger.ProtVer maj minor
    , _d            = case protocolParamDecentralization of
                                 -- The decentralization parameter is deprecated in Babbage
                                 -- so we default to 0 if no dentralization parameter is found
                                 -- in the api's 'ProtocolParameter' type. If we don't do this
                                 -- we won't be able to construct an Alonzo tx using the Babbage
                                 -- era's protocol parameter because our only other option is to
                                 -- error.
                                 Nothing -> minBound
                                 Just pDecentral ->
                                   fromMaybe
                                     (error "toAlonzoPParams: invalid Decentralization value")
                                     (Ledger.boundRational pDecentral)
    , _extraEntropy = toLedgerNonce protocolParamExtraPraosEntropy
    , _maxBHSize    = protocolParamMaxBlockHeaderSize
    , _maxBBSize    = protocolParamMaxBlockBodySize
    , _maxTxSize    = protocolParamMaxTxSize
    , _minfeeB      = protocolParamTxFeeFixed
    , _minfeeA      = protocolParamTxFeePerByte
    , _keyDeposit   = toShelleyLovelace protocolParamStakeAddressDeposit
    , _poolDeposit  = toShelleyLovelace protocolParamStakePoolDeposit
    , _minPoolCost  = toShelleyLovelace protocolParamMinPoolCost
    , _eMax         = protocolParamPoolRetireMaxEpoch
    , _nOpt         = protocolParamStakePoolTargetNum
    , _a0           = fromMaybe
                               (error "toAlonzoPParams: invalid PoolPledgeInfluence value")
                               (Ledger.boundRational protocolParamPoolPledgeInfluence)
    , _rho          = fromMaybe
                               (error "toAlonzoPParams: invalid MonetaryExpansion value")
                               (Ledger.boundRational protocolParamMonetaryExpansion)
    , _tau          = fromMaybe
                               (error "toAlonzoPParams: invalid TreasuryCut value")
                               (Ledger.boundRational protocolParamTreasuryCut)

      -- New params in Alonzo:
    , _coinsPerUTxOWord  = toShelleyLovelace coinsPerUTxOWord
    , _costmdls        = either
                                  (\e -> error $ "toAlonzoPParams: invalid cost models, error: " <> e)
                                  id
                                  (toAlonzoCostModels protocolParamCostModels)
    , _prices          = fromMaybe
                                  (error "toAlonzoPParams: invalid Price values")
                                  (toAlonzoPrices prices)
    , _maxTxExUnits    = toAlonzoExUnits maxTxExUnits
    , _maxBlockExUnits = toAlonzoExUnits maxBlockExUnits
    , _maxValSize      = maxValueSize
    , _collateralPercentage = collateralPercentage
    , _maxCollateralInputs  = maxCollateralInputs
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


toBabbagePParams :: ProtocolParameters -> BabbagePParams ledgerera
toBabbagePParams ProtocolParameters {
                   protocolParamProtocolVersion,
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
                   protocolParamUTxOCostPerByte = Just utxoCostPerByte,
                   protocolParamCostModels,
                   protocolParamPrices          = Just prices,
                   protocolParamMaxTxExUnits    = Just maxTxExUnits,
                   protocolParamMaxBlockExUnits = Just maxBlockExUnits,
                   protocolParamMaxValueSize    = Just maxValueSize,
                   protocolParamCollateralPercent   = Just collateralPercentage,
                   protocolParamMaxCollateralInputs = Just maxCollateralInputs
                 } =
    BabbagePParams {
      _protocolVersion
                           = let (maj, minor) = protocolParamProtocolVersion
                              in Ledger.ProtVer maj minor
    , _maxBHSize    = protocolParamMaxBlockHeaderSize
    , _maxBBSize    = protocolParamMaxBlockBodySize
    , _maxTxSize    = protocolParamMaxTxSize
    , _minfeeB      = protocolParamTxFeeFixed
    , _minfeeA      = protocolParamTxFeePerByte
    , _keyDeposit   = toShelleyLovelace protocolParamStakeAddressDeposit
    , _poolDeposit  = toShelleyLovelace protocolParamStakePoolDeposit
    , _minPoolCost  = toShelleyLovelace protocolParamMinPoolCost
    , _eMax         = protocolParamPoolRetireMaxEpoch
    , _nOpt         = protocolParamStakePoolTargetNum
    , _a0           = fromMaybe
                               (error "toBabbagePParams: invalid PoolPledgeInfluence value")
                               (Ledger.boundRational protocolParamPoolPledgeInfluence)
    , _rho          = fromMaybe
                               (error "toBabbagePParams: invalid MonetaryExpansion value")
                               (Ledger.boundRational protocolParamMonetaryExpansion)
    , _tau          = fromMaybe
                               (error "toBabbagePParams: invalid TreasuryCut value")
                               (Ledger.boundRational protocolParamTreasuryCut)

      -- New params in Babbage.
    , _coinsPerUTxOByte = toShelleyLovelace utxoCostPerByte

    , _costmdls        = either
                                  (\e -> error $ "toBabbagePParams: invalid cost models, error: " <> e)
                                  id
                                  (toAlonzoCostModels protocolParamCostModels)
    , _prices          = fromMaybe
                                  (error "toBabbagePParams: invalid Price values")
                                  (toAlonzoPrices prices)
    , _maxTxExUnits    = toAlonzoExUnits maxTxExUnits
    , _maxBlockExUnits = toAlonzoExUnits maxBlockExUnits
    , _maxValSize      = maxValueSize
    , _collateralPercentage = collateralPercentage
    , _maxCollateralInputs  = maxCollateralInputs
    }
toBabbagePParams ProtocolParameters { protocolParamUTxOCostPerByte = Nothing } =
  error "toBabbagePParams: must specify protocolParamUTxOCostPerByte"
toBabbagePParams ProtocolParameters { protocolParamPrices          = Nothing } =
  error "toBabbagePParams: must specify protocolParamPrices"
toBabbagePParams ProtocolParameters { protocolParamMaxTxExUnits    = Nothing } =
  error "toBabbagePParams: must specify protocolParamMaxTxExUnits"
toBabbagePParams ProtocolParameters { protocolParamMaxBlockExUnits = Nothing } =
  error "toBabbagePParams: must specify protocolParamMaxBlockExUnits"
toBabbagePParams ProtocolParameters { protocolParamMaxValueSize    = Nothing } =
  error "toBabbagePParams: must specify protocolParamMaxValueSize"
toBabbagePParams ProtocolParameters { protocolParamCollateralPercent = Nothing } =
  error "toBabbagePParams: must specify protocolParamCollateralPercent"
toBabbagePParams ProtocolParameters { protocolParamMaxCollateralInputs = Nothing } =
  error "toBabbagePParams: must specify protocolParamMaxCollateralInputs"

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
fromLedgerPParams ShelleyBasedEraBabbage = fromBabbagePParams


fromShelleyPParams :: ShelleyPParams ledgerera
                   -> ProtocolParameters
fromShelleyPParams
    ShelleyPParams {
      _minfeeA
    , _minfeeB
    , _maxBBSize
    , _maxTxSize
    , _maxBHSize
    , _keyDeposit
    , _poolDeposit
    , _eMax
    , _nOpt
    , _a0
    , _rho
    , _tau
    , _d
    , _extraEntropy
    , _protocolVersion
    , _minUTxOValue
    , _minPoolCost
    } =
    ProtocolParameters {
      protocolParamProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = Just $ Ledger.unboundRational _d
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
    , protocolParamUTxOCostPerWord     = Nothing    -- Only in Alonzo
    , protocolParamCostModels          = Map.empty  -- Only from Alonzo onwards
    , protocolParamPrices              = Nothing    -- Only from Alonzo onwards
    , protocolParamMaxTxExUnits        = Nothing    -- Only from Alonzo onwards
    , protocolParamMaxBlockExUnits     = Nothing    -- Only from Alonzo onwards
    , protocolParamMaxValueSize        = Nothing    -- Only from Alonzo onwards
    , protocolParamCollateralPercent   = Nothing    -- Only from Alonzo onwards
    , protocolParamMaxCollateralInputs = Nothing    -- Only from Alonzo onwards
    , protocolParamUTxOCostPerByte     = Nothing    -- Only from babbage onwards
    }


fromAlonzoPParams :: AlonzoPParams ledgerera -> ProtocolParameters
fromAlonzoPParams
    AlonzoPParams {
      _minfeeA
    , _minfeeB
    , _maxBBSize
    , _maxTxSize
    , _maxBHSize
    , _keyDeposit
    , _poolDeposit
    , _eMax
    , _nOpt
    , _a0
    , _rho
    , _tau
    , _d
    , _extraEntropy
    , _protocolVersion
    , _minPoolCost
    , _coinsPerUTxOWord
    , _costmdls
    , _prices
    , _maxTxExUnits
    , _maxBlockExUnits
    , _maxValSize
    , _collateralPercentage
    , _maxCollateralInputs
    } =
    ProtocolParameters {
      protocolParamProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = Just $ Ledger.unboundRational _d
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
    , protocolParamUTxOCostPerByte     = Nothing    -- Only from babbage onwards
    }

fromBabbagePParams :: BabbagePParams ledgerera -> ProtocolParameters
fromBabbagePParams
    BabbagePParams {
      _minfeeA
    , _minfeeB
    , _maxBBSize
    , _maxTxSize
    , _maxBHSize
    , _keyDeposit
    , _poolDeposit
    , _eMax
    , _nOpt
    , _a0
    , _rho
    , _tau
    , _protocolVersion
    , _minPoolCost
    , _coinsPerUTxOByte
    , _costmdls
    , _prices
    , _maxTxExUnits
    , _maxBlockExUnits
    , _maxValSize
    , _collateralPercentage
    , _maxCollateralInputs
    } =
    ProtocolParameters {
      protocolParamProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = Nothing
    , protocolParamExtraPraosEntropy   = Nothing
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
    , protocolParamUTxOCostPerWord     = Nothing    -- Obsolete from babbage onwards
    , protocolParamCostModels          = fromAlonzoCostModels _costmdls
    , protocolParamPrices              = Just (fromAlonzoPrices _prices)
    , protocolParamMaxTxExUnits        = Just (fromAlonzoExUnits _maxTxExUnits)
    , protocolParamMaxBlockExUnits     = Just (fromAlonzoExUnits _maxBlockExUnits)
    , protocolParamMaxValueSize        = Just _maxValSize
    , protocolParamCollateralPercent   = Just _collateralPercentage
    , protocolParamMaxCollateralInputs = Just _maxCollateralInputs
    , protocolParamUTxOCostPerByte     = Just (fromShelleyLovelace _coinsPerUTxOByte)
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
    ShelleyBasedEraBabbage -> checkBabbageParams
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
   costPerByte = isJust protocolParamUTxOCostPerByte
   decentralization = isJust protocolParamDecentralization
   extraPraosEntropy = isJust protocolParamExtraPraosEntropy

   alonzoPParamFieldsRequirements :: [Bool]
   alonzoPParamFieldsRequirements =
     [     costPerWord
     ,     cModel
     ,     prices
     ,     maxTxUnits
     ,     maxBlockExUnits
     ,     maxValueSize
     ,     collateralPercent
     ,     maxCollateralInputs
     , not costPerByte
     ]

   babbagePParamFieldsRequirements :: [Bool]
   babbagePParamFieldsRequirements =
     [ not costPerWord
     ,     cModel
     ,     prices
     ,     maxTxUnits
     ,     maxBlockExUnits
     ,     maxValueSize
     ,     collateralPercent
     ,     maxCollateralInputs
     ,     costPerByte
     , not decentralization
     , not extraPraosEntropy
     ]

   checkAlonzoParams :: Either ProtocolParametersError ()
   checkAlonzoParams = do
     if all (== True) alonzoPParamFieldsRequirements
     then return ()
     else Left PParamsErrorMissingAlonzoProtocolParameter

   checkBabbageParams :: Either ProtocolParametersError ()
   checkBabbageParams =
     if all (== True) babbagePParamFieldsRequirements
     then return ()
     else Left PParamsErrorMissingAlonzoProtocolParameter

   checkMinUTxOVal :: Either ProtocolParametersError ()
   checkMinUTxOVal =
     if isJust protocolParamMinUTxOValue
     then return ()
     else Left . PParamsErrorMissingMinUTxoValue
               $ AnyCardanoEra era
