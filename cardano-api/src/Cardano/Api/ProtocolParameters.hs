{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
    BundledProtocolParameters(..),
    bundleProtocolParams,
    unbundleProtocolParams,
    unbundleLedgerShelleyBasedProtocolParams,

    -- * Updates to the protocol parameters
    ProtocolParametersUpdate(..),

    -- * PraosNonce
    PraosNonce,
    makePraosNonce,

    -- * Execution units, prices and cost models,
    ExecutionUnits(..),
    ExecutionUnitPrices(..),
    CostModel(..),
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
    toLedgerPParamsEither,
    toLedgerPParamsUpdate,
    toLedgerPParamsUpdateEither,
    fromLedgerPParams,
    fromLedgerPParamsUpdate,
    toAlonzoPrices,
    fromAlonzoPrices,
    toAlonzoScriptLanguage,
    fromAlonzoScriptLanguage,
    toAlonzoCostModel,
    fromAlonzoCostModel,
    toAlonzoCostModels,

    -- * Data family instances
    AsType(..),
  ) where

import           Control.Monad
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?),
                   (.=))
import           Data.Bifunctor (bimap, first)
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Maybe.Strict (StrictMaybe(..))
import           Data.String (IsString)
import           GHC.Generics
import           Lens.Micro
import           Numeric.Natural

import           Cardano.Api.Json (toRationalJSON)
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochNo)

import qualified Cardano.Ledger.Api.Era as Ledger
import           Cardano.Ledger.Api.PParams
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger

-- Some of the things from Cardano.Ledger.ShelleyPParams are generic across all
-- eras, and some are specific to the Shelley era (and other pre-Alonzo eras).
-- So we import in twice under different names.

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
--import           Cardano.Ledger.Alonzo.PParams ()
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Shelley.API as Ledger

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
       protocolParamTxFeeFixed :: Lovelace,

       -- | Per byte linear factor for the minimum fee calculation.
       --
       protocolParamTxFeePerByte :: Lovelace,

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
       protocolUpdateTxFeeFixed :: Maybe Lovelace,

       -- | The linear factor for the minimum fee calculation.
       --
       protocolUpdateTxFeePerByte :: Maybe Lovelace,

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

    deserialiseFromRawBytes AsPraosNonce bs =
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


toAlonzoPrices :: ExecutionUnitPrices -> Either String Alonzo.Prices
toAlonzoPrices ExecutionUnitPrices {
                 priceExecutionSteps,
                 priceExecutionMemory
               } = do
  prSteps <- boundRationalEither "Steps" priceExecutionSteps
  prMem   <- boundRationalEither "Mem" priceExecutionMemory
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

newtype CostModel = CostModel [Integer] --TODO: decide if we need a Map or a list: (Map Text Integer)
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (ToCBOR, FromCBOR)

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
  Right (Alonzo.emptyCostModels { Alonzo.costModelsValid = Map.fromList f })
 where
  conv :: (AnyPlutusScriptVersion, CostModel) -> Either String (Alonzo.Language, Alonzo.CostModel)
  conv (anySVer, cModel) = do
    -- TODO: Propagate InvalidCostModel further
    alonzoCostModel <- first displayError $ toAlonzoCostModel cModel (toAlonzoScriptLanguage anySVer)
    Right (toAlonzoScriptLanguage anySVer, alonzoCostModel)

fromAlonzoCostModels
  :: Alonzo.CostModels
  -> Map AnyPlutusScriptVersion CostModel
fromAlonzoCostModels (Alonzo.CostModels m _ _) =
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
               => Ledger.EraCrypto ledgerera ~ StandardCrypto
               => ShelleyBasedEra era
               -> UpdateProposal
               -> Ledger.Update ledgerera
toLedgerUpdate era (UpdateProposal ppup epochno) =
    Ledger.Update (toLedgerProposedPPUpdates era ppup) epochno


toLedgerProposedPPUpdates :: forall era ledgerera.
                             ShelleyLedgerEra era ~ ledgerera
                          => Ledger.EraCrypto ledgerera ~ StandardCrypto
                          => ShelleyBasedEra era
                          -> Map (Hash GenesisKey) ProtocolParametersUpdate
                          -> Ledger.ProposedPPUpdates ledgerera
toLedgerProposedPPUpdates era =
    Ledger.ProposedPPUpdates
  . Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh)
  . Map.map (toLedgerPParamsUpdate era)

-- TODO: Stop using partial function and switch to the Either String variant
toLedgerPParamsUpdate :: ShelleyBasedEra era
                      -> ProtocolParametersUpdate
                      -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
toLedgerPParamsUpdate sbe = either error id . toLedgerPParamsUpdateEither sbe

toLedgerPParamsUpdateEither :: ShelleyBasedEra era
                            -> ProtocolParametersUpdate
                            -> Either String (PParamsUpdate (ShelleyLedgerEra era))
toLedgerPParamsUpdateEither ShelleyBasedEraShelley = toShelleyPParamsUpdate
toLedgerPParamsUpdateEither ShelleyBasedEraAllegra = toShelleyPParamsUpdate
toLedgerPParamsUpdateEither ShelleyBasedEraMary    = toShelleyPParamsUpdate
toLedgerPParamsUpdateEither ShelleyBasedEraAlonzo  = toAlonzoPParamsUpdate
toLedgerPParamsUpdateEither ShelleyBasedEraBabbage = toBabbagePParamsUpdate
toLedgerPParamsUpdateEither ShelleyBasedEraConway  = toConwayPParamsUpdate


toShelleyCommonPParamsUpdate :: EraPParams ledgerera
                             => ProtocolParametersUpdate
                             -> Either String (PParamsUpdate ledgerera)
toShelleyCommonPParamsUpdate
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
    } = do
  a0 <- mapM (boundRationalEither "A0") protocolUpdatePoolPledgeInfluence
  rho <- mapM (boundRationalEither "Rho") protocolUpdateMonetaryExpansion
  tau <- mapM (boundRationalEither "Tau") protocolUpdateTreasuryCut
  protVer <- mapM mkProtVer protocolUpdateProtocolVersion
  let ppuCommon =
        emptyPParamsUpdate
        & ppuMinFeeAL     .~
          (toShelleyLovelace <$> noInlineMaybeToStrictMaybe protocolUpdateTxFeePerByte)
        & ppuMinFeeBL     .~
          (toShelleyLovelace <$> noInlineMaybeToStrictMaybe protocolUpdateTxFeeFixed)
        & ppuMaxBBSizeL   .~ noInlineMaybeToStrictMaybe protocolUpdateMaxBlockBodySize
        & ppuMaxTxSizeL   .~ noInlineMaybeToStrictMaybe protocolUpdateMaxTxSize
        & ppuMaxBHSizeL   .~ noInlineMaybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
        & ppuKeyDepositL  .~
          (toShelleyLovelace <$> noInlineMaybeToStrictMaybe protocolUpdateStakeAddressDeposit)
        & ppuPoolDepositL .~
          (toShelleyLovelace <$> noInlineMaybeToStrictMaybe protocolUpdateStakePoolDeposit)
        & ppuEMaxL        .~ noInlineMaybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
        & ppuNOptL        .~ noInlineMaybeToStrictMaybe protocolUpdateStakePoolTargetNum
        & ppuA0L          .~ noInlineMaybeToStrictMaybe a0

        & ppuRhoL         .~ noInlineMaybeToStrictMaybe rho
        & ppuTauL         .~ noInlineMaybeToStrictMaybe tau
        & ppuProtocolVersionL .~ noInlineMaybeToStrictMaybe protVer
        & ppuMinPoolCostL     .~
          (toShelleyLovelace <$> noInlineMaybeToStrictMaybe protocolUpdateMinPoolCost)
  pure ppuCommon

toShelleyPParamsUpdate :: ( EraPParams ledgerera
                          , Ledger.AtMostEra Ledger.MaryEra ledgerera
                          , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                          )
                       => ProtocolParametersUpdate
                       -> Either String (PParamsUpdate ledgerera)
toShelleyPParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateDecentralization
    , protocolUpdateExtraPraosEntropy
    , protocolUpdateMinUTxOValue
    } = do
  ppuCommon <- toShelleyCommonPParamsUpdate protocolParametersUpdate
  d <- mapM (boundRationalEither "D") protocolUpdateDecentralization
  let ppuShelley =
        ppuCommon
        & ppuDL            .~ noInlineMaybeToStrictMaybe d
        & ppuExtraEntropyL .~
          (toLedgerNonce <$> noInlineMaybeToStrictMaybe protocolUpdateExtraPraosEntropy)
        & ppuMinUTxOValueL .~
          (toShelleyLovelace <$> noInlineMaybeToStrictMaybe protocolUpdateMinUTxOValue)
  pure ppuShelley


toAlonzoCommonPParamsUpdate :: AlonzoEraPParams ledgerera
                            => ProtocolParametersUpdate
                            -> Either String (PParamsUpdate ledgerera)
toAlonzoCommonPParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateCostModels
    , protocolUpdatePrices
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxCollateralInputs
    } = do
  ppuShelleyCommon <- toShelleyCommonPParamsUpdate protocolParametersUpdate
  costModels <-
    if Map.null protocolUpdateCostModels
    then pure SNothing
    else SJust <$> toAlonzoCostModels protocolUpdateCostModels
  prices <- mapM toAlonzoPrices protocolUpdatePrices
  let ppuAlonzoCommon =
        ppuShelleyCommon
        & ppuCostModelsL           .~ costModels
        & ppuPricesL               .~ noInlineMaybeToStrictMaybe prices
        & ppuMaxTxExUnitsL         .~
          (toAlonzoExUnits <$> noInlineMaybeToStrictMaybe protocolUpdateMaxTxExUnits)
        & ppuMaxBlockExUnitsL      .~
          (toAlonzoExUnits <$> noInlineMaybeToStrictMaybe protocolUpdateMaxBlockExUnits)
        & ppuMaxValSizeL           .~ noInlineMaybeToStrictMaybe protocolUpdateMaxValueSize
        & ppuCollateralPercentageL .~ noInlineMaybeToStrictMaybe protocolUpdateCollateralPercent
        & ppuMaxCollateralInputsL  .~ noInlineMaybeToStrictMaybe protocolUpdateMaxCollateralInputs
  pure ppuAlonzoCommon


toAlonzoPParamsUpdate :: Ledger.Crypto crypto
                      => ProtocolParametersUpdate
                      -> Either String (PParamsUpdate (Ledger.AlonzoEra crypto))
toAlonzoPParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateDecentralization
    , protocolUpdateUTxOCostPerWord
    } = do
  ppuAlonzoCommon <- toAlonzoCommonPParamsUpdate protocolParametersUpdate
  d <- mapM (boundRationalEither "D") protocolUpdateDecentralization
  let ppuAlonzo =
        ppuAlonzoCommon
        & ppuDL .~ noInlineMaybeToStrictMaybe d
        & ppuCoinsPerUTxOWordL .~
          (CoinPerWord . toShelleyLovelace <$>
           noInlineMaybeToStrictMaybe protocolUpdateUTxOCostPerWord)
  pure ppuAlonzo


toBabbagePParamsUpdate :: BabbageEraPParams ledgerera
                       => ProtocolParametersUpdate
                       -> Either String (PParamsUpdate ledgerera)
toBabbagePParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateUTxOCostPerByte
    } = do
  ppuAlonzoCommon <- toAlonzoCommonPParamsUpdate protocolParametersUpdate
  let ppuBabbage =
        ppuAlonzoCommon
        & ppuCoinsPerUTxOByteL .~
          (CoinPerByte . toShelleyLovelace <$>
           noInlineMaybeToStrictMaybe protocolUpdateUTxOCostPerByte)
  pure ppuBabbage

requireParam :: String -> (a -> Either String b) -> Maybe a -> Either String b
requireParam paramName f = maybe (Left $ "Must specify " ++ paramName) f

mkProtVer :: (Natural, Natural) -> Either String Ledger.ProtVer
mkProtVer (majorProtVer, minorProtVer) =
  case Ledger.mkVersion majorProtVer of
    Nothing -> Left $ "Major protocol version is invalid: " ++ show majorProtVer
    Just v -> Right $ Ledger.ProtVer v minorProtVer

boundRationalEither :: Ledger.BoundedRational b
                    => String
                    -> Rational
                    -> Either String b
boundRationalEither name r =
  case Ledger.boundRational r of
    Just br -> Right br
    Nothing -> Left $ "Rational value for '" ++ name ++ "' is outside of bounds: " ++ show r

-- Conway uses the same PParams as Babbage for now.
toConwayPParamsUpdate :: BabbageEraPParams ledgerera
                      => ProtocolParametersUpdate
                      -> Either String (PParamsUpdate ledgerera)
toConwayPParamsUpdate = toBabbagePParamsUpdate

-- ----------------------------------------------------------------------------
-- Conversion functions: updates from ledger types
--

fromLedgerUpdate :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => Ledger.EraCrypto ledgerera ~ StandardCrypto
                 => ShelleyBasedEra era
                 -> Ledger.Update ledgerera
                 -> UpdateProposal
fromLedgerUpdate era (Ledger.Update ppup epochno) =
    UpdateProposal (fromLedgerProposedPPUpdates era ppup) epochno


fromLedgerProposedPPUpdates :: forall era ledgerera.
                               ShelleyLedgerEra era ~ ledgerera
                            => Ledger.EraCrypto ledgerera ~ StandardCrypto
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
fromLedgerPParamsUpdate ShelleyBasedEraConway  = fromConwayPParamsUpdate



fromShelleyCommonPParamsUpdate :: EraPParams ledgerera
                               => PParamsUpdate ledgerera
                               -> ProtocolParametersUpdate
fromShelleyCommonPParamsUpdate ppu =
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion     = (\(Ledger.ProtVer a b) -> (Ledger.getVersion a,b)) <$>
                                          strictMaybeToMaybe (ppu ^. ppuProtocolVersionL)
    , protocolUpdateMaxBlockHeaderSize  = strictMaybeToMaybe (ppu ^. ppuMaxBHSizeL)
    , protocolUpdateMaxBlockBodySize    = strictMaybeToMaybe (ppu ^. ppuMaxBBSizeL)
    , protocolUpdateMaxTxSize           = strictMaybeToMaybe (ppu ^. ppuMaxTxSizeL)
    , protocolUpdateTxFeeFixed          = fromShelleyLovelace <$>
                                            strictMaybeToMaybe (ppu ^. ppuMinFeeBL)
    , protocolUpdateTxFeePerByte        = fromShelleyLovelace <$>
                                            strictMaybeToMaybe (ppu ^. ppuMinFeeAL)
    , protocolUpdateStakeAddressDeposit = fromShelleyLovelace <$>
                                            strictMaybeToMaybe (ppu ^. ppuKeyDepositL)
    , protocolUpdateStakePoolDeposit    = fromShelleyLovelace <$>
                                            strictMaybeToMaybe (ppu ^. ppuPoolDepositL)
    , protocolUpdateMinPoolCost         = fromShelleyLovelace <$>
                                            strictMaybeToMaybe (ppu ^. ppuMinPoolCostL)
    , protocolUpdatePoolRetireMaxEpoch  = strictMaybeToMaybe (ppu ^. ppuEMaxL)
    , protocolUpdateStakePoolTargetNum  = strictMaybeToMaybe (ppu ^. ppuNOptL)
    , protocolUpdatePoolPledgeInfluence = Ledger.unboundRational <$>
                                            strictMaybeToMaybe (ppu ^. ppuA0L)
    , protocolUpdateMonetaryExpansion   = Ledger.unboundRational <$>
                                            strictMaybeToMaybe (ppu ^. ppuRhoL)
    , protocolUpdateTreasuryCut         = Ledger.unboundRational <$>
                                            strictMaybeToMaybe (ppu ^. ppuTauL)
    , protocolUpdateUTxOCostPerWord     = Nothing
    , protocolUpdateCostModels          = mempty
    , protocolUpdatePrices              = Nothing
    , protocolUpdateMaxTxExUnits        = Nothing
    , protocolUpdateMaxBlockExUnits     = Nothing
    , protocolUpdateMaxValueSize        = Nothing
    , protocolUpdateCollateralPercent   = Nothing
    , protocolUpdateMaxCollateralInputs = Nothing
    , protocolUpdateUTxOCostPerByte     = Nothing
    , protocolUpdateDecentralization    = Nothing
    , protocolUpdateExtraPraosEntropy   = Nothing
    , protocolUpdateMinUTxOValue        = Nothing
    }

fromShelleyPParamsUpdate :: ( EraPParams ledgerera
                            , Ledger.AtMostEra Ledger.MaryEra ledgerera
                            , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                            )
                         => PParamsUpdate ledgerera
                         -> ProtocolParametersUpdate
fromShelleyPParamsUpdate ppu =
  (fromShelleyCommonPParamsUpdate ppu) {
      protocolUpdateDecentralization    = Ledger.unboundRational <$>
                                            strictMaybeToMaybe (ppu ^. ppuDL)
    , protocolUpdateExtraPraosEntropy   = fromLedgerNonce <$>
                                            strictMaybeToMaybe (ppu ^. ppuExtraEntropyL)
    , protocolUpdateMinUTxOValue        = fromShelleyLovelace <$>
                                            strictMaybeToMaybe (ppu ^. ppuMinUTxOValueL)
    }

fromAlonzoCommonPParamsUpdate :: AlonzoEraPParams ledgerera
                              => PParamsUpdate ledgerera
                              -> ProtocolParametersUpdate
fromAlonzoCommonPParamsUpdate ppu =
  (fromShelleyCommonPParamsUpdate ppu) {
      protocolUpdateCostModels          = maybe mempty fromAlonzoCostModels
                                               (strictMaybeToMaybe (ppu ^. ppuCostModelsL))
    , protocolUpdatePrices              = fromAlonzoPrices <$>
                                            strictMaybeToMaybe (ppu ^. ppuPricesL)
    , protocolUpdateMaxTxExUnits        = fromAlonzoExUnits <$>
                                            strictMaybeToMaybe (ppu ^. ppuMaxTxExUnitsL)
    , protocolUpdateMaxBlockExUnits     = fromAlonzoExUnits <$>
                                            strictMaybeToMaybe (ppu ^. ppuMaxBlockExUnitsL)
    , protocolUpdateMaxValueSize        = strictMaybeToMaybe (ppu ^. ppuMaxValSizeL)
    , protocolUpdateCollateralPercent   = strictMaybeToMaybe (ppu ^. ppuCollateralPercentageL)
    , protocolUpdateMaxCollateralInputs = strictMaybeToMaybe (ppu ^. ppuMaxCollateralInputsL)
    , protocolUpdateUTxOCostPerByte     = Nothing
    }


fromAlonzoPParamsUpdate :: Ledger.Crypto crypto
                        => PParamsUpdate (Ledger.AlonzoEra crypto)
                        -> ProtocolParametersUpdate
fromAlonzoPParamsUpdate ppu =
  (fromShelleyCommonPParamsUpdate ppu) {
    protocolUpdateUTxOCostPerWord = fromShelleyLovelace . unCoinPerWord <$>
                                      strictMaybeToMaybe (ppu ^. ppuCoinsPerUTxOWordL)
    }

fromBabbagePParamsUpdate :: BabbageEraPParams ledgerera
                         => PParamsUpdate ledgerera
                         -> ProtocolParametersUpdate
fromBabbagePParamsUpdate ppu =
  (fromAlonzoCommonPParamsUpdate ppu) {
    protocolUpdateUTxOCostPerByte = fromShelleyLovelace . unCoinPerByte <$>
                                      strictMaybeToMaybe (ppu ^. ppuCoinsPerUTxOByteL)
    }

fromConwayPParamsUpdate :: BabbageEraPParams ledgerera
                        => PParamsUpdate ledgerera
                        -> ProtocolParametersUpdate
fromConwayPParamsUpdate = fromBabbagePParamsUpdate

-- | Bundle cardano-api representation and ledger representation of protocol parameters together so
-- they can be computed once and passed around rather than re-computed unecessarily.
--
-- The consructor arguments are intentionally lazy so that the values are not computed if not used
-- (which may be the case for some code paths).
data BundledProtocolParameters era where
  BundleAsByronProtocolParameters
    :: ProtocolParameters
    -> BundledProtocolParameters ByronEra
  BundleAsShelleyBasedProtocolParameters
    :: ShelleyBasedEra era
    -> ProtocolParameters
    -> Ledger.PParams (ShelleyLedgerEra era)
    -> BundledProtocolParameters era

bundleProtocolParams :: CardanoEra era -> ProtocolParameters -> BundledProtocolParameters era
bundleProtocolParams cEra pp = case cardanoEraStyle cEra of
  LegacyByronEra -> BundleAsByronProtocolParameters pp
  ShelleyBasedEra sbe -> BundleAsShelleyBasedProtocolParameters sbe pp (toLedgerPParams sbe pp)

unbundleLedgerShelleyBasedProtocolParams
  :: ShelleyBasedEra era
  -> BundledProtocolParameters era
  -> Ledger.PParams (ShelleyLedgerEra era)
unbundleLedgerShelleyBasedProtocolParams = \case
  ShelleyBasedEraShelley -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraAllegra -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraMary -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraAlonzo -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraBabbage -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp
  ShelleyBasedEraConway -> \(BundleAsShelleyBasedProtocolParameters _ _ lpp) -> lpp

unbundleProtocolParams :: BundledProtocolParameters era -> ProtocolParameters
unbundleProtocolParams (BundleAsByronProtocolParameters pp) = pp
unbundleProtocolParams (BundleAsShelleyBasedProtocolParameters _ pp _) = pp

-- ----------------------------------------------------------------------------
-- Conversion functions: protocol parameters to ledger types
--

--TODO: Propagate the `Either String (PParams (ShelleyLedgerEra era))` to the use sites,
--rather than fail with `error` here.
toLedgerPParams
  :: ShelleyBasedEra era
  -> ProtocolParameters
  -> Ledger.PParams (ShelleyLedgerEra era)
toLedgerPParams era = either error id . toLedgerPParamsEither era

toLedgerPParamsEither :: ShelleyBasedEra era
                      -> ProtocolParameters
                      -> Either String (PParams (ShelleyLedgerEra era))
toLedgerPParamsEither ShelleyBasedEraShelley = toShelleyPParams
toLedgerPParamsEither ShelleyBasedEraAllegra = toShelleyPParams
toLedgerPParamsEither ShelleyBasedEraMary    = toShelleyPParams
toLedgerPParamsEither ShelleyBasedEraAlonzo  = toAlonzoPParams
toLedgerPParamsEither ShelleyBasedEraBabbage = toBabbagePParams
toLedgerPParamsEither ShelleyBasedEraConway  = toConwayPParams


toShelleyCommonPParams :: EraPParams ledgerera
                       => ProtocolParameters
                       -> Either String (PParams ledgerera)
toShelleyCommonPParams
    ProtocolParameters {
      protocolParamProtocolVersion
    , protocolParamMaxBlockHeaderSize
    , protocolParamMaxBlockBodySize
    , protocolParamMaxTxSize
    , protocolParamTxFeeFixed
    , protocolParamTxFeePerByte
    , protocolParamStakeAddressDeposit
    , protocolParamStakePoolDeposit
    , protocolParamMinPoolCost
    , protocolParamPoolRetireMaxEpoch
    , protocolParamStakePoolTargetNum
    , protocolParamPoolPledgeInfluence
    , protocolParamMonetaryExpansion
    , protocolParamTreasuryCut
    } = do
  a0 <- boundRationalEither "A0" protocolParamPoolPledgeInfluence
  rho <- boundRationalEither "Rho" protocolParamMonetaryExpansion
  tau <- boundRationalEither "Tau" protocolParamTreasuryCut
  protVer <- mkProtVer protocolParamProtocolVersion
  let ppCommon =
        emptyPParams
        & ppMinFeeAL         .~ toShelleyLovelace protocolParamTxFeePerByte
        & ppMinFeeBL         .~ toShelleyLovelace protocolParamTxFeeFixed
        & ppMaxBBSizeL       .~ protocolParamMaxBlockBodySize
        & ppMaxTxSizeL       .~ protocolParamMaxTxSize
        & ppMaxBHSizeL       .~ protocolParamMaxBlockHeaderSize
        & ppKeyDepositL      .~ toShelleyLovelace protocolParamStakeAddressDeposit
        & ppPoolDepositL     .~ toShelleyLovelace protocolParamStakePoolDeposit
        & ppEMaxL            .~ protocolParamPoolRetireMaxEpoch
        & ppNOptL            .~ protocolParamStakePoolTargetNum
        & ppA0L              .~ a0
        & ppRhoL             .~ rho
        & ppTauL             .~ tau
        & ppProtocolVersionL .~ protVer
        & ppMinPoolCostL     .~ toShelleyLovelace protocolParamMinPoolCost
  pure ppCommon

toShelleyPParams :: ( EraPParams ledgerera
                    , Ledger.AtMostEra Ledger.MaryEra ledgerera
                    , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                    )
                 => ProtocolParameters
                 -> Either String (PParams ledgerera)
toShelleyPParams
    protocolParameters@ProtocolParameters {
      protocolParamDecentralization
    , protocolParamExtraPraosEntropy
    , protocolParamMinUTxOValue
    } = do
  ppCommon <- toShelleyCommonPParams protocolParameters
  d <- case protocolParamDecentralization of
    Nothing -> Left "Missing Decentralization parameter"
    Just dr -> boundRationalEither "D" dr
  minUTxOValue <-
    maybe (Left "toShelleyPParams: must specify protocolParamMinUTxOValue") Right
    protocolParamMinUTxOValue
  let ppShelley =
        ppCommon
        & ppDL            .~ d
        & ppExtraEntropyL .~ toLedgerNonce protocolParamExtraPraosEntropy
        & ppMinUTxOValueL .~ toShelleyLovelace minUTxOValue
  pure ppShelley


toAlonzoCommonPParams :: AlonzoEraPParams ledgerera
                      => ProtocolParameters
                      -> Either String (PParams ledgerera)
toAlonzoCommonPParams
    protocolParameters@ProtocolParameters {
      protocolParamCostModels
    , protocolParamPrices
    , protocolParamMaxTxExUnits
    , protocolParamMaxBlockExUnits
    , protocolParamMaxValueSize
    , protocolParamCollateralPercent
    , protocolParamMaxCollateralInputs
    } = do
  ppShelleyCommon <- toShelleyCommonPParams protocolParameters
  costModels <- toAlonzoCostModels protocolParamCostModels
  prices <-
    requireParam "protocolParamPrices" toAlonzoPrices protocolParamPrices
  maxTxExUnits <-
    requireParam "protocolParamMaxTxExUnits" Right protocolParamMaxTxExUnits
  maxBlockExUnits <-
    requireParam "protocolParamMaxBlockExUnits" Right protocolParamMaxBlockExUnits
  maxValueSize <-
    requireParam "protocolParamMaxBlockExUnits" Right  protocolParamMaxValueSize
  collateralPercent <-
    requireParam "protocolParamCollateralPercent" Right protocolParamCollateralPercent
  maxCollateralInputs <-
    requireParam "protocolParamMaxCollateralInputs" Right protocolParamMaxCollateralInputs
  let ppAlonzoCommon =
        ppShelleyCommon
        & ppCostModelsL           .~ costModels
        & ppPricesL               .~ prices
        & ppMaxTxExUnitsL         .~ toAlonzoExUnits maxTxExUnits
        & ppMaxBlockExUnitsL      .~ toAlonzoExUnits maxBlockExUnits
        & ppMaxValSizeL           .~ maxValueSize
        & ppCollateralPercentageL .~ collateralPercent
        & ppMaxCollateralInputsL  .~ maxCollateralInputs
  pure ppAlonzoCommon

toAlonzoPParams :: Ledger.Crypto crypto
                => ProtocolParameters
                -> Either String (PParams (Ledger.AlonzoEra crypto))
toAlonzoPParams
    protocolParameters@ProtocolParameters {
      protocolParamDecentralization
    , protocolParamUTxOCostPerWord
    } = do
  ppAlonzoCommon <- toAlonzoCommonPParams protocolParameters
  -- QUESTION? This is strange, why do we need to construct Alonzo Tx with Babbage PParams?
  -- This feels to me like an issue with the api design, as there should never be such an
  -- incosistency, because PParams affect the validity of the transaction.
  d <- case protocolParamDecentralization of
         -- The decentralization parameter is deprecated in Babbage
         -- so we default to 0 if no decentralization parameter is found
         -- in the api's 'ProtocolParameter' type. If we don't do this
         -- we won't be able to construct an Alonzo tx using the Babbage
         -- era's protocol parameter because our only other option is to
         -- error.
         Nothing -> Right minBound
         Just dParam -> boundRationalEither "D" dParam
  -- This is the correct implementation that should be the used instead:
  -- d <- requireParam "protocolParamDecentralization"
  --                   (boundRationalEither "D")
  --                   protocolParamDecentralization
  utxoCostPerWord <-
    requireParam "protocolParamUTxOCostPerWord" Right protocolParamUTxOCostPerWord
  let ppAlonzo =
        ppAlonzoCommon
        & ppDL .~ d
        & ppCoinsPerUTxOWordL .~ CoinPerWord (toShelleyLovelace utxoCostPerWord)
  pure ppAlonzo


toBabbagePParams :: BabbageEraPParams ledgerera
                 => ProtocolParameters
                 -> Either String (PParams ledgerera)
toBabbagePParams
    protocolParameters@ProtocolParameters {
      protocolParamUTxOCostPerByte
    } = do
  ppAlonzoCommon <- toAlonzoCommonPParams protocolParameters
  utxoCostPerByte <-
    requireParam "protocolParamUTxOCostPerByte" Right protocolParamUTxOCostPerByte
  let ppBabbage =
        ppAlonzoCommon
        & ppCoinsPerUTxOByteL .~ CoinPerByte (toShelleyLovelace utxoCostPerByte)
  pure ppBabbage

toConwayPParams :: BabbageEraPParams ledgerera
                => ProtocolParameters
                -> Either String (PParams ledgerera)
toConwayPParams = toBabbagePParams

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
fromLedgerPParams ShelleyBasedEraConway  = fromConwayPParams


fromShelleyCommonPParams :: EraPParams ledgerera
                         => PParams ledgerera
                         -> ProtocolParameters
fromShelleyCommonPParams pp =
    ProtocolParameters {
      protocolParamProtocolVersion     = case pp ^. ppProtocolVersionL of
                                           Ledger.ProtVer a b -> (Ledger.getVersion a, b)
    , protocolParamMaxBlockHeaderSize  = pp ^. ppMaxBHSizeL
    , protocolParamMaxBlockBodySize    = pp ^. ppMaxBBSizeL
    , protocolParamMaxTxSize           = pp ^. ppMaxTxSizeL
    , protocolParamTxFeeFixed          = fromShelleyLovelace (pp ^. ppMinFeeBL)
    , protocolParamTxFeePerByte        = fromShelleyLovelace (pp ^. ppMinFeeAL)
    , protocolParamStakeAddressDeposit = fromShelleyLovelace (pp ^. ppKeyDepositL)
    , protocolParamStakePoolDeposit    = fromShelleyLovelace (pp ^. ppPoolDepositL)
    , protocolParamMinPoolCost         = fromShelleyLovelace (pp ^. ppMinPoolCostL)
    , protocolParamPoolRetireMaxEpoch  = pp ^. ppEMaxL
    , protocolParamStakePoolTargetNum  = pp ^. ppNOptL
    , protocolParamPoolPledgeInfluence = Ledger.unboundRational (pp ^. ppA0L)
    , protocolParamMonetaryExpansion   = Ledger.unboundRational (pp ^. ppRhoL)
    , protocolParamTreasuryCut         = Ledger.unboundRational (pp ^. ppTauL)
    , protocolParamUTxOCostPerWord     = Nothing -- Obsolete from Babbage onwards
    , protocolParamCostModels          = mempty  -- Only from Alonzo onwards
    , protocolParamPrices              = Nothing -- Only from Alonzo onwards
    , protocolParamMaxTxExUnits        = Nothing -- Only from Alonzo onwards
    , protocolParamMaxBlockExUnits     = Nothing -- Only from Alonzo onwards
    , protocolParamMaxValueSize        = Nothing -- Only from Alonzo onwards
    , protocolParamCollateralPercent   = Nothing -- Only from Alonzo onwards
    , protocolParamMaxCollateralInputs = Nothing -- Only from Alonzo onwards
    , protocolParamUTxOCostPerByte     = Nothing -- Only from Babbage onwards
    , protocolParamDecentralization    = Nothing -- Obsolete from Babbage onwards
    , protocolParamExtraPraosEntropy   = Nothing -- Obsolete from Alonzo onwards
    , protocolParamMinUTxOValue        = Nothing -- Obsolete from Alonzo onwards
    }

fromShelleyPParams :: ( EraPParams ledgerera
                      , Ledger.AtMostEra Ledger.MaryEra ledgerera
                      , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                      )
                   => PParams ledgerera
                   -> ProtocolParameters
fromShelleyPParams pp =
  (fromShelleyCommonPParams pp) {
      protocolParamDecentralization    = Just . Ledger.unboundRational $ pp ^. ppDL
    , protocolParamExtraPraosEntropy   = fromLedgerNonce $ pp ^. ppExtraEntropyL
    , protocolParamMinUTxOValue        = Just . fromShelleyLovelace $ pp ^. ppMinUTxOValueL
    }


fromAlonzoCommonPParams :: AlonzoEraPParams ledgerera
                        => PParams ledgerera
                        -> ProtocolParameters
fromAlonzoCommonPParams pp =
  (fromShelleyCommonPParams pp) {
      protocolParamCostModels          = fromAlonzoCostModels $ pp ^. ppCostModelsL
    , protocolParamPrices              = Just . fromAlonzoPrices $ pp ^. ppPricesL
    , protocolParamMaxTxExUnits        = Just . fromAlonzoExUnits $ pp ^. ppMaxTxExUnitsL
    , protocolParamMaxBlockExUnits     = Just . fromAlonzoExUnits $ pp ^. ppMaxBlockExUnitsL
    , protocolParamMaxValueSize        = Just $ pp ^. ppMaxValSizeL
    , protocolParamCollateralPercent   = Just $ pp ^. ppCollateralPercentageL
    , protocolParamMaxCollateralInputs = Just $ pp ^. ppMaxCollateralInputsL
    }


fromAlonzoPParams :: Ledger.Crypto crypto
                  => PParams (Ledger.AlonzoEra crypto)
                  -> ProtocolParameters
fromAlonzoPParams pp =
  (fromShelleyCommonPParams pp) {
    protocolParamUTxOCostPerWord = Just . fromShelleyLovelace . unCoinPerWord $
                                     pp ^. ppCoinsPerUTxOWordL
    }

fromBabbagePParams :: BabbageEraPParams ledgerera
                   => PParams ledgerera
                   -> ProtocolParameters
fromBabbagePParams pp =
  (fromAlonzoCommonPParams pp) {
    protocolParamUTxOCostPerByte = Just . fromShelleyLovelace . unCoinPerByte $
                                     pp ^. ppCoinsPerUTxOByteL
    }

fromConwayPParams :: BabbageEraPParams ledgerera
                  => PParams ledgerera
                  -> ProtocolParameters
fromConwayPParams = fromBabbagePParams

data ProtocolParametersError =
    PParamsErrorMissingMinUTxoValue AnyCardanoEra
  | PParamsErrorMissingAlonzoProtocolParameter
  deriving Show

instance Error ProtocolParametersError where
  displayError (PParamsErrorMissingMinUTxoValue (AnyCardanoEra era)) = mconcat
    [ "The " <> show era <> " protocol parameters value is missing the following "
    , "field: MinUTxoValue. Did you intend to use a " <> show era <> " protocol "
    , " parameters value?"
    ]
  displayError PParamsErrorMissingAlonzoProtocolParameter = mconcat
    [ "The Alonzo era protocol parameters in use is missing one or more of the "
    , "following fields: UTxOCostPerWord, CostModels, Prices, MaxTxExUnits, "
    , "MaxBlockExUnits, MaxValueSize, CollateralPercent, MaxCollateralInputs. Did "
    , "you intend to use an Alonzo era protocol parameters value?"
    ]

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
    ShelleyBasedEraConway -> checkBabbageParams
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
