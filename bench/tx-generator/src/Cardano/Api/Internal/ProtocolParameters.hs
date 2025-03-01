{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{--
Due to the changes to "cardano-api" listed below it was decided to move
cardano-api's `ProtocolParameters` type to tx-generator and maintain it.
Relevant comment (https://github.com/IntersectMBO/cardano-api/issues/384#issuecomment-2678888478)

cardano-api
- Issue:
- - Remove cardano-api's ProtocolParameters (it has been deprecated for a while)

- - https://github.com/IntersectMBO/cardano-api/issues/384#issuecomment-2678888478
- PR:
- - "Remove ProtocolParameters"
- - `ProtocolParameters` had been deprecated in favor the ledger's PParams type.
-   We are now removing ProtocolParameters altogether.
- - https://github.com/IntersectMBO/cardano-api/pull/729
--}

-- | The current values of updatable protocol parameters: 'ProtocolParameters'
module Cardano.Api.Internal.ProtocolParameters
  ( -- * The updatable protocol parameters
    ProtocolParameters (
      ProtocolParameters
    , protocolParamProtocolVersion
    , protocolParamCostModels
    , protocolParamPrices
    , protocolParamMaxTxExUnits
    , protocolParamMaxBlockExUnits
    )
  , convertToLedgerProtocolParameters
  , toLedgerPParams
  , fromLedgerPParams
  )
where

--------------------------------------------------------------------------------

import GHC.Generics

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Api.Era as Ledger
import           Cardano.Ledger.Api.PParams
import qualified Cardano.Ledger.Babbage.Core as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as L

import qualified Data.Aeson as Aeson
import Data.Aeson( (.!=), (.:), (.:?), (.=) )
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either.Combinators (maybeToRight)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import Data.Word (Word16)
import Lens.Micro ( (^.), (.~), (&) )
import Numeric.Natural (Natural)

import Cardano.Api (
    AnyPlutusScriptVersion (..)
  , ExecutionUnits

  , ProtocolParametersConversionError (..)

  , CostModel

  , PraosNonce, makePraosNonce
  )
import Cardano.Api.Shelley (
    LedgerProtocolParameters (..)
  , ShelleyBasedEra (
        ShelleyBasedEraShelley
      , ShelleyBasedEraAllegra
      , ShelleyBasedEraMary
      , ShelleyBasedEraAlonzo
      , ShelleyBasedEraBabbage
      , ShelleyBasedEraConway
    )
  , ShelleyLedgerEra
  , toAlonzoExUnits, fromAlonzoExUnits
  , ExecutionUnitPrices (..), toAlonzoPrices, fromAlonzoPrices
  , toAlonzoCostModels
  , fromAlonzoCostModels
  , toLedgerNonce
  )

-- -----------------------------------------------------------------------------
-- Era based ledger protocol parameters
--

{-# DEPRECATED
  convertToLedgerProtocolParameters
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
convertToLedgerProtocolParameters
  :: ShelleyBasedEra era
  -> ProtocolParameters
  -> Either ProtocolParametersConversionError (LedgerProtocolParameters era)
convertToLedgerProtocolParameters sbe pp =
  LedgerProtocolParameters <$> toLedgerPParams sbe pp

-- -----------------------------------------------------------------------------
-- Era based Ledger protocol parameters update
--

-- | The values of the set of /updatable/ protocol parameters. At any
-- particular point on the chain there is a current set of parameters in use.
--
-- There are also parameters fixed in the Genesis file. See 'GenesisParameters'.
{-# DEPRECATED
  ProtocolParameters
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}

data ProtocolParameters
  = ProtocolParameters
  { protocolParamProtocolVersion :: (Natural, Natural)
  -- ^ Protocol version, major and minor. Updating the major version is
  -- used to trigger hard forks.
  --                              (Major  , Minor  )
  , protocolParamDecentralization :: Maybe Rational
  -- ^ The decentralization parameter. This is fraction of slots that
  -- belong to the BFT overlay schedule, rather than the Praos schedule.
  -- So 1 means fully centralised, while 0 means fully decentralised.
  --
  -- This is the \"d\" parameter from the design document.
  --
  -- /Deprecated in Babbage/
  , protocolParamExtraPraosEntropy :: Maybe PraosNonce
  -- ^ Extra entropy for the Praos per-epoch nonce.
  --
  -- This can be used to add extra entropy during the decentralisation
  -- process. If the extra entropy can be demonstrated to be generated
  -- randomly then this method can be used to show that the initial
  -- federated operators did not subtly bias the initial schedule so that
  -- they retain undue influence after decentralisation.
  , protocolParamMaxBlockHeaderSize :: Natural
  -- ^ The maximum permitted size of a block header.
  --
  -- This must be at least as big as the largest legitimate block headers
  -- but should not be too much larger, to help prevent DoS attacks.
  --
  -- Caution: setting this to be smaller than legitimate block headers is
  -- a sure way to brick the system!
  , protocolParamMaxBlockBodySize :: Natural
  -- ^ The maximum permitted size of the block body (that is, the block
  -- payload, without the block header).
  --
  -- This should be picked with the Praos network delta security parameter
  -- in mind. Making this too large can severely weaken the Praos
  -- consensus properties.
  --
  -- Caution: setting this to be smaller than a transaction that can
  -- change the protocol parameters is a sure way to brick the system!
  , protocolParamMaxTxSize :: Natural
  -- ^ The maximum permitted size of a transaction.
  --
  -- Typically this should not be too high a fraction of the block size,
  -- otherwise wastage from block fragmentation becomes a problem, and
  -- the current implementation does not use any sophisticated box packing
  -- algorithm.
  , protocolParamTxFeeFixed :: L.Coin
  -- ^ The constant factor for the minimum fee calculation.
  , protocolParamTxFeePerByte :: L.Coin
  -- ^ Per byte linear factor for the minimum fee calculation.
  , protocolParamMinUTxOValue :: Maybe L.Coin
  -- ^ The minimum permitted value for new UTxO entries, ie for
  -- transaction outputs.
  , protocolParamStakeAddressDeposit :: L.Coin
  -- ^ The deposit required to register a stake address.
  , protocolParamStakePoolDeposit :: L.Coin
  -- ^ The deposit required to register a stake pool.
  , protocolParamMinPoolCost :: L.Coin
  -- ^ The minimum value that stake pools are permitted to declare for
  -- their cost parameter.
  , protocolParamPoolRetireMaxEpoch :: Ledger.EpochInterval
  -- ^ The maximum number of epochs into the future that stake pools
  -- are permitted to schedule a retirement.
  , protocolParamStakePoolTargetNum :: Word16
  -- ^ The equilibrium target number of stake pools.
  --
  -- This is the \"k\" incentives parameter from the design document.
  , protocolParamPoolPledgeInfluence :: Rational
  -- ^ The influence of the pledge in stake pool rewards.
  --
  -- This is the \"a_0\" incentives parameter from the design document.
  , protocolParamMonetaryExpansion :: Rational
  -- ^ The monetary expansion rate. This determines the fraction of the
  -- reserves that are added to the fee pot each epoch.
  --
  -- This is the \"rho\" incentives parameter from the design document.
  , protocolParamTreasuryCut :: Rational
  -- ^ The fraction of the fee pot each epoch that goes to the treasury.
  --
  -- This is the \"tau\" incentives parameter from the design document.
{-- To avoid defining `CostModel` and `CostModels` here again.
  , protocolParamCostModels :: Map AnyPlutusScriptVersion CostModel
--}
  , protocolParamCostModels :: Map.Map AnyPlutusScriptVersion CostModel
  -- ^ Cost models for script languages that use them.
  --
  -- /Introduced in Alonzo/
  , protocolParamPrices :: Maybe ExecutionUnitPrices
  -- ^ Price of execution units for script languages that use them.
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxTxExUnits :: Maybe ExecutionUnits
  -- ^ Max total script execution resources units allowed per tx
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxBlockExUnits :: Maybe ExecutionUnits
  -- ^ Max total script execution resources units allowed per block
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxValueSize :: Maybe Natural
  -- ^ Max size of a Value in a tx output.
  --
  -- /Introduced in Alonzo/
  , protocolParamCollateralPercent :: Maybe Natural
  -- ^ The percentage of the script contribution to the txfee that must be
  -- provided as collateral inputs when including Plutus scripts.
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxCollateralInputs :: Maybe Natural
  -- ^ The maximum number of collateral inputs allowed in a transaction.
  --
  -- /Introduced in Alonzo/
  , protocolParamUTxOCostPerByte :: Maybe L.Coin
  -- ^ Cost in ada per byte of UTxO storage.
  --
  -- /Introduced in Babbage/
  }
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON ProtocolParameters where
  parseJSON =
    Aeson.withObject "ProtocolParameters" $ \o -> do
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
{-- DIFF: To avoid defining `CostModel` and `CostModels` here again.
        <*> (fmap unCostModels <$> o .:? "costModels") .!= Map.empty
--}
        <*> (fmap (Map.map mapMap) <$> o .:? "costModels") .!= Map.empty
        <*> o .:? "executionUnitPrices"
        <*> o .:? "maxTxExecutionUnits"
        <*> o .:? "maxBlockExecutionUnits"
        <*> o .:? "maxValueSize"
        <*> o .:? "collateralPercentage"
        <*> o .:? "maxCollateralInputs"
        <*> o .:? "utxoCostPerByte"

-- Yes, goes to-from CBOR to make the copy-pasta work with to `unCostModels` =).
mapMap :: [Int64] -> CostModel
mapMap int64s = CBOR.unsafeDeserialize' (CBOR.serialize' int64s)

instance Aeson.ToJSON ProtocolParameters where
  toJSON ProtocolParameters{..} =
    Aeson.object
      [ "extraPraosEntropy" .= protocolParamExtraPraosEntropy
      , "stakePoolTargetNum" .= protocolParamStakePoolTargetNum
      , "minUTxOValue" .= protocolParamMinUTxOValue
      , "poolRetireMaxEpoch" .= protocolParamPoolRetireMaxEpoch
      , "decentralization" .= (toRationalJSON <$> protocolParamDecentralization)
      , "stakePoolDeposit" .= protocolParamStakePoolDeposit
      , "maxBlockHeaderSize" .= protocolParamMaxBlockHeaderSize
      , "maxBlockBodySize" .= protocolParamMaxBlockBodySize
      , "maxTxSize" .= protocolParamMaxTxSize
      , "treasuryCut" .= toRationalJSON protocolParamTreasuryCut
      , "minPoolCost" .= protocolParamMinPoolCost
      , "monetaryExpansion" .= toRationalJSON protocolParamMonetaryExpansion
      , "stakeAddressDeposit" .= protocolParamStakeAddressDeposit
      , "poolPledgeInfluence" .= toRationalJSON protocolParamPoolPledgeInfluence
      , "protocolVersion"
          .= let (major, minor) = protocolParamProtocolVersion
              in Aeson.object ["major" .= major, "minor" .= minor]
      , "txFeeFixed" .= protocolParamTxFeeFixed
      , "txFeePerByte" .= protocolParamTxFeePerByte
      , -- Alonzo era:
{-- DIFF: To avoid defining `CostModel` and `CostModels` here again.
        "costModels" .= CostModels protocolParamCostModels
--}
        ("costModels", costModelToAesonValue protocolParamCostModels)
      , "executionUnitPrices" .= protocolParamPrices
      , "maxTxExecutionUnits" .= protocolParamMaxTxExUnits
      , "maxBlockExecutionUnits" .= protocolParamMaxBlockExUnits
      , "maxValueSize" .= protocolParamMaxValueSize
      , "collateralPercentage" .= protocolParamCollateralPercent
      , "maxCollateralInputs" .= protocolParamMaxCollateralInputs
      , -- Babbage era:
        "utxoCostPerByte" .= protocolParamUTxOCostPerByte
      ]

costModelToAesonValue :: Map.Map AnyPlutusScriptVersion CostModel -> Aeson.Value
costModelToAesonValue costModels = Aeson.Object $ KeyMap.fromMapText $ Map.fromList $
  map
    (\(k,v) -> case Aeson.toJSON k of
      (Aeson.String text) ->
        ( text -- Aeson.toJSONKey k
        , let int64s = (CBOR.unsafeDeserialize' $ CBOR.serialize' v :: [Int64])
          in Aeson.toJSON int64s
        )
      _ -> error ""
    )
    (Map.toList costModels)
--  case Aeson.decode (Aeson.encode (Map.toList costModels)) of
--    (Just v) -> v
--    Nothing -> error ""

--
-- ----------------------------------------------------------------------------
-- Praos nonce
--

-- Duplicated from "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
fromLedgerNonce :: Ledger.Nonce -> Maybe PraosNonce
fromLedgerNonce Ledger.NeutralNonce = Nothing
{-- DIFF: Avoids defining `PraosNonce` again.
          Converts to ByteSring and back.
fromLedgerNonce (Ledger.Nonce h) = Just (PraosNonce (Crypto.castHash h))
--}
fromLedgerNonce (Ledger.Nonce h) = Just (makePraosNonce $ Crypto.hashToBytes (Crypto.castHash h))

-- ----------------------------------------------------------------------------
-- Conversion functions: updates to ledger types
--

-- Duplicated from "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
requireParam
  :: String
  -> (a -> Either ProtocolParametersConversionError b)
  -> Maybe a
  -> Either ProtocolParametersConversionError b
requireParam paramName = maybe (Left $ PpceMissingParameter paramName)

-- Duplicated from "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
mkProtVer :: (Natural, Natural) -> Either ProtocolParametersConversionError Ledger.ProtVer
mkProtVer (majorProtVer, minorProtVer) =
  maybeToRight (PpceVersionInvalid majorProtVer) $
    (`Ledger.ProtVer` minorProtVer) <$> Ledger.mkVersion majorProtVer

-- Duplicated from "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
boundRationalEither
  :: Ledger.BoundedRational b
  => String
  -> Rational
  -> Either ProtocolParametersConversionError b
boundRationalEither name r = maybeToRight (PpceOutOfBounds name r) $ Ledger.boundRational r

-- ----------------------------------------------------------------------------
-- Conversion functions: protocol parameters to ledger types
--

-- Was removed in "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
toLedgerPParams
  :: ShelleyBasedEra era
  -> ProtocolParameters
  -> Either ProtocolParametersConversionError (Ledger.PParams (ShelleyLedgerEra era))
toLedgerPParams ShelleyBasedEraShelley = toShelleyPParams
toLedgerPParams ShelleyBasedEraAllegra = toShelleyPParams
toLedgerPParams ShelleyBasedEraMary = toShelleyPParams
toLedgerPParams ShelleyBasedEraAlonzo = toAlonzoPParams
toLedgerPParams ShelleyBasedEraBabbage = toBabbagePParams
toLedgerPParams ShelleyBasedEraConway = toConwayPParams

-- Was removed in "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
toShelleyCommonPParams
  :: EraPParams ledgerera
  => ProtocolParameters
  -> Either ProtocolParametersConversionError (PParams ledgerera)
toShelleyCommonPParams
  ProtocolParameters
    { protocolParamProtocolVersion
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
            & ppMinFeeAL .~ protocolParamTxFeePerByte
            & ppMinFeeBL .~ protocolParamTxFeeFixed
            & ppMaxBBSizeL .~ fromIntegral protocolParamMaxBlockBodySize
            & ppMaxTxSizeL .~ fromIntegral protocolParamMaxTxSize
            & ppMaxBHSizeL .~ fromIntegral protocolParamMaxBlockHeaderSize
            & ppKeyDepositL .~ protocolParamStakeAddressDeposit
            & ppPoolDepositL .~ protocolParamStakePoolDeposit
            & ppEMaxL .~ protocolParamPoolRetireMaxEpoch
            & ppNOptL .~ protocolParamStakePoolTargetNum
            & ppA0L .~ a0
            & ppRhoL .~ rho
            & ppTauL .~ tau
            & ppProtocolVersionL .~ protVer
            & ppMinPoolCostL .~ protocolParamMinPoolCost
    pure ppCommon

-- Was removed in "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
toShelleyPParams
  :: ( EraPParams ledgerera
     , Ledger.AtMostEra Ledger.MaryEra ledgerera
     , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
     )
  => ProtocolParameters
  -> Either ProtocolParametersConversionError (PParams ledgerera)
toShelleyPParams
  protocolParameters@ProtocolParameters
    { protocolParamDecentralization
    , protocolParamExtraPraosEntropy
    , protocolParamMinUTxOValue
    } = do
    ppCommon <- toShelleyCommonPParams protocolParameters
    d <-
      boundRationalEither "D"
        =<< maybeToRight (PpceMissingParameter "decentralization") protocolParamDecentralization
    minUTxOValue <-
      maybeToRight (PpceMissingParameter "protocolParamMinUTxOValue") protocolParamMinUTxOValue
    let ppShelley =
          ppCommon
            & ppDL .~ d
            & ppExtraEntropyL .~ toLedgerNonce protocolParamExtraPraosEntropy
            & ppMinUTxOValueL .~ minUTxOValue
    pure ppShelley

-- Was removed in "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
toAlonzoCommonPParams
  :: AlonzoEraPParams ledgerera
  => ProtocolParameters
  -> Either ProtocolParametersConversionError (PParams ledgerera)
toAlonzoCommonPParams
  protocolParameters@ProtocolParameters
    { protocolParamCostModels
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
      requireParam "protocolParamMaxBlockExUnits" Right protocolParamMaxValueSize
    collateralPercent <-
      requireParam "protocolParamCollateralPercent" Right protocolParamCollateralPercent
    maxCollateralInputs <-
      requireParam "protocolParamMaxCollateralInputs" Right protocolParamMaxCollateralInputs
    let ppAlonzoCommon =
          ppShelleyCommon
            & ppCostModelsL .~ costModels
            & ppPricesL .~ prices
            & ppMaxTxExUnitsL .~ toAlonzoExUnits maxTxExUnits
            & ppMaxBlockExUnitsL .~ toAlonzoExUnits maxBlockExUnits
            & ppMaxValSizeL .~ maxValueSize
            & ppCollateralPercentageL .~ collateralPercent
            & ppMaxCollateralInputsL .~ maxCollateralInputs
    pure ppAlonzoCommon

-- Was removed in "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
toAlonzoPParams
  :: Ledger.Crypto crypto
  => ProtocolParameters
  -> Either ProtocolParametersConversionError (PParams (Ledger.AlonzoEra crypto))
toAlonzoPParams
  protocolParameters@ProtocolParameters
    { protocolParamDecentralization
    } = do
    ppAlonzoCommon <- toAlonzoCommonPParams protocolParameters
    d <-
      requireParam
        "protocolParamDecentralization"
        (boundRationalEither "D")
        protocolParamDecentralization
    let ppAlonzo =
          ppAlonzoCommon
            & ppDL .~ d
    pure ppAlonzo

-- Was removed in "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
toBabbagePParams
  :: BabbageEraPParams ledgerera
  => ProtocolParameters
  -> Either ProtocolParametersConversionError (PParams ledgerera)
toBabbagePParams
  protocolParameters@ProtocolParameters
    { protocolParamUTxOCostPerByte
    } = do
    ppAlonzoCommon <- toAlonzoCommonPParams protocolParameters
    utxoCostPerByte <-
      requireParam "protocolParamUTxOCostPerByte" Right protocolParamUTxOCostPerByte
    let ppBabbage =
          ppAlonzoCommon
            & ppCoinsPerUTxOByteL .~ CoinPerByte utxoCostPerByte
    pure ppBabbage

-- Was removed in "cardano-api" module "Cardano.Api.Internal.ProtocolParameters"
toConwayPParams
  :: BabbageEraPParams ledgerera
  => ProtocolParameters
  -> Either ProtocolParametersConversionError (PParams ledgerera)
toConwayPParams = toBabbagePParams

-- ----------------------------------------------------------------------------
-- Conversion functions: protocol parameters from ledger types
--

{-# DEPRECATED
  fromLedgerPParams
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
fromLedgerPParams
  :: ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> ProtocolParameters
fromLedgerPParams ShelleyBasedEraShelley = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraAllegra = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraMary = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraAlonzo = fromExactlyAlonzoPParams
fromLedgerPParams ShelleyBasedEraBabbage = fromBabbagePParams
fromLedgerPParams ShelleyBasedEraConway = fromConwayPParams

{-# DEPRECATED
  fromShelleyCommonPParams
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
fromShelleyCommonPParams
  :: EraPParams ledgerera
  => PParams ledgerera
  -> ProtocolParameters
fromShelleyCommonPParams pp =
  ProtocolParameters
    { protocolParamProtocolVersion = case pp ^. ppProtocolVersionL of
        Ledger.ProtVer a b -> (Ledger.getVersion a, b)
    , protocolParamMaxBlockHeaderSize = fromIntegral $ pp ^. ppMaxBHSizeL
    , protocolParamMaxBlockBodySize = fromIntegral $ pp ^. ppMaxBBSizeL
    , protocolParamMaxTxSize = fromIntegral $ pp ^. ppMaxTxSizeL
    , protocolParamTxFeeFixed = pp ^. ppMinFeeBL
    , protocolParamTxFeePerByte = pp ^. ppMinFeeAL
    , protocolParamStakeAddressDeposit = pp ^. ppKeyDepositL
    , protocolParamStakePoolDeposit = pp ^. ppPoolDepositL
    , protocolParamMinPoolCost = pp ^. ppMinPoolCostL
    , protocolParamPoolRetireMaxEpoch = pp ^. ppEMaxL
    , protocolParamStakePoolTargetNum = pp ^. ppNOptL
    , protocolParamPoolPledgeInfluence = Ledger.unboundRational (pp ^. ppA0L)
    , protocolParamMonetaryExpansion = Ledger.unboundRational (pp ^. ppRhoL)
    , protocolParamTreasuryCut = Ledger.unboundRational (pp ^. ppTauL)
    , protocolParamCostModels = mempty -- Only from Alonzo onwards
    , protocolParamPrices = Nothing -- Only from Alonzo onwards
    , protocolParamMaxTxExUnits = Nothing -- Only from Alonzo onwards
    , protocolParamMaxBlockExUnits = Nothing -- Only from Alonzo onwards
    , protocolParamMaxValueSize = Nothing -- Only from Alonzo onwards
    , protocolParamCollateralPercent = Nothing -- Only from Alonzo onwards
    , protocolParamMaxCollateralInputs = Nothing -- Only from Alonzo onwards
    , protocolParamUTxOCostPerByte = Nothing -- Only from Babbage onwards
    , protocolParamDecentralization = Nothing -- Obsolete from Babbage onwards
    , protocolParamExtraPraosEntropy = Nothing -- Obsolete from Alonzo onwards
    , protocolParamMinUTxOValue = Nothing -- Obsolete from Alonzo onwards
    }

{-# DEPRECATED
  fromShelleyPParams
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
fromShelleyPParams
  :: ( EraPParams ledgerera
     , Ledger.AtMostEra Ledger.MaryEra ledgerera
     , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
     )
  => PParams ledgerera
  -> ProtocolParameters
fromShelleyPParams pp =
  (fromShelleyCommonPParams pp)
    { protocolParamDecentralization = Just . Ledger.unboundRational $ pp ^. ppDL
    , protocolParamExtraPraosEntropy = fromLedgerNonce $ pp ^. ppExtraEntropyL
    , protocolParamMinUTxOValue = Just $ pp ^. ppMinUTxOValueL
    }

{-# DEPRECATED
  fromAlonzoPParams
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
fromAlonzoPParams
  :: AlonzoEraPParams ledgerera
  => PParams ledgerera
  -> ProtocolParameters
fromAlonzoPParams pp =
  (fromShelleyCommonPParams pp)
    { protocolParamCostModels = fromAlonzoCostModels $ pp ^. ppCostModelsL
    , protocolParamDecentralization = Just . Ledger.unboundRational $ pp ^. ppDG
    , protocolParamPrices = Just . fromAlonzoPrices $ pp ^. ppPricesL
    , protocolParamMaxTxExUnits = Just . fromAlonzoExUnits $ pp ^. ppMaxTxExUnitsL
    , protocolParamMaxBlockExUnits = Just . fromAlonzoExUnits $ pp ^. ppMaxBlockExUnitsL
    , protocolParamMaxValueSize = Just $ pp ^. ppMaxValSizeL
    , protocolParamCollateralPercent = Just $ pp ^. ppCollateralPercentageL
    , protocolParamMaxCollateralInputs = Just $ pp ^. ppMaxCollateralInputsL
    }

{-# DEPRECATED
  fromExactlyAlonzoPParams
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
fromExactlyAlonzoPParams
  :: (AlonzoEraPParams ledgerera, Ledger.ExactEra Ledger.AlonzoEra ledgerera)
  => PParams ledgerera
  -> ProtocolParameters
fromExactlyAlonzoPParams pp =
  (fromAlonzoPParams pp)
    { protocolParamUTxOCostPerByte = Just . unCoinPerWord $ pp ^. ppCoinsPerUTxOWordL
    }

{-# DEPRECATED
  fromBabbagePParams
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
fromBabbagePParams
  :: BabbageEraPParams ledgerera
  => PParams ledgerera
  -> ProtocolParameters
fromBabbagePParams pp =
  (fromAlonzoPParams pp)
    { protocolParamUTxOCostPerByte = Just . unCoinPerByte $ pp ^. ppCoinsPerUTxOByteL
    , protocolParamDecentralization = Nothing
    }

{-# DEPRECATED
  fromConwayPParams
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}
fromConwayPParams
  :: BabbageEraPParams ledgerera
  => PParams ledgerera
  -> ProtocolParameters
fromConwayPParams = fromBabbagePParams

--------------------------------------------------------------------------------
-- From: module Cardano.Api.Internal.Json
--------------------------------------------------------------------------------

-- Rationals and JSON are an awkward mix. We cannot convert rationals
-- like @1/3@ to JSON numbers. But _most_ of the numbers we want to use
-- in practice have simple decimal representations. Our solution here is
-- to use simple decimal representations where we can and representation
-- in a @{"numerator": 1, "denominator": 3}@ style otherwise.
--
toRationalJSON :: Rational -> Aeson.Value
toRationalJSON r =
  case Scientific.fromRationalRepetendLimited 20 r of
    Right (s, Nothing) -> Aeson.toJSON s
    _ -> Aeson.toJSON r
