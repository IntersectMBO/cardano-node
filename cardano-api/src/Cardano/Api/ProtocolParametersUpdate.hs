{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The various Cardano protocol parameters, including:
--
-- * the current values of updateable protocol parameters: 'ProtocolParameters'
-- * updates to protocol parameters: 'ProtocolParametersUpdate'
-- * update proposals that can be embedded in transactions: 'UpdateProposal'
-- * parameters fixed in the genesis file: 'GenesisParameters'
--
module Cardano.Api.ProtocolParametersUpdate (
    -- * Updates to the protocol paramaters
    ProtocolParametersUpdate(..),

    -- * Update proposals to change the protocol paramaters
    UpdateProposal(..),
    makeShelleyUpdateProposal,

    -- * Internal conversion functions
    toShelleyPParamsUpdate,
    toShelleyProposedPPUpdates,
    toShelleyUpdate,
    toUpdate,
    fromShelleyPParamsUpdate,
    fromShelleyProposedPPUpdates,
    fromShelleyUpdate,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Data.Word (Word64)
import           Numeric.Natural

import           Control.Monad

import qualified Cardano.Binary as CBOR

import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.Value

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Language.PlutusCore.Evaluation.Machine.ExBudgeting as Plutus
import           Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

-- ----------------------------------------------------------------------------
-- Updates to the protocol paramaters
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

       -- | Cost in ada per byte of UTxO storage (instead of
       --protocolParamMinUTxOValue in the Alonzo era onwards).
       protocolUpdateUTxOCostPerByte :: Maybe Lovelace,

       -- | Cost models for non-native script languages.
       protocolUpdateCostModels :: Map AnyScriptLanguage CostModel,

       -- | Map AnyScriptLanguage ExecutionUnitPrices of execution units (for non-native script languages).
       protocolUpdatePrices :: Map AnyScriptLanguage ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       protocolUpdateMaxTxExUnits :: Maybe MaxTxExecutionUnits,

       -- | Max total script execution resources units allowed per block
       protocolUpdateMaxBlockExUnits :: Maybe MaxBlockExecutionUnits,

       -- | Max size of a Value in a tx output.
       protocolUpdateParamMaxValSize :: Maybe Natural
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
      , protocolUpdateMonetaryExpansion   = merge protocolUpdateMonetaryExpansion
      , protocolUpdatePoolPledgeInfluence = merge protocolUpdatePoolPledgeInfluence
      , protocolUpdateTreasuryCut         = merge protocolUpdateTreasuryCut
      -- Intoduced in Alonzo below.
      , protocolUpdateUTxOCostPerByte     = merge protocolUpdateUTxOCostPerByte
      , protocolUpdateCostModels          = mergeMap protocolUpdateCostModels
      , protocolUpdatePrices              = mergeMap protocolUpdatePrices
      , protocolUpdateMaxTxExUnits        = merge protocolUpdateMaxTxExUnits
      , protocolUpdateMaxBlockExUnits     = merge protocolUpdateMaxBlockExUnits
      , protocolUpdateParamMaxValSize     = merge protocolUpdateParamMaxValSize
      }
      where
        -- prefer the right hand side:
        merge :: (ProtocolParametersUpdate -> Maybe a) -> Maybe a
        merge f = f ppu2 `mplus` f ppu1

        -- need to use map merge where:
        -- keys present in m1 and m2: remove/ignore m1 key
        -- keys present in m1 and not m2 : remove/ignore m1 key
        -- keys present in m2 and not m1: keep m2
        -- I.e prefer m2 -- TODO: left off here. Remember plutus json jared sent you
        mergeMap :: (ProtocolParametersUpdate -> Map AnyScriptLanguage a)
                 -> Map AnyScriptLanguage a
        mergeMap = undefined

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
      , protocolUpdateUTxOCostPerByte     = Nothing
      , protocolUpdateCostModels          = mempty
      , protocolUpdatePrices              = mempty
      , protocolUpdateMaxTxExUnits        = Nothing
      , protocolUpdateMaxBlockExUnits     = Nothing
      , protocolUpdateParamMaxValSize     = Nothing
      }

-- ----------------------------------------------------------------------------
-- Proposals embedded in transactions to update protocol parameters
--

data UpdateProposal =
     UpdateProposal
       !(Map (Hash GenesisKey) ProtocolParametersUpdate)
       !EpochNo
    deriving stock (Eq, Show)

instance HasTypeProxy UpdateProposal where
    data AsType UpdateProposal = AsUpdateProposal
    proxyToAsType _ = AsUpdateProposal

instance HasTextEnvelope UpdateProposal where
    textEnvelopeType _ = "UpdateProposalShelley"

--TODO: Jordan UpdateProposal needs to be parameterized by era or have access to the era
instance SerialiseAsCBOR UpdateProposal where
    serialiseToCBOR = CBOR.serializeEncoding' . toCBOR . toShelleyUpdate @StandardShelley
    deserialiseFromCBOR _ bs =
      fromShelleyUpdate @StandardShelley <$>
        CBOR.decodeAnnotator "UpdateProposal" fromCBOR (LBS.fromStrict bs)


makeShelleyUpdateProposal :: ProtocolParametersUpdate
                          -> [Hash GenesisKey]
                          -> EpochNo
                          -> UpdateProposal
makeShelleyUpdateProposal params genesisKeyHashes =
    --TODO decide how to handle parameter validation
    UpdateProposal (Map.fromList [ (kh, params) | kh <- genesisKeyHashes ])

newtype CostModel = CostModel (Map.Map Text Integer)
             deriving (Eq, Show)

_fromLanguage :: Alonzo.Language -> AnyScriptLanguage
_fromLanguage Alonzo.PlutusV1 = AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV1)

_fromCostModel :: Map Alonzo.Language Alonzo.CostModel -> Map AnyScriptLanguage CostModel
_fromCostModel aCostMap = Map.fromList . mapMaybe conv  $ Map.toList aCostMap
 where
   conv :: (Alonzo.Language, Alonzo.CostModel) -> Maybe (AnyScriptLanguage, CostModel)
   conv (Alonzo.PlutusV1, Alonzo.CostModel _aCostModel) =
     Just (undefined, CostModel $ error "Need to bump ledger spec dependency")
   conv (Alonzo.PlutusV1, _) = Nothing

_toCostModel :: Map AnyScriptLanguage CostModel -> Map Alonzo.Language Alonzo.CostModel
_toCostModel cMap = Map.fromList . mapMaybe conv $ Map.toList cMap
 where
   conv :: (AnyScriptLanguage, CostModel) -> Maybe (Alonzo.Language, Alonzo.CostModel)
   conv (AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV1), CostModel _costModel) =
     Just (Alonzo.PlutusV1, Alonzo.CostModel $ error "Need to bump ledger spec dependency")
   conv (AnyScriptLanguage (SimpleScriptLanguage _), _) = Nothing

-- TODO: Need to bump the plutus dependency to get access to
-- extractModelParams :: CostModel -> Maybe CostModelParams
toCostModelParams :: Plutus.CostModel -> Maybe (Map Text Integer)
toCostModelParams _ = Nothing

instance FromJSON CostModel where
  parseJSON v = do
    pCostModel <- parseJSON v :: Aeson.Parser Plutus.CostModel
    case toCostModelParams pCostModel of
      Just cModelParams -> return $ CostModel cModelParams
      Nothing ->
        error $ "Error converting Plutus cost model to cost model params: " <> show pCostModel


data ExecutionUnitPrices =
  ExecutionUnitPrices { perUnitSpace :: Lovelace
                      , perUnitTime :: Lovelace
                      } deriving (Eq, Show)

_fromPrices :: Alonzo.Prices -> ExecutionUnitPrices
_fromPrices (Alonzo.Prices pMem pStep) =
  ExecutionUnitPrices (fromShelleyLovelace pMem) (fromShelleyLovelace pStep)

_toPrices :: ExecutionUnitPrices -> Alonzo.Prices
_toPrices (ExecutionUnitPrices pMem pStep) =
  Alonzo.Prices (toShelleyLovelace pMem) (toShelleyLovelace pStep)

instance FromJSON ExecutionUnitPrices where
  parseJSON = withObject "ExecutionUnitPrices" $ \o -> do
    obj <- o .: "executionUnitPrices"
    ExecutionUnitPrices <$> obj .: "unitSpace" <*> obj .: "unitTime"

instance ToJSON ExecutionUnitPrices where
  toJSON (ExecutionUnitPrices perSpace perTime) =
    object [ "executionUnitPrices" .= object
             [ "unitSpace" .= perSpace , "unitTime" .= perTime]
           ]

newtype MaxTxExecutionUnits =
    MaxTxExecutionUnits { unMaxTxExecutionUnits :: ExecutionUnits}
    deriving (Eq, Show)

_fromMaxTxExec :: Alonzo.ExUnits -> MaxTxExecutionUnits
_fromMaxTxExec (Alonzo.ExUnits mMem mSteps) =
  MaxTxExecutionUnits $ ExecutionUnits mMem mSteps

_toTxExecUnits :: MaxTxExecutionUnits -> Alonzo.ExUnits
_toTxExecUnits (MaxTxExecutionUnits (ExecutionUnits mMem mSteps)) =
  Alonzo.ExUnits mMem mSteps

instance ToJSON MaxTxExecutionUnits where
  toJSON (MaxTxExecutionUnits (ExecutionUnits space time)) =
    object [ "maxTxExecutionUnits" .=
                object ["space" .= space, "time" .= time]
           ]

instance FromJSON MaxTxExecutionUnits where
  parseJSON = withObject "MaxTxExecutionUnits" $ \o -> do
    obj <- o .: "maxTxExecutionUnits"
    MaxTxExecutionUnits
      <$> (ExecutionUnits <$> obj .: "space" <*> obj .: "time")

newtype MaxBlockExecutionUnits =
    MaxBlockExecutionUnits { unMaxBlockExecutionUnits :: ExecutionUnits}
    deriving (Eq, Show)

_fromMaxBlockExec :: Alonzo.ExUnits -> MaxBlockExecutionUnits
_fromMaxBlockExec (Alonzo.ExUnits mMem mSteps) =
  MaxBlockExecutionUnits $ ExecutionUnits mMem mSteps

_toExUnits :: MaxBlockExecutionUnits -> Alonzo.ExUnits
_toExUnits (MaxBlockExecutionUnits (ExecutionUnits mMem mSteps)) =
  Alonzo.ExUnits mMem mSteps

instance ToJSON MaxBlockExecutionUnits where
  toJSON (MaxBlockExecutionUnits (ExecutionUnits space time)) =
    object [ "maxBlockExecutionUnits" .=
                object ["space" .= space, "time" .= time]
           ]

instance FromJSON MaxBlockExecutionUnits where
  parseJSON = withObject "MaxBlockExecutionUnits" $ \o -> do
    obj <- o .: "maxBlockExecutionUnits"
    MaxBlockExecutionUnits
      <$> (ExecutionUnits <$> obj .: "space" <*> obj .: "time")


data ExecutionUnits
    = ExecutionUnits { space :: Word64
                     , time :: Word64
                     } deriving (Eq, Show)



-- ----------------------------------------------------------------------------
-- Conversion functions
--

toShelleyUpdate :: ( Ledger.Crypto ledgerera ~ StandardCrypto
                   , Shelley.PParamsDelta ledgerera
                     ~ Shelley.PParamsUpdate ledgerera
                   )
                => UpdateProposal -> Shelley.Update ledgerera
toShelleyUpdate (UpdateProposal ppup epochno) =
    Shelley.Update (toShelleyProposedPPUpdates ppup) epochno


toShelleyProposedPPUpdates :: forall ledgerera.
                              ( Ledger.Crypto ledgerera ~ StandardCrypto
                              , Shelley.PParamsDelta ledgerera
                                ~ Shelley.PParamsUpdate ledgerera
                              )
                            => Map (Hash GenesisKey) ProtocolParametersUpdate
                            -> Shelley.ProposedPPUpdates ledgerera
toShelleyProposedPPUpdates =
    Shelley.ProposedPPUpdates
  . Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh)
  . Map.map (toShelleyPParamsUpdate @ledgerera)

toUpdate  :: ShelleyLedgerEra era ~ ledgerera
          => Ledger.Crypto ledgerera ~ StandardCrypto
          => ShelleyBasedEra era
          -> UpdateProposal -> Shelley.Update ledgerera
toUpdate sbe (UpdateProposal ppup epochno) =
    Shelley.Update (toProposedPPUpdates sbe ppup) epochno

toProposedPPUpdates :: ShelleyLedgerEra era ~ ledgerera
                    => Ledger.Crypto ledgerera ~ StandardCrypto
                    => ShelleyBasedEra era
                    -> Map (Hash GenesisKey) ProtocolParametersUpdate
                    -> Shelley.ProposedPPUpdates ledgerera
toProposedPPUpdates sbe m =
  let f = case sbe of
            ShelleyBasedEraShelley -> toShelleyPParamsUpdate
            ShelleyBasedEraAllegra -> toShelleyPParamsUpdate
            ShelleyBasedEraMary -> toShelleyPParamsUpdate
        --  ShelleyBasedEraAlonzo -> toAlonzoPParamsUpdate
  in Shelley.ProposedPPUpdates
       . Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh)
       $ Map.map f m

{-
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
      , protocolUpdateStakeAddressDeposit
      , protocolUpdateStakePoolDeposit
      , protocolUpdateMinPoolCost
      , protocolUpdatePoolRetireMaxEpoch
      , protocolUpdateStakePoolTargetNum
      , protocolUpdatePoolPledgeInfluence
      , protocolUpdateMonetaryExpansion
      , protocolUpdateTreasuryCut

      , protocolUpdateTxFeePerByte
      , protocolUpdateMaxBlockExUnits
      , protocolUpdateMaxTxExUnits
      , protocolUpdatePrices
      , protocolUpdateCostModels
      , protocolUpdateUTxOCostPerByte
     -- , protocolUpdateParamMaxValSize
      } =
      Alonzo.PParams
           { Alonzo._minfeeA =    maybeToStrictMaybe protocolUpdateTxFeePerByte,
             Alonzo._minfeeB    = maybeToStrictMaybe protocolUpdateTxFeeFixed,
             Alonzo._maxBBSize  = maybeToStrictMaybe protocolUpdateMaxBlockBodySize,
             Alonzo._maxTxSize  = maybeToStrictMaybe protocolUpdateMaxTxSize,
             Alonzo._maxBHSize  = maybeToStrictMaybe protocolUpdateMaxBlockHeaderSize,
             Alonzo._keyDeposit = toShelleyLovelace <$>
                                    maybeToStrictMaybe protocolUpdateStakeAddressDeposit,
             Alonzo._poolDeposit = toShelleyLovelace <$>
                                     maybeToStrictMaybe protocolUpdateStakePoolDeposit,
             Alonzo._eMax = maybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch,
             Alonzo._nOpt = maybeToStrictMaybe protocolUpdateStakePoolTargetNum,
             Alonzo._a0   = maybeToStrictMaybe protocolUpdatePoolPledgeInfluence,
             Alonzo._rho  = Shelley.unitIntervalFromRational <$>
                               maybeToStrictMaybe protocolUpdateMonetaryExpansion,
             Alonzo._tau = Shelley.unitIntervalFromRational <$>
                               maybeToStrictMaybe protocolUpdateTreasuryCut,
             Alonzo._d = Shelley.unitIntervalFromRational <$>
                                      maybeToStrictMaybe protocolUpdateDecentralization,
             Alonzo._extraEntropy = toShelleyNonce <$>
                                          maybeToStrictMaybe protocolUpdateExtraPraosEntropy,
             Alonzo._protocolVersion = uncurry Shelley.ProtVer <$>
                                          maybeToStrictMaybe protocolUpdateProtocolVersion,
             Alonzo._minPoolCost = toShelleyLovelace <$>
                                       maybeToStrictMaybe protocolUpdateMinPoolCost,

             Alonzo._adaPerUTxOByte = toShelleyLovelace <$> maybeToStrictMaybe protocolUpdateUTxOCostPerByte,
             Alonzo._costmdls = toCostModel <$> maybeToStrictMaybe protocolUpdateCostModels,
             Alonzo._prices = toPrices <$> maybeToStrictMaybe protocolUpdatePrices,
             Alonzo._maxTxExUnits = toTxExecUnits <$> maybeToStrictMaybe protocolUpdateMaxTxExUnits,
             Alonzo._maxBlockExUnits = toExUnits <$> maybeToStrictMaybe protocolUpdateMaxBlockExUnits
           }
-}

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
      Shelley._minfeeA     = maybeToStrictMaybe protocolUpdateTxFeePerByte
    , Shelley._minfeeB     = maybeToStrictMaybe protocolUpdateTxFeeFixed
    , Shelley._maxBBSize   = maybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , Shelley._maxTxSize   = maybeToStrictMaybe protocolUpdateMaxTxSize
    , Shelley._maxBHSize   = maybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , Shelley._keyDeposit  = toShelleyLovelace <$>
                               maybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , Shelley._poolDeposit = toShelleyLovelace <$>
                               maybeToStrictMaybe protocolUpdateStakePoolDeposit
    , Shelley._eMax        = maybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , Shelley._nOpt        = maybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , Shelley._a0          = maybeToStrictMaybe protocolUpdatePoolPledgeInfluence
    , Shelley._rho         = Shelley.unitIntervalFromRational <$>
                               maybeToStrictMaybe protocolUpdateMonetaryExpansion
    , Shelley._tau         = Shelley.unitIntervalFromRational <$>
                               maybeToStrictMaybe protocolUpdateTreasuryCut
    , Shelley._d           = Shelley.unitIntervalFromRational <$>
                               maybeToStrictMaybe protocolUpdateDecentralization
    , Shelley._extraEntropy    = toShelleyNonce <$>
                                   maybeToStrictMaybe protocolUpdateExtraPraosEntropy
    , Shelley._protocolVersion = uncurry Shelley.ProtVer <$>
                                   maybeToStrictMaybe protocolUpdateProtocolVersion
    , Shelley._minUTxOValue    = toShelleyLovelace <$>
                                   maybeToStrictMaybe protocolUpdateMinUTxOValue
    , Shelley._minPoolCost     = toShelleyLovelace <$>
                                   maybeToStrictMaybe protocolUpdateMinPoolCost
    }

fromShelleyUpdate :: ( Ledger.Crypto ledgerera ~ StandardCrypto
                     , Shelley.PParamsDelta ledgerera
                       ~ Shelley.PParamsUpdate ledgerera
                     )
                  => Shelley.Update ledgerera -> UpdateProposal
fromShelleyUpdate (Shelley.Update ppup epochno) =
    UpdateProposal (fromShelleyProposedPPUpdates ppup) epochno


fromShelleyProposedPPUpdates :: ( Ledger.Crypto ledgerera ~ StandardCrypto
                                , Shelley.PParamsDelta ledgerera
                                  ~ Shelley.PParamsUpdate ledgerera
                                )
                             => Shelley.ProposedPPUpdates ledgerera
                             -> Map (Hash GenesisKey) ProtocolParametersUpdate
fromShelleyProposedPPUpdates =
    Map.map fromShelleyPParamsUpdate
  . Map.mapKeysMonotonic GenesisKeyHash
  . (\(Shelley.ProposedPPUpdates ppup) -> ppup)


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
      protocolUpdateProtocolVersion     = (\(Shelley.ProtVer a b) -> (a,b)) <$>
                                          strictMaybeToMaybe _protocolVersion
    , protocolUpdateDecentralization    = Shelley.unitIntervalToRational <$>
                                            strictMaybeToMaybe _d
    , protocolUpdateExtraPraosEntropy   = fromPraosNonce <$>
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
    , protocolUpdatePoolPledgeInfluence = strictMaybeToMaybe _a0
    , protocolUpdateMonetaryExpansion   = Shelley.unitIntervalToRational <$>
                                            strictMaybeToMaybe _rho
    , protocolUpdateTreasuryCut         = Shelley.unitIntervalToRational <$>
                                            strictMaybeToMaybe _tau
    , protocolUpdateUTxOCostPerByte     = Nothing
    , protocolUpdateCostModels          = mempty
    , protocolUpdatePrices              = mempty
    , protocolUpdateMaxTxExUnits        = Nothing
    , protocolUpdateMaxBlockExUnits     = Nothing
    , protocolUpdateParamMaxValSize     = Nothing
    }

