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
module Cardano.Api.ProtocolParameters (
    -- * The updateable protocol paramaters
    ProtocolParameters(..),
    ProtocolParametersError(..),
    renderProtocolParamsErr,
    EpochNo,

    -- * PraosNonce
    PraosNonce,
    makePraosNonce,
    fromPraosNonce,
    toShelleyNonce,

    -- * Protocol paramaters fixed in the genesis file
    GenesisParameters(..),
    EpochSize(..),

    -- * Internal conversion functions
    fromShelleyPParams,
    fromShelleyGenesis,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.!=), (.:),
                   (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (NominalDiffTime, UTCTime)
import           Data.Word (Word64)
import           GHC.Generics
import           Numeric.Natural

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochNo, EpochSize (..))

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.Hash
import           Cardano.Api.KeysByron
import           Cardano.Api.NetworkId
import           Cardano.Api.Script
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.TxMetadata
import           Cardano.Api.Value

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Core as Core
import qualified PlutusCore.Evaluation.Machine.ExBudgeting as Plutus
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

-- | The values of the set of /updateable/ protocol paramaters. At any
-- particular point on the chain there is a current set of paramaters in use.
--
-- These paramaters can be updated (at epoch boundaries) via an
-- 'UpdateProposal', which contains a 'ProtocolParametersUpdate'.
--
-- The 'ProtocolParametersUpdate' is essentially a diff for the
-- 'ProtocolParameters'.
--
-- There are also paramaters fixed in the Genesis file. See 'GenesisParameters'.
--
data ProtocolParameters era =
     ProtocolParameters {

       -- | Protocol version, major and minor. Updating the major version is
       -- used to trigger hard forks.
       --
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
       -- NB: This parameter is not available in the Alonzo era
       -- and is replaced with 'protocolParamUTxOCostPerByte'.
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

       -- Introduced in Alonzo

       -- | Cost in ada per byte of UTxO storage (instead of
       -- 'protocolParamMinUTxOValue' in the Alonzo era onwards).
       protocolParamUTxOCostPerByte :: Maybe Lovelace,

       -- | Cost models for non-native script languages.
       protocolParamCostModels :: Map AnyScriptLanguage CostModel,

       -- | Price of execution units for non-native script languages.
       protocolParamPrices :: Maybe ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       protocolParamMaxTxExUnits :: Maybe MaxTxExecutionUnits,

       -- | Max total script execution resources units allowed per block
       protocolParamMaxBlockExUnits :: Maybe MaxBlockExecutionUnits,

       -- | Max size of a Value in a tx ouput.
       protocolParamMaxValSize :: Maybe Natural
    }
  deriving (Eq, Generic, Show)

data ProtocolParametersError
  = ProtocolParamsErrNoParamsInByron
  | ProtocolParamsErrMustDefineMinUTxo AnyCardanoEra
  -- Alonzo required protocol params
  | ProtocolParamsErrMustDefineCostPerByte AnyCardanoEra
  | ProtocolParamsErrMustDefineCostModel AnyCardanoEra
  | ProtocolParamsErrMustDefineScriptExecPrices AnyCardanoEra
  | ProtocolParamsErrMustDefineMaxTxExec AnyCardanoEra
  | ProtocolParamsErrMustDefineMaxBlockExec AnyCardanoEra
  | ProtocolParamsErrMustDefineMaxValueSize AnyCardanoEra
  -- Error Checking
  | ProtocolParametersErrorWrongVersion
      (Natural,Natural)
      (Natural,Natural)
  deriving Show

renderEra :: AnyCardanoEra -> Text
renderEra (AnyCardanoEra ByronEra)   = "Byron"
renderEra (AnyCardanoEra ShelleyEra) = "Shelley"
renderEra (AnyCardanoEra AllegraEra) = "Allegra"
renderEra (AnyCardanoEra MaryEra)    = "Mary"

renderProtocolParamsErr :: ProtocolParametersError -> Text
renderProtocolParamsErr ProtocolParamsErrNoParamsInByron =
  "Protocol parameters are not supported in the Byron era."
renderProtocolParamsErr (ProtocolParamsErrMustDefineMinUTxo era) =
  "protocolParamMinUTxOValue must be defined in " <> renderEra era
renderProtocolParamsErr (ProtocolParamsErrMustDefineCostPerByte era) =
  "protocolParamUTxOCostPerByte must be defined in " <> renderEra era
renderProtocolParamsErr (ProtocolParamsErrMustDefineCostModel era) =
  "protocolParamCostModels must be defined in " <> renderEra era
renderProtocolParamsErr (ProtocolParamsErrMustDefineScriptExecPrices era) =
  "protocolParamPrices must be defined in " <> renderEra era
renderProtocolParamsErr (ProtocolParamsErrMustDefineMaxTxExec era) =
  "protocolParamMaxTxExUnits must be defined in " <> renderEra era
renderProtocolParamsErr (ProtocolParamsErrMustDefineMaxBlockExec era) =
  "protocolParamMaxBlockExUnits must be defined in " <> renderEra era
renderProtocolParamsErr (ProtocolParamsErrMustDefineMaxValueSize era) =
  "protocolParamMaxValSize must be defined in " <> renderEra era
renderProtocolParamsErr (ProtocolParametersErrorWrongVersion expected got) =
     "Incorrect protocol version. Got: " <> Text.pack (show got)
  <> " Expected: " <> Text.pack (show expected)

createProtocolParameters
  :: (Natural, Natural)              -- ^ Protocol version (major,minor)
  -> Rational                        -- ^ Decentralization parameter
  -> Maybe PraosNonce                -- ^ Extra entropy
  -> Natural                         -- ^ Max block header size
  -> Natural                         -- ^ Max block body size
  -> Natural                         -- ^ Max tx size
  -> Natural                         -- ^ Tx fee fixed (constant factor)
  -> Natural                         -- ^ Tx fee per bytes (linear factor)
  -> Maybe Lovelace                  -- ^ Min UTxO value
  -> Lovelace                        -- ^ Stake address deposit
  -> Lovelace                        -- ^ Stake pool deposit
  -> Lovelace                        -- ^ Min pool cost
  -> EpochNo                         -- ^ Max pool retire epoch
  -> Natural                         -- ^ Stake pool target number
  -> Rational                        -- ^ Pool pledge influence
  -> Rational                        -- ^ Monetary expansion
  -> Rational                        -- ^ Treasury cut
  -> Maybe Lovelace                  -- ^ UTxO cost per byte
  -> Map AnyScriptLanguage CostModel -- ^ Cost model
  -> Maybe ExecutionUnitPrices       -- ^ Price of execution units
  -> Maybe MaxTxExecutionUnits       -- ^ Script execution resources allowed per tx
  -> Maybe MaxBlockExecutionUnits    -- ^ Script execution resources allowed per block
  -> Maybe Natural                   -- ^ Max size of a Value in tx output
  -> ProtocolParameters era
createProtocolParameters ver decent exEntropy bHeadSize bBodySize maxTxSize
                         txFeeFixed txFeePerByte minUtxo stakeAddrDep stakePoolDep
                         minPoolCost poolRetireEpochMax poolTargetNum poolPledge
                         monExpansion treasuryCut utxoCostPerByte costModels
                         prices maxTxExUnits maxBlockExUnits maxValueSize =
  ProtocolParameters
   { protocolParamProtocolVersion = ver
   , protocolParamDecentralization = decent
   , protocolParamExtraPraosEntropy = exEntropy
   , protocolParamMaxBlockHeaderSize = bHeadSize
   , protocolParamMaxBlockBodySize = bBodySize
   , protocolParamMaxTxSize = maxTxSize
   , protocolParamTxFeeFixed = txFeeFixed
   , protocolParamTxFeePerByte = txFeePerByte
   , protocolParamMinUTxOValue = minUtxo
   , protocolParamStakeAddressDeposit = stakeAddrDep
   , protocolParamStakePoolDeposit = stakePoolDep
   , protocolParamMinPoolCost = minPoolCost
   , protocolParamPoolRetireMaxEpoch = poolRetireEpochMax
   , protocolParamStakePoolTargetNum = poolTargetNum
   , protocolParamPoolPledgeInfluence = poolPledge
   , protocolParamMonetaryExpansion = monExpansion
   , protocolParamTreasuryCut = treasuryCut
   -- Fields below Introduced in Alonzo
   , protocolParamUTxOCostPerByte = utxoCostPerByte
   , protocolParamCostModels = costModels
   , protocolParamPrices = prices
   , protocolParamMaxTxExUnits = maxTxExUnits
   , protocolParamMaxBlockExUnits = maxBlockExUnits
   , protocolParamMaxValSize = maxValueSize
   }

createProtocolParametersInEra
  :: CardanoEra era
  -> (Natural, Natural)                        -- ^ Protocol version (major,minor)
  -> Rational                                  -- ^ Decentralization parameter
  -> Maybe PraosNonce                          -- ^ Extra entropy
  -> Natural                                   -- ^ Max block header size
  -> Natural                                   -- ^ Max block body size
  -> Natural                                   -- ^ Max tx size
  -> Natural                                   -- ^ Tx fee fixed (constant factor)
  -> Natural                                   -- ^ Tx fee per bytes (linear factor)
  -> Maybe Lovelace                            -- ^ Min UTxO value
  -> Lovelace                                  -- ^ Stake address deposit
  -> Lovelace                                  -- ^ Stake pool deposit
  -> Lovelace                                  -- ^ Min pool cost
  -> EpochNo                                   -- ^ Max pool retire epoch
  -> Natural                                   -- ^ Stake pool target number
  -> Rational                                  -- ^ Pool pledge influence
  -> Rational                                  -- ^ Monetary expansion
  -> Rational                                  -- ^ Treasury cut
  -> Maybe Lovelace                            -- ^ UTxO cost per byte
  -> Map AnyScriptLanguage CostModel           -- ^ Cost model
  -> Map AnyScriptLanguage ExecutionUnitPrices -- ^ Price of execution units
  -> Maybe MaxTxExecutionUnits                 -- ^ Script execution resources allowed per tx
  -> Maybe MaxBlockExecutionUnits              -- ^ Script execution resources allowed per block
  -> Maybe Natural                             -- ^ Max size of a Value in tx output
  -> Either ProtocolParametersError (ProtocolParameters era)
createProtocolParametersInEra
   era ver decent exEntropy bHeadSize bBodySize maxTxSize
   txFeeFixed txFeePerByte minUtxo stakeAddrDep stakePoolDep
   minPoolCost poolRetireEpochMax poolTargetNum poolPledge
   monExpansion treasuryCut _utxoCostPerByte _costModels
   _prices _maxTxExUnits _maxBlockExUnits _maxValueSize =
  case cardanoEraStyle era of
    LegacyByronEra -> Left ProtocolParamsErrNoParamsInByron
    ShelleyBasedEra sbe -> do
      checkNecessaryParamsSpecified sbe
      case sbe of
        ShelleyBasedEraShelley -> do
          return $ createProtocolParameters
                     ver decent exEntropy bHeadSize bBodySize maxTxSize
                     txFeeFixed txFeePerByte minUtxo stakeAddrDep stakePoolDep
                     minPoolCost poolRetireEpochMax poolTargetNum poolPledge
                     monExpansion treasuryCut Nothing mempty Nothing Nothing
                     Nothing Nothing
        ShelleyBasedEraAllegra -> do
          return $ createProtocolParameters
                     ver decent exEntropy bHeadSize bBodySize maxTxSize
                     txFeeFixed txFeePerByte minUtxo stakeAddrDep stakePoolDep
                     minPoolCost poolRetireEpochMax poolTargetNum poolPledge
                     monExpansion treasuryCut Nothing mempty Nothing Nothing
                     Nothing Nothing
        ShelleyBasedEraMary -> do
          return $ createProtocolParameters
                     ver decent exEntropy bHeadSize bBodySize maxTxSize
                     txFeeFixed txFeePerByte minUtxo stakeAddrDep stakePoolDep
                     minPoolCost poolRetireEpochMax poolTargetNum poolPledge
                     monExpansion treasuryCut Nothing mempty Nothing Nothing
                     Nothing Nothing
     -- ShelleyBasedEraAlonzo ->
     --   return $ createProtocolParameters
     --              ver decent exEntropy bHeadSize bBodySize maxTxSize
     --              txFeeFixed txFeePerByte Nothing stakeAddrDep stakePoolDep
     --              minPoolCost poolRetireEpochMax poolTargetNum poolPledge
     --              monExpansion treasuryCut utxoCostPerByte costModels
     --              prices maxTxExUnits maxBlockExUnits maxValueSize
 where
  -- | Protocol parameters change in Alonzo. We check
  --that the required protocol parameters are specified
  --in the relevant era.
  checkNecessaryParamsSpecified
    :: ShelleyBasedEra era -> Either ProtocolParametersError ()
  checkNecessaryParamsSpecified sbe' = do
    let anyEra = anyCardanoEra era
    case sbe' of
      ShelleyBasedEraShelley ->
         parameterExists minUtxo (ProtocolParamsErrMustDefineMinUTxo anyEra)
      ShelleyBasedEraAllegra ->
         parameterExists minUtxo (ProtocolParamsErrMustDefineMinUTxo anyEra)
      ShelleyBasedEraMary ->
         parameterExists minUtxo (ProtocolParamsErrMustDefineMinUTxo anyEra)
  --  ShelleyBasedEraAlonzo -> do
  --     parameterExists utxoCostPerByte (ProtocolParamsErrMustDefineCostPerByte anyEra)
  --     parameterExists costModels (ProtocolParamsErrMustDefineCostModel anyEra)
  --     parameterExists prices (ProtocolParamsErrMustDefineScriptExecPrices anyEra)
  --     parameterExists maxTxExUnits (ProtocolParamsErrMustDefineMaxTxExec anyEra)
  --     parameterExists maxBlockExUnits (ProtocolParamsErrMustDefineMaxBlockExec anyEra)
  --     parameterExists maxValueSize (ProtocolParamsErrMustDefineMaxValueSize anyEra)

  -- | Check if a protocol parameter was specified and fail if not.
  parameterExists
    :: Maybe a
    -> ProtocolParametersError
    -> Either ProtocolParametersError ()
  parameterExists value err =
    maybe (Left err) Right value >> Right ()


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

instance IsCardanoEra era => FromJSON (ProtocolParameters era) where
  parseJSON = parseProtocolParameters cardanoEra

parseProtocolParameters :: CardanoEra era -> Aeson.Value -> Aeson.Parser (ProtocolParameters era)
parseProtocolParameters era =
         withObject "ProtocolParameters" $ \o -> do
           v <- o .: "protocolVersion"
           eParams <- createProtocolParametersInEra era
                        <$> ((,) <$> v .: "major" <*> v .: "minor")
                        <*> o .: "decentralization"
                        <*> o .: "extraPraosEntropy"
                        <*> o .: "maxBlockHeaderSize"
                        <*> o .: "maxBlockBodySize"
                        <*> o .: "maxTxSize"
                        <*> o .: "txFeeFixed"
                        <*> o .: "txFeePerByte"
                        <*> o .:? "minUTxOValue"
                        <*> o .: "stakeAddressDeposit"
                        <*> o .: "stakePoolDeposit"
                        <*> o .: "minPoolCost"
                        <*> o .: "poolRetireMaxEpoch"
                        <*> o .: "stakePoolTargetNum"
                        <*> o .: "poolPledgeInfluence"
                        <*> o .: "monetaryExpansion"
                        <*> o .: "treasuryCut"
                        <*> o .:? "utxoCostPerByte"
                        <*> o .:? "costModel" .!= mempty
                        <*> o .:? "executionUnitPrices" .!= mempty
                        <*> o .:? "maxTxExecUnits"
                        <*> o .:? "maxBlockExecUnits"
                        <*> o .:? "maxValueSize"
           case eParams of
             Left err -> fail . Text.unpack $ renderProtocolParamsErr err
             Right pparams -> return pparams


instance IsCardanoEra era => ToJSON (ProtocolParameters era) where
  toJSON pp =
    case cardanoEraStyle cardanoEra :: CardanoEraStyle era of
      LegacyByronEra -> error "Protocol parameters are not supported in the Byron era."
      ShelleyBasedEra sbe -> createProtocolParametersObject sbe pp

createProtocolParametersObject
  :: ShelleyBasedEra era  -> ProtocolParameters era -> Aeson.Value
createProtocolParametersObject sbe pp =
  object $ paramsCommonInAllEras <> eraDependentParams sbe
 where
  paramsCommonInAllEras :: [Aeson.Pair]
  paramsCommonInAllEras =
    [ "extraPraosEntropy" .= protocolParamExtraPraosEntropy pp
    , "stakePoolTargetNum" .= protocolParamStakePoolTargetNum pp
    , "poolRetireMaxEpoch" .= protocolParamPoolRetireMaxEpoch pp
    , "decentralization" .= (fromRational $ protocolParamDecentralization pp :: Scientific)
    , "stakePoolDeposit" .= protocolParamStakePoolDeposit pp
    , "maxBlockHeaderSize" .= protocolParamMaxBlockHeaderSize pp
    , "maxBlockBodySize" .= protocolParamMaxBlockBodySize pp
    , "maxTxSize" .= protocolParamMaxTxSize pp
    , "treasuryCut" .= (fromRational $ protocolParamTreasuryCut pp :: Scientific)
    , "minPoolCost" .= protocolParamMinPoolCost pp
    , "monetaryExpansion" .= (fromRational $ protocolParamMonetaryExpansion pp :: Scientific)
    , "stakeAddressDeposit" .= protocolParamStakeAddressDeposit pp
    , "poolPledgeInfluence" .= (fromRational $ protocolParamPoolPledgeInfluence pp :: Scientific)
    , "protocolVersion" .= let (major, minor) = protocolParamProtocolVersion pp
                           in object ["major" .= major, "minor" .= minor]
    , "txFeeFixed" .= protocolParamTxFeeFixed pp
    , "txFeePerByte" .= protocolParamTxFeePerByte pp
    ]

  eraDependentParams :: ShelleyBasedEra era -> [Aeson.Pair]
  eraDependentParams sbe' =
    case sbe' of
      ShelleyBasedEraShelley ->
        ["minUTxOValue" .= protocolParamMinUTxOValue pp]
      ShelleyBasedEraAllegra ->
        ["minUTxOValue" .= protocolParamMinUTxOValue pp]
      ShelleyBasedEraMary ->
        ["minUTxOValue" .= protocolParamMinUTxOValue pp]
  --  ShelleyBasedEraAlonzo ->
  --    [ "costModels"  .= protocolParamCostModels pp
  --    , "executionUnitPrices" .= protocolParamPrices pp
  --    , "maxTxExecutionUnits" .= protocolParamMaxTxExUnits pp
  --    , "maxBlockExecutionUnits" .= protocolParamMaxBlockExUnits pp
  --    , "maxValSize" .= protocolParamMaxValSize pp
  --    ]

-- ----------------------------------------------------------------------------
-- Praos nonce
--

newtype PraosNonce = PraosNonce (Shelley.Hash StandardCrypto ByteString)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PraosNonce where
  toJSON (PraosNonce h) =
    Aeson.String $ Crypto.hashToTextAsHex h

instance FromJSON PraosNonce where
  parseJSON = withText "PraosNonce" $ \h ->
                case Crypto.hashFromTextAsHex h of
                  Nothing -> fail $ "Failed to decode PraosNonce: " <> Text.unpack h
                  Just nonce -> return $ PraosNonce nonce

makePraosNonce :: ByteString -> PraosNonce
makePraosNonce = PraosNonce . Crypto.hashWith id

toShelleyNonce :: Maybe PraosNonce -> Shelley.Nonce
toShelleyNonce Nothing               = Shelley.NeutralNonce
toShelleyNonce (Just (PraosNonce h)) = Shelley.Nonce (Crypto.castHash h)

fromPraosNonce :: Shelley.Nonce -> Maybe PraosNonce
fromPraosNonce Shelley.NeutralNonce = Nothing
fromPraosNonce (Shelley.Nonce h)    = Just (PraosNonce (Crypto.castHash h))


-- ----------------------------------------------------------------------------
-- Genesis paramaters
--

data GenesisParameters era =
     GenesisParameters {

       -- | The reference time the system started. The time of slot zero.
       -- The time epoch against which all Ouroboros time slots are measured.
       --
       protocolParamSystemStart :: UTCTime,

       -- | The network identifier for this blockchain instance. This
       -- distinguishes the mainnet from testnets, and different testnets from
       -- each other.
       --
       protocolParamNetworkId :: NetworkId,

       -- | The Ouroboros Praos active slot coefficient, aka @f@.
       --
       protocolParamActiveSlotsCoefficient :: Rational,

       -- | The Ouroboros security paramaters, aka @k@. This is the maximum
       -- number of blocks the node would ever be prepared to roll back by.
       --
       -- Clients of the node following the chain should be prepared to handle
       -- the node switching forks up to this long.
       --
       protocolParamSecurity :: Int,

       -- | The number of Ouroboros time slots in an Ouroboros epoch.
       --
       protocolParamEpochLength :: EpochSize,

       -- | The time duration of a slot.
       --
       protocolParamSlotLength :: NominalDiffTime,

       -- | For Ouroboros Praos, the length of a KES period as a number of time
       -- slots. The KES keys get evolved once per KES period.
       --
       protocolParamSlotsPerKESPeriod :: Int,

       -- | The maximum number of times a KES key can be evolved before it is
       -- no longer considered valid. This can be less than the maximum number
       -- of times given the KES key size. For example the mainnet KES key size
       -- would allow 64 evolutions, but the max KES evolutions param is 62.
       --
       protocolParamMaxKESEvolutions ::  Int,

       -- | In the Shelley era, prior to decentralised governance, this is the
       -- number of genesis key delegates that need to agree for an update
       -- proposal to be enacted.
       --
       protocolParamUpdateQuorum ::  Int,

       -- | The maximum supply for Lovelace. This determines the initial value
       -- of the reserves.
       --
       protocolParamMaxLovelaceSupply :: Lovelace,

       -- | The initial values of the updateable 'ProtocolParameters'.
       --
       protocolInitialUpdateableProtocolParameters :: ProtocolParameters era
     }

fromShelleyPParams
  :: ShelleyBasedEra era
  -> Core.PParams (ShelleyLedgerEra era)
  -> Either ProtocolParametersError (ProtocolParameters era)
fromShelleyPParams sbe pparams =
  let pp = case sbe of
             ShelleyBasedEraShelley -> toProtocolParamsShelley pparams
             ShelleyBasedEraAllegra -> toProtocolParamsShelley pparams
             ShelleyBasedEraMary -> toProtocolParamsShelley pparams
--           ShelleyBasedEraAlonzo -> toProtocolParamsAlonzo pparams
  in do checkProtocolVersion sbe (protocolParamProtocolVersion pp)
        return pp

toProtocolParamsShelley :: Shelley.PParams ledgerera -> ProtocolParameters era
toProtocolParamsShelley pparams =
   ProtocolParameters
        { protocolParamProtocolVersion     = (\(Shelley.ProtVer a b) -> (a,b))
                                               $ Shelley._protocolVersion pparams
        , protocolParamDecentralization    = Shelley.unitIntervalToRational $ Shelley._d pparams
        , protocolParamExtraPraosEntropy   = fromPraosNonce $ Shelley._extraEntropy pparams
        , protocolParamMaxBlockHeaderSize  = Shelley._maxBHSize pparams
        , protocolParamMaxBlockBodySize    = Shelley._maxBBSize pparams
        , protocolParamMaxTxSize           = Shelley._maxTxSize pparams
        , protocolParamTxFeeFixed          = Shelley._minfeeB pparams
        , protocolParamTxFeePerByte        = Shelley._minfeeA pparams
        , protocolParamMinUTxOValue        = Just . fromShelleyLovelace $ Shelley._minUTxOValue pparams
        , protocolParamStakeAddressDeposit = fromShelleyLovelace $ Shelley._keyDeposit pparams
        , protocolParamStakePoolDeposit    = fromShelleyLovelace $ Shelley._poolDeposit pparams
        , protocolParamMinPoolCost         = fromShelleyLovelace $ Shelley._minPoolCost pparams
        , protocolParamPoolRetireMaxEpoch  = Shelley._eMax pparams
        , protocolParamStakePoolTargetNum  = Shelley._nOpt pparams
        , protocolParamPoolPledgeInfluence = Shelley._a0 pparams
        , protocolParamMonetaryExpansion   = Shelley.unitIntervalToRational $ Shelley._rho pparams
        , protocolParamTreasuryCut         = Shelley.unitIntervalToRational $ Shelley._tau pparams
        , protocolParamUTxOCostPerByte     = Nothing
        , protocolParamCostModels          = mempty
        , protocolParamPrices              = Nothing
        , protocolParamMaxTxExUnits        = Nothing
        , protocolParamMaxBlockExUnits     = Nothing
        , protocolParamMaxValSize          = Nothing
        }


checkProtocolVersion
  :: ShelleyBasedEra era
  -> (Natural, Natural)
  -> Either ProtocolParametersError ()
checkProtocolVersion sbe protoVer  =
  let expectedPVer = case sbe of
                       ShelleyBasedEraShelley -> (2,0)
                       ShelleyBasedEraAllegra -> (3,0)
                       ShelleyBasedEraMary -> (4,0)
              --      ShelleyBasedEraAlonzo -> (5,0)
  in checkCondition (protoVer == expectedPVer)
       $ ProtocolParametersErrorWrongVersion expectedPVer protoVer

checkCondition :: Bool -> ProtocolParametersError -> Either ProtocolParametersError ()
checkCondition True _ = Right ()
checkCondition False ppe = Left ppe

fromShelleyGenesis
  :: ShelleyBasedEra era
  -> Shelley.ShelleyGenesis (ShelleyLedgerEra era)
  -> Either ProtocolParametersError (GenesisParameters era)
fromShelleyGenesis
    sbe
    Shelley.ShelleyGenesis {
      Shelley.sgSystemStart
    , Shelley.sgNetworkMagic
    , Shelley.sgNetworkId
    , Shelley.sgActiveSlotsCoeff
    , Shelley.sgSecurityParam
    , Shelley.sgEpochLength
    , Shelley.sgSlotsPerKESPeriod
    , Shelley.sgMaxKESEvolutions
    , Shelley.sgSlotLength
    , Shelley.sgUpdateQuorum
    , Shelley.sgMaxLovelaceSupply
    , Shelley.sgProtocolParams
    , Shelley.sgGenDelegs    = _  -- unused, might be of interest
    , Shelley.sgInitialFunds = _  -- unused, not retained by the node
    , Shelley.sgStaking      = _  -- unused, not retained by the node
    } =
    case sbe of
      ShelleyBasedEraShelley -> do
       pparams <- fromShelleyPParams sbe sgProtocolParams
       return $ GenesisParameters {
         protocolParamSystemStart            = sgSystemStart
       , protocolParamNetworkId              = fromShelleyNetwork sgNetworkId
                                                 (NetworkMagic sgNetworkMagic)
       , protocolParamActiveSlotsCoefficient = sgActiveSlotsCoeff
       , protocolParamSecurity               = fromIntegral sgSecurityParam
       , protocolParamEpochLength            = sgEpochLength
       , protocolParamSlotLength             = sgSlotLength
       , protocolParamSlotsPerKESPeriod      = fromIntegral sgSlotsPerKESPeriod
       , protocolParamMaxKESEvolutions       = fromIntegral sgMaxKESEvolutions
       , protocolParamUpdateQuorum           = fromIntegral sgUpdateQuorum
       , protocolParamMaxLovelaceSupply      = Lovelace
                                                 (fromIntegral sgMaxLovelaceSupply)
       , protocolInitialUpdateableProtocolParameters = pparams
       }
      ShelleyBasedEraAllegra -> do
         pparams <- fromShelleyPParams sbe sgProtocolParams
         return $ GenesisParameters {
           protocolParamSystemStart            = sgSystemStart
         , protocolParamNetworkId              = fromShelleyNetwork sgNetworkId
                                                   (NetworkMagic sgNetworkMagic)
         , protocolParamActiveSlotsCoefficient = sgActiveSlotsCoeff
         , protocolParamSecurity               = fromIntegral sgSecurityParam
         , protocolParamEpochLength            = sgEpochLength
         , protocolParamSlotLength             = sgSlotLength
         , protocolParamSlotsPerKESPeriod      = fromIntegral sgSlotsPerKESPeriod
         , protocolParamMaxKESEvolutions       = fromIntegral sgMaxKESEvolutions
         , protocolParamUpdateQuorum           = fromIntegral sgUpdateQuorum
         , protocolParamMaxLovelaceSupply      = Lovelace
                                                   (fromIntegral sgMaxLovelaceSupply)
         , protocolInitialUpdateableProtocolParameters = pparams
         }
      ShelleyBasedEraMary -> do
         pparams <- fromShelleyPParams sbe sgProtocolParams
         return $ GenesisParameters {
           protocolParamSystemStart            = sgSystemStart
         , protocolParamNetworkId              = fromShelleyNetwork sgNetworkId
                                                   (NetworkMagic sgNetworkMagic)
         , protocolParamActiveSlotsCoefficient = sgActiveSlotsCoeff
         , protocolParamSecurity               = fromIntegral sgSecurityParam
         , protocolParamEpochLength            = sgEpochLength
         , protocolParamSlotLength             = sgSlotLength
         , protocolParamSlotsPerKESPeriod      = fromIntegral sgSlotsPerKESPeriod
         , protocolParamMaxKESEvolutions       = fromIntegral sgMaxKESEvolutions
         , protocolParamUpdateQuorum           = fromIntegral sgUpdateQuorum
         , protocolParamMaxLovelaceSupply      = Lovelace
                                                   (fromIntegral sgMaxLovelaceSupply)
         , protocolInitialUpdateableProtocolParameters = pparams
         }
{-
      -- TODO: Ledger needs to update ShelleyGenesis with Core.PParams
      -- type family.
      ShelleyBasedEraAlonzo -> do
         pparams <- fromShelleyPParams sbe (error "sgProtocolParams")
         return $ GenesisParameters {
           protocolParamSystemStart            = sgSystemStart
         , protocolParamNetworkId              = fromShelleyNetwork sgNetworkId
                                                   (NetworkMagic sgNetworkMagic)
         , protocolParamActiveSlotsCoefficient = sgActiveSlotsCoeff
         , protocolParamSecurity               = fromIntegral sgSecurityParam
         , protocolParamEpochLength            = sgEpochLength
         , protocolParamSlotLength             = sgSlotLength
         , protocolParamSlotsPerKESPeriod      = fromIntegral sgSlotsPerKESPeriod
         , protocolParamMaxKESEvolutions       = fromIntegral sgMaxKESEvolutions
         , protocolParamUpdateQuorum           = fromIntegral sgUpdateQuorum
         , protocolParamMaxLovelaceSupply      = Lovelace
                                                   (fromIntegral sgMaxLovelaceSupply)
         , protocolInitialUpdateableProtocolParameters = pparams
         }
-}
