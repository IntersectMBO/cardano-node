{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use section" -}

-- | Transaction bodies
--
module Cardano.Api.TxBody (
    parseTxId,
    -- * Transaction bodies
    TxBody(.., TxBody),
    makeTransactionBody,
    createTransactionBody,
    createAndValidateTransactionBody,
    TxBodyContent(..),
    -- ** Transaction body builders
    defaultTxBodyContent,
    defaultTxFee,
    defaultTxValidityUpperBound,
    setTxIns,
    modTxIns,
    addTxIn,
    setTxInsCollateral,
    setTxInsReference,
    setTxOuts,
    modTxOuts,
    addTxOut,
    setTxTotalCollateral,
    setTxReturnCollateral,
    setTxFee,
    setTxValidityRange,
    setTxMetadata,
    setTxAuxScripts,
    setTxExtraKeyWits,
    setTxProtocolParams,
    setTxWithdrawals,
    setTxCertificates,
    setTxUpdateProposal,
    setTxMintValue,
    setTxScriptValidity,
    TxBodyError(..),
    TxBodyScriptData(..),
    TxScriptValidity(..),
    TxScriptValiditySupportedInEra(..),

    ScriptValidity(..),
    scriptValidityToIsValid,
    isValidToScriptValidity,
    scriptValidityToTxScriptValidity,
    txScriptValidityToIsValid,
    txScriptValidityToScriptValidity,

    -- * Transaction Ids
    TxId(..),
    getTxId,
    getTxIdShelley,

    -- * Transaction inputs
    TxIn(..),
    TxIns,
    TxIx(..),
    genesisUTxOPseudoTxIn,

    -- * Transaction outputs
    CtxTx, CtxUTxO,
    TxOut(..),
    TxOutValue(..),
    TxOutDatum(TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline),
    toCtxUTxOTxOut,
    lovelaceToTxOutValue,
    prettyRenderTxOut,
    txOutValueToLovelace,
    txOutValueToValue,
    parseHash,
    TxOutInAnyEra(..),
    txOutInAnyEra,

    -- * Other transaction body types
    TxInsCollateral(..),
    TxInsReference(..),
    TxReturnCollateral(..),
    TxTotalCollateral(..),
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxExtraKeyWitnesses(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),

    -- ** Building vs viewing transactions
    BuildTxWith(..),
    BuildTx,
    ViewTx,

    -- * Era-dependent transaction body features
    CollateralSupportedInEra(..),
    MultiAssetSupportedInEra(..),
    OnlyAdaSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    TxExtraKeyWitnessesSupportedInEra(..),
    ScriptDataSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),
    TxTotalAndReturnCollateralSupportedInEra(..),

    -- ** Feature availability functions
    collateralSupportedInEra,
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    extraKeyWitnessesSupportedInEra,
    scriptDataSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,
    txScriptValiditySupportedInShelleyBasedEra,
    txScriptValiditySupportedInCardanoEra,
    totalAndReturnCollateralSupportedInEra,

    -- ** Era-dependent protocol features
    ProtocolUTxOCostPerByteSupportedInEra(..),
    ProtocolUTxOCostPerWordSupportedInEra(..),

    -- ** Era-dependent protocol feature availability functions
    protocolUTxOCostPerByteSupportedInEra,
    protocolUTxOCostPerWordSupportedInEra,

    -- * Inspecting 'ScriptWitness'es
    AnyScriptWitness(..),
    ScriptWitnessIndex(..),
    renderScriptWitnessIndex,
    collectTxBodyScriptWitnesses,

    -- * Conversion to inline data
    scriptDataToInlineDatum,

    -- * Internal conversion functions & types
    toByronTxId,
    toShelleyTxId,
    toShelleyTxIn,
    toShelleyTxOut,
    toShelleyTxOutAny,
    fromShelleyTxId,
    fromShelleyTxIn,
    fromShelleyTxOut,
    toAlonzoRdmrPtr,
    fromAlonzoRdmrPtr,
    fromByronTxIn,
    fromLedgerTxOuts,
    renderTxIn,

    -- * Misc helpers
    calculateExecutionUnitsLovelace,
    orderStakeAddrs,
    orderTxIns,

    -- * Data family instances
    AsType(AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody, AsMaryTxBody),
  ) where

import           Control.Applicative (some)
import           Control.Monad (guard, unless)
import           Data.Aeson (object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (for_, toList)
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.List (intercalate, sortBy)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import           Data.Scientific (toBoundedInteger)
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Word (Word16, Word32, Word64)
import           GHC.Generics
import           Lens.Micro hiding (ix)
import           Lens.Micro.Extras (view)
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>))
import qualified Text.Parsec.String as Parsec

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Binary (Annotated (..), reAnnotate, recoverBytes)
import qualified Cardano.Ledger.Binary as CBOR
import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron

import qualified Cardano.Ledger.Api as L

import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.TxIn as L
import           Cardano.Ledger.Val as L (isZero)

import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.Delegation.Certificates as Shelley
import qualified Cardano.Ledger.Shelley.Genesis as Shelley

import           Cardano.Ledger.Mary.Value as L (MaryValue (..), MultiAsset)

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo

import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Conway.Core as L
import qualified Cardano.Ledger.Conway.Delegation.Certificates as Conway
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardAlonzo, StandardBabbage,
                   StandardConway, StandardMary, StandardShelley)

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.EraCast
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.ScriptData
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxIn
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value
import           Cardano.Api.ValueParser

-- | Indicates whether a script is expected to fail or pass validation.
data ScriptValidity
  = ScriptInvalid -- ^ Script is expected to fail validation.
                  -- Transactions marked as such can include scripts that fail validation.
                  -- Such transactions may be submitted to the chain, in which case the
                  -- collateral will be taken upon on chain script validation failure.

  | ScriptValid   -- ^ Script is expected to pass validation.
                  -- Transactions marked as such cannot include scripts that fail validation.

  deriving (Eq, Show)

instance CBOR.EncCBOR ScriptValidity where
  encCBOR = CBOR.encCBOR . scriptValidityToIsValid

instance CBOR.DecCBOR ScriptValidity where
  decCBOR = isValidToScriptValidity <$> CBOR.decCBOR

scriptValidityToIsValid :: ScriptValidity -> L.IsValid
scriptValidityToIsValid ScriptInvalid = L.IsValid False
scriptValidityToIsValid ScriptValid = L.IsValid True

isValidToScriptValidity :: L.IsValid -> ScriptValidity
isValidToScriptValidity (L.IsValid False) = ScriptInvalid
isValidToScriptValidity (L.IsValid True) = ScriptValid

-- | A representation of whether the era supports tx script validity.
--
-- The Alonzo and subsequent eras support script validity.
--
data TxScriptValidity era where
  TxScriptValidityNone :: TxScriptValidity era

  -- | Tx script validity is supported in transactions in the 'Alonzo' era onwards.
  TxScriptValidity
    :: TxScriptValiditySupportedInEra era
    -> ScriptValidity
    -> TxScriptValidity era

deriving instance Eq   (TxScriptValiditySupportedInEra era)
deriving instance Show (TxScriptValiditySupportedInEra era)

data TxScriptValiditySupportedInEra era where
  TxScriptValiditySupportedInAlonzoEra  :: TxScriptValiditySupportedInEra AlonzoEra
  TxScriptValiditySupportedInBabbageEra :: TxScriptValiditySupportedInEra BabbageEra
  TxScriptValiditySupportedInConwayEra  :: TxScriptValiditySupportedInEra ConwayEra

deriving instance Eq   (TxScriptValidity era)
deriving instance Show (TxScriptValidity era)

txScriptValiditySupportedInCardanoEra :: CardanoEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInCardanoEra ByronEra   = Nothing
txScriptValiditySupportedInCardanoEra ShelleyEra = Nothing
txScriptValiditySupportedInCardanoEra AllegraEra = Nothing
txScriptValiditySupportedInCardanoEra MaryEra    = Nothing
txScriptValiditySupportedInCardanoEra AlonzoEra  = Just TxScriptValiditySupportedInAlonzoEra
txScriptValiditySupportedInCardanoEra BabbageEra = Just TxScriptValiditySupportedInBabbageEra
txScriptValiditySupportedInCardanoEra ConwayEra = Just TxScriptValiditySupportedInConwayEra

txScriptValiditySupportedInShelleyBasedEra :: ShelleyBasedEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraShelley = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraAllegra = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraMary    = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraAlonzo  = Just TxScriptValiditySupportedInAlonzoEra
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraBabbage = Just TxScriptValiditySupportedInBabbageEra
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraConway = Just TxScriptValiditySupportedInConwayEra

txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
txScriptValidityToScriptValidity TxScriptValidityNone = ScriptValid
txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity

scriptValidityToTxScriptValidity :: ShelleyBasedEra era -> ScriptValidity -> TxScriptValidity era
scriptValidityToTxScriptValidity era scriptValidity = case txScriptValiditySupportedInShelleyBasedEra era of
  Nothing -> TxScriptValidityNone
  Just witness -> TxScriptValidity witness scriptValidity

txScriptValidityToIsValid :: TxScriptValidity era -> L.IsValid
txScriptValidityToIsValid = scriptValidityToIsValid . txScriptValidityToScriptValidity

-- ----------------------------------------------------------------------------
-- Transaction outputs
--

-- | The context is a transaction body
data CtxTx
-- | The context is the UTxO
data CtxUTxO

data TxOut ctx era = TxOut (AddressInEra    era)
                           (TxOutValue      era)
                           (TxOutDatum ctx  era)
                           (ReferenceScript era)

deriving instance Eq   (TxOut ctx era)
deriving instance Show (TxOut ctx era)

instance EraCast (TxOut ctx) where
  eraCast toEra (TxOut addressInEra txOutValue txOutDatum referenceScript) =
    TxOut
      <$> eraCast toEra addressInEra
      <*> eraCast toEra txOutValue
      <*> eraCast toEra txOutDatum
      <*> eraCast toEra referenceScript

data TxOutInAnyEra where
     TxOutInAnyEra :: CardanoEra era
                   -> TxOut CtxTx era
                   -> TxOutInAnyEra

deriving instance Show TxOutInAnyEra

instance Eq TxOutInAnyEra where
  TxOutInAnyEra era1 out1 == TxOutInAnyEra era2 out2 =
    case testEquality era1 era2 of
      Just Refl -> out1 == out2
      Nothing   -> False

-- | Convenience constructor for 'TxOutInAnyEra'
txOutInAnyEra :: IsCardanoEra era => TxOut CtxTx era -> TxOutInAnyEra
txOutInAnyEra = TxOutInAnyEra cardanoEra

toCtxUTxOTxOut :: TxOut CtxTx  era -> TxOut CtxUTxO era
toCtxUTxOTxOut (TxOut addr val d refS) =
  let dat = case d of
              TxOutDatumNone -> TxOutDatumNone
              TxOutDatumHash s h -> TxOutDatumHash s h
              TxOutDatumInTx' s h _ -> TxOutDatumHash s h
              TxOutDatumInline s sd -> TxOutDatumInline s sd
  in TxOut addr val dat refS

instance IsCardanoEra era => ToJSON (TxOut ctx era) where
  toJSON  = txOutToJsonValue cardanoEra

txOutToJsonValue :: CardanoEra era -> TxOut ctx era -> Aeson.Value
txOutToJsonValue era (TxOut addr val dat refScript) =
  case era of
    ByronEra -> object ["address" .= addr, "value" .= val]
    ShelleyEra -> object ["address" .= addr, "value" .= val]
    AllegraEra -> object ["address" .= addr, "value" .= val]
    MaryEra -> object ["address" .= addr, "value" .= val]
    AlonzoEra -> object
                   [ "address" .= addr
                   , "value" .= val
                   , datHashJsonVal dat
                   , "datum" .= datJsonVal dat
                   ]
    BabbageEra ->
      object
        [ "address" .= addr
        , "value" .= val
        , datHashJsonVal dat
        , "datum" .= datJsonVal dat
        , "inlineDatum" .= inlineDatumJsonVal dat
        , "referenceScript" .= refScriptJsonVal refScript
        ]
    ConwayEra ->
      object
        [ "address" .= addr
        , "value" .= val
        , datHashJsonVal dat
        , "datum" .= datJsonVal dat
        , "inlineDatum" .= inlineDatumJsonVal dat
        , "referenceScript" .= refScriptJsonVal refScript
        ]
 where
   datHashJsonVal :: TxOutDatum ctx era -> Aeson.Pair
   datHashJsonVal d =
     case d of
       TxOutDatumNone ->
         "datumhash" .= Aeson.Null
       TxOutDatumHash _ h ->
         "datumhash" .= toJSON h
       TxOutDatumInTx' _ h _ ->
         "datumhash" .= toJSON h
       TxOutDatumInline _ datum ->
         "inlineDatumhash"  .= toJSON (hashScriptDataBytes datum)

   datJsonVal :: TxOutDatum ctx era -> Aeson.Value
   datJsonVal d =
     case d of
       TxOutDatumNone -> Aeson.Null
       TxOutDatumHash _ _ -> Aeson.Null
       TxOutDatumInTx' _ _ datum -> scriptDataToJson ScriptDataJsonDetailedSchema datum
       TxOutDatumInline _ _ -> Aeson.Null

   inlineDatumJsonVal :: TxOutDatum ctx era -> Aeson.Value
   inlineDatumJsonVal d =
     case d of
       TxOutDatumNone -> Aeson.Null
       TxOutDatumHash {} -> Aeson.Null
       TxOutDatumInTx'{} -> Aeson.Null
       TxOutDatumInline _ datum -> scriptDataToJson ScriptDataJsonDetailedSchema datum

   refScriptJsonVal :: ReferenceScript era -> Aeson.Value
   refScriptJsonVal rScript =
     case rScript of
       ReferenceScript _ s -> toJSON s
       ReferenceScriptNone -> Aeson.Null

instance IsShelleyBasedEra era => FromJSON (TxOut CtxTx era) where
      parseJSON = withObject "TxOut" $ \o -> do
        case shelleyBasedEra :: ShelleyBasedEra era of
          ShelleyBasedEraShelley ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraMary ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAllegra ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAlonzo -> alonzoTxOutParser ScriptDataInAlonzoEra o

          ShelleyBasedEraBabbage -> do
            alonzoTxOutInBabbage <- alonzoTxOutParser ScriptDataInBabbageEra o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) -> do
                  case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right hashableData -> do
                      if hashScriptDataBytes hashableData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra hashableData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            mReferenceScript <- o .:? "referenceScript"

            reconcileBabbage alonzoTxOutInBabbage mInlineDatum mReferenceScript

          ShelleyBasedEraConway -> do
            alonzoTxOutInConway <- alonzoTxOutParser ScriptDataInConwayEra o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) ->
                  case scriptDataFromJson ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right sData ->
                      if hashScriptDataBytes sData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInConwayEra sData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            mReferenceScript <- o .:? "referenceScript"

            reconcileConway alonzoTxOutInConway mInlineDatum mReferenceScript
         where
           reconcileBabbage
             :: TxOut CtxTx BabbageEra -- ^ Alonzo era datum in Babbage era
             -> TxOutDatum CtxTx BabbageEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxTx BabbageEra)
           reconcileBabbage top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (alonzoDat, babbageDat) ->
                             fail $ "Parsed an Alonzo era datum and a Babbage era datum " <>
                                    "TxOut: " <> show top <>
                                    "Alonzo datum: " <> show alonzoDat <>
                                    "Babbage dat: " <> show babbageDat
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra anyScript
             return $ TxOut addr v finalDat finalRefScript

           reconcileConway
             :: TxOut CtxTx ConwayEra -- ^ Alonzo era datum in Conway era
             -> TxOutDatum CtxTx ConwayEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxTx ConwayEra)
           reconcileConway top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (alonzoDat, babbageDat) ->
                             fail $ "Parsed an Alonzo era datum and a Conway era datum " <>
                                    "TxOut: " <> show top <>
                                    "Alonzo datum: " <> show alonzoDat <>
                                    "Conway dat: " <> show babbageDat
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript ReferenceTxInsScriptsInlineDatumsInConwayEra anyScript
             return $ TxOut addr v finalDat finalRefScript

           alonzoTxOutParser
             :: ScriptDataSupportedInEra era -> Aeson.Object -> Aeson.Parser (TxOut CtxTx era)
           alonzoTxOutParser supp o = do
            mDatumHash <- o .:? "datumhash"
            mDatumVal <- o .:? "datum"
            case (mDatumVal, mDatumHash) of
               (Nothing,Nothing) -> TxOut <$> o .: "address"
                                          <*> o .: "value"
                                          <*> return TxOutDatumNone
                                          <*> return ReferenceScriptNone
               (Just dVal, Just dHash) -> do
                 case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
                   Left e -> fail $ "Error parsing ScriptData: " <> show e
                   Right hashableData ->
                      TxOut <$> o .: "address"
                            <*> o .: "value"
                            <*> return (TxOutDatumInTx' supp dHash hashableData)
                            <*> return ReferenceScriptNone
               (Nothing, Just dHash) ->
                 TxOut <$> o .: "address"
                       <*> o .: "value"
                       <*> return (TxOutDatumHash supp dHash)
                       <*> return ReferenceScriptNone
               (Just _dVal, Nothing) -> fail "Only datum JSON was found, this should not be possible."

instance IsShelleyBasedEra era => FromJSON (TxOut CtxUTxO era) where
      parseJSON = withObject "TxOut" $ \o -> do
        case shelleyBasedEra :: ShelleyBasedEra era of
          ShelleyBasedEraShelley ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraMary ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAllegra ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAlonzo -> alonzoTxOutParser ScriptDataInAlonzoEra o

          ShelleyBasedEraBabbage -> do
            alonzoTxOutInBabbage <- alonzoTxOutParser ScriptDataInBabbageEra o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) -> do
                     case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
                        Left err ->
                          fail $ "Error parsing TxOut JSON: " <> displayError err
                        Right hashableData -> do
                          if hashScriptDataBytes hashableData /= h
                          then fail "Inline datum not equivalent to inline datum hash"
                          else return $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra hashableData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            -- We check for a reference script
            mReferenceScript <- o .:? "referenceScript"

            reconcileBabbage alonzoTxOutInBabbage mInlineDatum mReferenceScript

          ShelleyBasedEraConway -> do
            alonzoTxOutInConway <- alonzoTxOutParser ScriptDataInConwayEra o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) ->
                  case scriptDataFromJson ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right sData ->
                      if hashScriptDataBytes sData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInConwayEra sData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            -- We check for a reference script
            mReferenceScript <- o .:? "referenceScript"

            reconcileConway alonzoTxOutInConway mInlineDatum mReferenceScript
         where
           reconcileBabbage
             :: TxOut CtxUTxO BabbageEra -- ^ Alonzo era datum in Babbage era
             -> TxOutDatum CtxUTxO BabbageEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxUTxO BabbageEra)
           reconcileBabbage (TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (_,_) -> fail "Parsed an Alonzo era datum and a Babbage era datum"
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra anyScript

             return $ TxOut addr v finalDat finalRefScript

           reconcileConway
             :: TxOut CtxUTxO ConwayEra -- ^ Alonzo era datum in Conway era
             -> TxOutDatum CtxUTxO ConwayEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxUTxO ConwayEra)
           reconcileConway (TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (_,_) -> fail "Parsed an Alonzo era datum and a Conway era datum"
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript ReferenceTxInsScriptsInlineDatumsInConwayEra anyScript

             return $ TxOut addr v finalDat finalRefScript

           alonzoTxOutParser :: ScriptDataSupportedInEra era -> Aeson.Object -> Aeson.Parser (TxOut CtxUTxO era)
           alonzoTxOutParser supp o = do
            mDatumHash <- o .:? "datumhash"
            case mDatumHash of
               Nothing -> TxOut <$> o .: "address"
                                          <*> o .: "value"
                                          <*> return TxOutDatumNone
                                          <*> return ReferenceScriptNone
               Just dHash ->
                 TxOut <$> o .: "address"
                        <*> o .: "value"
                        <*> return (TxOutDatumHash supp dHash)
                        <*> return ReferenceScriptNone

fromByronTxOut :: Byron.TxOut -> TxOut ctx ByronEra
fromByronTxOut (Byron.TxOut addr value) =
  TxOut
    (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
    (TxOutAdaOnly AdaOnlyInByronEra (fromByronLovelace value))
     TxOutDatumNone ReferenceScriptNone


toByronTxOut :: TxOut ctx ByronEra -> Maybe Byron.TxOut
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
                    (TxOutAdaOnly AdaOnlyInByronEra value) _ _) =
    Byron.TxOut addr <$> toByronLovelace value

toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
                    (TxOutValue era _) _ _) = case era of {}

toByronTxOut (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
                    _ _ _) = case era of {}


toShelleyTxOut :: forall era ledgerera.
                  ShelleyLedgerEra era ~ ledgerera
               => ShelleyBasedEra era
               -> TxOut CtxUTxO era
               -> Ledger.TxOut ledgerera
toShelleyTxOut era (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _) _ _) =
    case era of {}

toShelleyTxOut _ (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutValue MultiAssetInMaryEra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOut _ (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
    & L.dataHashTxOutL .~ toAlonzoTxOutDataHash txoutdata

toShelleyTxOut era (TxOut addr (TxOutValue MultiAssetInBabbageEra value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra era
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

toShelleyTxOut era (TxOut addr (TxOutValue MultiAssetInConwayEra value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra era
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

fromShelleyTxOut :: ShelleyLedgerEra era ~ ledgerera
                 => ShelleyBasedEra era
                 -> Core.TxOut ledgerera
                 -> TxOut ctx era
fromShelleyTxOut era ledgerTxOut =
  case era of
    ShelleyBasedEraShelley ->
        TxOut (fromShelleyAddr era addr)
              (TxOutAdaOnly AdaOnlyInShelleyEra
                            (fromShelleyLovelace value))
               TxOutDatumNone ReferenceScriptNone
      where
        addr = ledgerTxOut ^. L.addrTxOutL
        value = ledgerTxOut ^. L.valueTxOutL

    ShelleyBasedEraAllegra ->
        TxOut (fromShelleyAddr era addr)
              (TxOutAdaOnly AdaOnlyInAllegraEra
                            (fromShelleyLovelace value))
               TxOutDatumNone ReferenceScriptNone
      where
        addr = ledgerTxOut ^. L.addrTxOutL
        value = ledgerTxOut ^. L.valueTxOutL

    ShelleyBasedEraMary ->
        TxOut (fromShelleyAddr era addr)
              (TxOutValue MultiAssetInMaryEra
                          (fromMaryValue value))
               TxOutDatumNone ReferenceScriptNone
      where
        addr = ledgerTxOut ^. L.addrTxOutL
        value = ledgerTxOut ^. L.valueTxOutL

    ShelleyBasedEraAlonzo ->
       TxOut (fromShelleyAddr era addr)
             (TxOutValue MultiAssetInAlonzoEra
                         (fromMaryValue value))
             (fromAlonzoTxOutDataHash ScriptDataInAlonzoEra datahash)
             ReferenceScriptNone
      where
        addr = ledgerTxOut ^. L.addrTxOutL
        value = ledgerTxOut ^. L.valueTxOutL
        datahash = ledgerTxOut ^. L.dataHashTxOutL

    ShelleyBasedEraBabbage ->
       TxOut (fromShelleyAddr era addr)
             (TxOutValue MultiAssetInBabbageEra
                         (fromMaryValue value))
             (fromBabbageTxOutDatum
               ScriptDataInBabbageEra
               ReferenceTxInsScriptsInlineDatumsInBabbageEra
               datum)
             (case mRefScript of
                SNothing -> ReferenceScriptNone
                SJust refScript ->
                  fromShelleyScriptToReferenceScript ShelleyBasedEraBabbage refScript)
      where
        addr = ledgerTxOut ^. L.addrTxOutL
        value = ledgerTxOut ^. L.valueTxOutL
        datum = ledgerTxOut ^. L.datumTxOutL
        mRefScript = ledgerTxOut ^. L.referenceScriptTxOutL

    ShelleyBasedEraConway ->
       TxOut (fromShelleyAddr era addr)
             (TxOutValue MultiAssetInConwayEra
                         (fromMaryValue value))
             (fromBabbageTxOutDatum
               ScriptDataInConwayEra
               ReferenceTxInsScriptsInlineDatumsInConwayEra
               datum)
             (case mRefScript of
                SNothing -> ReferenceScriptNone
                SJust refScript ->
                  fromShelleyScriptToReferenceScript ShelleyBasedEraConway refScript)
      where
        addr = ledgerTxOut ^. L.addrTxOutL
        value = ledgerTxOut ^. L.valueTxOutL
        datum = ledgerTxOut ^. L.datumTxOutL
        mRefScript = ledgerTxOut ^. L.referenceScriptTxOutL


-- TODO: If ledger creates an open type family for datums
-- we can consolidate this function with the Babbage version
toAlonzoTxOutDataHash
  :: TxOutDatum CtxUTxO AlonzoEra
  -> StrictMaybe (L.DataHash StandardCrypto)
toAlonzoTxOutDataHash  TxOutDatumNone                        = SNothing
toAlonzoTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDataHash (TxOutDatumInline inlineDatumSupp _sd) =
  case inlineDatumSupp :: ReferenceTxInsScriptsInlineDatumsSupportedInEra AlonzoEra of {}

fromAlonzoTxOutDataHash :: ScriptDataSupportedInEra era
                        -> StrictMaybe (L.DataHash StandardCrypto)
                        -> TxOutDatum ctx era
fromAlonzoTxOutDataHash _    SNothing  = TxOutDatumNone
fromAlonzoTxOutDataHash s (SJust dh)   = TxOutDatumHash s (ScriptDataHash dh)

-- TODO: If ledger creates an open type family for datums
-- we can consolidate this function with the Alonzo version
toBabbageTxOutDatum
  :: (L.Era (ShelleyLedgerEra era), Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => TxOutDatum CtxUTxO era -> Babbage.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum  TxOutDatumNone = Babbage.NoDatum
toBabbageTxOutDatum (TxOutDatumHash _ (ScriptDataHash dh)) = Babbage.DatumHash dh
toBabbageTxOutDatum (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

fromBabbageTxOutDatum
  :: (L.Era ledgerera, Ledger.EraCrypto ledgerera ~ StandardCrypto)
  => ScriptDataSupportedInEra era
  -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -> Babbage.Datum ledgerera
  -> TxOutDatum ctx era
fromBabbageTxOutDatum _ _ Babbage.NoDatum = TxOutDatumNone
fromBabbageTxOutDatum supp _ (Babbage.DatumHash dh) =
  TxOutDatumHash supp $ ScriptDataHash dh
fromBabbageTxOutDatum _ supp (Babbage.Datum binData) =
  TxOutDatumInline supp $ binaryDataToScriptData supp binData



-- ----------------------------------------------------------------------------
-- Era-dependent transaction body features
--

-- | A representation of whether the era supports transactions with inputs used
-- only for collateral for script fees.
--
-- The Alonzo and subsequent eras support collateral inputs.
--
data CollateralSupportedInEra era where

     CollateralInAlonzoEra  :: CollateralSupportedInEra AlonzoEra
     CollateralInBabbageEra :: CollateralSupportedInEra BabbageEra
     CollateralInConwayEra  :: CollateralSupportedInEra ConwayEra

deriving instance Eq   (CollateralSupportedInEra era)
deriving instance Show (CollateralSupportedInEra era)

collateralSupportedInEra :: CardanoEra era
                         -> Maybe (CollateralSupportedInEra era)
collateralSupportedInEra ByronEra   = Nothing
collateralSupportedInEra ShelleyEra = Nothing
collateralSupportedInEra AllegraEra = Nothing
collateralSupportedInEra MaryEra    = Nothing
collateralSupportedInEra AlonzoEra  = Just CollateralInAlonzoEra
collateralSupportedInEra BabbageEra = Just CollateralInBabbageEra
collateralSupportedInEra ConwayEra = Just CollateralInConwayEra


-- | A representation of whether the era supports multi-asset transactions.
--
-- The Mary and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyAdaSupportedInEra'.
--
data MultiAssetSupportedInEra era where

     -- | Multi-asset transactions are supported in the 'Mary' era.
     MultiAssetInMaryEra :: MultiAssetSupportedInEra MaryEra

     -- | Multi-asset transactions are supported in the 'Alonzo' era.
     MultiAssetInAlonzoEra :: MultiAssetSupportedInEra AlonzoEra

     -- | Multi-asset transactions are supported in the 'Babbage' era.
     MultiAssetInBabbageEra :: MultiAssetSupportedInEra BabbageEra

     -- | Multi-asset transactions are supported in the 'Conway' era.
     MultiAssetInConwayEra :: MultiAssetSupportedInEra ConwayEra

deriving instance Eq   (MultiAssetSupportedInEra era)
deriving instance Show (MultiAssetSupportedInEra era)

instance ToJSON (MultiAssetSupportedInEra era) where
  toJSON = Aeson.String . Text.pack . show

-- | A representation of whether the era supports only ada transactions.
--
-- Prior to the Mary era only ada transactions are supported. Multi-assets are
-- supported from the Mary era onwards.
--
-- This is the negation of 'MultiAssetSupportedInEra'. It exists since we need
-- evidence to be positive.
--
data OnlyAdaSupportedInEra era where

     AdaOnlyInByronEra   :: OnlyAdaSupportedInEra ByronEra
     AdaOnlyInShelleyEra :: OnlyAdaSupportedInEra ShelleyEra
     AdaOnlyInAllegraEra :: OnlyAdaSupportedInEra AllegraEra

deriving instance Eq   (OnlyAdaSupportedInEra era)
deriving instance Show (OnlyAdaSupportedInEra era)

multiAssetSupportedInEra :: CardanoEra era
                         -> Either (OnlyAdaSupportedInEra era)
                                   (MultiAssetSupportedInEra era)
multiAssetSupportedInEra ByronEra   = Left AdaOnlyInByronEra
multiAssetSupportedInEra ShelleyEra = Left AdaOnlyInShelleyEra
multiAssetSupportedInEra AllegraEra = Left AdaOnlyInAllegraEra
multiAssetSupportedInEra MaryEra    = Right MultiAssetInMaryEra
multiAssetSupportedInEra AlonzoEra  = Right MultiAssetInAlonzoEra
multiAssetSupportedInEra BabbageEra = Right MultiAssetInBabbageEra
multiAssetSupportedInEra ConwayEra = Right MultiAssetInConwayEra


-- | A representation of whether the era requires explicitly specified fees in
-- transactions.
--
-- The Byron era tx fees are implicit (as the difference bettween the sum of
-- outputs and sum of inputs), but all later eras the fees are specified in the
-- transaction explicitly.
--
data TxFeesExplicitInEra era where

     TxFeesExplicitInShelleyEra :: TxFeesExplicitInEra ShelleyEra
     TxFeesExplicitInAllegraEra :: TxFeesExplicitInEra AllegraEra
     TxFeesExplicitInMaryEra    :: TxFeesExplicitInEra MaryEra
     TxFeesExplicitInAlonzoEra  :: TxFeesExplicitInEra AlonzoEra
     TxFeesExplicitInBabbageEra :: TxFeesExplicitInEra BabbageEra
     TxFeesExplicitInConwayEra  :: TxFeesExplicitInEra ConwayEra

deriving instance Eq   (TxFeesExplicitInEra era)
deriving instance Show (TxFeesExplicitInEra era)

-- | A representation of whether the era requires implicitly specified fees in
-- transactions.
--
-- This is the negation of 'TxFeesExplicitInEra'.
--
data TxFeesImplicitInEra era where
     TxFeesImplicitInByronEra :: TxFeesImplicitInEra ByronEra

deriving instance Eq   (TxFeesImplicitInEra era)
deriving instance Show (TxFeesImplicitInEra era)

txFeesExplicitInEra :: CardanoEra era
                    -> Either (TxFeesImplicitInEra era)
                              (TxFeesExplicitInEra era)
txFeesExplicitInEra ByronEra   = Left  TxFeesImplicitInByronEra
txFeesExplicitInEra ShelleyEra = Right TxFeesExplicitInShelleyEra
txFeesExplicitInEra AllegraEra = Right TxFeesExplicitInAllegraEra
txFeesExplicitInEra MaryEra    = Right TxFeesExplicitInMaryEra
txFeesExplicitInEra AlonzoEra  = Right TxFeesExplicitInAlonzoEra
txFeesExplicitInEra BabbageEra = Right TxFeesExplicitInBabbageEra
txFeesExplicitInEra ConwayEra = Right TxFeesExplicitInConwayEra


-- | A representation of whether the era supports transactions with an upper
-- bound on the range of slots in which they are valid.
--
-- The Shelley and subsequent eras support an upper bound on the validity
-- range. In the Shelley era specifically it is actually required. It is
-- optional in later eras.
--
data ValidityUpperBoundSupportedInEra era where

     ValidityUpperBoundInShelleyEra :: ValidityUpperBoundSupportedInEra ShelleyEra
     ValidityUpperBoundInAllegraEra :: ValidityUpperBoundSupportedInEra AllegraEra
     ValidityUpperBoundInMaryEra    :: ValidityUpperBoundSupportedInEra MaryEra
     ValidityUpperBoundInAlonzoEra  :: ValidityUpperBoundSupportedInEra AlonzoEra
     ValidityUpperBoundInBabbageEra :: ValidityUpperBoundSupportedInEra BabbageEra
     ValidityUpperBoundInConwayEra :: ValidityUpperBoundSupportedInEra ConwayEra

deriving instance Eq   (ValidityUpperBoundSupportedInEra era)
deriving instance Show (ValidityUpperBoundSupportedInEra era)

validityUpperBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityUpperBoundSupportedInEra era)
validityUpperBoundSupportedInEra ByronEra   = Nothing
validityUpperBoundSupportedInEra ShelleyEra = Just ValidityUpperBoundInShelleyEra
validityUpperBoundSupportedInEra AllegraEra = Just ValidityUpperBoundInAllegraEra
validityUpperBoundSupportedInEra MaryEra    = Just ValidityUpperBoundInMaryEra
validityUpperBoundSupportedInEra AlonzoEra  = Just ValidityUpperBoundInAlonzoEra
validityUpperBoundSupportedInEra BabbageEra = Just ValidityUpperBoundInBabbageEra
validityUpperBoundSupportedInEra ConwayEra = Just ValidityUpperBoundInConwayEra


-- | A representation of whether the era supports transactions having /no/
-- upper bound on the range of slots in which they are valid.
--
-- Note that the 'ShelleyEra' /does not support/ omitting a validity upper
-- bound. It was introduced as a /required/ field in Shelley and then made
-- optional in Allegra and subsequent eras.
--
-- The Byron era supports this by virtue of the fact that it does not support
-- validity ranges at all.
--
data ValidityNoUpperBoundSupportedInEra era where

     ValidityNoUpperBoundInByronEra   :: ValidityNoUpperBoundSupportedInEra ByronEra
     ValidityNoUpperBoundInAllegraEra :: ValidityNoUpperBoundSupportedInEra AllegraEra
     ValidityNoUpperBoundInMaryEra    :: ValidityNoUpperBoundSupportedInEra MaryEra
     ValidityNoUpperBoundInAlonzoEra  :: ValidityNoUpperBoundSupportedInEra AlonzoEra
     ValidityNoUpperBoundInBabbageEra :: ValidityNoUpperBoundSupportedInEra BabbageEra
     ValidityNoUpperBoundInConwayEra  :: ValidityNoUpperBoundSupportedInEra ConwayEra

deriving instance Eq   (ValidityNoUpperBoundSupportedInEra era)
deriving instance Show (ValidityNoUpperBoundSupportedInEra era)

validityNoUpperBoundSupportedInEra :: CardanoEra era
                                   -> Maybe (ValidityNoUpperBoundSupportedInEra era)
validityNoUpperBoundSupportedInEra ByronEra   = Just ValidityNoUpperBoundInByronEra
validityNoUpperBoundSupportedInEra ShelleyEra = Nothing
validityNoUpperBoundSupportedInEra AllegraEra = Just ValidityNoUpperBoundInAllegraEra
validityNoUpperBoundSupportedInEra MaryEra    = Just ValidityNoUpperBoundInMaryEra
validityNoUpperBoundSupportedInEra AlonzoEra  = Just ValidityNoUpperBoundInAlonzoEra
validityNoUpperBoundSupportedInEra BabbageEra = Just ValidityNoUpperBoundInBabbageEra
validityNoUpperBoundSupportedInEra ConwayEra = Just ValidityNoUpperBoundInConwayEra


-- | A representation of whether the era supports transactions with a lower
-- bound on the range of slots in which they are valid.
--
-- The Allegra and subsequent eras support an optional lower bound on the
-- validity range. No equivalent of 'ValidityNoUpperBoundSupportedInEra' is
-- needed since all eras support having no lower bound.
--
data ValidityLowerBoundSupportedInEra era where

     ValidityLowerBoundInAllegraEra :: ValidityLowerBoundSupportedInEra AllegraEra
     ValidityLowerBoundInMaryEra    :: ValidityLowerBoundSupportedInEra MaryEra
     ValidityLowerBoundInAlonzoEra  :: ValidityLowerBoundSupportedInEra AlonzoEra
     ValidityLowerBoundInBabbageEra :: ValidityLowerBoundSupportedInEra BabbageEra
     ValidityLowerBoundInConwayEra  :: ValidityLowerBoundSupportedInEra ConwayEra

deriving instance Eq   (ValidityLowerBoundSupportedInEra era)
deriving instance Show (ValidityLowerBoundSupportedInEra era)

validityLowerBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityLowerBoundSupportedInEra era)
validityLowerBoundSupportedInEra ByronEra   = Nothing
validityLowerBoundSupportedInEra ShelleyEra = Nothing
validityLowerBoundSupportedInEra AllegraEra = Just ValidityLowerBoundInAllegraEra
validityLowerBoundSupportedInEra MaryEra    = Just ValidityLowerBoundInMaryEra
validityLowerBoundSupportedInEra AlonzoEra  = Just ValidityLowerBoundInAlonzoEra
validityLowerBoundSupportedInEra BabbageEra = Just ValidityLowerBoundInBabbageEra
validityLowerBoundSupportedInEra ConwayEra  = Just ValidityLowerBoundInConwayEra

-- | A representation of whether the era supports transaction metadata.
--
-- Transaction metadata is supported from the Shelley era onwards.
--
data TxMetadataSupportedInEra era where

     TxMetadataInShelleyEra :: TxMetadataSupportedInEra ShelleyEra
     TxMetadataInAllegraEra :: TxMetadataSupportedInEra AllegraEra
     TxMetadataInMaryEra    :: TxMetadataSupportedInEra MaryEra
     TxMetadataInAlonzoEra  :: TxMetadataSupportedInEra AlonzoEra
     TxMetadataInBabbageEra :: TxMetadataSupportedInEra BabbageEra
     TxMetadataInConwayEra  :: TxMetadataSupportedInEra ConwayEra

deriving instance Eq   (TxMetadataSupportedInEra era)
deriving instance Show (TxMetadataSupportedInEra era)

txMetadataSupportedInEra :: CardanoEra era
                         -> Maybe (TxMetadataSupportedInEra era)
txMetadataSupportedInEra ByronEra   = Nothing
txMetadataSupportedInEra ShelleyEra = Just TxMetadataInShelleyEra
txMetadataSupportedInEra AllegraEra = Just TxMetadataInAllegraEra
txMetadataSupportedInEra MaryEra    = Just TxMetadataInMaryEra
txMetadataSupportedInEra AlonzoEra  = Just TxMetadataInAlonzoEra
txMetadataSupportedInEra BabbageEra = Just TxMetadataInBabbageEra
txMetadataSupportedInEra ConwayEra  = Just TxMetadataInConwayEra


-- | A representation of whether the era supports auxiliary scripts in
-- transactions.
--
-- Auxiliary scripts are supported from the Allegra era onwards.
--
data AuxScriptsSupportedInEra era where

     AuxScriptsInAllegraEra :: AuxScriptsSupportedInEra AllegraEra
     AuxScriptsInMaryEra    :: AuxScriptsSupportedInEra MaryEra
     AuxScriptsInAlonzoEra  :: AuxScriptsSupportedInEra AlonzoEra
     AuxScriptsInBabbageEra :: AuxScriptsSupportedInEra BabbageEra
     AuxScriptsInConwayEra  :: AuxScriptsSupportedInEra ConwayEra

deriving instance Eq   (AuxScriptsSupportedInEra era)
deriving instance Show (AuxScriptsSupportedInEra era)

auxScriptsSupportedInEra :: CardanoEra era
                         -> Maybe (AuxScriptsSupportedInEra era)
auxScriptsSupportedInEra ByronEra   = Nothing
auxScriptsSupportedInEra ShelleyEra = Nothing
auxScriptsSupportedInEra AllegraEra = Just AuxScriptsInAllegraEra
auxScriptsSupportedInEra MaryEra    = Just AuxScriptsInMaryEra
auxScriptsSupportedInEra AlonzoEra  = Just AuxScriptsInAlonzoEra
auxScriptsSupportedInEra BabbageEra = Just AuxScriptsInBabbageEra
auxScriptsSupportedInEra ConwayEra  = Just AuxScriptsInConwayEra


-- | A representation of whether the era supports transactions that specify
-- in the body that they need extra key witnesses, and where this fact is
-- visible to scripts.
--
-- Extra key witnesses visible to scripts are supported from the Alonzo era
-- onwards.
--
data TxExtraKeyWitnessesSupportedInEra era where

     ExtraKeyWitnessesInAlonzoEra  :: TxExtraKeyWitnessesSupportedInEra AlonzoEra
     ExtraKeyWitnessesInBabbageEra :: TxExtraKeyWitnessesSupportedInEra BabbageEra
     ExtraKeyWitnessesInConwayEra  :: TxExtraKeyWitnessesSupportedInEra ConwayEra

deriving instance Eq   (TxExtraKeyWitnessesSupportedInEra era)
deriving instance Show (TxExtraKeyWitnessesSupportedInEra era)

extraKeyWitnessesSupportedInEra :: CardanoEra era
                                -> Maybe (TxExtraKeyWitnessesSupportedInEra era)
extraKeyWitnessesSupportedInEra ByronEra   = Nothing
extraKeyWitnessesSupportedInEra ShelleyEra = Nothing
extraKeyWitnessesSupportedInEra AllegraEra = Nothing
extraKeyWitnessesSupportedInEra MaryEra    = Nothing
extraKeyWitnessesSupportedInEra AlonzoEra  = Just ExtraKeyWitnessesInAlonzoEra
extraKeyWitnessesSupportedInEra BabbageEra = Just ExtraKeyWitnessesInBabbageEra
extraKeyWitnessesSupportedInEra ConwayEra  = Just ExtraKeyWitnessesInConwayEra


-- | A representation of whether the era supports script data in transactions.
data ScriptDataSupportedInEra era where

     -- | Script data is supported in transactions in the 'Alonzo' era.
     ScriptDataInAlonzoEra  :: ScriptDataSupportedInEra AlonzoEra
     ScriptDataInBabbageEra :: ScriptDataSupportedInEra BabbageEra
     ScriptDataInConwayEra :: ScriptDataSupportedInEra ConwayEra

deriving instance Eq   (ScriptDataSupportedInEra era)
deriving instance Show (ScriptDataSupportedInEra era)

scriptDataSupportedInEra :: CardanoEra era
                         -> Maybe (ScriptDataSupportedInEra era)
scriptDataSupportedInEra ByronEra   = Nothing
scriptDataSupportedInEra ShelleyEra = Nothing
scriptDataSupportedInEra AllegraEra = Nothing
scriptDataSupportedInEra MaryEra    = Nothing
scriptDataSupportedInEra AlonzoEra  = Just ScriptDataInAlonzoEra
scriptDataSupportedInEra BabbageEra = Just ScriptDataInBabbageEra
scriptDataSupportedInEra ConwayEra  = Just ScriptDataInConwayEra


-- | A representation of whether the era supports withdrawals from reward
-- accounts.
--
-- The Shelley and subsequent eras support stake addresses, their associated
-- reward accounts and support for withdrawals from them.
--
data WithdrawalsSupportedInEra era where

     WithdrawalsInShelleyEra :: WithdrawalsSupportedInEra ShelleyEra
     WithdrawalsInAllegraEra :: WithdrawalsSupportedInEra AllegraEra
     WithdrawalsInMaryEra    :: WithdrawalsSupportedInEra MaryEra
     WithdrawalsInAlonzoEra  :: WithdrawalsSupportedInEra AlonzoEra
     WithdrawalsInBabbageEra :: WithdrawalsSupportedInEra BabbageEra
     WithdrawalsInConwayEra :: WithdrawalsSupportedInEra ConwayEra

deriving instance Eq   (WithdrawalsSupportedInEra era)
deriving instance Show (WithdrawalsSupportedInEra era)

withdrawalsSupportedInEra :: CardanoEra era
                          -> Maybe (WithdrawalsSupportedInEra era)
withdrawalsSupportedInEra ByronEra   = Nothing
withdrawalsSupportedInEra ShelleyEra = Just WithdrawalsInShelleyEra
withdrawalsSupportedInEra AllegraEra = Just WithdrawalsInAllegraEra
withdrawalsSupportedInEra MaryEra    = Just WithdrawalsInMaryEra
withdrawalsSupportedInEra AlonzoEra  = Just WithdrawalsInAlonzoEra
withdrawalsSupportedInEra BabbageEra = Just WithdrawalsInBabbageEra
withdrawalsSupportedInEra ConwayEra  = Just WithdrawalsInConwayEra


-- | A representation of whether the era supports 'Certificate's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such certificates.
--
data CertificatesSupportedInEra era where

     CertificatesInShelleyEra :: CertificatesSupportedInEra ShelleyEra
     CertificatesInAllegraEra :: CertificatesSupportedInEra AllegraEra
     CertificatesInMaryEra    :: CertificatesSupportedInEra MaryEra
     CertificatesInAlonzoEra  :: CertificatesSupportedInEra AlonzoEra
     CertificatesInBabbageEra :: CertificatesSupportedInEra BabbageEra
     CertificatesInConwayEra  :: CertificatesSupportedInEra ConwayEra

deriving instance Eq   (CertificatesSupportedInEra era)
deriving instance Show (CertificatesSupportedInEra era)

certificatesSupportedInEra :: CardanoEra era
                           -> Maybe (CertificatesSupportedInEra era)
certificatesSupportedInEra ByronEra   = Nothing
certificatesSupportedInEra ShelleyEra = Just CertificatesInShelleyEra
certificatesSupportedInEra AllegraEra = Just CertificatesInAllegraEra
certificatesSupportedInEra MaryEra    = Just CertificatesInMaryEra
certificatesSupportedInEra AlonzoEra  = Just CertificatesInAlonzoEra
certificatesSupportedInEra BabbageEra = Just CertificatesInBabbageEra
certificatesSupportedInEra ConwayEra  = Just CertificatesInConwayEra


-- | A representation of whether the era supports 'UpdateProposal's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such update proposals. They Byron
-- era has a notion of an update proposal, but it is a standalone chain object
-- and not embedded in a transaction.
--
data UpdateProposalSupportedInEra era where

     UpdateProposalInShelleyEra :: UpdateProposalSupportedInEra ShelleyEra
     UpdateProposalInAllegraEra :: UpdateProposalSupportedInEra AllegraEra
     UpdateProposalInMaryEra    :: UpdateProposalSupportedInEra MaryEra
     UpdateProposalInAlonzoEra  :: UpdateProposalSupportedInEra AlonzoEra
     UpdateProposalInBabbageEra :: UpdateProposalSupportedInEra BabbageEra
     UpdateProposalInConwayEra  :: UpdateProposalSupportedInEra ConwayEra

deriving instance Eq   (UpdateProposalSupportedInEra era)
deriving instance Show (UpdateProposalSupportedInEra era)

updateProposalSupportedInEra :: CardanoEra era
                             -> Maybe (UpdateProposalSupportedInEra era)
updateProposalSupportedInEra ByronEra   = Nothing
updateProposalSupportedInEra ShelleyEra = Just UpdateProposalInShelleyEra
updateProposalSupportedInEra AllegraEra = Just UpdateProposalInAllegraEra
updateProposalSupportedInEra MaryEra    = Just UpdateProposalInMaryEra
updateProposalSupportedInEra AlonzoEra  = Just UpdateProposalInAlonzoEra
updateProposalSupportedInEra BabbageEra = Just UpdateProposalInBabbageEra
updateProposalSupportedInEra ConwayEra  = Just UpdateProposalInConwayEra

-- | A representation of whether the era supports the 'UTxO Cost Per Word'
-- protocol parameter.
--
-- The Babbage and subsequent eras support such a protocol parameter.
--
data ProtocolUTxOCostPerWordSupportedInEra era where
  ProtocolUpdateUTxOCostPerWordInAlonzoEra :: ProtocolUTxOCostPerWordSupportedInEra AlonzoEra

deriving instance Eq   (ProtocolUTxOCostPerWordSupportedInEra era)
deriving instance Show (ProtocolUTxOCostPerWordSupportedInEra era)

protocolUTxOCostPerWordSupportedInEra
  :: CardanoEra era
  -> Maybe (ProtocolUTxOCostPerWordSupportedInEra era)
protocolUTxOCostPerWordSupportedInEra ByronEra   = Nothing
protocolUTxOCostPerWordSupportedInEra ShelleyEra = Nothing
protocolUTxOCostPerWordSupportedInEra AllegraEra = Nothing
protocolUTxOCostPerWordSupportedInEra MaryEra    = Nothing
protocolUTxOCostPerWordSupportedInEra AlonzoEra  = Just ProtocolUpdateUTxOCostPerWordInAlonzoEra
protocolUTxOCostPerWordSupportedInEra BabbageEra = Nothing
protocolUTxOCostPerWordSupportedInEra ConwayEra  = Nothing

-- | A representation of whether the era supports the 'UTxO Cost Per Byte'
-- protocol parameter.
--
-- The Babbage and subsequent eras support such a protocol parameter.
--
data ProtocolUTxOCostPerByteSupportedInEra era where
  ProtocolUpdateUTxOCostPerByteInBabbageEra :: ProtocolUTxOCostPerByteSupportedInEra BabbageEra
  ProtocolUpdateUTxOCostPerByteInConwayEra  :: ProtocolUTxOCostPerByteSupportedInEra ConwayEra

deriving instance Eq   (ProtocolUTxOCostPerByteSupportedInEra era)
deriving instance Show (ProtocolUTxOCostPerByteSupportedInEra era)

protocolUTxOCostPerByteSupportedInEra
  :: CardanoEra era
  -> Maybe (ProtocolUTxOCostPerByteSupportedInEra era)
protocolUTxOCostPerByteSupportedInEra ByronEra   = Nothing
protocolUTxOCostPerByteSupportedInEra ShelleyEra = Nothing
protocolUTxOCostPerByteSupportedInEra AllegraEra = Nothing
protocolUTxOCostPerByteSupportedInEra MaryEra    = Nothing
protocolUTxOCostPerByteSupportedInEra AlonzoEra  = Nothing
protocolUTxOCostPerByteSupportedInEra BabbageEra = Just ProtocolUpdateUTxOCostPerByteInBabbageEra
protocolUTxOCostPerByteSupportedInEra ConwayEra  = Just ProtocolUpdateUTxOCostPerByteInConwayEra

-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data BuildTx
data ViewTx

data BuildTxWith build a where

     ViewTx      ::      BuildTxWith ViewTx  a
     BuildTxWith :: a -> BuildTxWith BuildTx a

instance Functor (BuildTxWith build) where
    fmap _ ViewTx = ViewTx
    fmap f (BuildTxWith x) = BuildTxWith (f x)

deriving instance Eq   a => Eq   (BuildTxWith build a)
deriving instance Show a => Show (BuildTxWith build a)

-- ----------------------------------------------------------------------------
-- Transaction input values (era-dependent)
--

type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

data TxInsCollateral era where

     TxInsCollateralNone :: TxInsCollateral era

     TxInsCollateral     :: CollateralSupportedInEra era
                         -> [TxIn] -- Only key witnesses, no scripts.
                         -> TxInsCollateral era

deriving instance Eq   (TxInsCollateral era)
deriving instance Show (TxInsCollateral era)

data TxInsReference build era where

     TxInsReferenceNone :: TxInsReference build era

     TxInsReference     :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
                        -> [TxIn]
                        -> TxInsReference build era

deriving instance Eq   (TxInsReference build era)
deriving instance Show (TxInsReference build era)

-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where

     TxOutAdaOnly :: OnlyAdaSupportedInEra era -> Lovelace -> TxOutValue era

     TxOutValue   :: MultiAssetSupportedInEra era -> Value -> TxOutValue era

instance EraCast TxOutValue where
  eraCast toEra v = case v of
    TxOutAdaOnly _previousEra lovelace ->
      case multiAssetSupportedInEra toEra of
        Left adaOnly -> Right $ TxOutAdaOnly adaOnly lovelace
        Right multiAssetSupp -> Right $ TxOutValue multiAssetSupp $ lovelaceToValue lovelace
    TxOutValue  (_ :: MultiAssetSupportedInEra fromEra) value  ->
      case multiAssetSupportedInEra toEra of
        Left _adaOnly -> Left $ EraCastError v (cardanoEra @fromEra) toEra
        Right multiAssetSupp -> Right $ TxOutValue multiAssetSupp value


deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)
deriving instance Generic (TxOutValue era)

instance ToJSON (TxOutValue era) where
  toJSON (TxOutAdaOnly _ ll) = toJSON ll
  toJSON (TxOutValue _ val) = toJSON val

instance IsCardanoEra era => FromJSON (TxOutValue era) where
  parseJSON = withObject "TxOutValue" $ \o -> do
    case multiAssetSupportedInEra cardanoEra of
      Left onlyAda -> do
        ll <- o .: "lovelace"
        pure $ TxOutAdaOnly onlyAda $ selectLovelace ll
      Right maSupported -> do
        let l = KeyMap.toList o
        vals <- mapM decodeAssetId l
        pure $ TxOutValue maSupported $ mconcat vals
    where
     decodeAssetId :: (Aeson.Key, Aeson.Value) -> Aeson.Parser Value
     decodeAssetId (polid, Aeson.Object assetNameHm) = do
       let polId = fromString . Text.unpack $ Aeson.toText polid
       aNameQuantity <- decodeAssets assetNameHm
       pure . valueFromList
         $ map (first $ AssetId polId) aNameQuantity

     decodeAssetId ("lovelace", Aeson.Number sci) =
       case toBoundedInteger sci of
         Just (ll :: Word64) ->
           pure $ valueFromList [(AdaAssetId, Quantity $ toInteger ll)]
         Nothing ->
           fail $ "Expected a Bounded number but got: " <> show sci
     decodeAssetId wrong = fail $ "Expected a policy id and a JSON object but got: " <> show wrong

     decodeAssets :: Aeson.Object -> Aeson.Parser [(AssetName, Quantity)]
     decodeAssets assetNameHm =
       let l = KeyMap.toList assetNameHm
       in mapM (\(aName, q) -> (,) <$> parseAssetName aName <*> decodeQuantity q) l

     parseAssetName :: Aeson.Key -> Aeson.Parser AssetName
     parseAssetName aName = runParsecParser assetName (Aeson.toText aName)

     decodeQuantity :: Aeson.Value -> Aeson.Parser Quantity
     decodeQuantity (Aeson.Number sci) =
       case toBoundedInteger sci of
         Just (ll :: Word64) -> return . Quantity $ toInteger ll
         Nothing -> fail $ "Expected a Bounded number but got: " <> show sci
     decodeQuantity wrong = fail $ "Expected aeson Number but got: " <> show wrong

lovelaceToTxOutValue :: IsCardanoEra era => Lovelace -> TxOutValue era
lovelaceToTxOutValue l =
    case multiAssetSupportedInEra cardanoEra of
      Left adaOnly     -> TxOutAdaOnly adaOnly  l
      Right multiAsset -> TxOutValue multiAsset (lovelaceToValue l)

txOutValueToLovelace :: TxOutValue era -> Lovelace
txOutValueToLovelace tv =
  case tv of
    TxOutAdaOnly _ l -> l
    TxOutValue _ v -> selectLovelace v

txOutValueToValue :: TxOutValue era -> Value
txOutValueToValue tv =
  case tv of
    TxOutAdaOnly _ l -> lovelaceToValue l
    TxOutValue _ v -> v

prettyRenderTxOut :: TxOutInAnyEra -> Text
prettyRenderTxOut (TxOutInAnyEra _ (TxOut (AddressInEra _ addr) txOutVal _ _)) =
     serialiseAddress (toAddressAny addr) <> " + "
  <> renderValue (txOutValueToValue txOutVal)

data TxReturnCollateral ctx era where

     TxReturnCollateralNone :: TxReturnCollateral ctx era

     TxReturnCollateral     :: TxTotalAndReturnCollateralSupportedInEra era
                            -> TxOut ctx era
                            -> TxReturnCollateral ctx era

deriving instance Eq   (TxReturnCollateral ctx era)
deriving instance Show (TxReturnCollateral ctx era)

data TxTotalCollateral era where

     TxTotalCollateralNone :: TxTotalCollateral era

     TxTotalCollateral     :: TxTotalAndReturnCollateralSupportedInEra era
                           -> Lovelace
                           -> TxTotalCollateral era

deriving instance Eq   (TxTotalCollateral era)
deriving instance Show (TxTotalCollateral era)

data TxTotalAndReturnCollateralSupportedInEra era where

     TxTotalAndReturnCollateralInBabbageEra :: TxTotalAndReturnCollateralSupportedInEra BabbageEra
     TxTotalAndReturnCollateralInConwayEra :: TxTotalAndReturnCollateralSupportedInEra ConwayEra

deriving instance Eq   (TxTotalAndReturnCollateralSupportedInEra era)
deriving instance Show (TxTotalAndReturnCollateralSupportedInEra era)

totalAndReturnCollateralSupportedInEra
  :: CardanoEra era -> Maybe (TxTotalAndReturnCollateralSupportedInEra era)
totalAndReturnCollateralSupportedInEra ByronEra = Nothing
totalAndReturnCollateralSupportedInEra ShelleyEra = Nothing
totalAndReturnCollateralSupportedInEra AllegraEra = Nothing
totalAndReturnCollateralSupportedInEra MaryEra = Nothing
totalAndReturnCollateralSupportedInEra AlonzoEra = Nothing
totalAndReturnCollateralSupportedInEra BabbageEra = Just TxTotalAndReturnCollateralInBabbageEra
totalAndReturnCollateralSupportedInEra ConwayEra = Just TxTotalAndReturnCollateralInConwayEra

-- ----------------------------------------------------------------------------
-- Transaction output datum (era-dependent)
--

data TxOutDatum ctx era where

     TxOutDatumNone   :: TxOutDatum ctx era

     -- | A transaction output that only specifies the hash of the datum, but
     -- not the full datum value.
     --
     TxOutDatumHash   :: ScriptDataSupportedInEra era
                      -> Hash ScriptData
                      -> TxOutDatum ctx era

     -- | A transaction output that specifies the whole datum value. This can
     -- only be used in the context of the transaction body, and does not occur
     -- in the UTxO. The UTxO only contains the datum hash.
     --
     TxOutDatumInTx'  :: ScriptDataSupportedInEra era
                      -> Hash ScriptData
                      -> HashableScriptData
                      -> TxOutDatum CtxTx era

     -- | A transaction output that specifies the whole datum instead of the
     -- datum hash. Note that the datum map will not be updated with this datum,
     -- it only exists at the transaction output.
     --
     TxOutDatumInline :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
                      -> HashableScriptData
                      -> TxOutDatum ctx era

deriving instance Eq   (TxOutDatum ctx era)
deriving instance Show (TxOutDatum ctx era)

instance EraCast (TxOutDatum ctx)  where
  eraCast toEra v = case v of
    TxOutDatumNone -> pure TxOutDatumNone
    TxOutDatumHash (_ :: ScriptDataSupportedInEra fromEra) hash ->
      case scriptDataSupportedInEra toEra of
        Nothing -> Left $ EraCastError v (cardanoEra @fromEra) toEra
        Just sDatumsSupported ->
          Right $ TxOutDatumHash sDatumsSupported hash
    TxOutDatumInTx' (_ :: ScriptDataSupportedInEra fromEra) scriptData hash ->
      case scriptDataSupportedInEra toEra of
        Nothing -> Left $ EraCastError v (cardanoEra @fromEra) toEra
        Just sDatumsSupported ->
          Right $ TxOutDatumInTx' sDatumsSupported scriptData hash
    TxOutDatumInline (_ :: ReferenceTxInsScriptsInlineDatumsSupportedInEra fromEra) scriptData ->
      case refInsScriptsAndInlineDatsSupportedInEra toEra of
        Nothing -> Left $ EraCastError v (cardanoEra @fromEra) toEra
        Just refInsAndInlineSupported ->
          Right $ TxOutDatumInline refInsAndInlineSupported scriptData

pattern TxOutDatumInTx
  :: ScriptDataSupportedInEra era
  -> HashableScriptData
  -> TxOutDatum CtxTx era
pattern TxOutDatumInTx s d <- TxOutDatumInTx' s _ d
  where
    TxOutDatumInTx s d = TxOutDatumInTx' s (hashScriptDataBytes d) d

{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx', TxOutDatumInline #-}
{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx , TxOutDatumInline #-}

parseHash :: SerialiseAsRawBytes (Hash a) => AsType (Hash a) -> Parsec.Parser (Hash a)
parseHash asType = do
  str <- some Parsec.hexDigit <?> "hash"
  failEitherWith (\e -> "Failed to parse hash: " ++ displayError e) $
    deserialiseFromRawBytesHex asType (BSC.pack str)

-- ----------------------------------------------------------------------------
-- Transaction fees
--

data TxFee era where

     TxFeeImplicit :: TxFeesImplicitInEra era -> TxFee era

     TxFeeExplicit :: TxFeesExplicitInEra era -> Lovelace -> TxFee era

deriving instance Eq   (TxFee era)
deriving instance Show (TxFee era)

defaultTxFee :: forall era. IsCardanoEra era => TxFee era
defaultTxFee = case cardanoEra @era of
  ByronEra -> TxFeeImplicit TxFeesImplicitInByronEra
  ShelleyEra -> TxFeeExplicit TxFeesExplicitInShelleyEra mempty
  AllegraEra -> TxFeeExplicit TxFeesExplicitInAllegraEra mempty
  MaryEra -> TxFeeExplicit TxFeesExplicitInMaryEra mempty
  AlonzoEra -> TxFeeExplicit TxFeesExplicitInAlonzoEra mempty
  BabbageEra -> TxFeeExplicit TxFeesExplicitInBabbageEra mempty
  ConwayEra -> TxFeeExplicit TxFeesExplicitInConwayEra mempty


-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
--
data TxValidityUpperBound era where

     TxValidityNoUpperBound :: ValidityNoUpperBoundSupportedInEra era
                            -> TxValidityUpperBound era

     TxValidityUpperBound   :: ValidityUpperBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityUpperBound era

deriving instance Eq   (TxValidityUpperBound era)
deriving instance Show (TxValidityUpperBound era)

defaultTxValidityUpperBound :: forall era. IsCardanoEra era => TxValidityUpperBound era
defaultTxValidityUpperBound = case cardanoEra @era of
    ByronEra -> TxValidityNoUpperBound ValidityNoUpperBoundInByronEra
    ShelleyEra -> TxValidityUpperBound ValidityUpperBoundInShelleyEra maxBound
    AllegraEra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    MaryEra -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    AlonzoEra -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
    BabbageEra -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
    ConwayEra -> TxValidityNoUpperBound ValidityNoUpperBoundInConwayEra

data TxValidityLowerBound era where

     TxValidityNoLowerBound :: TxValidityLowerBound era

     TxValidityLowerBound   :: ValidityLowerBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityLowerBound era

deriving instance Eq   (TxValidityLowerBound era)
deriving instance Show (TxValidityLowerBound era)

-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxMetadataInEra era where

     TxMetadataNone  :: TxMetadataInEra era

     TxMetadataInEra :: TxMetadataSupportedInEra era
                     -> TxMetadata
                     -> TxMetadataInEra era

deriving instance Eq   (TxMetadataInEra era)
deriving instance Show (TxMetadataInEra era)

-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where

     TxAuxScriptsNone :: TxAuxScripts era

     TxAuxScripts     :: AuxScriptsSupportedInEra era
                      -> [ScriptInEra era]
                      -> TxAuxScripts era

deriving instance Eq   (TxAuxScripts era)
deriving instance Show (TxAuxScripts era)

-- ----------------------------------------------------------------------------
-- Optionally required signatures (era-dependent)
--

data TxExtraKeyWitnesses era where

  TxExtraKeyWitnessesNone :: TxExtraKeyWitnesses era

  TxExtraKeyWitnesses     :: TxExtraKeyWitnessesSupportedInEra era
                          -> [Hash PaymentKey]
                          -> TxExtraKeyWitnesses era

deriving instance Eq   (TxExtraKeyWitnesses era)
deriving instance Show (TxExtraKeyWitnesses era)

-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals build era where

     TxWithdrawalsNone :: TxWithdrawals build era

     TxWithdrawals     :: WithdrawalsSupportedInEra era
                       -> [(StakeAddress, Lovelace,
                            BuildTxWith build (Witness WitCtxStake era))]
                       -> TxWithdrawals build era

deriving instance Eq   (TxWithdrawals build era)
deriving instance Show (TxWithdrawals build era)

-- ----------------------------------------------------------------------------
-- Certificates within transactions (era-dependent)
--

data TxCertificates build era where

     TxCertificatesNone :: TxCertificates build era

     TxCertificates     :: CertificatesSupportedInEra era
                        -> [Certificate]
                        -> BuildTxWith build
                             (Map StakeCredential (Witness WitCtxStake era))
                        -> TxCertificates build era

deriving instance Eq   (TxCertificates build era)
deriving instance Show (TxCertificates build era)

-- ----------------------------------------------------------------------------
-- Transaction update proposal (era-dependent)
--

data TxUpdateProposal era where

     TxUpdateProposalNone :: TxUpdateProposal era

     TxUpdateProposal     :: UpdateProposalSupportedInEra era
                          -> UpdateProposal
                          -> TxUpdateProposal era

deriving instance Eq   (TxUpdateProposal era)
deriving instance Show (TxUpdateProposal era)

-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue build era where

     TxMintNone  :: TxMintValue build era

     TxMintValue :: MultiAssetSupportedInEra era
                 -> Value
                 -> BuildTxWith build
                      (Map PolicyId (ScriptWitness WitCtxMint era))
                 -> TxMintValue build era

deriving instance Eq   (TxMintValue build era)
deriving instance Show (TxMintValue build era)

-- ----------------------------------------------------------------------------
-- Transaction body content
--

data TxBodyContent build era =
     TxBodyContent {
       txIns              :: TxIns build era,
       txInsCollateral    :: TxInsCollateral era,
       txInsReference     :: TxInsReference build era,
       txOuts             :: [TxOut CtxTx era],
       txTotalCollateral  :: TxTotalCollateral era,
       txReturnCollateral :: TxReturnCollateral CtxTx era,
       txFee              :: TxFee era,
       txValidityRange    :: (TxValidityLowerBound era,
                              TxValidityUpperBound era),
       txMetadata         :: TxMetadataInEra era,
       txAuxScripts       :: TxAuxScripts era,
       txExtraKeyWits     :: TxExtraKeyWitnesses era,
       txProtocolParams   :: BuildTxWith build (Maybe ProtocolParameters),
       txWithdrawals      :: TxWithdrawals  build era,
       txCertificates     :: TxCertificates build era,
       txUpdateProposal   :: TxUpdateProposal era,
       txMintValue        :: TxMintValue    build era,
       txScriptValidity   :: TxScriptValidity era
     }
     deriving (Eq, Show)

defaultTxBodyContent :: IsCardanoEra era => TxBodyContent BuildTx era
defaultTxBodyContent = TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txInsReference = TxInsReferenceNone
    , txOuts = []
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee = defaultTxFee
    , txValidityRange = (TxValidityNoLowerBound, defaultTxValidityUpperBound)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }

setTxIns :: TxIns build era -> TxBodyContent build era -> TxBodyContent build era
setTxIns v txBodyContent = txBodyContent { txIns = v }

modTxIns :: (TxIns build era -> TxIns build era) -> TxBodyContent build era -> TxBodyContent build era
modTxIns f txBodyContent = txBodyContent { txIns = f (txIns txBodyContent) }

addTxIn :: (TxIn, BuildTxWith build (Witness WitCtxTxIn era)) -> TxBodyContent build era -> TxBodyContent build era
addTxIn txIn = modTxIns (txIn:)

setTxInsCollateral :: TxInsCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxInsCollateral v txBodyContent = txBodyContent { txInsCollateral = v }

setTxInsReference :: TxInsReference build era -> TxBodyContent build era -> TxBodyContent build era
setTxInsReference v txBodyContent = txBodyContent { txInsReference = v }

setTxOuts :: [TxOut CtxTx era] -> TxBodyContent build era -> TxBodyContent build era
setTxOuts v txBodyContent = txBodyContent { txOuts = v }

modTxOuts :: ([TxOut CtxTx era] -> [TxOut CtxTx era]) -> TxBodyContent build era -> TxBodyContent build era
modTxOuts f txBodyContent = txBodyContent { txOuts = f (txOuts txBodyContent) }

addTxOut :: TxOut CtxTx era -> TxBodyContent build era -> TxBodyContent build era
addTxOut txOut = modTxOuts (txOut:)

setTxTotalCollateral :: TxTotalCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxTotalCollateral v txBodyContent = txBodyContent { txTotalCollateral = v }

setTxReturnCollateral :: TxReturnCollateral CtxTx era -> TxBodyContent build era -> TxBodyContent build era
setTxReturnCollateral v txBodyContent = txBodyContent { txReturnCollateral = v }

setTxFee :: TxFee era -> TxBodyContent build era -> TxBodyContent build era
setTxFee v txBodyContent = txBodyContent { txFee = v }

setTxValidityRange :: (TxValidityLowerBound era, TxValidityUpperBound era) -> TxBodyContent build era -> TxBodyContent build era
setTxValidityRange v txBodyContent = txBodyContent { txValidityRange = v }

setTxMetadata :: TxMetadataInEra era -> TxBodyContent build era -> TxBodyContent build era
setTxMetadata v txBodyContent = txBodyContent { txMetadata = v }

setTxAuxScripts :: TxAuxScripts era -> TxBodyContent build era -> TxBodyContent build era
setTxAuxScripts v txBodyContent = txBodyContent { txAuxScripts = v }

setTxExtraKeyWits :: TxExtraKeyWitnesses era -> TxBodyContent build era -> TxBodyContent build era
setTxExtraKeyWits v txBodyContent = txBodyContent { txExtraKeyWits = v }

setTxProtocolParams :: BuildTxWith build (Maybe ProtocolParameters) -> TxBodyContent build era -> TxBodyContent build era
setTxProtocolParams v txBodyContent = txBodyContent { txProtocolParams = v }

setTxWithdrawals :: TxWithdrawals build era -> TxBodyContent build era -> TxBodyContent build era
setTxWithdrawals v txBodyContent = txBodyContent { txWithdrawals = v }

setTxCertificates :: TxCertificates build era -> TxBodyContent build era -> TxBodyContent build era
setTxCertificates v txBodyContent = txBodyContent { txCertificates = v }

setTxUpdateProposal :: TxUpdateProposal era -> TxBodyContent build era -> TxBodyContent build era
setTxUpdateProposal v txBodyContent = txBodyContent { txUpdateProposal = v }

setTxMintValue :: TxMintValue build era -> TxBodyContent build era -> TxBodyContent build era
setTxMintValue v txBodyContent = txBodyContent { txMintValue = v }

setTxScriptValidity :: TxScriptValidity era -> TxBodyContent build era -> TxBodyContent build era
setTxScriptValidity v txBodyContent = txBodyContent { txScriptValidity = v }

-- ----------------------------------------------------------------------------
-- Transaction bodies
--

data TxBody era where

     ByronTxBody
       :: Annotated Byron.Tx ByteString
       -> TxBody ByronEra

     ShelleyTxBody
       :: ShelleyBasedEra era
       -> Ledger.TxBody (ShelleyLedgerEra era)

          -- We include the scripts along with the tx body, rather than the
          -- witnesses set, since they need to be known when building the body.
       -> [Ledger.Script (ShelleyLedgerEra era)]

          -- The info for each use of each script: the script input data, both
          -- the UTxO input data (called the "datum") and the supplied input
          -- data (called the "redeemer") and the execution units.
       -> TxBodyScriptData era

          -- The 'L.TxAuxData' consists of one or several things,
          -- depending on era:
          -- + transaction metadata  (in Shelley and later)
          -- + auxiliary scripts     (in Allegra and later)
          -- Note that there is no auxiliary script data as such, because the
          -- extra script data has to be passed to scripts and hence is needed
          -- for validation. It is thus part of the witness data, not the
          -- auxiliary data.
       -> Maybe (L.TxAuxData (ShelleyLedgerEra era))

       -> TxScriptValidity era -- ^ Mark script as expected to pass or fail validation

       -> TxBody era
     -- The 'ShelleyBasedEra' GADT tells us what era we are in.
     -- The 'ShelleyLedgerEra' type family maps that to the era type from the
     -- ledger lib. The 'Ledger.TxBody' type family maps that to a specific
     -- tx body type, which is different for each Shelley-based era.


data TxBodyScriptData era where
     TxBodyNoScriptData :: TxBodyScriptData era
     TxBodyScriptData   :: ScriptDataSupportedInEra era
                        -> Alonzo.TxDats (ShelleyLedgerEra era)
                        -> Alonzo.Redeemers (ShelleyLedgerEra era)
                        -> TxBodyScriptData era

deriving instance Eq   (TxBodyScriptData era)
deriving instance L.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto => Show (TxBodyScriptData era)


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Eq (TxBody era) where
    (==) (ByronTxBody txbodyA)
         (ByronTxBody txbodyB) = txbodyA == txbodyB

    (==) (ShelleyTxBody era txbodyA txscriptsA redeemersA txmetadataA scriptValidityA)
         (ShelleyTxBody _   txbodyB txscriptsB redeemersB txmetadataB scriptValidityB) =
         case era of
           ShelleyBasedEraShelley -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraAllegra -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraMary    -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraAlonzo  -> txbodyA         == txbodyB
                                  && txscriptsA      == txscriptsB
                                  && redeemersA      == redeemersB
                                  && txmetadataA     == txmetadataB
                                  && scriptValidityA == scriptValidityB

           ShelleyBasedEraBabbage -> txbodyA         == txbodyB
                                  && txscriptsA      == txscriptsB
                                  && redeemersA      == redeemersB
                                  && txmetadataA     == txmetadataB
                                  && scriptValidityA == scriptValidityB

           ShelleyBasedEraConway  -> txbodyA         == txbodyB
                                  && txscriptsA      == txscriptsB
                                  && redeemersA      == redeemersB
                                  && txmetadataA     == txmetadataB
                                  && scriptValidityA == scriptValidityB

    (==) ByronTxBody{} (ShelleyTxBody era _ _ _ _ _) = case era of {}
    (==) (ShelleyTxBody era _ _ _ _ _) ByronTxBody{} = case era of {}


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Show (TxBody era) where
    showsPrec p (ByronTxBody txbody) =
      showParen (p >= 11)
        ( showString "ByronTxBody "
        . showsPrec 11 txbody
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraShelley
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraShelley "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAllegra
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAllegra "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraMary
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraMary "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAlonzo
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAlonzo "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraBabbage
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraBabbage "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraConway
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraConway "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )


instance HasTypeProxy era => HasTypeProxy (TxBody era) where
    data AsType (TxBody era) = AsTxBody (AsType era)
    proxyToAsType _ = AsTxBody (proxyToAsType (Proxy :: Proxy era))

pattern AsByronTxBody :: AsType (TxBody ByronEra)
pattern AsByronTxBody   = AsTxBody AsByronEra
{-# COMPLETE AsByronTxBody #-}

pattern AsShelleyTxBody :: AsType (TxBody ShelleyEra)
pattern AsShelleyTxBody = AsTxBody AsShelleyEra
{-# COMPLETE AsShelleyTxBody #-}

pattern AsMaryTxBody :: AsType (TxBody MaryEra)
pattern AsMaryTxBody = AsTxBody AsMaryEra
{-# COMPLETE AsMaryTxBody #-}

instance IsCardanoEra era => SerialiseAsCBOR (TxBody era) where

    serialiseToCBOR (ByronTxBody txbody) =
      recoverBytes txbody

    serialiseToCBOR (ShelleyTxBody era txbody txscripts redeemers txmetadata scriptValidity) =
      case era of
        -- Use the same serialisation impl, but at different types:
        ShelleyBasedEraShelley -> serialiseShelleyBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        ShelleyBasedEraAllegra -> serialiseShelleyBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        ShelleyBasedEraMary    -> serialiseShelleyBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        ShelleyBasedEraAlonzo  -> serialiseShelleyBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        ShelleyBasedEraBabbage -> serialiseShelleyBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        ShelleyBasedEraConway -> serialiseShelleyBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              CBOR.byronProtVer
              "Byron TxBody"
              CBOR.decCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same deserialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraShelley bs
        AllegraEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraAllegra bs
        MaryEra    -> deserialiseShelleyBasedTxBody ShelleyBasedEraMary    bs
        AlonzoEra  -> deserialiseShelleyBasedTxBody ShelleyBasedEraAlonzo  bs
        BabbageEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraBabbage bs
        ConwayEra  -> deserialiseShelleyBasedTxBody ShelleyBasedEraConway  bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
serialiseShelleyBasedTxBody
  :: forall era ledgerera.
     L.Era ledgerera
  => ShelleyLedgerEra era ~ ledgerera
  => CBOR.EncCBOR (Ledger.TxBody ledgerera)
  => CBOR.EncCBOR (Ledger.Script ledgerera)
  => CBOR.EncCBOR (Alonzo.TxDats ledgerera)
  => CBOR.EncCBOR (Alonzo.Redeemers ledgerera)
  => CBOR.EncCBOR (L.TxAuxData ledgerera)
  => ShelleyBasedEra era
  -> Ledger.TxBody ledgerera
  -> [Ledger.Script ledgerera]
  -> TxBodyScriptData era
  -> Maybe (L.TxAuxData ledgerera)
  -> TxScriptValidity era -- ^ Mark script as expected to pass or fail validation
  -> ByteString
serialiseShelleyBasedTxBody era txbody txscripts
                            TxBodyNoScriptData txmetadata scriptValidity =
    -- Backwards compat for pre-Alonzo era tx body files
    case era of
      ShelleyBasedEraShelley -> preAlonzo (L.eraProtVerLow @L.Shelley)
      ShelleyBasedEraAllegra -> preAlonzo (L.eraProtVerLow @L.Allegra)
      ShelleyBasedEraMary -> preAlonzo (L.eraProtVerLow @L.Mary)
      ShelleyBasedEraAlonzo ->
        CBOR.serialize' (L.eraProtVerLow @L.Alonzo)
          $ CBOR.encodeListLen 4
         <> CBOR.encCBOR txbody
         <> CBOR.encCBOR txscripts
         <> CBOR.encCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata
      ShelleyBasedEraBabbage ->
        CBOR.serialize' (L.eraProtVerLow @L.Babbage)
          $ CBOR.encodeListLen 4
         <> CBOR.encCBOR txbody
         <> CBOR.encCBOR txscripts
         <> CBOR.encCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata
      ShelleyBasedEraConway ->
        CBOR.serialize' (L.eraProtVerLow @L.Babbage)
          $ CBOR.encodeListLen 4
         <> CBOR.encCBOR txbody
         <> CBOR.encCBOR txscripts
         <> CBOR.encCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata
 where
   preAlonzo v = CBOR.serialize' v
                 $ CBOR.encodeListLen 3
                <> CBOR.encCBOR txbody
                <> CBOR.encCBOR txscripts
                <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata

serialiseShelleyBasedTxBody _era txbody txscripts
                            (TxBodyScriptData _ datums redeemers)
                            txmetadata txBodyScriptValidity =
    CBOR.serialize' (L.eraProtVerLow @ledgerera) $
        CBOR.encodeListLen 6
     <> CBOR.encCBOR txbody
     <> CBOR.encCBOR txscripts
     <> CBOR.encCBOR datums
     <> CBOR.encCBOR redeemers
     <> CBOR.encCBOR (txScriptValidityToScriptValidity txBodyScriptValidity)
     <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata

deserialiseShelleyBasedTxBody
  :: forall era ledgerera.
     L.Era ledgerera
  => ShelleyLedgerEra era ~ ledgerera
  => CBOR.DecCBOR (CBOR.Annotator (Ledger.TxBody ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (Ledger.Script ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (Alonzo.TxDats ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (Alonzo.Redeemers ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (L.TxAuxData ledgerera))
  => ShelleyBasedEra era
  -> ByteString
  -> Either CBOR.DecoderError (TxBody era)
deserialiseShelleyBasedTxBody era bs =
    CBOR.decodeFullAnnotator
      (L.eraProtVerLow @ledgerera)
      "Shelley TxBody"
      decodeAnnotatedTuple
      (LBS.fromStrict bs)
  where
    decodeAnnotatedTuple :: CBOR.Decoder s (CBOR.Annotator (TxBody era))
    decodeAnnotatedTuple = do
      len <- CBOR.decodeListLen

      case len of
        -- Backwards compat for pre-Alonzo era tx body files
        2 -> do
          txbody     <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              [] -- scripts
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return TxScriptValidityNone))
        3 -> do
          txbody     <- CBOR.decCBOR
          txscripts  <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return TxScriptValidityNone))
        4 -> do
          sValiditySupported <-
            case txScriptValiditySupportedInShelleyBasedEra era of
              Nothing -> fail $ mconcat
                [ "deserialiseShelleyBasedTxBody: Expected an era that supports the "
                , "script validity flag but got: "
                , show era
                ]
              Just supported -> return supported

          txbody     <- CBOR.decCBOR
          txscripts  <- CBOR.decCBOR
          scriptValidity <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return $ TxScriptValidity sValiditySupported scriptValidity))
        6 -> do
          sDataSupported <-
            case scriptDataSupportedInEra (shelleyBasedToCardanoEra era) of
              Nothing -> fail $ mconcat
                [ "deserialiseShelleyBasedTxBody: Expected an era that supports script"
                , " data but got: "
                , show era
                ]
              Just supported -> return supported

          sValiditySupported <-
            case txScriptValiditySupportedInShelleyBasedEra era of
              Nothing -> fail $ mconcat
                [ "deserialiseShelleyBasedTxBody: Expected an era that supports the "
                , "script validity flag but got: "
                , show era
                ]
              Just supported -> return supported

          txbody    <- CBOR.decCBOR
          txscripts <- CBOR.decCBOR
          datums    <- CBOR.decCBOR
          redeemers <- CBOR.decCBOR
          scriptValidity <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR

          let txscriptdata = CBOR.Annotator $ \fbs ->
                               TxBodyScriptData sDataSupported
                                 (flip CBOR.runAnnotator fbs datums)
                                 (flip CBOR.runAnnotator fbs redeemers)

          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs txscriptdata)
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return $ TxScriptValidity sValiditySupported scriptValidity))
        _ -> fail $ "expected tx body tuple of size 2, 3, 4 or 6, got " <> show len

instance IsCardanoEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxUnsignedByron"
        ShelleyEra -> "TxUnsignedShelley"
        AllegraEra -> "TxBodyAllegra"
        MaryEra    -> "TxBodyMary"
        AlonzoEra  -> "TxBodyAlonzo"
        BabbageEra -> "TxBodyBabbage"
        ConwayEra  -> "TxBodyConway"

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: forall era. TxBody era -> TxId
getTxId (ByronTxBody tx) =
    TxId
  . fromMaybe impossible
  . Crypto.hashFromBytesShort
  . Byron.abstractHashToShort
  . Byron.hashDecoded
  $ tx
  where
    impossible =
      error "getTxId: byron and shelley hash sizes do not match"

getTxId (ShelleyTxBody era tx _ _ _ _) =
  obtainConstraints era $ getTxIdShelley era tx
 where
  obtainConstraints
    :: ShelleyBasedEra era
    -> ((Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto, Ledger.EraTxBody (ShelleyLedgerEra era)) => a)
    -> a
  obtainConstraints ShelleyBasedEraShelley f = f
  obtainConstraints ShelleyBasedEraAllegra f = f
  obtainConstraints ShelleyBasedEraMary    f = f
  obtainConstraints ShelleyBasedEraAlonzo  f = f
  obtainConstraints ShelleyBasedEraBabbage f = f
  obtainConstraints ShelleyBasedEraConway  f = f

getTxIdShelley
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.EraTxBody (ShelleyLedgerEra era)
  => ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxId
getTxIdShelley _ tx =
    TxId
  . Crypto.castHash
  . (\(Ledger.TxId txhash) -> SafeHash.extractHash txhash)
  $ Ledger.txid tx

-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError =
       TxBodyEmptyTxIns
     | TxBodyEmptyTxInsCollateral
     | TxBodyEmptyTxOuts
     | TxBodyOutputNegative Quantity TxOutInAnyEra
     | TxBodyOutputOverflow Quantity TxOutInAnyEra
     | TxBodyMetadataError [(Word64, TxMetadataRangeError)]
     | TxBodyMintAdaError
     | TxBodyMissingProtocolParams
     | TxBodyInIxOverflow TxIn
     deriving (Eq, Show)

instance Error TxBodyError where
    displayError TxBodyEmptyTxIns  = "Transaction body has no inputs"
    displayError TxBodyEmptyTxInsCollateral =
      "Transaction body has no collateral inputs, but uses Plutus scripts"
    displayError TxBodyEmptyTxOuts = "Transaction body has no outputs"
    displayError (TxBodyOutputNegative (Quantity q) txout) =
      "Negative quantity (" ++ show q ++ ") in transaction output: " ++
      show txout
    displayError (TxBodyOutputOverflow (Quantity q) txout) =
      "Quantity too large (" ++ show q ++ " >= 2^64) in transaction output: " ++
      show txout
    displayError (TxBodyMetadataError [(k, err)]) =
      "Error in metadata entry " ++ show k ++ ": " ++ displayError err
    displayError (TxBodyMetadataError errs) =
      "Error in metadata entries: " ++
      intercalate "; "
        [ show k ++ ": " ++ displayError err
        | (k, err) <- errs ]
    displayError TxBodyMintAdaError =
      "Transaction cannot mint ada, only non-ada assets"
    displayError TxBodyMissingProtocolParams =
      "Transaction uses Plutus scripts but does not provide the protocol " ++
      "parameters to hash"
    displayError (TxBodyInIxOverflow txin) =
      "Transaction input index is too big, " ++
      "acceptable value is up to 2^32-1, " ++
      "in input " ++ show txin

createTransactionBody
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> TxBody era
createTransactionBody era txBodyContent =
  let apiTxOuts = txOuts txBodyContent
      apiScriptWitnesses = collectTxBodyScriptWitnesses txBodyContent
      apiScriptValidity = txScriptValidity txBodyContent
      apiMintValue = txMintValue txBodyContent
      apiProtocolParameters = txProtocolParams txBodyContent
      apiCollateralTxIns = txInsCollateral txBodyContent
      apiReferenceInputs = txInsReference txBodyContent
      apiExtraKeyWitnesses = txExtraKeyWits txBodyContent
      apiReturnCollateral = txReturnCollateral txBodyContent
      apiTotalCollateral = txTotalCollateral txBodyContent

      -- Ledger types
      collTxIns = convCollateralTxIns apiCollateralTxIns
      refTxIns = convReferenceInputs apiReferenceInputs
      returnCollateral = convReturnCollateral era apiReturnCollateral
      totalCollateral = convTotalCollateral apiTotalCollateral
      certs = convCertificates $ txCertificates txBodyContent
      conwayCerts = convConwayCertificates $ txCertificates txBodyContent
      txAuxData = toAuxiliaryData era (txMetadata txBodyContent) (txAuxScripts txBodyContent)
      scripts = convScripts apiScriptWitnesses
      validityInterval = convValidityInterval $ txValidityRange txBodyContent
      languages = convLanguages apiScriptWitnesses

      mkTxBody :: ( L.EraTxBody (ShelleyLedgerEra era)
                  , L.EraTxAuxData (ShelleyLedgerEra era)
                  , L.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto)
               => ShelleyBasedEra era
               -> TxBodyContent BuildTx era
               -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
               -> L.TxBody (ShelleyLedgerEra era)
      mkTxBody era' bc aux =
        mkCommonTxBody
          era'
          (txIns bc)
          (txOuts bc)
          (txFee bc)
          (txWithdrawals bc)
          aux

  in case era of
       ShelleyBasedEraShelley ->
        let (_, upperBound) = txValidityRange txBodyContent
            ttl = case upperBound of
                    TxValidityNoUpperBound era' -> case era' of {}
                    TxValidityUpperBound _ ttl'  -> ttl'
            update = convTxUpdateProposal era (txUpdateProposal txBodyContent)
            ledgerTxBody = mkTxBody ShelleyBasedEraShelley txBodyContent txAuxData
                           & L.certsTxBodyL  .~ certs
                           & L.ttlTxBodyL    .~ ttl
                           & L.updateTxBodyL .~ update

            sData = convScriptData (shelleyBasedToCardanoEra era) apiTxOuts apiScriptWitnesses

        in ShelleyTxBody era
              ledgerTxBody
              scripts
              sData
              txAuxData
              apiScriptValidity

       ShelleyBasedEraAllegra ->
        let update = convTxUpdateProposal era (txUpdateProposal txBodyContent)
            ledgerTxBody = mkTxBody ShelleyBasedEraAllegra txBodyContent txAuxData
                           & L.certsTxBodyL  .~ certs
                           & L.updateTxBodyL .~ update
                           & L.vldtTxBodyL   .~ validityInterval
        in ShelleyTxBody era
              ledgerTxBody
              scripts
              (convScriptData (shelleyBasedToCardanoEra era) apiTxOuts apiScriptWitnesses)
              txAuxData
              apiScriptValidity

       ShelleyBasedEraMary ->
        let update = convTxUpdateProposal era (txUpdateProposal txBodyContent)
            ledgerTxBody = mkTxBody ShelleyBasedEraMary txBodyContent txAuxData
                           & L.certsTxBodyL  .~ certs
                           & L.updateTxBodyL .~ update
                           & L.vldtTxBodyL   .~ validityInterval
                           & L.mintTxBodyL   .~ convMintValue apiMintValue
        in ShelleyTxBody era
              ledgerTxBody
              scripts
              (convScriptData (shelleyBasedToCardanoEra era) apiTxOuts apiScriptWitnesses)
              txAuxData
              apiScriptValidity
       ShelleyBasedEraAlonzo ->
        let sData = convScriptData (shelleyBasedToCardanoEra era) apiTxOuts apiScriptWitnesses
            scriptIntegrityHash =
              case sData of
                TxBodyNoScriptData -> SNothing
                TxBodyScriptData _ datums redeemers ->
                  convPParamsToScriptIntegrityHash
                    era
                    apiProtocolParameters
                    redeemers
                    datums
                    languages

            update = convTxUpdateProposal era (txUpdateProposal txBodyContent)
            ledgerTxBody = mkTxBody ShelleyBasedEraAlonzo txBodyContent txAuxData
                           & L.certsTxBodyL               .~ certs
                           & L.updateTxBodyL              .~ update
                           & L.vldtTxBodyL                .~ validityInterval
                           & L.collateralInputsTxBodyL    .~ collTxIns
                           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses
                                                               apiExtraKeyWitnesses
                           & L.mintTxBodyL                .~ convMintValue apiMintValue
                           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
                           -- TODO: NetworkId for hardware wallets. We don't always want this
                           -- & L.networkIdTxBodyL .~ ...
        in ShelleyTxBody era
              ledgerTxBody
              scripts
              (convScriptData (shelleyBasedToCardanoEra era) apiTxOuts apiScriptWitnesses)
              txAuxData
              apiScriptValidity
       ShelleyBasedEraBabbage ->
        let sData = convScriptData (shelleyBasedToCardanoEra era) apiTxOuts apiScriptWitnesses

            scriptIntegrityHash =
                    case sData of
                      TxBodyNoScriptData -> SNothing
                      TxBodyScriptData _sDataSupported datums redeemers ->
                        getLedgerEraConstraint era
                          $ convPParamsToScriptIntegrityHash
                              era
                              apiProtocolParameters
                              redeemers
                              datums
                              languages
            update = convTxUpdateProposal era (txUpdateProposal txBodyContent)
            ledgerTxBody = mkTxBody ShelleyBasedEraBabbage txBodyContent txAuxData
                           & L.certsTxBodyL               .~ certs
                           & L.updateTxBodyL              .~ update
                           & L.vldtTxBodyL                .~ validityInterval
                           & L.collateralInputsTxBodyL    .~ collTxIns
                           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses
                                                               apiExtraKeyWitnesses
                           & L.mintTxBodyL                .~ convMintValue apiMintValue
                           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
                           & L.referenceInputsTxBodyL     .~ refTxIns
                           & L.collateralReturnTxBodyL    .~ returnCollateral
                           & L.totalCollateralTxBodyL     .~ totalCollateral
                           -- TODO: NetworkId for hardware wallets. We don't always want this
                           -- & L.networkIdTxBodyL .~ ...
        in ShelleyTxBody era
              ledgerTxBody
              scripts
              sData
              txAuxData
              apiScriptValidity

       ShelleyBasedEraConway ->
        let sData = convScriptData (shelleyBasedToCardanoEra era) apiTxOuts apiScriptWitnesses

            scriptIntegrityHash =
                    case sData of
                      TxBodyNoScriptData -> SNothing
                      TxBodyScriptData _sDataSupported datums redeemers ->
                        getLedgerEraConstraint era
                          $ convPParamsToScriptIntegrityHash
                              era
                              apiProtocolParameters
                              redeemers
                              datums
                              languages
            ledgerTxBody = mkTxBody ShelleyBasedEraConway txBodyContent txAuxData
                           & L.conwayCertsTxBodyL         .~ conwayCerts
                           & L.vldtTxBodyL                .~ validityInterval
                           & L.collateralInputsTxBodyL    .~ collTxIns
                           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses
                                                               apiExtraKeyWitnesses
                           & L.mintTxBodyL                .~ convMintValue apiMintValue
                           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
                           & L.referenceInputsTxBodyL     .~ refTxIns
                           & L.collateralReturnTxBodyL    .~ returnCollateral
                           & L.totalCollateralTxBodyL     .~ totalCollateral
                           -- TODO: NetworkId for hardware wallets. We don't always want this
                           -- & L.networkIdTxBodyL .~ ...
        in ShelleyTxBody era
              ledgerTxBody
              scripts
              sData
              txAuxData
              apiScriptValidity

validateTxBodyContent
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError ()
validateTxBodyContent era txBodContent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txOuts,
                             txProtocolParams,
                             txMintValue,
                             txMetadata} =
  let witnesses = collectTxBodyScriptWitnesses txBodContent
      languages = Set.fromList
                    [ toAlonzoLanguage (AnyPlutusScriptVersion v)
                    | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
                    ]
  in case era of
       ShelleyBasedEraShelley -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts era txOuts
         validateMetadata txMetadata
       ShelleyBasedEraAllegra -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts era txOuts
         validateMetadata txMetadata
       ShelleyBasedEraMary -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts era txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
       ShelleyBasedEraAlonzo -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts era txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
         validateTxInsCollateral txInsCollateral languages
         validateProtocolParameters txProtocolParams languages
       ShelleyBasedEraBabbage -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts era txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
         validateTxInsCollateral txInsCollateral languages
         validateProtocolParameters txProtocolParams languages
       ShelleyBasedEraConway -> do
         validateTxIns txIns
         validateTxOuts era txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
         validateTxInsCollateral txInsCollateral languages
         validateProtocolParameters txProtocolParams languages

validateMetadata :: TxMetadataInEra era -> Either TxBodyError ()
validateMetadata txMetadata =
  case txMetadata of
    TxMetadataNone      -> return ()
    TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

validateProtocolParameters
  :: BuildTxWith BuildTx (Maybe ProtocolParameters)
  -> Set Alonzo.Language
  -> Either TxBodyError ()
validateProtocolParameters txProtocolParams languages =
  case txProtocolParams of
    BuildTxWith Nothing | not (Set.null languages)
      -> Left TxBodyMissingProtocolParams
    _ -> return () --TODO alonzo: validate protocol params for the Alonzo era.
                     --             All the necessary params must be provided.


validateTxIns :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))] -> Either TxBodyError ()
validateTxIns txIns =
  sequence_ [ inputIndexDoesNotExceedMax txIns
            , txBodyContentHasTxIns txIns
            ]

validateTxInsCollateral
  :: TxInsCollateral era -> Set Alonzo.Language -> Either TxBodyError ()
validateTxInsCollateral txInsCollateral languages =
  case txInsCollateral of
    TxInsCollateralNone ->
      unless (Set.null languages) (Left TxBodyEmptyTxInsCollateral)
    TxInsCollateral _ collateralTxIns ->
      guardShelleyTxInsOverflow collateralTxIns

validateTxOuts :: ShelleyBasedEra era -> [TxOut CtxTx era] -> Either TxBodyError ()
validateTxOuts era txOuts =
  let cEra = shelleyBasedToCardanoEra era
  in sequence_ [ do getIsCardanoEraConstraint cEra $ positiveOutput (txOutValueToValue v) txout
                    getIsCardanoEraConstraint cEra $ outputDoesNotExceedMax (txOutValueToValue v) txout
               | txout@(TxOut _ v _ _) <- txOuts
               ]

validateMintValue :: TxMintValue build era -> Either TxBodyError ()
validateMintValue txMintValue =
  case txMintValue of
    TxMintNone        -> return ()
    TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError


inputIndexDoesNotExceedMax :: [(TxIn, a)] -> Either TxBodyError ()
inputIndexDoesNotExceedMax txIns =
   for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
                guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

outputDoesNotExceedMax
  :: IsCardanoEra era => Value -> TxOut CtxTx era -> Either TxBodyError ()
outputDoesNotExceedMax v txout =
  case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
    []  -> Right ()
    q:_ -> Left (TxBodyOutputOverflow q (txOutInAnyEra txout))

positiveOutput
  :: IsCardanoEra era
  => Value -> TxOut CtxTx era -> Either TxBodyError ()
positiveOutput v txout =
    case [ q | (_, q) <- valueToList v, q < 0 ] of
      []  -> Right ()
      q:_ -> Left (TxBodyOutputNegative q (txOutInAnyEra txout))

txBodyContentHasTxIns :: TxIns BuildTx era -> Either TxBodyError ()
txBodyContentHasTxIns txIns = guard (not (null txIns)) ?! TxBodyEmptyTxIns

maxShelleyTxInIx :: Word
maxShelleyTxInIx = fromIntegral $ maxBound @Word16

maxTxOut :: Quantity
maxTxOut = fromIntegral (maxBound :: Word64)

createAndValidateTransactionBody
  :: forall era.
     IsCardanoEra era
  => TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
createAndValidateTransactionBody =
    case cardanoEraStyle (cardanoEra :: CardanoEra era) of
      LegacyByronEra      -> makeByronTransactionBody
      ShelleyBasedEra era -> makeShelleyTransactionBody era

{-# DEPRECATED makeTransactionBody "Use createAndValidateTransactionBody." #-}
makeTransactionBody :: forall era.
     IsCardanoEra era
  => TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeTransactionBody = createAndValidateTransactionBody


pattern TxBody :: TxBodyContent ViewTx era -> TxBody era
pattern TxBody txbodycontent <- (getTxBodyContent -> txbodycontent)
{-# COMPLETE TxBody #-}

getTxBodyContent :: TxBody era -> TxBodyContent ViewTx era
getTxBodyContent (ByronTxBody body) = getByronTxBodyContent body
getTxBodyContent (ShelleyTxBody era body _scripts scriptdata mAux scriptValidity) =
    fromLedgerTxBody era scriptValidity body scriptdata mAux

fromLedgerTxBody
  :: ShelleyBasedEra era
  -> TxScriptValidity era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxBodyScriptData era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> TxBodyContent ViewTx era
fromLedgerTxBody era scriptValidity body scriptdata mAux =
    TxBodyContent
      { txIns              = fromLedgerTxIns               era body
      , txInsCollateral    = fromLedgerTxInsCollateral     era body
      , txInsReference     = fromLedgerTxInsReference      era body
      , txOuts             = fromLedgerTxOuts              era body scriptdata
      , txTotalCollateral  = fromLedgerTxTotalCollateral   era body
      , txReturnCollateral = fromLedgerTxReturnCollateral  era body
      , txFee              = fromLedgerTxFee               era body
      , txValidityRange    = fromLedgerTxValidityRange     era body
      , txWithdrawals      = fromLedgerTxWithdrawals       era body
      , txCertificates     = fromLedgerTxCertificates      era body
      , txUpdateProposal   = fromLedgerTxUpdateProposal    era body
      , txMintValue        = fromLedgerTxMintValue         era body
      , txExtraKeyWits     = fromLedgerTxExtraKeyWitnesses era body
      , txProtocolParams   = ViewTx
      , txMetadata
      , txAuxScripts
      , txScriptValidity   = scriptValidity
      }
  where
    (txMetadata, txAuxScripts) = fromLedgerTxAuxiliaryData era mAux


fromLedgerTxIns
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> [(TxIn,BuildTxWith ViewTx (Witness WitCtxTxIn era))]
fromLedgerTxIns era body =
    [ (fromShelleyTxIn input, ViewTx)
    | input <- Set.toList (inputs_ era body) ]
  where
    inputs_ :: ShelleyBasedEra era
           -> Ledger.TxBody (ShelleyLedgerEra era)
           -> Set (Ledger.TxIn StandardCrypto)
    inputs_ ShelleyBasedEraShelley = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraAllegra = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraMary    = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraAlonzo  = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraBabbage = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraConway  = view L.inputsTxBodyL


fromLedgerTxInsCollateral
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxInsCollateral era
fromLedgerTxInsCollateral era body =
    case collateralSupportedInEra (shelleyBasedToCardanoEra era) of
      Nothing        -> TxInsCollateralNone
      Just supported ->
        TxInsCollateral supported $ map fromShelleyTxIn collateral_
  where
    collateral_ :: [Ledger.TxIn StandardCrypto]
    collateral_ = case era of
      ShelleyBasedEraShelley -> []
      ShelleyBasedEraAllegra -> []
      ShelleyBasedEraMary    -> []
      ShelleyBasedEraAlonzo  -> toList $ body ^. L.collateralInputsTxBodyL
      ShelleyBasedEraBabbage -> toList $ body ^. L.collateralInputsTxBodyL
      ShelleyBasedEraConway  -> toList $ body ^. L.collateralInputsTxBodyL

fromLedgerTxInsReference
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxInsReference ViewTx era
fromLedgerTxInsReference era txBody =
  case refInsScriptsAndInlineDatsSupportedInEra $ shelleyBasedToCardanoEra era of
    Nothing -> TxInsReferenceNone
    Just suppInEra ->
      let ledgerRefInputs =
            obtainReferenceInputsHasFieldConstraint suppInEra $ txBody ^. L.referenceInputsTxBodyL
      in TxInsReference suppInEra
           $ map fromShelleyTxIn . Set.toList $ ledgerRefInputs
 where
  obtainReferenceInputsHasFieldConstraint
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
    -> ((L.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto, L.BabbageEraTxBody (ShelleyLedgerEra era)) => a)
    -> a
  obtainReferenceInputsHasFieldConstraint ReferenceTxInsScriptsInlineDatumsInBabbageEra f = f
  obtainReferenceInputsHasFieldConstraint ReferenceTxInsScriptsInlineDatumsInConwayEra f = f

fromLedgerTxOuts
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxBodyScriptData era
  -> [TxOut CtxTx era]
fromLedgerTxOuts era body scriptdata =
  case era of
    ShelleyBasedEraShelley ->
      [ fromShelleyTxOut era txout | txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraAllegra ->
      [ fromShelleyTxOut era txout | txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraMary ->
      [ fromShelleyTxOut era txout | txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraAlonzo ->
      [ fromAlonzoTxOut
          MultiAssetInAlonzoEra
          ScriptDataInAlonzoEra
          txdatums
          txout
      | let txdatums = selectTxDatums scriptdata
      , txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraBabbage ->
      [ fromBabbageTxOut
          MultiAssetInBabbageEra
          ScriptDataInBabbageEra
          ReferenceTxInsScriptsInlineDatumsInBabbageEra
          txdatums
          txouts
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (body ^. L.outputsTxBodyL)
      ]

    ShelleyBasedEraConway ->
      [ fromBabbageTxOut
          MultiAssetInConwayEra
          ScriptDataInConwayEra
          ReferenceTxInsScriptsInlineDatumsInConwayEra
          txdatums
          txouts
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (body ^. L.outputsTxBodyL)
      ]
  where
    selectTxDatums  TxBodyNoScriptData                            = Map.empty
    selectTxDatums (TxBodyScriptData _ (Alonzo.TxDats' datums) _) = datums

fromAlonzoTxOut :: forall era ledgerera.
                   IsShelleyBasedEra era
                => L.AlonzoEraTxOut ledgerera
                => Ledger.EraCrypto ledgerera ~ StandardCrypto
                => Ledger.Value ledgerera ~ MaryValue StandardCrypto
                => MultiAssetSupportedInEra era
                -> ScriptDataSupportedInEra era
                -> Map (L.DataHash StandardCrypto)
                       (L.Data ledgerera)
                -> L.TxOut ledgerera
                -> TxOut CtxTx era
fromAlonzoTxOut multiAssetInEra scriptDataInEra txdatums txOut =
   TxOut (fromShelleyAddr shelleyBasedEra (txOut ^. L.addrTxOutL))
         (TxOutValue multiAssetInEra (fromMaryValue (txOut ^. L.valueTxOutL)))
         (fromAlonzoTxOutDatum scriptDataInEra (txOut ^. L.dataHashTxOutL))
         ReferenceScriptNone
  where
    fromAlonzoTxOutDatum :: ScriptDataSupportedInEra era
                         -> StrictMaybe (L.DataHash StandardCrypto)
                         -> TxOutDatum CtxTx era
    fromAlonzoTxOutDatum _          SNothing = TxOutDatumNone
    fromAlonzoTxOutDatum supported (SJust dh)
      | Just d <- Map.lookup dh txdatums
                  = TxOutDatumInTx' supported (ScriptDataHash dh)
                                              (fromAlonzoData d)
      | otherwise = TxOutDatumHash supported (ScriptDataHash dh)

fromBabbageTxOut
  :: forall ledgerera era.
     L.BabbageEraTxOut ledgerera
  => IsShelleyBasedEra era
  => ShelleyLedgerEra era ~ ledgerera
  => Ledger.EraCrypto ledgerera ~ StandardCrypto
  => Ledger.Value ledgerera ~ MaryValue StandardCrypto
  => MultiAssetSupportedInEra era
  -> ScriptDataSupportedInEra era
  -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -> Map (L.DataHash StandardCrypto)
         (L.Data ledgerera)
  -> L.TxOut ledgerera
  -> TxOut CtxTx era
fromBabbageTxOut multiAssetInEra scriptDataInEra inlineDatumsInEra txdatums txout =
   TxOut
     (fromShelleyAddr shelleyBasedEra (txout ^. L.addrTxOutL))
     (TxOutValue multiAssetInEra (fromMaryValue (txout ^. L.valueTxOutL)))
     babbageTxOutDatum
     (case txout ^. L.referenceScriptTxOutL of
       SNothing -> ReferenceScriptNone
       SJust rScript -> fromShelleyScriptToReferenceScript shelleyBasedEra rScript
     )
 where
   -- NOTE: This is different to 'fromBabbageTxOutDatum' as it may resolve
   -- 'DatumHash' values using the datums included in the transaction.
   babbageTxOutDatum :: TxOutDatum CtxTx era
   babbageTxOutDatum =
     case txout ^. L.datumTxOutL of
       L.NoDatum -> TxOutDatumNone
       L.DatumHash dh -> resolveDatumInTx dh
       L.Datum d ->
         TxOutDatumInline inlineDatumsInEra $
           binaryDataToScriptData inlineDatumsInEra d

   resolveDatumInTx :: L.DataHash StandardCrypto -> TxOutDatum CtxTx era
   resolveDatumInTx dh
      | Just d <- Map.lookup dh txdatums
                  = TxOutDatumInTx' scriptDataInEra (ScriptDataHash dh) (fromAlonzoData d)
      | otherwise = TxOutDatumHash scriptDataInEra (ScriptDataHash dh)


fromLedgerTxTotalCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxTotalCollateral era
fromLedgerTxTotalCollateral era txbody =
  case totalAndReturnCollateralSupportedInEra $ shelleyBasedToCardanoEra era of
    Nothing -> TxTotalCollateralNone
    Just supp ->
      case obtainTotalCollateralHasFieldConstraint supp $ txbody ^. L.totalCollateralTxBodyL of
        SNothing -> TxTotalCollateralNone
        SJust totColl -> TxTotalCollateral supp $ fromShelleyLovelace totColl
 where
  obtainTotalCollateralHasFieldConstraint
    :: TxTotalAndReturnCollateralSupportedInEra era
    -> (L.BabbageEraTxBody (ShelleyLedgerEra era) => a)
    -> a
  obtainTotalCollateralHasFieldConstraint TxTotalAndReturnCollateralInBabbageEra f = f
  obtainTotalCollateralHasFieldConstraint TxTotalAndReturnCollateralInConwayEra f = f

fromLedgerTxReturnCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxReturnCollateral CtxTx era
fromLedgerTxReturnCollateral era txbody =
  case totalAndReturnCollateralSupportedInEra $ shelleyBasedToCardanoEra era of
    Nothing -> TxReturnCollateralNone
    Just supp ->
      case obtainBabbageEraTxOutConstraint supp $ txbody ^. L.collateralReturnTxBodyL of
        SNothing -> TxReturnCollateralNone
        SJust collReturnOut ->
          TxReturnCollateral supp $ fromShelleyTxOut era collReturnOut
 where
  obtainBabbageEraTxOutConstraint
    :: TxTotalAndReturnCollateralSupportedInEra era
    -> ((L.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
        , L.BabbageEraTxBody (ShelleyLedgerEra era)
        ) => a)
    -> a
  obtainBabbageEraTxOutConstraint TxTotalAndReturnCollateralInBabbageEra f = f
  obtainBabbageEraTxOutConstraint TxTotalAndReturnCollateralInConwayEra f = f


fromLedgerTxFee
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxFee era
fromLedgerTxFee era body =
  case era of
    ShelleyBasedEraShelley ->
      TxFeeExplicit TxFeesExplicitInShelleyEra $
      fromShelleyLovelace $ body ^. L.feeTxBodyL
    ShelleyBasedEraAllegra ->
      TxFeeExplicit TxFeesExplicitInAllegraEra $
      fromShelleyLovelace $ body ^. L.feeTxBodyL
    ShelleyBasedEraMary ->
      TxFeeExplicit TxFeesExplicitInMaryEra $
      fromShelleyLovelace $ body ^. L.feeTxBodyL
    ShelleyBasedEraAlonzo ->
      TxFeeExplicit TxFeesExplicitInAlonzoEra $
      fromShelleyLovelace $ body ^. L.feeTxBodyL
    ShelleyBasedEraBabbage ->
      TxFeeExplicit TxFeesExplicitInBabbageEra $
      fromShelleyLovelace $ body ^. L.feeTxBodyL
    ShelleyBasedEraConway ->
      TxFeeExplicit TxFeesExplicitInConwayEra $
      fromShelleyLovelace $ body ^. L.feeTxBodyL

fromLedgerTxValidityRange
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
fromLedgerTxValidityRange era body =
  case era of
    ShelleyBasedEraShelley ->
      ( TxValidityNoLowerBound
      , TxValidityUpperBound ValidityUpperBoundInShelleyEra $ body ^. L.ttlTxBodyL
      )

    ShelleyBasedEraAllegra ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInAllegraEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAllegraEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraMary ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInMaryEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInMaryEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraAlonzo ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInAlonzoEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAlonzoEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraBabbage ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInBabbageEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInBabbageEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraConway ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInConwayEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInConwayEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInConwayEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

fromLedgerAuxiliaryData
  :: ShelleyBasedEra era
  -> L.TxAuxData (ShelleyLedgerEra era)
  -> (Map Word64 TxMetadataValue, [ScriptInEra era])
fromLedgerAuxiliaryData ShelleyBasedEraShelley (L.ShelleyTxAuxData metadata) =
  (fromShelleyMetadata metadata, [])
fromLedgerAuxiliaryData ShelleyBasedEraAllegra (L.AllegraTxAuxData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraAllegra <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraMary (L.AllegraTxAuxData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraMary <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraAlonzo txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraAlonzo <$>
    toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraBabbage txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraBabbage <$>
    toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraConway txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraConway <$>
    toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )

fromLedgerTxAuxiliaryData
  :: ShelleyBasedEra era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> (TxMetadataInEra era, TxAuxScripts era)
fromLedgerTxAuxiliaryData _ Nothing = (TxMetadataNone, TxAuxScriptsNone)
fromLedgerTxAuxiliaryData era (Just auxData) =
  case era of
    ShelleyBasedEraShelley ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInShelleyEra $ TxMetadata ms
      , TxAuxScriptsNone
      )
    ShelleyBasedEraAllegra ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInAllegraEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInAllegraEra ss
      )
    ShelleyBasedEraMary ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInMaryEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInMaryEra ss
      )
    ShelleyBasedEraAlonzo ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInAlonzoEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInAlonzoEra ss
      )
    ShelleyBasedEraBabbage ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInBabbageEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInBabbageEra ss
      )
    ShelleyBasedEraConway ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInConwayEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInConwayEra ss
      )
  where
    (ms, ss) = fromLedgerAuxiliaryData era auxData


fromLedgerTxExtraKeyWitnesses :: ShelleyBasedEra era
                              -> Ledger.TxBody (ShelleyLedgerEra era)
                              -> TxExtraKeyWitnesses era
fromLedgerTxExtraKeyWitnesses sbe body =
  case sbe of
    ShelleyBasedEraShelley -> TxExtraKeyWitnessesNone
    ShelleyBasedEraAllegra -> TxExtraKeyWitnessesNone
    ShelleyBasedEraMary    -> TxExtraKeyWitnessesNone
    ShelleyBasedEraAlonzo
      | Set.null keyhashes -> TxExtraKeyWitnessesNone
      | otherwise          -> TxExtraKeyWitnesses
                                ExtraKeyWitnessesInAlonzoEra
                                [ PaymentKeyHash (Shelley.coerceKeyRole keyhash)
                                | keyhash <- Set.toList keyhashes ]
      where
        keyhashes = body ^. L.reqSignerHashesTxBodyL
    ShelleyBasedEraBabbage
      | Set.null keyhashes -> TxExtraKeyWitnessesNone
      | otherwise          -> TxExtraKeyWitnesses
                                ExtraKeyWitnessesInBabbageEra
                                [ PaymentKeyHash (Shelley.coerceKeyRole keyhash)
                                | keyhash <- Set.toList keyhashes ]
      where
        keyhashes = body ^. L.reqSignerHashesTxBodyL
    ShelleyBasedEraConway
      | Set.null keyhashes -> TxExtraKeyWitnessesNone
      | otherwise          -> TxExtraKeyWitnesses
                                ExtraKeyWitnessesInConwayEra
                                [ PaymentKeyHash (Shelley.coerceKeyRole keyhash)
                                | keyhash <- Set.toList keyhashes ]
      where
        keyhashes = body ^. L.reqSignerHashesTxBodyL

fromLedgerTxWithdrawals
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxWithdrawals ViewTx era
fromLedgerTxWithdrawals era body =
  case era of
    ShelleyBasedEraShelley
      | null (L.unWithdrawals withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInShelleyEra $
          fromShelleyWithdrawal withdrawals
      where
        withdrawals = body ^. L.withdrawalsTxBodyL

    ShelleyBasedEraAllegra
      | null (L.unWithdrawals withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInAllegraEra $
          fromShelleyWithdrawal withdrawals
      where
        withdrawals = body ^. L.withdrawalsTxBodyL

    ShelleyBasedEraMary
      | null (L.unWithdrawals withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInMaryEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = body ^. L.withdrawalsTxBodyL

    ShelleyBasedEraAlonzo
      | null (L.unWithdrawals withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInAlonzoEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = body ^. L.withdrawalsTxBodyL

    ShelleyBasedEraBabbage
      | null (L.unWithdrawals withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInBabbageEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = body ^. L.withdrawalsTxBodyL

    ShelleyBasedEraConway
      | null (L.unWithdrawals withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInConwayEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = body ^. L.withdrawalsTxBodyL

fromLedgerTxCertificates
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxCertificates ViewTx era
fromLedgerTxCertificates era body =
  case era of
    ShelleyBasedEraShelley
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInShelleyEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = body ^. L.certsTxBodyL

    ShelleyBasedEraAllegra
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInAllegraEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = body ^. L.certsTxBodyL

    ShelleyBasedEraMary
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInMaryEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = body ^. L.certsTxBodyL

    ShelleyBasedEraAlonzo
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInAlonzoEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = body ^. L.certsTxBodyL

    ShelleyBasedEraBabbage
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInBabbageEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = body ^. L.certsTxBodyL

    -- TODO: Implement once certificates are done in Conway.
    ShelleyBasedEraConway -> TxCertificatesNone

fromLedgerTxUpdateProposal
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxUpdateProposal era
fromLedgerTxUpdateProposal era body =
  case era of
    ShelleyBasedEraShelley ->
      case body ^. L.updateTxBodyL of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInShelleyEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraAllegra ->
      case body ^. L.updateTxBodyL of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInAllegraEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraMary ->
      case body ^. L.updateTxBodyL of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInMaryEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraAlonzo ->
      case body ^. L.updateTxBodyL of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInAlonzoEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraBabbage ->
      case body ^. L.updateTxBodyL of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInBabbageEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraConway -> TxUpdateProposalNone

fromLedgerTxMintValue
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxMintValue ViewTx era
fromLedgerTxMintValue era body =
  case era of
    ShelleyBasedEraShelley -> TxMintNone
    ShelleyBasedEraAllegra -> TxMintNone
    ShelleyBasedEraMary    -> toMintValue body MultiAssetInMaryEra
    ShelleyBasedEraAlonzo  -> toMintValue body MultiAssetInAlonzoEra
    ShelleyBasedEraBabbage -> toMintValue body MultiAssetInBabbageEra
    ShelleyBasedEraConway  -> toMintValue body MultiAssetInConwayEra
  where
    toMintValue txBody maInEra
      | L.isZero mint = TxMintNone
      | otherwise     = TxMintValue maInEra (fromMaryValue mint) ViewTx
      where
        mint = MaryValue 0 (txBody ^. L.mintTxBodyL)


makeByronTransactionBody :: TxBodyContent BuildTx ByronEra
                         -> Either TxBodyError (TxBody ByronEra)
makeByronTransactionBody TxBodyContent { txIns, txOuts } = do
    ins' <- NonEmpty.nonEmpty (map fst txIns) ?! TxBodyEmptyTxIns
    for_ ins' $ \txin@(TxIn _ (TxIx txix)) ->
      guard (fromIntegral txix <= maxByronTxInIx) ?! TxBodyInIxOverflow txin
    let ins'' = fmap toByronTxIn ins'

    outs'  <- NonEmpty.nonEmpty txOuts    ?! TxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toByronTxOut out ?! classifyRangeError out)
                outs'
    return $
      ByronTxBody $
        reAnnotate CBOR.byronProtVer $
          Annotated
            (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
            ()
  where
    maxByronTxInIx :: Word
    maxByronTxInIx = fromIntegral (maxBound :: Word32)

    classifyRangeError :: TxOut CtxTx ByronEra -> TxBodyError
    classifyRangeError
      txout@(TxOut (AddressInEra ByronAddressInAnyEra ByronAddress{})
                   (TxOutAdaOnly AdaOnlyInByronEra value) _ _)
      | value < 0        = TxBodyOutputNegative (lovelaceToQuantity value)
                                                (txOutInAnyEra txout)
      | otherwise        = TxBodyOutputOverflow (lovelaceToQuantity value)
                                                (txOutInAnyEra txout)

    classifyRangeError
      (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
             (TxOutValue era _) _ _) = case era of {}

    classifyRangeError
      (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
             _ _ _) = case era of {}

getByronTxBodyContent :: Annotated Byron.Tx ByteString
                      -> TxBodyContent ViewTx ByronEra
getByronTxBodyContent (Annotated Byron.UnsafeTx{txInputs, txOutputs} _) =
  TxBodyContent
  { txIns              = [(fromByronTxIn input, ViewTx) | input <- toList txInputs]
  , txInsCollateral    = TxInsCollateralNone
  , txInsReference     = TxInsReferenceNone
  , txOuts             = fromByronTxOut <$> toList txOutputs
  , txReturnCollateral = TxReturnCollateralNone
  , txTotalCollateral  = TxTotalCollateralNone
  , txFee              = TxFeeImplicit TxFeesImplicitInByronEra
  , txValidityRange    = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInByronEra)
  , txMetadata         = TxMetadataNone
  , txAuxScripts       = TxAuxScriptsNone
  , txExtraKeyWits     = TxExtraKeyWitnessesNone
  , txProtocolParams   = ViewTx
  , txWithdrawals      = TxWithdrawalsNone
  , txCertificates     = TxCertificatesNone
  , txUpdateProposal   = TxUpdateProposalNone
  , txMintValue        = TxMintNone
  , txScriptValidity   = TxScriptValidityNone
  }

convTxIns :: TxIns BuildTx era -> Set (L.TxIn StandardCrypto)
convTxIns txIns = Set.fromList (map (toShelleyTxIn . fst) txIns)

convCollateralTxIns :: TxInsCollateral era -> Set (Ledger.TxIn StandardCrypto)
convCollateralTxIns txInsCollateral =
  case txInsCollateral of
    TxInsCollateralNone -> Set.empty
    TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)

convReturnCollateral
  :: ShelleyBasedEra era
  -> TxReturnCollateral ctx era
  -> StrictMaybe (Ledger.TxOut (ShelleyLedgerEra era))
convReturnCollateral era txReturnCollateral =
  case txReturnCollateral of
    TxReturnCollateralNone -> SNothing
    TxReturnCollateral _ colTxOut -> SJust $ getCBORConstraint era $ toShelleyTxOutAny era colTxOut

convTotalCollateral :: TxTotalCollateral era -> StrictMaybe Ledger.Coin
convTotalCollateral txTotalCollateral =
  case txTotalCollateral of
    TxTotalCollateralNone -> SNothing
    TxTotalCollateral _ totCollLovelace -> SJust $ toShelleyLovelace totCollLovelace

convTxOuts
  :: forall ctx era ledgerera. ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era -> [TxOut ctx era] -> Seq.StrictSeq (Ledger.TxOut ledgerera)
convTxOuts era txOuts = Seq.fromList $ map (toShelleyTxOutAny era) txOuts

convCertificates
  :: TxCertificates build era -> Seq.StrictSeq (Shelley.DCert StandardCrypto)
convCertificates txCertificates =
  case txCertificates of
    TxCertificatesNone    -> Seq.empty
    TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs)

convConwayCertificates
  :: TxCertificates build era -> Seq.StrictSeq (Conway.ConwayDCert StandardCrypto)
convConwayCertificates txCertificates =
  case txCertificates of
    TxCertificatesNone    -> Seq.empty
    TxCertificates _ cs _ ->
      Seq.fromList (mapMaybe (fromShelleyDCertMaybe . toShelleyCertificate) cs)

fromShelleyDCertMaybe :: Shelley.DCert c -> Maybe (Conway.ConwayDCert c)
fromShelleyDCertMaybe (Shelley.DCertDeleg dc) = Just $ Conway.ConwayDCertDeleg dc
fromShelleyDCertMaybe (Shelley.DCertPool pc) = Just $ Conway.ConwayDCertPool pc
fromShelleyDCertMaybe (Shelley.DCertGenesis gdc) = Just $ Conway.ConwayDCertConstitutional gdc
fromShelleyDCertMaybe Shelley.DCertMir {} = Nothing

convWithdrawals :: TxWithdrawals build era -> L.Withdrawals StandardCrypto
convWithdrawals txWithdrawals =
  case txWithdrawals of
    TxWithdrawalsNone  -> L.Withdrawals Map.empty
    TxWithdrawals _ ws -> toShelleyWithdrawal ws

convTransactionFee :: ShelleyBasedEra era -> TxFee era -> Ledger.Coin
convTransactionFee sbe txFee =
  case txFee of
    TxFeeImplicit TxFeesImplicitInByronEra  -> case sbe of {}
    TxFeeExplicit _ fee -> toShelleyLovelace fee


convValidityInterval
  :: (TxValidityLowerBound era, TxValidityUpperBound era)
  -> L.ValidityInterval
convValidityInterval (lowerBound, upperBound) =
  L.ValidityInterval
    { invalidBefore = case lowerBound of
                        TxValidityNoLowerBound   -> SNothing
                        TxValidityLowerBound _ s -> SJust s
    , invalidHereafter = case upperBound of
                           TxValidityNoUpperBound _ -> SNothing
                           TxValidityUpperBound _ s -> SJust s
    }

convTxUpdateProposal
  :: forall era ledgerera. ShelleyLedgerEra era ~ ledgerera
  => Ledger.EraCrypto ledgerera ~ StandardCrypto
  => ShelleyBasedEra era
  -> TxUpdateProposal era
  -> StrictMaybe (Ledger.Update ledgerera)
convTxUpdateProposal era txUpdateProposal =
  case txUpdateProposal of
    TxUpdateProposalNone -> SNothing
    TxUpdateProposal _ p -> SJust (toLedgerUpdate era p)

convMintValue :: TxMintValue build era -> MultiAsset StandardCrypto
convMintValue txMintValue =
  case txMintValue of
    TxMintNone        -> mempty
    TxMintValue _ v _ ->
      case toMaryValue v of
        MaryValue _ ma -> ma

convExtraKeyWitnesses :: TxExtraKeyWitnesses era -> Set (Shelley.KeyHash r' StandardCrypto)
convExtraKeyWitnesses txExtraKeyWits =
  case txExtraKeyWits of
    TxExtraKeyWitnessesNone   -> Set.empty
    TxExtraKeyWitnesses _ khs -> Set.fromList
                                   [ Shelley.coerceKeyRole kh
                                   | PaymentKeyHash kh <- khs ]

convScripts
  :: ShelleyLedgerEra era ~ ledgerera
  => [(ScriptWitnessIndex, AnyScriptWitness era)]
  -> [Ledger.Script ledgerera]
convScripts scriptWitnesses =
  catMaybes
    [ toShelleyScript <$> scriptWitnessScript scriptwitness
    | (_, AnyScriptWitness scriptwitness) <- scriptWitnesses
    ]

-- ScriptData collectively refers to datums and/or redeemers
convScriptData
  :: Ledger.Era (ShelleyLedgerEra era)
  => CardanoEra era
  -> [TxOut CtxTx era]
  -> [(ScriptWitnessIndex, AnyScriptWitness era)]
  -> TxBodyScriptData era
convScriptData era txOuts scriptWitnesses =
  case scriptDataSupportedInEra era of
    Nothing -> TxBodyNoScriptData
    Just scriptDataInEra ->
      let redeemers =
            Alonzo.Redeemers $
              Map.fromList
                [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
                | (idx, AnyScriptWitness
                          (PlutusScriptWitness _ _ _ _ d e)) <- scriptWitnesses
                ]
          datums =
            Alonzo.TxDats $
              Map.fromList
                [ (L.hashData d', d')
                | d <- scriptdata
                , let d' = toAlonzoData d
                ]

          scriptdata :: [HashableScriptData]
          scriptdata =
              [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
           ++ [ d | (_, AnyScriptWitness
                          (PlutusScriptWitness
                             _ _ _ (ScriptDatumForTxIn d) _ _)) <- scriptWitnesses
                  ]
      in TxBodyScriptData scriptDataInEra datums redeemers

convPParamsToScriptIntegrityHash
  :: L.AlonzoEraPParams (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> BuildTxWith BuildTx (Maybe ProtocolParameters)
  -> Alonzo.Redeemers (ShelleyLedgerEra era)
  -> Alonzo.TxDats (ShelleyLedgerEra era)
  -> Set Alonzo.Language
  -> StrictMaybe (L.ScriptIntegrityHash (Ledger.EraCrypto (ShelleyLedgerEra era)))
convPParamsToScriptIntegrityHash sbe txProtocolParams redeemers datums languages =
  case txProtocolParams of
    BuildTxWith Nothing -> SNothing
    BuildTxWith (Just pparams) ->
      Alonzo.hashScriptIntegrity
        (Set.map
           (L.getLanguageView (toLedgerPParams sbe pparams))
           languages
        )
        redeemers
        datums

convLanguages :: [(ScriptWitnessIndex, AnyScriptWitness era)] -> Set Alonzo.Language
convLanguages witnesses =
  Set.fromList
    [ toAlonzoLanguage (AnyPlutusScriptVersion v)
    | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
    ]

convReferenceInputs :: TxInsReference build era -> Set (Ledger.TxIn StandardCrypto)
convReferenceInputs txInsReference =
  case txInsReference of
    TxInsReferenceNone -> mempty
    TxInsReference _ refTxins -> Set.fromList $ map toShelleyTxIn refTxins

getCBORConstraint
  :: ShelleyBasedEra era
  -> (ToCBOR (Ledger.TxOut (ShelleyLedgerEra era)) => a)
  -> a
getCBORConstraint ShelleyBasedEraShelley f = f
getCBORConstraint ShelleyBasedEraAllegra f = f
getCBORConstraint ShelleyBasedEraMary f = f
getCBORConstraint ShelleyBasedEraAlonzo f = f
getCBORConstraint ShelleyBasedEraBabbage f = f
getCBORConstraint ShelleyBasedEraConway f = f

getLedgerEraConstraint
  :: ShelleyBasedEra era
  -> (Ledger.Era (ShelleyLedgerEra era) => a)
  -> a
getLedgerEraConstraint ShelleyBasedEraShelley f = f
getLedgerEraConstraint ShelleyBasedEraAllegra f = f
getLedgerEraConstraint ShelleyBasedEraMary f = f
getLedgerEraConstraint ShelleyBasedEraAlonzo f = f
getLedgerEraConstraint ShelleyBasedEraBabbage f = f
getLedgerEraConstraint ShelleyBasedEraConway f = f

guardShelleyTxInsOverflow :: [TxIn] -> Either TxBodyError ()
guardShelleyTxInsOverflow txIns = do
    for_ txIns $ \txin@(TxIn _ (TxIx txix)) ->
      guard (txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

-- | A helper function that constructs a TxBody with all of the fields that are common for
-- all eras
mkCommonTxBody ::
     ( L.EraTxBody (ShelleyLedgerEra era)
     , L.EraTxAuxData (ShelleyLedgerEra era)
     , L.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
     )
  => ShelleyBasedEra era
  -> TxIns BuildTx era
  -> [TxOut ctx era]
  -> TxFee era
  -> TxWithdrawals build era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> L.TxBody (ShelleyLedgerEra era)
mkCommonTxBody era txIns txOuts txFee txWithdrawals txAuxData =
  L.mkBasicTxBody
  & L.inputsTxBodyL .~ convTxIns txIns
  & L.outputsTxBodyL .~ convTxOuts era txOuts
  & L.feeTxBodyL .~ convTransactionFee era txFee
  & L.withdrawalsTxBodyL .~ convWithdrawals txWithdrawals
  & L.auxDataHashTxBodyL .~ maybe SNothing (SJust . Ledger.hashTxAuxData) txAuxData


makeShelleyTransactionBody
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeShelleyTransactionBody era@ShelleyBasedEraShelley
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (_, upperBound),
                             txMetadata,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    validateTxBodyContent era txbodycontent

    return $
      ShelleyTxBody era
        (mkCommonTxBody era txIns txOuts txFee txWithdrawals txAuxData
           & L.certsTxBodyL  .~ convCertificates txCertificates
           & L.updateTxBodyL .~ convTxUpdateProposal era txUpdateProposal
           & L.ttlTxBodyL    .~ case upperBound of
                                  TxValidityNoUpperBound era' -> case era' of {}
                                  TxValidityUpperBound _ ttl  -> ttl
        )
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts_ :: [Ledger.Script StandardShelley]
    scripts_ = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (L.TxAuxData StandardShelley)
    txAuxData = toAuxiliaryData era txMetadata TxAuxScriptsNone

makeShelleyTransactionBody era@ShelleyBasedEraAllegra
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    validateTxBodyContent era txbodycontent

    return $
      ShelleyTxBody era
        (mkCommonTxBody era txIns txOuts txFee txWithdrawals txAuxData
           & L.certsTxBodyL  .~ convCertificates txCertificates
           & L.vldtTxBodyL   .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL .~ convTxUpdateProposal era txUpdateProposal
        )
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts_ :: [Ledger.Script StandardAllegra]
    scripts_ = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (L.TxAuxData StandardAllegra)
    txAuxData = toAuxiliaryData era txMetadata txAuxScripts

makeShelleyTransactionBody era@ShelleyBasedEraMary
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue
                           } = do

    validateTxBodyContent era txbodycontent

    return $
      ShelleyTxBody era
        (mkCommonTxBody era txIns txOuts txFee txWithdrawals txAuxData
           & L.certsTxBodyL  .~ convCertificates txCertificates
           & L.vldtTxBodyL   .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL .~ convTxUpdateProposal era txUpdateProposal
           & L.mintTxBodyL   .~ convMintValue txMintValue
        )
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts :: [Ledger.Script StandardMary]
    scripts = List.nub $ catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (L.TxAuxData StandardMary)
    txAuxData = toAuxiliaryData era txMetadata txAuxScripts

makeShelleyTransactionBody era@ShelleyBasedEraAlonzo
                           txbodycontent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txExtraKeyWits,
                             txProtocolParams,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue,
                             txScriptValidity
                           } = do

    validateTxBodyContent era txbodycontent

    return $
      ShelleyTxBody era
        (mkCommonTxBody era txIns txOuts txFee txWithdrawals txAuxData
           & L.collateralInputsTxBodyL    .~ convCollateralTxIns txInsCollateral
           & L.certsTxBodyL               .~ convCertificates txCertificates
           & L.vldtTxBodyL                .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL              .~ convTxUpdateProposal era txUpdateProposal
           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses txExtraKeyWits
           & L.mintTxBodyL                .~ convMintValue txMintValue
           & L.scriptIntegrityHashTxBodyL .~ convPParamsToScriptIntegrityHash
                                             era txProtocolParams redeemers datums languages
           -- TODO Alonzo: support optional network id in TxBodyContent
           -- & L.networkIdTxBodyL .~ SNothing
        )
        scripts
        (TxBodyScriptData ScriptDataInAlonzoEra datums redeemers)
        txAuxData
        txScriptValidity
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AlonzoEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardAlonzo]
    scripts = List.nub $ catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: Alonzo.TxDats StandardAlonzo
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
        [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
     ++ [ d | (_, AnyScriptWitness
                    (PlutusScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Alonzo.Redeemers StandardAlonzo
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set Alonzo.Language
    languages =
      Set.fromList
        [ toAlonzoLanguage (AnyPlutusScriptVersion v)
        | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
        ]

    txAuxData :: Maybe (L.TxAuxData StandardAlonzo)
    txAuxData = toAuxiliaryData era txMetadata txAuxScripts

makeShelleyTransactionBody era@ShelleyBasedEraBabbage
                            txbodycontent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txInsReference,
                             txReturnCollateral,
                             txTotalCollateral,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txExtraKeyWits,
                             txProtocolParams,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue,
                             txScriptValidity
                           } = do

    validateTxBodyContent era txbodycontent

    return $
      ShelleyTxBody era
        (mkCommonTxBody era txIns txOuts txFee txWithdrawals txAuxData
           & L.collateralInputsTxBodyL .~
               case txInsCollateral of
                TxInsCollateralNone     -> Set.empty
                TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)
           & L.referenceInputsTxBodyL     .~ convReferenceInputs txInsReference
           & L.collateralReturnTxBodyL    .~ convReturnCollateral era txReturnCollateral
           & L.totalCollateralTxBodyL     .~ convTotalCollateral txTotalCollateral
           & L.certsTxBodyL               .~ convCertificates txCertificates
           & L.vldtTxBodyL                .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL              .~ convTxUpdateProposal era txUpdateProposal
           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses txExtraKeyWits
           & L.mintTxBodyL                .~ convMintValue txMintValue
           & L.scriptIntegrityHashTxBodyL .~ convPParamsToScriptIntegrityHash
                                             era txProtocolParams redeemers datums languages
           -- TODO Babbage: support optional network id in TxBodyContent
           -- & L.networkIdTxBodyL .~ SNothing
        )
        scripts
        (TxBodyScriptData ScriptDataInBabbageEra
          datums redeemers)
        txAuxData
        txScriptValidity
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness BabbageEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardBabbage]
    scripts = List.nub $ catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats StandardBabbage
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (L.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
        [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
     ++ [ d | (_, AnyScriptWitness
                    (PlutusScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Alonzo.Redeemers StandardBabbage
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set Alonzo.Language
    languages =
      Set.fromList $ catMaybes
        [ getScriptLanguage sw
        | (_, AnyScriptWitness sw) <- witnesses
        ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Alonzo.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData StandardBabbage)
    txAuxData = toAuxiliaryData era txMetadata txAuxScripts

makeShelleyTransactionBody era@ShelleyBasedEraConway
                            txbodycontent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txInsReference,
                             txReturnCollateral,
                             txTotalCollateral,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txExtraKeyWits,
                             txProtocolParams,
                             txWithdrawals,
                             txCertificates,
                             txMintValue,
                             txScriptValidity
                           } = do

    validateTxBodyContent era txbodycontent

    return $
      ShelleyTxBody era
        (mkCommonTxBody era txIns txOuts txFee txWithdrawals txAuxData
           & L.collateralInputsTxBodyL .~
               case txInsCollateral of
                TxInsCollateralNone     -> Set.empty
                TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)
           & L.referenceInputsTxBodyL     .~ convReferenceInputs txInsReference
           & L.collateralReturnTxBodyL    .~ convReturnCollateral era txReturnCollateral
           & L.totalCollateralTxBodyL     .~ convTotalCollateral txTotalCollateral
           & L.conwayCertsTxBodyL         .~ convConwayCertificates txCertificates
           & L.vldtTxBodyL                .~ convValidityInterval (lowerBound, upperBound)
           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses txExtraKeyWits
           & L.mintTxBodyL                .~ convMintValue txMintValue
           & L.scriptIntegrityHashTxBodyL .~ convPParamsToScriptIntegrityHash
                                             era txProtocolParams redeemers datums languages
           -- TODO Conway: support optional network id in TxBodyContent
           -- & L.networkIdTxBodyL .~ SNothing
        )
        scripts
        (TxBodyScriptData ScriptDataInConwayEra
          datums redeemers)
        txAuxData
        txScriptValidity
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness ConwayEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardConway]
    scripts = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats StandardConway
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
        [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
     ++ [ d | (_, AnyScriptWitness
                    (PlutusScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Alonzo.Redeemers StandardConway
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set Alonzo.Language
    languages =
      Set.fromList $ catMaybes
        [ getScriptLanguage sw
        | (_, AnyScriptWitness sw) <- witnesses
        ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Alonzo.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData StandardConway)
    txAuxData = toAuxiliaryData era txMetadata txAuxScripts



-- | A variant of 'toShelleyTxOutAny that is used only internally to this module
-- that works with a 'TxOut' in any context (including CtxTx) by ignoring
-- embedded datums (taking only their hash).
--
toShelleyTxOutAny :: forall ctx era ledgerera.
                   ShelleyLedgerEra era ~ ledgerera
                => ShelleyBasedEra era
                -> TxOut ctx era
                -> Ledger.TxOut ledgerera
toShelleyTxOutAny era (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _) _ _) =
    case era of {}

toShelleyTxOutAny _ (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOutAny _ (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOutAny _ (TxOut addr (TxOutValue MultiAssetInMaryEra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOutAny _ (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
    & L.dataHashTxOutL .~ toAlonzoTxOutDataHash' txoutdata

toShelleyTxOutAny era (TxOut addr (TxOutValue MultiAssetInBabbageEra value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra era
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum' txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

toShelleyTxOutAny era (TxOut addr (TxOutValue MultiAssetInConwayEra value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra era
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum' txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

toAlonzoTxOutDataHash' :: TxOutDatum ctx AlonzoEra
                       -> StrictMaybe (L.DataHash StandardCrypto)
toAlonzoTxOutDataHash'  TxOutDatumNone                          = SNothing
toAlonzoTxOutDataHash' (TxOutDatumHash _ (ScriptDataHash dh))   = SJust dh
toAlonzoTxOutDataHash' (TxOutDatumInTx' _ (ScriptDataHash dh) _) = SJust dh
toAlonzoTxOutDataHash' (TxOutDatumInline inlineDatumSupp _sd) =
  case inlineDatumSupp :: ReferenceTxInsScriptsInlineDatumsSupportedInEra AlonzoEra of {}

-- TODO: Consolidate with alonzo function and rename
toBabbageTxOutDatum'
  :: (L.Era (ShelleyLedgerEra era), Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => TxOutDatum ctx era -> Babbage.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum'  TxOutDatumNone = Babbage.NoDatum
toBabbageTxOutDatum' (TxOutDatumHash _ (ScriptDataHash dh)) = Babbage.DatumHash dh
toBabbageTxOutDatum' (TxOutDatumInTx' _ (ScriptDataHash dh) _) = Babbage.DatumHash dh
toBabbageTxOutDatum' (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd


-- ----------------------------------------------------------------------------
-- Script witnesses within the tx body
--

-- | A 'ScriptWitness' in any 'WitCtx'. This lets us handle heterogeneous
-- collections of script witnesses from multiple contexts.
--
data AnyScriptWitness era where
     AnyScriptWitness :: ScriptWitness witctx era -> AnyScriptWitness era

-- | Identify the location of a 'ScriptWitness' within the context of a
-- 'TxBody'. These are indexes of the objects within the transaction that
-- need or can use script witnesses: inputs, minted assets, withdrawals and
-- certificates. These are simple numeric indices, enumerated from zero.
-- Thus the indices are not stable if the transaction body is modified.
--
data ScriptWitnessIndex =

     -- | The n'th transaction input, in the order of the 'TxId's.
     ScriptWitnessIndexTxIn !Word

     -- | The n'th minting 'PolicyId', in the order of the 'PolicyId's.
   | ScriptWitnessIndexMint !Word

     -- | The n'th certificate, in the list order of the certificates.
   | ScriptWitnessIndexCertificate !Word

     -- | The n'th withdrawal, in the order of the 'StakeAddress's.
   | ScriptWitnessIndexWithdrawal !Word
  deriving (Eq, Ord, Show)

instance ToJSON ScriptWitnessIndex where
  toJSON = \case
    ScriptWitnessIndexTxIn n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexTxIn"
      , "value" .= n
      ]
    ScriptWitnessIndexMint n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexMint"
      , "value" .= n
      ]
    ScriptWitnessIndexCertificate n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexCertificate"
      , "value" .= n
      ]
    ScriptWitnessIndexWithdrawal n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexWithdrawal"
      , "value" .= n
      ]

renderScriptWitnessIndex :: ScriptWitnessIndex -> String
renderScriptWitnessIndex (ScriptWitnessIndexTxIn index) =
  "transaction input " <> show index <> " (in ascending order of the TxIds)"
renderScriptWitnessIndex (ScriptWitnessIndexMint index) =
  "policyId " <> show index <> " (in ascending order of the PolicyIds)"
renderScriptWitnessIndex (ScriptWitnessIndexCertificate index) =
  "certificate " <> show index <> " (in the list order of the certificates)"
renderScriptWitnessIndex (ScriptWitnessIndexWithdrawal index) =
  "withdrawal " <> show index <> " (in ascending order of the StakeAddresses)"

toAlonzoRdmrPtr :: ScriptWitnessIndex -> Alonzo.RdmrPtr
toAlonzoRdmrPtr widx =
    case widx of
      ScriptWitnessIndexTxIn        n -> Alonzo.RdmrPtr Alonzo.Spend (fromIntegral n)
      ScriptWitnessIndexMint        n -> Alonzo.RdmrPtr Alonzo.Mint  (fromIntegral n)
      ScriptWitnessIndexCertificate n -> Alonzo.RdmrPtr Alonzo.Cert  (fromIntegral n)
      ScriptWitnessIndexWithdrawal  n -> Alonzo.RdmrPtr Alonzo.Rewrd (fromIntegral n)

fromAlonzoRdmrPtr :: Alonzo.RdmrPtr -> ScriptWitnessIndex
fromAlonzoRdmrPtr (Alonzo.RdmrPtr tag n) =
    case tag of
      Alonzo.Spend -> ScriptWitnessIndexTxIn        (fromIntegral n)
      Alonzo.Mint  -> ScriptWitnessIndexMint        (fromIntegral n)
      Alonzo.Cert  -> ScriptWitnessIndexCertificate (fromIntegral n)
      Alonzo.Rewrd -> ScriptWitnessIndexWithdrawal  (fromIntegral n)

collectTxBodyScriptWitnesses :: forall era.
                                TxBodyContent BuildTx era
                             -> [(ScriptWitnessIndex, AnyScriptWitness era)]
collectTxBodyScriptWitnesses TxBodyContent {
                               txIns,
                               txWithdrawals,
                               txCertificates,
                               txMintValue
                             } =
    concat
      [ scriptWitnessesTxIns        txIns
      , scriptWitnessesWithdrawals  txWithdrawals
      , scriptWitnessesCertificates txCertificates
      , scriptWitnessesMinting      txMintValue
      ]
  where
    scriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesTxIns txins =
        [ (ScriptWitnessIndexTxIn ix, AnyScriptWitness witness)
          -- The tx ins are indexed in the map order by txid
        | (ix, (_, BuildTxWith (ScriptWitness _ witness)))
            <- zip [0..] (orderTxIns txins)
        ]

    scriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesWithdrawals  TxWithdrawalsNone = []
    scriptWitnessesWithdrawals (TxWithdrawals _ withdrawals) =
        [ (ScriptWitnessIndexWithdrawal ix, AnyScriptWitness witness)
          -- The withdrawals are indexed in the map order by stake credential
        | (ix, (_, _, BuildTxWith (ScriptWitness _ witness)))
             <- zip [0..] (orderStakeAddrs withdrawals)
        ]

    scriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesCertificates  TxCertificatesNone = []
    scriptWitnessesCertificates (TxCertificates _ certs (BuildTxWith witnesses)) =
        [ (ScriptWitnessIndexCertificate ix, AnyScriptWitness witness)
          -- The certs are indexed in list order
        | (ix, cert) <- zip [0..] certs
        , ScriptWitness _ witness <- maybeToList $ do
                                       stakecred <- selectStakeCredential cert
                                       Map.lookup stakecred witnesses
        ]

    selectStakeCredential cert =
      case cert of
        StakeAddressDeregistrationCertificate stakecred   -> Just stakecred
        StakeAddressPoolDelegationCertificate stakecred _ -> Just stakecred
        _                                                 -> Nothing

    scriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesMinting  TxMintNone = []
    scriptWitnessesMinting (TxMintValue _ value (BuildTxWith witnesses)) =
        [ (ScriptWitnessIndexMint ix, AnyScriptWitness witness)
          -- The minting policies are indexed in policy id order in the value
        | let ValueNestedRep bundle = valueToNestedRep value
        , (ix, ValueNestedBundle policyid _) <- zip [0..] bundle
        , witness <- maybeToList (Map.lookup policyid witnesses)
        ]

-- This relies on the TxId Ord instance being consistent with the
-- Ledger.TxId Ord instance via the toShelleyTxId conversion
-- This is checked by prop_ord_distributive_TxId
orderTxIns :: [(TxIn, v)] -> [(TxIn, v)]
orderTxIns = sortBy (compare `on` fst)

-- This relies on the StakeAddress Ord instance being consistent with the
-- Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion
-- This is checked by prop_ord_distributive_StakeAddress
orderStakeAddrs :: [(StakeAddress, x, v)] -> [(StakeAddress, x, v)]
orderStakeAddrs = sortBy (compare `on` (\(k, _, _) -> k))


toShelleyWithdrawal :: [(StakeAddress, Lovelace, a)] -> L.Withdrawals StandardCrypto
toShelleyWithdrawal withdrawals =
    L.Withdrawals $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value, _) <- withdrawals ]


fromShelleyWithdrawal
  :: L.Withdrawals StandardCrypto
  -> [(StakeAddress, Lovelace, BuildTxWith ViewTx (Witness WitCtxStake era))]
fromShelleyWithdrawal (L.Withdrawals withdrawals) =
  [ (fromShelleyStakeAddr stakeAddr, fromShelleyLovelace value, ViewTx)
  | (stakeAddr, value) <- Map.assocs withdrawals
  ]


-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts. In the Alonzo and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
--
toAuxiliaryData
  :: ShelleyBasedEra era
  -> TxMetadataInEra era
  -> TxAuxScripts era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
toAuxiliaryData sbe txMetadata txAuxScripts =
  let ms = case txMetadata of
             TxMetadataNone                     -> Map.empty
             TxMetadataInEra _ (TxMetadata ms') -> toShelleyMetadata ms'
      ss = case txAuxScripts of
             TxAuxScriptsNone   -> []
             TxAuxScripts _ ss' -> map toShelleyScript ss'
  in case sbe of
       ShelleyBasedEraShelley ->
         guard (not (Map.null ms)) $> L.ShelleyTxAuxData ms
       ShelleyBasedEraAllegra ->
         guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (Seq.fromList ss)
       ShelleyBasedEraMary ->
         guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (Seq.fromList ss)
       ShelleyBasedEraAlonzo ->
         guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
       ShelleyBasedEraBabbage ->
         guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
       ShelleyBasedEraConway ->
         guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss

-- ----------------------------------------------------------------------------
-- Other utilities helpful with making transaction bodies
--

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
--
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
    --TODO: should handle Byron UTxO case too.
    fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
  where
    addr :: L.Addr StandardCrypto
    addr = L.Addr
             (toShelleyNetwork nw)
             (Shelley.KeyHashObj kh)
             Shelley.StakeRefNull

calculateExecutionUnitsLovelace :: ExecutionUnitPrices -> ExecutionUnits -> Maybe Lovelace
calculateExecutionUnitsLovelace euPrices eUnits =
  case toAlonzoPrices euPrices of
    Left _ -> Nothing
    Right prices ->
      return . fromShelleyLovelace $ Alonzo.txscriptfee prices (toAlonzoExUnits eUnits)

-- ----------------------------------------------------------------------------
-- Inline data
--
-- | Conversion of ScriptData to binary data which allows for the storage of data
-- onchain within a transaction output.
--

scriptDataToInlineDatum :: L.Era ledgerera => HashableScriptData -> L.Datum ledgerera
scriptDataToInlineDatum d =
  L.Datum . L.dataToBinaryData $ toAlonzoData d

binaryDataToScriptData
  :: L.Era ledgerera
  => ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -> L.BinaryData ledgerera -> HashableScriptData
binaryDataToScriptData ReferenceTxInsScriptsInlineDatumsInBabbageEra d =
  fromAlonzoData $ L.binaryDataToData d
binaryDataToScriptData ReferenceTxInsScriptsInlineDatumsInConwayEra  d =
  fromAlonzoData $ L.binaryDataToData d
