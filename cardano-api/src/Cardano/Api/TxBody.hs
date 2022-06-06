{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


-- | Transaction bodies
--
module Cardano.Api.TxBody (
    parseTxId,
    -- * Transaction bodies
    TxBody(.., TxBody),
    makeTransactionBody,
    TxBodyContent(..),
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

import           Prelude

import           Control.Applicative (some)
import           Control.Monad (guard)
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
import           Data.List (intercalate, sortBy)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, maybeToList)
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
import           GHC.Records (HasField (..))
import           Text.Parsec ((<?>))
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec

import           Cardano.Binary (Annotated (..), reAnnotate, recoverBytes)
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Serialization as CBOR (decodeNullMaybe, encodeNullMaybe, mkSized,
                   sizedValue)
import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron

import qualified Cardano.Ledger.Address as Shelley
import qualified Cardano.Ledger.AuxiliaryData as Ledger (hashAuxiliaryData)
import           Cardano.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Shelley
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.Constraints as Ledger

import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import qualified Cardano.Ledger.Shelley.Metadata as Shelley
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Allegra
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Mary
import qualified Cardano.Ledger.ShelleyMA.TxBody as Allegra
import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
import           Cardano.Ledger.Val (isZero)

import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo

import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardAlonzo, StandardBabbage,
                   StandardMary, StandardShelley)

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
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


{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use section" -}

-- | Indicates whether a script is expected to fail or pass validation.
data ScriptValidity
  = ScriptInvalid -- ^ Script is expected to fail validation.
                  -- Transactions marked as such can include scripts that fail validation.
                  -- Such transactions may be submitted to the chain, in which case the
                  -- collateral will be taken upon on chain script validation failure.

  | ScriptValid   -- ^ Script is expected to pass validation.
                  -- Transactions marked as such cannot include scripts that fail validation.

  deriving (Eq, Show)

instance ToCBOR ScriptValidity where
  toCBOR = toCBOR . scriptValidityToIsValid

instance FromCBOR ScriptValidity where
  fromCBOR = isValidToScriptValidity <$> fromCBOR

scriptValidityToIsValid :: ScriptValidity -> Alonzo.IsValid
scriptValidityToIsValid ScriptInvalid = Alonzo.IsValid False
scriptValidityToIsValid ScriptValid = Alonzo.IsValid True

isValidToScriptValidity :: Alonzo.IsValid -> ScriptValidity
isValidToScriptValidity (Alonzo.IsValid False) = ScriptInvalid
isValidToScriptValidity (Alonzo.IsValid True) = ScriptValid

-- | A representation of whether the era supports tx script validity.
--
-- The Mary and subsequent eras support script validity.
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

deriving instance Eq   (TxScriptValidity era)
deriving instance Show (TxScriptValidity era)

txScriptValiditySupportedInCardanoEra :: CardanoEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInCardanoEra ByronEra   = Nothing
txScriptValiditySupportedInCardanoEra ShelleyEra = Nothing
txScriptValiditySupportedInCardanoEra AllegraEra = Nothing
txScriptValiditySupportedInCardanoEra MaryEra    = Nothing
txScriptValiditySupportedInCardanoEra AlonzoEra  = Just TxScriptValiditySupportedInAlonzoEra
txScriptValiditySupportedInCardanoEra BabbageEra = Just TxScriptValiditySupportedInBabbageEra

txScriptValiditySupportedInShelleyBasedEra :: ShelleyBasedEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraShelley = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraAllegra = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraMary    = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraAlonzo  = Just TxScriptValiditySupportedInAlonzoEra
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraBabbage = Just TxScriptValiditySupportedInBabbageEra

txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
txScriptValidityToScriptValidity TxScriptValidityNone = ScriptValid
txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity

scriptValidityToTxScriptValidity :: ShelleyBasedEra era -> ScriptValidity -> TxScriptValidity era
scriptValidityToTxScriptValidity era scriptValidity = case txScriptValiditySupportedInShelleyBasedEra era of
  Nothing -> TxScriptValidityNone
  Just witness -> TxScriptValidity witness scriptValidity

txScriptValidityToIsValid :: TxScriptValidity era -> Alonzo.IsValid
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
         "inlineDatumhash"  .= toJSON (hashScriptData datum)

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

instance (IsShelleyBasedEra era, IsCardanoEra era)
  => FromJSON (TxOut CtxTx era) where
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
                (Just dVal, Just h) ->
                  case scriptDataFromJson ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right sData ->
                      if hashScriptData sData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra sData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            mReferenceScript <- o .:? "referenceScript"

            reconcile alonzoTxOutInBabbage mInlineDatum mReferenceScript
         where
           reconcile
             :: TxOut CtxTx BabbageEra -- ^ Alonzo era datum in Babbage era
             -> TxOutDatum CtxTx BabbageEra -- ^ Babbagae inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxTx BabbageEra)
           reconcile top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
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
               (Just dVal, Just dHash) ->
                 case scriptDataFromJson ScriptDataJsonDetailedSchema dVal of
                   Left err ->
                     fail $ "Error parsing TxOut JSON: " <> displayError err
                   Right sData -> TxOut <$> o .: "address"
                                        <*> o .: "value"
                                        <*> return (TxOutDatumInTx' supp dHash sData)
                                        <*> return ReferenceScriptNone
               (Nothing, Just dHash) ->
                 TxOut <$> o .: "address"
                       <*> o .: "value"
                       <*> return (TxOutDatumHash supp dHash)
                       <*> return ReferenceScriptNone
               (Just _dVal, Nothing) -> fail "Only datum JSON was found, this should not be possible."

instance (IsShelleyBasedEra era, IsCardanoEra era)
  => FromJSON (TxOut CtxUTxO era) where
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
                (Just dVal, Just h) ->
                  case scriptDataFromJson ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right sData ->
                      if hashScriptData sData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra sData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            -- We check for a reference script
            mReferenceScript <- o .:? "referenceScript"

            reconcile alonzoTxOutInBabbage mInlineDatum mReferenceScript
         where
           reconcile
             :: TxOut CtxUTxO BabbageEra -- ^ Alonzo era datum in Babbage era
             -> TxOutDatum CtxUTxO BabbageEra -- ^ Babbagae inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxUTxO BabbageEra)
           reconcile (TxOut addr v dat r) babbageDatum mBabRefScript = do
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
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _ _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutValue MultiAssetInMaryEra value) _ _) =
    Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOut _ (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata _) =
    Alonzo.TxOut (toShelleyAddr addr) (toMaryValue value)
                 (toAlonzoTxOutDataHash txoutdata)

toShelleyTxOut era (TxOut addr (TxOutValue MultiAssetInBabbageEra value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra era
    in Babbage.TxOut (toShelleyAddr addr) (toMaryValue value)
                     (toBabbageTxOutDatum txoutdata) (refScriptToShelleyScript cEra refScript)


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
        Shelley.TxOut addr value = ledgerTxOut

    ShelleyBasedEraAllegra ->
        TxOut (fromShelleyAddr era addr)
              (TxOutAdaOnly AdaOnlyInAllegraEra
                            (fromShelleyLovelace value))
               TxOutDatumNone ReferenceScriptNone
      where
        Shelley.TxOut addr value = ledgerTxOut

    ShelleyBasedEraMary ->
        TxOut (fromShelleyAddr era addr)
              (TxOutValue MultiAssetInMaryEra
                          (fromMaryValue value))
               TxOutDatumNone ReferenceScriptNone
      where
        Shelley.TxOut addr value = ledgerTxOut

    ShelleyBasedEraAlonzo ->
       TxOut (fromShelleyAddr era addr)
             (TxOutValue MultiAssetInAlonzoEra
                         (fromMaryValue value))
             (fromAlonzoTxOutDataHash ScriptDataInAlonzoEra datahash)
             ReferenceScriptNone
      where
        Alonzo.TxOut addr value datahash = ledgerTxOut

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
        Babbage.TxOut addr value datum mRefScript = ledgerTxOut



-- TODO: If ledger creates an open type family for datums
-- we can consolidate this function with the Babbage version
toAlonzoTxOutDataHash
  :: TxOutDatum CtxUTxO AlonzoEra
  -> StrictMaybe (Alonzo.DataHash StandardCrypto)
toAlonzoTxOutDataHash  TxOutDatumNone                        = SNothing
toAlonzoTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDataHash (TxOutDatumInline inlineDatumSupp _sd) =
  case inlineDatumSupp :: ReferenceTxInsScriptsInlineDatumsSupportedInEra AlonzoEra of {}

fromAlonzoTxOutDataHash :: ScriptDataSupportedInEra era
                        -> StrictMaybe (Alonzo.DataHash StandardCrypto)
                        -> TxOutDatum ctx era
fromAlonzoTxOutDataHash _    SNothing  = TxOutDatumNone
fromAlonzoTxOutDataHash s (SJust dh)   = TxOutDatumHash s (ScriptDataHash dh)

-- TODO: If ledger creates an open type family for datums
-- we can consolidate this function with the Alonzo version
toBabbageTxOutDatum
  :: Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto
  => TxOutDatum CtxUTxO era -> Babbage.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum  TxOutDatumNone = Babbage.NoDatum
toBabbageTxOutDatum (TxOutDatumHash _ (ScriptDataHash dh)) = Babbage.DatumHash dh
toBabbageTxOutDatum (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

fromBabbageTxOutDatum
  :: Ledger.Crypto ledgerera ~ StandardCrypto
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


-- | A representation of whether the era supports multi-asset transactions.
--
-- The Mary and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyAdaSupportedInEra'.
--
data ScriptDataSupportedInEra era where

     -- | Script data is supported in transactions in the 'Alonzo' era.
     ScriptDataInAlonzoEra  :: ScriptDataSupportedInEra AlonzoEra
     ScriptDataInBabbageEra :: ScriptDataSupportedInEra BabbageEra

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


-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data BuildTx
data ViewTx

data BuildTxWith build a where

     ViewTx      ::      BuildTxWith ViewTx  a
     BuildTxWith :: a -> BuildTxWith BuildTx a

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
                      -> ScriptData
                      -> TxOutDatum CtxTx era

     -- | A transaction output that specifies the whole datum instead of the
     -- datum hash. Note that the datum map will not be updated with this datum,
     -- it only exists at the transaction output.
     --
     TxOutDatumInline :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
                      -> ScriptData
                      -> TxOutDatum ctx era

deriving instance Eq   (TxOutDatum ctx era)
deriving instance Show (TxOutDatum ctx era)


pattern TxOutDatumInTx
  :: ScriptDataSupportedInEra era
  -> ScriptData
  -> TxOutDatum CtxTx era
pattern TxOutDatumInTx s d <- TxOutDatumInTx' s _ d
  where
    TxOutDatumInTx s d = TxOutDatumInTx' s (hashScriptData d) d

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

          -- The 'Ledger.AuxiliaryData' consists of one or several things,
          -- depending on era:
          -- + transaction metadata  (in Shelley and later)
          -- + auxiliary scripts     (in Allegra and later)
          -- Note that there is no auxiliary script data as such, because the
          -- extra script data has to be passed to scripts and hence is needed
          -- for validation. It is thus part of the witness data, not the
          -- auxiliary data.
       -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))

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
deriving instance Show (TxBodyScriptData era)


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

    (==) ByronTxBody{} (ShelleyTxBody era _ _ _ _ _) = case era of {}


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
    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron TxBody"
              CBOR.fromCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraShelley bs
        AllegraEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraAllegra bs
        MaryEra    -> deserialiseShelleyBasedTxBody ShelleyBasedEraMary    bs
        AlonzoEra  -> deserialiseShelleyBasedTxBody ShelleyBasedEraAlonzo  bs
        BabbageEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraBabbage bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
serialiseShelleyBasedTxBody
  :: forall era ledgerera.
     ShelleyLedgerEra era ~ ledgerera
  => ToCBOR (Ledger.TxBody ledgerera)
  => ToCBOR (Ledger.Script ledgerera)
  => ToCBOR (Alonzo.TxDats ledgerera)
  => ToCBOR (Alonzo.Redeemers ledgerera)
  => ToCBOR (Ledger.AuxiliaryData ledgerera)
  => ShelleyBasedEra era
  -> Ledger.TxBody ledgerera
  -> [Ledger.Script ledgerera]
  -> TxBodyScriptData era
  -> Maybe (Ledger.AuxiliaryData ledgerera)
  -> TxScriptValidity era -- ^ Mark script as expected to pass or fail validation
  -> ByteString
serialiseShelleyBasedTxBody era txbody txscripts
                            TxBodyNoScriptData txmetadata scriptValidity =
    -- Backwards compat for pre-Alonzo era tx body files
    case era of
      ShelleyBasedEraShelley -> preAlonzo
      ShelleyBasedEraAllegra -> preAlonzo
      ShelleyBasedEraMary -> preAlonzo
      ShelleyBasedEraAlonzo ->
        CBOR.serializeEncoding'
          $ CBOR.encodeListLen 4
         <> CBOR.toCBOR txbody
         <> CBOR.toCBOR txscripts
         <> CBOR.toCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata
      ShelleyBasedEraBabbage ->
        CBOR.serializeEncoding'
          $ CBOR.encodeListLen 4
         <> CBOR.toCBOR txbody
         <> CBOR.toCBOR txscripts
         <> CBOR.toCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata
 where
   preAlonzo = CBOR.serializeEncoding'
                 $ CBOR.encodeListLen 3
                <> CBOR.toCBOR txbody
                <> CBOR.toCBOR txscripts
                <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

serialiseShelleyBasedTxBody _era txbody txscripts
                            (TxBodyScriptData _ datums redeemers)
                            txmetadata txBodycriptValidity =
    CBOR.serializeEncoding' $
        CBOR.encodeListLen 6
     <> CBOR.toCBOR txbody
     <> CBOR.toCBOR txscripts
     <> CBOR.toCBOR datums
     <> CBOR.toCBOR redeemers
     <> CBOR.toCBOR (txScriptValidityToScriptValidity txBodycriptValidity)
     <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

deserialiseShelleyBasedTxBody
  :: forall era ledgerera.
     ShelleyLedgerEra era ~ ledgerera
  => FromCBOR (CBOR.Annotator (Ledger.TxBody ledgerera))
  => FromCBOR (CBOR.Annotator (Ledger.Script ledgerera))
  => FromCBOR (CBOR.Annotator (Alonzo.TxDats ledgerera))
  => FromCBOR (CBOR.Annotator (Alonzo.Redeemers ledgerera))
  => FromCBOR (CBOR.Annotator (Ledger.AuxiliaryData ledgerera))
  => ShelleyBasedEra era
  -> ByteString
  -> Either CBOR.DecoderError (TxBody era)
deserialiseShelleyBasedTxBody era bs =
    CBOR.decodeAnnotator
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
          txbody     <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              [] -- scripts
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return TxScriptValidityNone))
        3 -> do
          txbody     <- fromCBOR
          txscripts  <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
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
              Nothing -> fail $ "deserialiseShelleyBasedTxBody: Expected an era that supports the \
                                \script validity flag but got: "
                              <> show era
              Just supported -> return supported

          txbody     <- fromCBOR
          txscripts  <- fromCBOR
          scriptValidity <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
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
              Nothing -> fail $ "deserialiseShelleyBasedTxBody: Expected an era that supports script\
                                \ data but got: "
                             <> show era
              Just supported -> return supported

          sValiditySupported <-
            case txScriptValiditySupportedInShelleyBasedEra era of
              Nothing -> fail $ "deserialiseShelleyBasedTxBody: Expected an era that supports the \
                                \script validity flag but got: "
                              <> show era
              Just supported -> return supported

          txbody    <- fromCBOR
          txscripts <- fromCBOR
          datums    <- fromCBOR
          redeemers <- fromCBOR
          scriptValidity <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR

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
    -> (( Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto
        , Ledger.UsesTxBody (ShelleyLedgerEra era)
        ) => a)
    -> a
  obtainConstraints ShelleyBasedEraShelley f = f
  obtainConstraints ShelleyBasedEraAllegra f = f
  obtainConstraints ShelleyBasedEraMary    f = f
  obtainConstraints ShelleyBasedEraAlonzo  f = f
  obtainConstraints ShelleyBasedEraBabbage f = f

getTxIdShelley
  :: Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.UsesTxBody (ShelleyLedgerEra era)
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


makeTransactionBody :: forall era.
     IsCardanoEra era
  => TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeTransactionBody =
    case cardanoEraStyle (cardanoEra :: CardanoEra era) of
      LegacyByronEra      -> makeByronTransactionBody
      ShelleyBasedEra era -> makeShelleyTransactionBody era


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
  -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))
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
    | input <- Set.toList (inputs era body) ]
  where
    inputs :: ShelleyBasedEra era
           -> Ledger.TxBody (ShelleyLedgerEra era)
           -> Set (Ledger.TxIn StandardCrypto)
    inputs ShelleyBasedEraShelley = Shelley._inputs
    inputs ShelleyBasedEraAllegra = Allegra.inputs'
    inputs ShelleyBasedEraMary    = Mary.inputs'
    inputs ShelleyBasedEraAlonzo  = Alonzo.inputs'
    inputs ShelleyBasedEraBabbage = Babbage.inputs


fromLedgerTxInsCollateral
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxInsCollateral era
fromLedgerTxInsCollateral era body =
    case collateralSupportedInEra (shelleyBasedToCardanoEra era) of
      Nothing        -> TxInsCollateralNone
      Just supported ->
        TxInsCollateral supported $ map fromShelleyTxIn collateral
  where
    collateral :: [Ledger.TxIn StandardCrypto]
    collateral = case era of
      ShelleyBasedEraShelley -> []
      ShelleyBasedEraAllegra -> []
      ShelleyBasedEraMary    -> []
      ShelleyBasedEraAlonzo  -> toList $ Alonzo.collateral' body
      ShelleyBasedEraBabbage -> toList $ Babbage.collateral body

fromLedgerTxInsReference
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxInsReference ViewTx era
fromLedgerTxInsReference era txBody =
  case refInsScriptsAndInlineDatsSupportedInEra $ shelleyBasedToCardanoEra era of
    Nothing -> TxInsReferenceNone
    Just suppInEra ->
      let ledgerRefInputs = obtainReferenceInputsHasFieldConstraint suppInEra $ getField @"referenceInputs" txBody
      in TxInsReference suppInEra
           $ map fromShelleyTxIn . Set.toList $ ledgerRefInputs
 where
  obtainReferenceInputsHasFieldConstraint
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
    -> (HasField "referenceInputs" (Ledger.TxBody (ShelleyLedgerEra era)) (Set (Ledger.TxIn StandardCrypto)) => a)
    -> a
  obtainReferenceInputsHasFieldConstraint ReferenceTxInsScriptsInlineDatumsInBabbageEra f = f

fromLedgerTxOuts
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxBodyScriptData era
  -> [TxOut CtxTx era]
fromLedgerTxOuts era body scriptdata =
  case era of
    ShelleyBasedEraShelley ->
      [ fromShelleyTxOut era txout | txout <- toList (Shelley._outputs body) ]

    ShelleyBasedEraAllegra ->
      [ fromShelleyTxOut era txout | txout <- toList (Allegra.outputs' body) ]

    ShelleyBasedEraMary ->
      [ fromShelleyTxOut era txout | txout <- toList (Mary.outputs' body) ]

    ShelleyBasedEraAlonzo ->
      [ fromAlonzoTxOut
          MultiAssetInAlonzoEra
          ScriptDataInAlonzoEra
          txdatums
          txout
      | let txdatums = selectTxDatums scriptdata
      , txout <- toList (Alonzo.outputs' body) ]

    ShelleyBasedEraBabbage ->
      [ fromBabbageTxOut
          MultiAssetInBabbageEra
          ScriptDataInBabbageEra
          ReferenceTxInsScriptsInlineDatumsInBabbageEra
          txdatums
          (CBOR.sizedValue txouts)
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (Babbage.outputs body)
      ]
  where
    selectTxDatums  TxBodyNoScriptData                            = Map.empty
    selectTxDatums (TxBodyScriptData _ (Alonzo.TxDats' datums) _) = datums

fromAlonzoTxOut :: forall era ledgerera.
                   IsShelleyBasedEra era
                => Ledger.Era ledgerera
                => Ledger.Crypto ledgerera ~ StandardCrypto
                => Ledger.Value ledgerera ~ Mary.Value StandardCrypto
                => MultiAssetSupportedInEra era
                -> ScriptDataSupportedInEra era
                -> Map (Alonzo.DataHash StandardCrypto)
                       (Alonzo.Data ledgerera)
                -> Alonzo.TxOut ledgerera
                -> TxOut CtxTx era
fromAlonzoTxOut multiAssetInEra scriptDataInEra txdatums
                (Alonzo.TxOut addr value datahash) =
   TxOut (fromShelleyAddr shelleyBasedEra addr)
         (TxOutValue multiAssetInEra (fromMaryValue value))
         (fromAlonzoTxOutDatum scriptDataInEra datahash)
         ReferenceScriptNone
  where
    fromAlonzoTxOutDatum :: ScriptDataSupportedInEra era
                         -> StrictMaybe (Alonzo.DataHash StandardCrypto)
                         -> TxOutDatum CtxTx era
    fromAlonzoTxOutDatum _          SNothing = TxOutDatumNone
    fromAlonzoTxOutDatum supported (SJust dh)
      | Just d <- Map.lookup dh txdatums
                  = TxOutDatumInTx' supported (ScriptDataHash dh)
                                              (fromAlonzoData d)
      | otherwise = TxOutDatumHash supported (ScriptDataHash dh)

fromBabbageTxOut
  :: forall ledgerera era. Ledger.Era ledgerera
  => IsShelleyBasedEra era
  => ShelleyLedgerEra era ~ ledgerera
  => Ledger.Crypto ledgerera ~ StandardCrypto
  => Ledger.Value ledgerera ~ Mary.Value StandardCrypto
  => MultiAssetSupportedInEra era
  -> ScriptDataSupportedInEra era
  -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -> Map (Alonzo.DataHash StandardCrypto)
         (Alonzo.Data ledgerera)
  -> Babbage.TxOut ledgerera
  -> TxOut CtxTx era
fromBabbageTxOut multiAssetInEra scriptDataInEra inlineDatumsInEra txdatums txout =
   TxOut
     (fromShelleyAddr shelleyBasedEra addr)
     (TxOutValue multiAssetInEra (fromMaryValue val))
     babbageTxOutDatum
     (case mRefScript of
       SNothing -> ReferenceScriptNone
       SJust rScript -> fromShelleyScriptToReferenceScript shelleyBasedEra rScript
     )
 where
   -- NOTE: This is different to 'fromBabbageTxOutDatum' as it may resolve
   -- 'DatumHash' values using the datums included in the transaction.
   babbageTxOutDatum :: TxOutDatum CtxTx era
   babbageTxOutDatum =
     case datum of
       Babbage.NoDatum -> TxOutDatumNone
       Babbage.DatumHash dh -> resolveDatumInTx dh
       Babbage.Datum d ->
         TxOutDatumInline inlineDatumsInEra $
           binaryDataToScriptData inlineDatumsInEra d

   resolveDatumInTx :: Alonzo.DataHash StandardCrypto -> TxOutDatum CtxTx era
   resolveDatumInTx dh
      | Just d <- Map.lookup dh txdatums
                  = TxOutDatumInTx' scriptDataInEra (ScriptDataHash dh) (fromAlonzoData d)
      | otherwise = TxOutDatumHash scriptDataInEra (ScriptDataHash dh)

   (Babbage.TxOut addr val datum mRefScript) = txout

fromLedgerTxTotalCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxTotalCollateral era
fromLedgerTxTotalCollateral era txbody =
  case totalAndReturnCollateralSupportedInEra $ shelleyBasedToCardanoEra era of
    Nothing -> TxTotalCollateralNone
    Just supp ->
      case obtainTotalCollateralHasFieldConstraint supp $ getField @"totalCollateral" txbody of
        SNothing -> TxTotalCollateralNone
        SJust totColl -> TxTotalCollateral supp $ fromShelleyLovelace totColl
 where
  obtainTotalCollateralHasFieldConstraint
    :: TxTotalAndReturnCollateralSupportedInEra era
    -> (HasField "totalCollateral" (Ledger.TxBody (ShelleyLedgerEra era)) (StrictMaybe Ledger.Coin) => a)
    -> a
  obtainTotalCollateralHasFieldConstraint TxTotalAndReturnCollateralInBabbageEra f = f

fromLedgerTxReturnCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxReturnCollateral CtxTx era
fromLedgerTxReturnCollateral era txbody =
  case totalAndReturnCollateralSupportedInEra $ shelleyBasedToCardanoEra era of
    Nothing -> TxReturnCollateralNone
    Just supp ->
      case obtainCollateralReturnHasFieldConstraint supp $ getField @"collateralReturn" txbody of
        SNothing -> TxReturnCollateralNone
        SJust collReturnOut ->
          TxReturnCollateral supp $ fromShelleyTxOut era collReturnOut
 where
  obtainCollateralReturnHasFieldConstraint
    :: TxTotalAndReturnCollateralSupportedInEra era
    -> (HasField "collateralReturn"
          (Ledger.TxBody (ShelleyLedgerEra era))
          (StrictMaybe (Ledger.TxOut (ShelleyLedgerEra era))) => a)
    -> a
  obtainCollateralReturnHasFieldConstraint TxTotalAndReturnCollateralInBabbageEra f = f


fromLedgerTxFee
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxFee era
fromLedgerTxFee era body =
  case era of
    ShelleyBasedEraShelley ->
      TxFeeExplicit TxFeesExplicitInShelleyEra $
      fromShelleyLovelace $ Shelley._txfee body
    ShelleyBasedEraAllegra ->
      TxFeeExplicit TxFeesExplicitInAllegraEra $
      fromShelleyLovelace $ Allegra.txfee' body
    ShelleyBasedEraMary ->
      TxFeeExplicit TxFeesExplicitInMaryEra $
      fromShelleyLovelace $ Mary.txfee' body
    ShelleyBasedEraAlonzo ->
      TxFeeExplicit TxFeesExplicitInAlonzoEra $
      fromShelleyLovelace $ Alonzo.txfee' body
    ShelleyBasedEraBabbage ->
      TxFeeExplicit TxFeesExplicitInBabbageEra $
      fromShelleyLovelace $ Babbage.txfee body

fromLedgerTxValidityRange
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
fromLedgerTxValidityRange era body =
  case era of
    ShelleyBasedEraShelley ->
      ( TxValidityNoLowerBound
      , TxValidityUpperBound ValidityUpperBoundInShelleyEra $ Shelley._ttl body
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
        Allegra.ValidityInterval{invalidBefore, invalidHereafter} =
          Allegra.vldt' body

    ShelleyBasedEraMary ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInMaryEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInMaryEra s
      )
      where
        Mary.ValidityInterval{invalidBefore, invalidHereafter} = Mary.vldt' body

    ShelleyBasedEraAlonzo ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInAlonzoEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAlonzoEra s
      )
      where
        Mary.ValidityInterval{invalidBefore, invalidHereafter} = Alonzo.vldt' body

    ShelleyBasedEraBabbage ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInBabbageEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInBabbageEra s
      )
      where
        Mary.ValidityInterval{invalidBefore, invalidHereafter} = Babbage.txvldt body

fromLedgerAuxiliaryData
  :: ShelleyBasedEra era
  -> Ledger.AuxiliaryData (ShelleyLedgerEra era)
  -> (Map Word64 TxMetadataValue, [ScriptInEra era])
fromLedgerAuxiliaryData ShelleyBasedEraShelley (Shelley.Metadata metadata) =
  (fromShelleyMetadata metadata, [])
fromLedgerAuxiliaryData ShelleyBasedEraAllegra (Allegra.AuxiliaryData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraAllegra <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraMary (Mary.AuxiliaryData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraMary <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraAlonzo (Alonzo.AuxiliaryData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraAlonzo <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraBabbage (Alonzo.AuxiliaryData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraBabbage <$> toList ss
  )

fromLedgerTxAuxiliaryData
  :: ShelleyBasedEra era
  -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))
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
        keyhashes = Alonzo.reqSignerHashes body
    ShelleyBasedEraBabbage
      | Set.null keyhashes -> TxExtraKeyWitnessesNone
      | otherwise          -> TxExtraKeyWitnesses
                                ExtraKeyWitnessesInBabbageEra
                                [ PaymentKeyHash (Shelley.coerceKeyRole keyhash)
                                | keyhash <- Set.toList keyhashes ]
      where
        keyhashes = Babbage.reqSignerHashes body

fromLedgerTxWithdrawals
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxWithdrawals ViewTx era
fromLedgerTxWithdrawals era body =
  case era of
    ShelleyBasedEraShelley
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInShelleyEra $
          fromShelleyWithdrawal withdrawals
      where
        withdrawals = Shelley._wdrls body

    ShelleyBasedEraAllegra
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInAllegraEra $
          fromShelleyWithdrawal withdrawals
      where
        withdrawals = Allegra.wdrls' body

    ShelleyBasedEraMary
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInMaryEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = Mary.wdrls' body

    ShelleyBasedEraAlonzo
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInAlonzoEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = Alonzo.wdrls' body

    ShelleyBasedEraBabbage
      | null (Shelley.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInBabbageEra $ fromShelleyWithdrawal withdrawals
      where
        withdrawals = Babbage.wdrls' body

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
        certificates = Shelley._certs body

    ShelleyBasedEraAllegra
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInAllegraEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Allegra.certs' body

    ShelleyBasedEraMary
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInMaryEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Mary.certs' body

    ShelleyBasedEraAlonzo
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInAlonzoEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Alonzo.certs' body

    ShelleyBasedEraBabbage
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInBabbageEra
            (map fromShelleyCertificate $ toList certificates)
            ViewTx
      where
        certificates = Babbage.certs' body

fromLedgerTxUpdateProposal
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxUpdateProposal era
fromLedgerTxUpdateProposal era body =
  case era of
    ShelleyBasedEraShelley ->
      case Shelley._txUpdate body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInShelleyEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraAllegra ->
      case Allegra.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInAllegraEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraMary ->
      case Mary.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInMaryEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraAlonzo ->
      case Alonzo.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInAlonzoEra
                           (fromLedgerUpdate era p)

    ShelleyBasedEraBabbage ->
      case Babbage.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInBabbageEra
                           (fromLedgerUpdate era p)

fromLedgerTxMintValue
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxMintValue ViewTx era
fromLedgerTxMintValue era body =
  case era of
    ShelleyBasedEraShelley -> TxMintNone
    ShelleyBasedEraAllegra -> TxMintNone
    ShelleyBasedEraMary
      | isZero mint        -> TxMintNone
      | otherwise          -> TxMintValue MultiAssetInMaryEra
                                          (fromMaryValue mint) ViewTx
      where
        mint = Mary.mint' body

    ShelleyBasedEraAlonzo
      | isZero mint         -> TxMintNone
      | otherwise           -> TxMintValue MultiAssetInAlonzoEra
                                           (fromMaryValue mint) ViewTx
      where
        mint = Alonzo.mint' body

    ShelleyBasedEraBabbage
      | isZero mint         -> TxMintNone
      | otherwise           -> TxMintValue MultiAssetInBabbageEra
                                           (fromMaryValue mint) ViewTx
      where
        mint = Babbage.mint' body


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
        reAnnotate $
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
    TxBodyContent {
      txIns              = [ (fromByronTxIn input, ViewTx)
                           | input <- toList txInputs],
      txInsCollateral    = TxInsCollateralNone,
      txInsReference     = TxInsReferenceNone,
      txOuts             = fromByronTxOut <$> toList txOutputs,
      txReturnCollateral = TxReturnCollateralNone,
      txTotalCollateral  = TxTotalCollateralNone,
      txFee              = TxFeeImplicit TxFeesImplicitInByronEra,
      txValidityRange    = (TxValidityNoLowerBound,
                            TxValidityNoUpperBound
                              ValidityNoUpperBoundInByronEra),
      txMetadata         = TxMetadataNone,
      txAuxScripts       = TxAuxScriptsNone,
      txExtraKeyWits     = TxExtraKeyWitnessesNone,
      txProtocolParams   = ViewTx,
      txWithdrawals      = TxWithdrawalsNone,
      txCertificates     = TxCertificatesNone,
      txUpdateProposal   = TxUpdateProposalNone,
      txMintValue        = TxMintNone,
      txScriptValidity   = TxScriptValidityNone
    }

makeShelleyTransactionBody :: ()
  => ShelleyBasedEra era
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

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (lovelaceToQuantity v)
                                                  (txOutInAnyEra txout)
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (lovelaceToQuantity v)
                                                         (txOutInAnyEra txout)
           for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
              guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Lovelace
      , txout@(TxOut _ (TxOutAdaOnly AdaOnlyInShelleyEra v) _ _) <- txOuts ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

    return $
      ShelleyTxBody era
        (Shelley.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map (toShelleyTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (case upperBound of
             TxValidityNoUpperBound era' -> case era' of {}
             TxValidityUpperBound _ ttl  -> ttl)
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData)))
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where

    maxShelleyTxInIx :: Word
    maxShelleyTxInIx = fromIntegral $ maxBound @Word16

    scripts :: [Ledger.Script StandardShelley]
    scripts = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardShelley)
    txAuxData
      | Map.null ms = Nothing
      | otherwise   = Just (toShelleyAuxiliaryData ms)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'

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

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (lovelaceToQuantity v)
                                                  (txOutInAnyEra txout)
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (lovelaceToQuantity v)
                                                         (txOutInAnyEra txout)
           for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
              guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Lovelace
      , txout@(TxOut _ (TxOutAdaOnly AdaOnlyInAllegraEra v) _ _) <- txOuts
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map (toShelleyTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData))
          mempty) -- No minting in Allegra, only Mary
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where

    maxShelleyTxInIx :: Word
    maxShelleyTxInIx = fromIntegral $ maxBound @Word16

    scripts :: [Ledger.Script StandardAllegra]
    scripts = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardAllegra)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAllegraAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

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

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
           for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
              guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInMaryEra v) _ _) <- txOuts
      , let allPositive =
              case [ q | (_,q) <- valueToList v, q < 0 ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputNegative q (txOutInAnyEra txout))
            allWithinMaxBound =
              case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputOverflow q (txOutInAnyEra txout))
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map (toShelleyTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData))
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toMaryValue v))
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where

    maxShelleyTxInIx :: Word
    maxShelleyTxInIx = fromIntegral $ maxBound @Word16

    scripts :: [Ledger.Script StandardMary]
    scripts = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardMary)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAllegraAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

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

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
           for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
              guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInAlonzoEra v) _ _) <- txOuts
      , let allPositive =
              case [ q | (_,q) <- valueToList v, q < 0 ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputNegative q (txOutInAnyEra txout))
            allWithinMaxBound =
              case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputOverflow q (txOutInAnyEra txout))
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError
    case txInsCollateral of
      TxInsCollateralNone | not (Set.null languages)
        -> Left TxBodyEmptyTxInsCollateral
      _ -> return ()
    case txProtocolParams of
      BuildTxWith Nothing | not (Set.null languages)
        -> Left TxBodyMissingProtocolParams
      _ -> return () --TODO alonzo: validate protocol params for the Alonzo era.
                     --             All the necessary params must be provided.

    return $
      ShelleyTxBody era
        (Alonzo.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (case txInsCollateral of
             TxInsCollateralNone     -> Set.empty
             TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins))
          (Seq.fromList (map (toShelleyTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (case txExtraKeyWits of
             TxExtraKeyWitnessesNone   -> Set.empty
             TxExtraKeyWitnesses _ khs -> Set.fromList
                                            [ Shelley.coerceKeyRole kh
                                            | PaymentKeyHash kh <- khs ])
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toMaryValue v)
          (case txProtocolParams of
             BuildTxWith Nothing        -> SNothing
             BuildTxWith (Just pparams) ->
               Alonzo.hashScriptIntegrity
                 (Set.map
                    (Alonzo.getLanguageView (toLedgerPParams ShelleyBasedEraAlonzo pparams))
                    languages
                 )
                 redeemers
                 datums)
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData))
          SNothing) -- TODO alonzo: support optional network id in TxBodyContent
        scripts
        (TxBodyScriptData ScriptDataInAlonzoEra datums redeemers)
        txAuxData
        txScriptValidity
  where

    maxShelleyTxInIx :: Word
    maxShelleyTxInIx = fromIntegral $ maxBound @Word16

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AlonzoEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardAlonzo]
    scripts = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: Alonzo.TxDats StandardAlonzo
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (Alonzo.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [ScriptData]
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

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardAlonzo)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAlonzoAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

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
    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
           for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
              guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInBabbageEra v) _ _) <- txOuts
      , let allPositive =
              case [ q | (_,q) <- valueToList v, q < 0 ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputNegative q (txOutInAnyEra txout))
            allWithinMaxBound =
              case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputOverflow q (txOutInAnyEra txout))
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError
    case txInsCollateral of
      TxInsCollateralNone | not (Set.null languages)
        -> Left TxBodyEmptyTxInsCollateral
      _ -> return ()
    case txProtocolParams of
      BuildTxWith Nothing | not (Set.null languages)
        -> Left TxBodyMissingProtocolParams
      _ -> return () --TODO alonzo: validate protocol params for the Babbage era.
                     --             All the necessary params must be provided.

    return $
      ShelleyTxBody era
        (Babbage.TxBody
           { Babbage.inputs = Set.fromList $ map (toShelleyTxIn . fst) txIns
           , Babbage.collateral =
               case txInsCollateral of
                TxInsCollateralNone     -> Set.empty
                TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)
           , Babbage.referenceInputs =
               Set.fromList (map toShelleyTxIn referenceTxIns)

           , Babbage.outputs = Seq.fromList (map (CBOR.mkSized . toShelleyTxOutAny era) txOuts)
           , Babbage.collateralReturn =
               case txReturnCollateral of
                 TxReturnCollateralNone -> SNothing
                 TxReturnCollateral _ colTxOut -> SJust $ CBOR.mkSized $ toShelleyTxOutAny era colTxOut
           , Babbage.totalCollateral =
               case txTotalCollateral of
                 TxTotalCollateralNone -> SNothing
                 TxTotalCollateral _ totCollLovelace -> SJust $ toShelleyLovelace totCollLovelace
           , Babbage.txcerts =
               case txCertificates of
                 TxCertificatesNone    -> Seq.empty
                 TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs)
           , Babbage.txwdrls =
               case txWithdrawals of
                 TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
                 TxWithdrawals _ ws -> toShelleyWithdrawal ws
           , Babbage.txfee =
               case txFee of
                 TxFeeImplicit era'  -> case era' of {}
                 TxFeeExplicit _ fee -> toShelleyLovelace fee
           , Babbage.txvldt =
               Allegra.ValidityInterval {
                 invalidBefore    = case lowerBound of
                                              TxValidityNoLowerBound   -> SNothing
                                              TxValidityLowerBound _ s -> SJust s,
                 invalidHereafter = case upperBound of
                                              TxValidityNoUpperBound _ -> SNothing
                                              TxValidityUpperBound _ s -> SJust s}
           , Babbage.txUpdates =
               case txUpdateProposal of
                 TxUpdateProposalNone -> SNothing
                 TxUpdateProposal _ p -> SJust (toLedgerUpdate era p)
           , Babbage.reqSignerHashes =
               case txExtraKeyWits of
                 TxExtraKeyWitnessesNone   -> Set.empty
                 TxExtraKeyWitnesses _ khs -> Set.fromList
                                                [ Shelley.coerceKeyRole kh
                                                | PaymentKeyHash kh <- khs ]
           , Babbage.mint =
               case txMintValue of
                 TxMintNone        -> mempty
                 TxMintValue _ v _ -> toMaryValue v
           , Babbage.scriptIntegrityHash =
               case txProtocolParams of
                 BuildTxWith Nothing        -> SNothing
                 BuildTxWith (Just pparams) ->
                    Alonzo.hashScriptIntegrity
                      (Set.map
                         (Alonzo.getLanguageView (toLedgerPParams ShelleyBasedEraBabbage pparams))
                         languages
                      )
                      redeemers
                      datums
           , Babbage.adHash =
                maybeToStrictMaybe (Ledger.hashAuxiliaryData <$> txAuxData)
           , Babbage.txnetworkid = SNothing
           })
        scripts
        (TxBodyScriptData ScriptDataInBabbageEra
          datums redeemers)
        txAuxData
        txScriptValidity
  where
    referenceTxIns :: [TxIn]
    referenceTxIns =
      case txInsReference of
        TxInsReferenceNone -> []
        TxInsReference _ refTxins -> refTxins

    maxShelleyTxInIx :: Word
    maxShelleyTxInIx = fromIntegral $ maxBound @Word16

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness BabbageEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardBabbage]
    scripts = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats StandardBabbage
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (Alonzo.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [ScriptData]
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

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardBabbage)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAlonzoAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'


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
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOutAny _ (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _ _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOutAny _ (TxOut addr (TxOutValue MultiAssetInMaryEra value) _ _) =
    Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOutAny _ (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata _) =
    Alonzo.TxOut (toShelleyAddr addr) (toMaryValue value)
                 (toAlonzoTxOutDataHash' txoutdata)

toShelleyTxOutAny era (TxOut addr (TxOutValue MultiAssetInBabbageEra value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra era
    in Babbage.TxOut (toShelleyAddr addr) (toMaryValue value)
                    (toBabbageTxOutDatum' txoutdata) (refScriptToShelleyScript cEra refScript)


toAlonzoTxOutDataHash' :: TxOutDatum ctx AlonzoEra
                       -> StrictMaybe (Alonzo.DataHash StandardCrypto)
toAlonzoTxOutDataHash'  TxOutDatumNone                          = SNothing
toAlonzoTxOutDataHash' (TxOutDatumHash _ (ScriptDataHash dh))   = SJust dh
toAlonzoTxOutDataHash' (TxOutDatumInTx' _ (ScriptDataHash dh) _) = SJust dh
toAlonzoTxOutDataHash' (TxOutDatumInline inlineDatumSupp _sd) =
  case inlineDatumSupp :: ReferenceTxInsScriptsInlineDatumsSupportedInEra AlonzoEra of {}

-- TODO: Consolidate with alonzo function and rename
toBabbageTxOutDatum'
  :: Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto
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

renderScriptWitnessIndex :: ScriptWitnessIndex -> String
renderScriptWitnessIndex (ScriptWitnessIndexTxIn index) =
  "transaction input " <> show index <> " (in the order of the TxIds)"
renderScriptWitnessIndex (ScriptWitnessIndexMint index) =
  "policyId " <> show index <> " (in the order of the PolicyIds)"
renderScriptWitnessIndex (ScriptWitnessIndexCertificate index) =
  "certificate " <> show index <> " (in the list order of the certificates)"
renderScriptWitnessIndex (ScriptWitnessIndexWithdrawal index) =
  "withdrawal " <> show index <> " (in the order of the StakeAddresses)"

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
        StakeAddressDelegationCertificate     stakecred _ -> Just stakecred
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


toShelleyWithdrawal :: [(StakeAddress, Lovelace, a)] -> Shelley.Wdrl StandardCrypto
toShelleyWithdrawal withdrawals =
    Shelley.Wdrl $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value, _) <- withdrawals ]


fromShelleyWithdrawal
  :: Shelley.Wdrl StandardCrypto
  -> [(StakeAddress, Lovelace, BuildTxWith ViewTx (Witness WitCtxStake era))]
fromShelleyWithdrawal (Shelley.Wdrl withdrawals) =
  [ (fromShelleyStakeAddr stakeAddr, fromShelleyLovelace value, ViewTx)
  | (stakeAddr, value) <- Map.assocs withdrawals
  ]


-- | In the Shelley era the auxiliary data consists only of the tx metadata
toShelleyAuxiliaryData :: Map Word64 TxMetadataValue
                       -> Ledger.AuxiliaryData StandardShelley
toShelleyAuxiliaryData m =
    Shelley.Metadata
      (toShelleyMetadata m)


-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts.
--
toAllegraAuxiliaryData :: forall era ledgerera.
                          ShelleyLedgerEra era ~ ledgerera
                       => Ledger.AuxiliaryData ledgerera ~ Allegra.AuxiliaryData ledgerera
                       => Ledger.AnnotatedData (Ledger.Script ledgerera)
                       => Ord (Ledger.Script ledgerera)
                       => Map Word64 TxMetadataValue
                       -> [ScriptInEra era]
                       -> Ledger.AuxiliaryData ledgerera
toAllegraAuxiliaryData m ss =
    Allegra.AuxiliaryData
      (toShelleyMetadata m)
      (Seq.fromList (map toShelleyScript ss))


-- | In the Alonzo and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
--
toAlonzoAuxiliaryData :: forall era ledgerera.
                         ShelleyLedgerEra era ~ ledgerera
                      => Ledger.AuxiliaryData ledgerera ~ Alonzo.AuxiliaryData ledgerera
                      => Ledger.Script ledgerera ~ Alonzo.Script ledgerera
                      => Ledger.Era ledgerera
                      => Map Word64 TxMetadataValue
                      -> [ScriptInEra era]
                      -> Ledger.AuxiliaryData ledgerera
toAlonzoAuxiliaryData m ss =
    Alonzo.AuxiliaryData
      (toShelleyMetadata m)
      (Seq.fromList (map toShelleyScript ss))


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
    addr :: Shelley.Addr StandardCrypto
    addr = Shelley.Addr
             (toShelleyNetwork nw)
             (Shelley.KeyHashObj kh)
             Shelley.StakeRefNull

calculateExecutionUnitsLovelace :: ExecutionUnitPrices -> ExecutionUnits -> Maybe Lovelace
calculateExecutionUnitsLovelace euPrices eUnits =
  case toAlonzoPrices euPrices of
    Nothing -> Nothing
    Just prices ->
      return . fromShelleyLovelace $ Alonzo.txscriptfee prices (toAlonzoExUnits eUnits)

-- ----------------------------------------------------------------------------
-- Inline data
--
-- | Conversion of ScriptData to binary data which allows for the storage of data
-- onchain within a transaction output.
--

scriptDataToInlineDatum :: ScriptData -> Babbage.Datum ledgerera
scriptDataToInlineDatum = Babbage.Datum . Alonzo.dataToBinaryData . toAlonzoData

binaryDataToScriptData
  :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> Alonzo.BinaryData ledgerera -> ScriptData
binaryDataToScriptData ReferenceTxInsScriptsInlineDatumsInBabbageEra  d =
  fromAlonzoData $ Alonzo.binaryDataToData d


