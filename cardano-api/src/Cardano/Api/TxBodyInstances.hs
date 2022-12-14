{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use section" -}

-- | Transaction bodies
--
module Cardano.Api.TxBodyInstances
  ( serialiseShelleyBasedTxBody
  , deserialiseShelleyBasedTxBody
  ) where

import           Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Typeable (Typeable)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Serialization as CBOR

import           Cardano.Api.Eras
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.Tx
import           Cardano.Api.TxBody

instance IsCardanoEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> ["TxUnsignedByron", "Unwitnessed Tx ByronEra"]
        ShelleyEra -> ["TxUnsignedShelley", "Unwitnessed Tx ShelleyEra"]
        AllegraEra -> ["TxBodyAllegra", "Unwitnessed Tx AllegraEra"]
        MaryEra    -> ["TxBodyMary", "Unwitnessed Tx MaryEra"]
        AlonzoEra  -> ["TxBodyAlonzo", "Unwitnessed Tx AlonzoEra"]
        BabbageEra -> ["TxBodyBabbage", "Unwitnessed Tx BabbageEra"]
    textEnvelopeDefaultDescr _ = "Ledger Cddl Format"

instance IsCardanoEra era => SerialiseAsCBOR (TxBody era) where

    serialiseToCBOR (ByronTxBody txbody) =
      CBOR.recoverBytes txbody

    serialiseToCBOR txbody =
      let tx = makeSignedTransaction [] txbody
      in serialiseToCBOR tx

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron TxBody"
              CBOR.fromCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> do
          tx <- deserialiseShelleyBasedTx (ShelleyTx ShelleyBasedEraShelley) bs
          let (txBody, _) = getTxBodyAndWitnesses tx
          return txBody
        AllegraEra -> do
          tx <- deserialiseShelleyBasedTx (ShelleyTx ShelleyBasedEraAllegra) bs
          let (txBody, _) = getTxBodyAndWitnesses tx
          return txBody
        MaryEra    -> do
          tx <- deserialiseShelleyBasedTx (ShelleyTx ShelleyBasedEraMary) bs
          let (txBody, _) = getTxBodyAndWitnesses tx
          return txBody
        AlonzoEra  -> do
          tx <- deserialiseShelleyBasedTx (ShelleyTx ShelleyBasedEraAlonzo) bs
          let (txBody, _) = getTxBodyAndWitnesses tx
          return txBody
        BabbageEra -> do
          tx <- deserialiseShelleyBasedTx (ShelleyTx ShelleyBasedEraBabbage) bs
          let (txBody, _) = getTxBodyAndWitnesses tx
          return txBody

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
serialiseShelleyBasedTxBody
  :: ToCBOR (Ledger.TxBody (ShelleyLedgerEra era))
  => ToCBOR (Ledger.Script (ShelleyLedgerEra era))
  => ToCBOR (Ledger.AuxiliaryData (ShelleyLedgerEra era))
  => Typeable (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> TxBody era
  -> BS.ByteString
serialiseShelleyBasedTxBody _ (ShelleyTxBody era txbody txscripts
                            TxBodyNoScriptData txmetadata scriptValidity) =
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

serialiseShelleyBasedTxBody _ (ShelleyTxBody _era txbody txscripts
                            (TxBodyScriptData _ datums redeemers)
                            txmetadata txBodycriptValidity) =
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
  -> BS.ByteString
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
