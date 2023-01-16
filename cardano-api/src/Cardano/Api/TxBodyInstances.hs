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
module Cardano.Api.TxBodyInstances () where

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
