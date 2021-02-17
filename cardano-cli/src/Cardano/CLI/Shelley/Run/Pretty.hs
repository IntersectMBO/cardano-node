{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Pretty (prettyTx) where

import           Cardano.Api (ShelleyBasedEra (..))
import           Cardano.Api.Byron (TxBody (ByronTxBody))
import           Cardano.Api.Shelley (TxBody (ShelleyTxBody))
import           Cardano.CLI.Helpers (textShow)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMA
import           Cardano.Prelude
import           Data.Aeson as JSON (Value, object, (.=))
import           Data.Aeson.Encode.Pretty (Config (confCompare), defConfig, encodePretty')
import qualified Shelley.Spec.Ledger.API as Shelley

prettyTx :: TxBody era -> LByteString
prettyTx body0 =
  encodePretty' defConfig{confCompare = compare} $
  object $
  case body0 of
    ByronTxBody tx -> ["era" .= ("Byron" :: Text), "tx" .= tx]
    ShelleyTxBody ShelleyBasedEraShelley body aux ->
      [ "era" .= ("Shelley" :: Text)
      , "inputs" .= _inputs
      , "outputs" .= _outputs
      , "certificates" .= fmap textShow _certs
      , "withdrawals" .= withdrawals
      , "fee" .= _txfee
      , "timetolive" .= _ttl
      , "update" .= fmap textShow _txUpdate
      , "metadata_hash" .= fmap textShow _mdHash
      , "auxiliary_data" .= fmap textShow aux
      ]
      where
        Shelley.TxBody
          { _inputs
          , _outputs
          , _certs
          , _wdrls
          , _txfee
          , _ttl
          , _txUpdate
          , _mdHash
          } =
            body
        Shelley.Wdrl withdrawals = _wdrls
    ShelleyTxBody ShelleyBasedEraAllegra body aux ->
      [ "era" .= ("Allegra" :: Text)
      , "inputs" .= inputs
      , "outputs" .= outputs
      , "certificates" .= fmap textShow certificates
      , "withdrawals" .= withdrawals
      , "fee" .= txfee
      , "validity_interval" .= prettyValidityInterval validity
      , "update" .= fmap textShow update
      , "auxiliary_data_hash" .= fmap textShow adHash
      , "mint" .= mint
      , "auxiliary_data" .= fmap textShow aux
      ]
      where
        ShelleyMA.TxBody
          inputs
          outputs
          certificates
          (Shelley.Wdrl withdrawals)
          txfee
          validity
          update
          adHash
          mint =
            body
    ShelleyTxBody ShelleyBasedEraMary body aux ->
      [ "era" .= ("Mary" :: Text)
      , "inputs" .= inputs
      , "outputs" .= outputs
      , "certificates" .= fmap textShow certificates
      , "withdrawals" .= withdrawals
      , "fee" .= txfee
      , "validity_interval" .= prettyValidityInterval validity
      , "update" .= fmap textShow update
      , "auxiliary_data_hash" .= fmap textShow adHash
      , "mint" .= mint
      , "auxiliary_data" .= fmap textShow aux
      ]
      where
        ShelleyMA.TxBody
          inputs
          outputs
          certificates
          (Shelley.Wdrl withdrawals)
          txfee
          validity
          update
          adHash
          mint =
            body
    ShelleyTxBody ShelleyBasedEraAlonzo _body _aux -> panic "TODO"


prettyValidityInterval :: ShelleyMA.ValidityInterval -> JSON.Value
prettyValidityInterval
  ShelleyMA.ValidityInterval{invalidBefore, invalidHereafter} =
    object
      [ "invalid_before" .= invalidBefore
      , "invalid_hereafter" .= invalidHereafter
      ]
