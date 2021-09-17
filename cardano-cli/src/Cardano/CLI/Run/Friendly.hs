{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBodyBS) where

import           Cardano.Prelude

import           Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import           Data.Yaml (array)
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)

import           Cardano.Api
import           Cardano.Api.Byron (Lovelace (..))
import           Cardano.Api.Shelley (Address (ShelleyAddress), StakeAddress (..))
import           Cardano.Ledger.Crypto (Crypto)
import qualified Shelley.Spec.Ledger.API as Shelley

import           Cardano.CLI.Helpers (textShow)

friendlyTxBodyBS :: CardanoEra era -> TxBody era -> ByteString
friendlyTxBodyBS era =
  encodePretty (setConfCompare compare defConfig) . friendlyTxBody era

friendlyTxBody :: CardanoEra era -> TxBody era -> Aeson.Value
friendlyTxBody
  era
  (TxBody
    TxBodyContent
      { txAuxScripts
      , txCertificates
      , txFee
      , txIns
      , txMetadata
      , txMintValue
      , txOuts
      , txUpdateProposal
      , txValidityRange
      , txWithdrawals
      }) =
  object
    [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
    , "certificates" .= friendlyCertificates txCertificates
    , "era" .= era
    , "fee" .= friendlyFee txFee
    , "inputs" .= friendlyInputs txIns
    , "metadata" .= friendlyMetadata txMetadata
    , "mint" .= friendlyMintValue txMintValue
    , "outputs" .= map friendlyTxOut txOuts
    , "update proposal" .= friendlyUpdateProposal txUpdateProposal
    , "validity range" .= friendlyValidityRange era txValidityRange
    , "withdrawals" .= friendlyWithdrawals txWithdrawals
    ]

-- | Special case of validity range:
-- in Shelley, upper bound is TTL, and no lower bound
pattern ShelleyTtl
  :: SlotNo -> (TxValidityLowerBound era, TxValidityUpperBound era)
pattern ShelleyTtl ttl <-
  ( TxValidityNoLowerBound
  , TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
  )

friendlyValidityRange
  :: CardanoEra era
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
  -> Aeson.Value
friendlyValidityRange era = \case
  ShelleyTtl ttl -> object ["time to live" .= ttl]
  (lowerBound, upperBound)
    | isLowerBoundSupported || isUpperBoundSupported ->
        object
          [ "lower bound" .=
                case lowerBound of
                  TxValidityNoLowerBound -> Null
                  TxValidityLowerBound _ s -> toJSON s
          , "upper bound" .=
              case upperBound of
                TxValidityNoUpperBound _ -> Null
                TxValidityUpperBound _ s -> toJSON s
          ]
    | otherwise -> Null
  where
    isLowerBoundSupported = isJust $ validityLowerBoundSupportedInEra era
    isUpperBoundSupported = isJust $ validityUpperBoundSupportedInEra era

friendlyWithdrawals :: TxWithdrawals ViewTx era -> Aeson.Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object
        [ "address" .= serialiseAddress addr
        , "network" .= net
        , "credential" .= cred
        , "amount" .= friendlyLovelace amount
        ]
    | (addr@(StakeAddress net cred), amount, _) <- withdrawals
    ]

friendlyTxOut :: TxOut era -> Aeson.Value
friendlyTxOut (TxOut addr amount mdatum) =
  case addr of
    AddressInEra ByronAddressInAnyEra _ ->
      object $ ("address era" .= String "Byron") : common
    AddressInEra (ShelleyAddressInEra _) (ShelleyAddress net cred stake) ->
      object $
        [ "address era" .= String "Shelley"
        , "network" .= net
        , "payment credential" .= cred
        , "stake reference" .= friendlyStakeReference stake
        ]
        ++ ["datum" .= datum | TxOutDatumHash _ datum <- [mdatum]]
        ++ common
  where
    common :: [(Text, Aeson.Value)]
    common =
      [ "address" .= serialiseAddressForTxOut addr
      , "amount" .= friendlyTxOutValue amount
      ]

friendlyStakeReference :: Crypto crypto => Shelley.StakeReference crypto -> Aeson.Value
friendlyStakeReference = \case
  Shelley.StakeRefBase cred -> toJSON cred
  Shelley.StakeRefNull -> Null
  Shelley.StakeRefPtr ptr -> toJSON ptr

friendlyUpdateProposal :: TxUpdateProposal era -> Aeson.Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ p -> String $ textShow p

friendlyCertificates :: TxCertificates ViewTx era -> Aeson.Value
friendlyCertificates = \case
  TxCertificatesNone -> Null
  TxCertificates _ cs _ -> toJSON $ map textShow cs

friendlyFee :: TxFee era -> Aeson.Value
friendlyFee = \case
  TxFeeImplicit _ -> "implicit"
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Aeson.Value
friendlyLovelace (Lovelace value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: TxMintValue ViewTx era -> Aeson.Value
friendlyMintValue = \case
  TxMintNone -> Null
  TxMintValue _ v _ ->
    object
      [ friendlyAssetId assetId .= quantity
      | (assetId, quantity) <- valueToList v
      ]

friendlyAssetId :: AssetId -> Text
friendlyAssetId = \case
  AdaAssetId -> "ADA"
  AssetId policyId (AssetName assetName) ->
    decodeUtf8 $ serialiseToRawBytesHex policyId <> suffix
    where
      suffix =
        case assetName of
          "" -> ""
          _ -> "." <> assetName

friendlyTxOutValue :: TxOutValue era -> Aeson.Value
friendlyTxOutValue = \case
  TxOutAdaOnly _ lovelace -> friendlyLovelace lovelace
  TxOutValue _ multiasset -> toJSON multiasset

friendlyMetadata :: TxMetadataInEra era -> Aeson.Value
friendlyMetadata = \case
  TxMetadataNone -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Aeson.Value
friendlyMetadataValue = \case
  TxMetaNumber int -> toJSON int
  TxMetaBytes bytes -> String $ textShow bytes
  TxMetaList lst -> array $ map friendlyMetadataValue lst
  TxMetaMap m ->
    array
      [array [friendlyMetadataValue k, friendlyMetadataValue v] | (k, v) <- m]
  TxMetaText text -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Aeson.Value
friendlyAuxScripts = \case
  TxAuxScriptsNone -> Null
  TxAuxScripts _ scripts -> String $ textShow scripts

friendlyInputs :: [(TxIn, build)] -> Aeson.Value
friendlyInputs = toJSON . map fst
