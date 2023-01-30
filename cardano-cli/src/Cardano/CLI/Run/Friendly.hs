{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBS, friendlyTxBodyBS) where

import           Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isAscii)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, isJust)
import           Data.Ratio (numerator)
import qualified Data.Text as Text
import           Data.Yaml (array)
import           Data.Yaml.Pretty (setConfCompare)
import qualified Data.Yaml.Pretty as Yaml
import           GHC.Real (denominator)
import           GHC.Unicode (isAlphaNum)

import           Cardano.Api as Api
import           Cardano.Api.Byron (KeyWitness (ByronKeyWitness))
import           Cardano.Api.Shelley (Address (ShelleyAddress),
                   KeyWitness (ShelleyBootstrapWitness, ShelleyKeyWitness), StakeAddress (..),
                   StakeCredential (..), StakePoolParameters (..), fromShelleyPaymentCredential,
                   fromShelleyStakeCredential, fromShelleyStakeReference)
import qualified Cardano.Ledger.Shelley.API as Shelley

yamlConfig :: Yaml.Config
yamlConfig = Yaml.defConfig & setConfCompare compare

friendlyTxBS :: IsCardanoEra era => CardanoEra era -> Tx era -> ByteString
friendlyTxBS era = Yaml.encodePretty yamlConfig . object . friendlyTx era

friendlyTx :: IsCardanoEra era => CardanoEra era -> Tx era -> [Aeson.Pair]
friendlyTx era (Tx body witnesses) =
  ("witnesses" .= map friendlyKeyWitness witnesses) : friendlyTxBody era body

friendlyKeyWitness :: KeyWitness era -> Aeson.Value
friendlyKeyWitness =
  object
  . \case
      ByronKeyWitness txInWitness -> ["Byron witness" .= textShow txInWitness]
      ShelleyBootstrapWitness _era bootstrapWitness ->
        ["bootstrap witness" .= textShow bootstrapWitness]
      ShelleyKeyWitness _era (Shelley.WitVKey key signature) ->
        ["key" .= textShow key, "signature" .= textShow signature]

friendlyTxBodyBS
  :: IsCardanoEra era => CardanoEra era -> TxBody era -> ByteString
friendlyTxBodyBS era =
  Yaml.encodePretty yamlConfig . object . friendlyTxBody era

friendlyTxBody
  :: IsCardanoEra era => CardanoEra era -> TxBody era -> [Aeson.Pair]
friendlyTxBody
  era
  (TxBody
    TxBodyContent
      { txAuxScripts
      , txCertificates
      , txExtraKeyWits
      , txFee
      , txIns
      , txInsCollateral
      , txMetadata
      , txMintValue
      , txOuts
      , txTotalCollateral
      , txReturnCollateral
      , txInsReference
      , txUpdateProposal
      , txValidityRange
      , txWithdrawals
      }) =
    [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
    , "certificates" .= friendlyCertificates txCertificates
    , "collateral inputs" .= friendlyCollateralInputs txInsCollateral
    , "era" .= era
    , "fee" .= friendlyFee txFee
    , "inputs" .= friendlyInputs txIns
    , "metadata" .= friendlyMetadata txMetadata
    , "mint" .= friendlyMintValue txMintValue
    , "outputs" .= map friendlyTxOut txOuts
    , "reference inputs" .= friendlyReferenceInputs txInsReference
    , "total collateral" .= friendlyTotalCollateral txTotalCollateral
    , "return collateral" .= friendlyReturnCollateral txReturnCollateral
    , "required signers (payment key hashes needed for scripts)" .=
        friendlyExtraKeyWits txExtraKeyWits
    , "update proposal" .= friendlyUpdateProposal txUpdateProposal
    , "validity range" .= friendlyValidityRange era txValidityRange
    , "withdrawals" .= friendlyWithdrawals txWithdrawals
    ]

friendlyTotalCollateral :: TxTotalCollateral era -> Aeson.Value
friendlyTotalCollateral TxTotalCollateralNone = Aeson.Null
friendlyTotalCollateral (TxTotalCollateral _ coll) = toJSON coll

friendlyReturnCollateral
  :: IsCardanoEra era => TxReturnCollateral CtxTx era -> Aeson.Value
friendlyReturnCollateral TxReturnCollateralNone = Aeson.Null
friendlyReturnCollateral (TxReturnCollateral _ collOut) = friendlyTxOut collOut

friendlyExtraKeyWits :: TxExtraKeyWitnesses era -> Aeson.Value
friendlyExtraKeyWits = \case
  TxExtraKeyWitnessesNone -> Null
  TxExtraKeyWitnesses _supported paymentKeyHashes ->
    toJSON $ map serialiseToRawBytesHexText paymentKeyHashes

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
    [ object $
        "address" .= serialiseAddress addr :
        "amount" .= friendlyLovelace amount :
        friendlyStakeAddress addr
    | (addr, amount, _) <- withdrawals
    ]

friendlyStakeAddress :: StakeAddress -> [Aeson.Pair]
friendlyStakeAddress (StakeAddress net cred) =
  [ "network" .= net
  , friendlyStakeCredential $ fromShelleyStakeCredential cred
  ]

friendlyTxOut :: IsCardanoEra era => TxOut CtxTx era -> Aeson.Value
friendlyTxOut (TxOut addr amount mdatum script) =
  object $
    case addr of
      AddressInEra ByronAddressInAnyEra byronAdr ->
        [ "address era" .= String "Byron"
        , "address" .= serialiseAddress byronAdr
        , "amount" .= friendlyTxOutValue amount
        ]
      AddressInEra (ShelleyAddressInEra sbe) saddr@(ShelleyAddress net cred stake) ->
        let preAlonzo =
              friendlyPaymentCredential (fromShelleyPaymentCredential cred) :
              [ "address era" .= Aeson.String "Shelley"
              , "network" .= net
              , "address" .= serialiseAddress saddr
              , "amount" .= friendlyTxOutValue amount
              , "stake reference" .=
                  friendlyStakeReference (fromShelleyStakeReference stake)
              ]
            datum =
              [ "datum" .= renderDatum mdatum
              | isJust $ scriptDataSupportedInEra $ shelleyBasedToCardanoEra sbe
              ]
            sinceAlonzo = ["reference script" .= script]
        in preAlonzo ++ datum ++ sinceAlonzo
 where
  renderDatum :: TxOutDatum CtxTx era -> Aeson.Value
  renderDatum TxOutDatumNone = Aeson.Null
  renderDatum (TxOutDatumHash _ h) =
    Aeson.String $ serialiseToRawBytesHexText h
  renderDatum (TxOutDatumInTx _ sData) =
    scriptDataToJson ScriptDataJsonDetailedSchema sData
  renderDatum (TxOutDatumInline _ sData) =
    scriptDataToJson ScriptDataJsonDetailedSchema sData


friendlyStakeReference :: StakeAddressReference -> Aeson.Value
friendlyStakeReference = \case
  NoStakeAddress -> Null
  StakeAddressByPointer ptr -> String (textShow ptr)
  StakeAddressByValue cred -> object [friendlyStakeCredential cred]

friendlyUpdateProposal :: TxUpdateProposal era -> Aeson.Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ (UpdateProposal parameterUpdates epoch) ->
    object
      [ "epoch" .= epoch
      , "updates" .=
        [ object
            [ "genesis key hash" .= serialiseToRawBytesHexText genesisKeyHash
            , "update" .= friendlyProtocolParametersUpdate parameterUpdate
            ]
        | (genesisKeyHash, parameterUpdate) <- Map.assocs parameterUpdates
        ]
      ]

friendlyProtocolParametersUpdate :: ProtocolParametersUpdate -> Aeson.Value
friendlyProtocolParametersUpdate
  ProtocolParametersUpdate
    { protocolUpdateProtocolVersion
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
    , protocolUpdateUTxOCostPerWord
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxCollateralInputs
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdatePrices
    , protocolUpdateUTxOCostPerByte
    } =
  object . catMaybes $
    [ protocolUpdateProtocolVersion <&> \(major, minor) ->
        "protocol version" .= (textShow major <> "." <> textShow minor)
    , protocolUpdateDecentralization <&>
        ("decentralization parameter" .=) . friendlyRational
    , protocolUpdateExtraPraosEntropy <&>
        ("extra entropy" .=) . maybe "reset" toJSON
    , protocolUpdateMaxBlockHeaderSize <&> ("max block header size" .=)
    , protocolUpdateMaxBlockBodySize<&> ("max block body size" .=)
    , protocolUpdateMaxTxSize <&> ("max transaction size" .=)
    , protocolUpdateTxFeeFixed <&> ("transaction fee constant" .=)
    , protocolUpdateTxFeePerByte <&> ("transaction fee linear per byte" .=)
    , protocolUpdateMinUTxOValue <&> ("min UTxO value" .=) . friendlyLovelace
    , protocolUpdateStakeAddressDeposit <&>
        ("key registration deposit" .=) . friendlyLovelace
    , protocolUpdateStakePoolDeposit <&>
        ("pool registration deposit" .=) . friendlyLovelace
    , protocolUpdateMinPoolCost <&> ("min pool cost" .=) . friendlyLovelace
    , protocolUpdatePoolRetireMaxEpoch <&> ("pool retirement epoch boundary" .=)
    , protocolUpdateStakePoolTargetNum <&> ("number of pools" .=)
    , protocolUpdatePoolPledgeInfluence <&>
        ("pool influence" .=) . friendlyRational
    , protocolUpdateMonetaryExpansion <&>
        ("monetary expansion" .=) . friendlyRational
    , protocolUpdateTreasuryCut <&> ("treasury expansion" .=) . friendlyRational
    , protocolUpdateUTxOCostPerWord <&>
        ("UTxO storage cost per word" .=) . friendlyLovelace
    , protocolUpdateCollateralPercent <&>
        ("collateral inputs share" .=) . (<> "%") . textShow
    , protocolUpdateMaxBlockExUnits <&> ("max block execution units" .=)
    , protocolUpdateMaxCollateralInputs <&> ("max collateral inputs" .=)
    , protocolUpdateMaxTxExUnits <&> ("max transaction execution units" .=)
    , protocolUpdateMaxValueSize <&> ("max value size" .=)
    , protocolUpdatePrices <&> ("execution prices" .=) . friendlyPrices
    , protocolUpdateUTxOCostPerByte <&>
        ("UTxO storage cost per byte" .=) . friendlyLovelace
    ]

friendlyPrices :: ExecutionUnitPrices -> Aeson.Value
friendlyPrices ExecutionUnitPrices{priceExecutionMemory, priceExecutionSteps} =
  object
    [ "memory" .= friendlyRational priceExecutionMemory
    , "steps" .= friendlyRational priceExecutionSteps
    ]

friendlyCertificates :: TxCertificates ViewTx era -> Aeson.Value
friendlyCertificates = \case
  TxCertificatesNone -> Null
  TxCertificates _ cs _ -> array $ map friendlyCertificate cs

friendlyCertificate :: Certificate -> Aeson.Value
friendlyCertificate =
  object
    . (: [])
    . \case
      -- Stake address certificates
      StakeAddressRegistrationCertificate credential ->
        "stake address registration"
          .= object [friendlyStakeCredential credential]
      StakeAddressDeregistrationCertificate credential ->
        "stake address deregistration"
          .= object [friendlyStakeCredential credential]
      StakeAddressDelegationCertificate credential poolId ->
        "stake address delegation"
          .= object [friendlyStakeCredential credential, "pool" .= poolId]

      -- Stake pool certificates
      StakePoolRegistrationCertificate parameters ->
        "stake pool registration" .= friendlyStakePoolParameters parameters
      StakePoolRetirementCertificate poolId epochNo ->
        "stake pool retirement" .= object ["pool" .= poolId, "epoch" .= epochNo]

      -- Special certificates
      GenesisKeyDelegationCertificate
        genesisKeyHash
        delegateKeyHash
        vrfKeyHash ->
          "genesis key delegation"
            .= object
              [ "genesis key hash"
                  .= serialiseToRawBytesHexText genesisKeyHash,
                "delegate key hash"
                  .= serialiseToRawBytesHexText delegateKeyHash,
                "VRF key hash" .= serialiseToRawBytesHexText vrfKeyHash
              ]
      MIRCertificate pot target ->
        "MIR" .= object ["pot" .= friendlyMirPot pot, friendlyMirTarget target]

friendlyMirTarget :: MIRTarget -> Aeson.Pair
friendlyMirTarget = \case
  StakeAddressesMIR addresses ->
    "target stake addresses" .=
      [ object
          [ friendlyStakeCredential credential
          , "amount" .= friendlyLovelace lovelace
          ]
      | (credential, lovelace) <- addresses
      ]
  SendToReservesMIR amount -> "send to reserves" .= friendlyLovelace amount
  SendToTreasuryMIR amount -> "send to treasury" .= friendlyLovelace amount

friendlyStakeCredential :: StakeCredential -> Aeson.Pair
friendlyStakeCredential = \case
  StakeCredentialByKey keyHash ->
    "stake credential key hash" .= serialiseToRawBytesHexText keyHash
  StakeCredentialByScript scriptHash ->
    "stake credential script hash" .= serialiseToRawBytesHexText scriptHash

friendlyPaymentCredential :: PaymentCredential -> Aeson.Pair
friendlyPaymentCredential = \case
  PaymentCredentialByKey keyHash ->
    "payment credential key hash" .= serialiseToRawBytesHexText keyHash
  PaymentCredentialByScript scriptHash ->
    "payment credential script hash" .= serialiseToRawBytesHexText scriptHash

friendlyMirPot :: Shelley.MIRPot -> Aeson.Value
friendlyMirPot = \case
  Shelley.ReservesMIR -> "reserves"
  Shelley.TreasuryMIR -> "treasury"

friendlyStakePoolParameters :: StakePoolParameters -> Aeson.Value
friendlyStakePoolParameters
  StakePoolParameters
    { stakePoolId
    , stakePoolVRF
    , stakePoolCost
    , stakePoolMargin
    , stakePoolRewardAccount
    , stakePoolPledge
    , stakePoolOwners
    , stakePoolRelays
    , stakePoolMetadata
    } =
  object
    [ "pool" .= stakePoolId
    , "VRF key hash" .= serialiseToRawBytesHexText stakePoolVRF
    , "cost" .= friendlyLovelace stakePoolCost
    , "margin" .= friendlyRational stakePoolMargin
    , "reward account" .= object (friendlyStakeAddress stakePoolRewardAccount)
    , "pledge" .= friendlyLovelace stakePoolPledge
    , "owners (stake key hashes)"
        .= map serialiseToRawBytesHexText stakePoolOwners
    , "relays" .= map textShow stakePoolRelays
    , "metadata" .= fmap textShow stakePoolMetadata
    ]

friendlyRational :: Rational -> Aeson.Value
friendlyRational r =
  String $
    case d of
      1 -> textShow n
      _ -> textShow n <> "/" <> textShow d
 where
  n = numerator r
  d = denominator r

friendlyFee :: TxFee era -> Aeson.Value
friendlyFee = \case
  TxFeeImplicit _ -> "implicit"
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Aeson.Value
friendlyLovelace (Lovelace value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: TxMintValue ViewTx era -> Aeson.Value
friendlyMintValue = \case
  TxMintNone -> Null
  TxMintValue _ v _ -> friendlyValue v

friendlyTxOutValue :: TxOutValue era -> Aeson.Value
friendlyTxOutValue = \case
  TxOutAdaOnly _ lovelace -> friendlyLovelace lovelace
  TxOutValue _ v -> friendlyValue v

friendlyValue :: Api.Value -> Aeson.Value
friendlyValue v =
  object
    [ case bundle of
        ValueNestedBundleAda q -> "lovelace" .= q
        ValueNestedBundle policy assets ->
          Aeson.fromText (friendlyPolicyId policy) .= friendlyAssets assets
    | bundle <- bundles
    ]
 where

  ValueNestedRep bundles = valueToNestedRep v

  friendlyPolicyId = ("policy " <>) . serialiseToRawBytesHexText

  friendlyAssets = Map.mapKeys friendlyAssetName

  friendlyAssetName = \case
    "" -> "default asset"
    name@(AssetName nameBS) ->
      "asset " <> serialiseToRawBytesHexText name <> nameAsciiSuffix
     where
      nameAsciiSuffix
        | nameIsAscii = " (" <> nameAscii <> ")"
        | otherwise = ""
      nameIsAscii = BSC.all (\c -> isAscii c && isAlphaNum c) nameBS
      nameAscii = Text.pack $ BSC.unpack nameBS

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

friendlyReferenceInputs :: TxInsReference build era -> Aeson.Value
friendlyReferenceInputs TxInsReferenceNone = Null
friendlyReferenceInputs (TxInsReference _ txins) = toJSON txins

friendlyInputs :: [(TxIn, build)] -> Aeson.Value
friendlyInputs = toJSON . map fst

friendlyCollateralInputs :: TxInsCollateral era -> Aeson.Value
friendlyCollateralInputs = \case
  TxInsCollateralNone -> Null
  TxInsCollateral _ txins -> toJSON txins
