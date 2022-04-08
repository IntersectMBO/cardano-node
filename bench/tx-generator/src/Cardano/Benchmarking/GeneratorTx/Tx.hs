{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Tx
  ( Fund
  , fundTxIn
  , fundAdaValue
  , keyAddress
  , mkGenesisTransaction
  , mkFund
  , mkFee
  , mkTxOutValueAdaOnly
  , mkValidityUpperBound
  , txOutValueToLovelace
  , txInModeCardano
  )
where

import           Prelude
import           Cardano.Benchmarking.Types (TxAdditionalSize (..))

import           Cardano.Api hiding (txOutValueToLovelace)

type Fund = (TxIn, InAnyCardanoEra TxOutValue)

mkFund :: forall era. IsCardanoEra era => TxIn -> TxOutValue era -> Fund
mkFund txIn val = (txIn, InAnyCardanoEra cardanoEra val)

fundTxIn :: Fund -> TxIn
fundTxIn (x,_) = x

fundAdaValue :: Fund -> Lovelace
fundAdaValue (_, InAnyCardanoEra _ txOut) = txOutValueToLovelace txOut

keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

--{-# DEPRECATED mkGenesisTransaction "to be removed" #-}
mkGenesisTransaction :: forall era .
     IsShelleyBasedEra era
  => SigningKey GenesisUTxOKey
  -> TxAdditionalSize
  -> SlotNo
  -> Lovelace
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> Tx era
mkGenesisTransaction key _payloadSize ttl fee txins txouts
  = case makeTransactionBody txBodyContent of
    Right b -> signShelleyTransaction b [WitnessGenesisUTxOKey key]
    Left err -> error $ show err
 where
  txBodyContent = TxBodyContent {
      txIns = zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending
    , txInsCollateral = TxInsCollateralNone
    , txOuts = txouts
    , txFee = fees
    , txValidityRange = (TxValidityNoLowerBound, validityUpperBound)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    , txReturnCollateral = TxReturnCollateralNone
    , txTotalCollateral = TxTotalCollateralNone
    }
  fees = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxFeeExplicit TxFeesExplicitInShelleyEra fee
    ShelleyBasedEraAllegra -> TxFeeExplicit TxFeesExplicitInAllegraEra fee
    ShelleyBasedEraMary    -> TxFeeExplicit TxFeesExplicitInMaryEra fee
    ShelleyBasedEraAlonzo  -> TxFeeExplicit TxFeesExplicitInAlonzoEra fee
    ShelleyBasedEraBabbage -> TxFeeExplicit TxFeesExplicitInBabbageEra fee
  validityUpperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
    ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
    ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl
    ShelleyBasedEraAlonzo  -> TxValidityUpperBound ValidityUpperBoundInAlonzoEra ttl
    ShelleyBasedEraBabbage -> TxValidityUpperBound ValidityUpperBoundInBabbageEra ttl

mkFee :: forall era .
     IsShelleyBasedEra era
  => Lovelace
  -> TxFee era
mkFee f = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxFeeExplicit TxFeesExplicitInShelleyEra f
  ShelleyBasedEraAllegra -> TxFeeExplicit TxFeesExplicitInAllegraEra f
  ShelleyBasedEraMary    -> TxFeeExplicit TxFeesExplicitInMaryEra f
  ShelleyBasedEraAlonzo  -> TxFeeExplicit TxFeesExplicitInAlonzoEra f
  ShelleyBasedEraBabbage -> TxFeeExplicit TxFeesExplicitInBabbageEra f

mkValidityUpperBound :: forall era .
     IsShelleyBasedEra era
  => SlotNo
  -> TxValidityUpperBound era
mkValidityUpperBound ttl = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
  ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
  ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl
  ShelleyBasedEraAlonzo  -> TxValidityUpperBound ValidityUpperBoundInAlonzoEra ttl
  ShelleyBasedEraBabbage -> TxValidityUpperBound ValidityUpperBoundInBabbageEra ttl

mkTxOutValueAdaOnly :: forall era . IsShelleyBasedEra era => Lovelace -> TxOutValue era
mkTxOutValueAdaOnly l = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxOutAdaOnly AdaOnlyInShelleyEra l
  ShelleyBasedEraAllegra -> TxOutAdaOnly AdaOnlyInAllegraEra l
  ShelleyBasedEraMary    -> TxOutValue MultiAssetInMaryEra $ lovelaceToValue l
  ShelleyBasedEraAlonzo  -> TxOutValue MultiAssetInAlonzoEra $ lovelaceToValue l
  ShelleyBasedEraBabbage -> TxOutValue MultiAssetInBabbageEra $ lovelaceToValue l
  
txOutValueToLovelace :: TxOutValue era -> Lovelace
txOutValueToLovelace = \case
  TxOutAdaOnly AdaOnlyInByronEra   x -> x
  TxOutAdaOnly AdaOnlyInShelleyEra x -> x
  TxOutAdaOnly AdaOnlyInAllegraEra x -> x
  TxOutValue _ v -> case valueToLovelace v of
    Just c -> c
    Nothing -> error "txOutValueLovelace  TxOut contains no ADA"

txInModeCardano :: forall era . IsShelleyBasedEra era => Tx era -> TxInMode CardanoMode
txInModeCardano tx = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxInMode tx ShelleyEraInCardanoMode
  ShelleyBasedEraAllegra -> TxInMode tx AllegraEraInCardanoMode
  ShelleyBasedEraMary    -> TxInMode tx MaryEraInCardanoMode
  ShelleyBasedEraAlonzo  -> TxInMode tx AlonzoEraInCardanoMode
  ShelleyBasedEraBabbage -> TxInMode tx BabbageEraInCardanoMode
