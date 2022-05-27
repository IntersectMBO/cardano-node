{-# LANGUAGE GADTs #-}
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

import           Cardano.Api

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
    , txInsReference = TxInsReferenceNone
    , txOuts = txouts
    , txFee = mkFee fee
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
mkFee f = case txFeesExplicitInEra (cardanoEra @ era) of
    Right e -> TxFeeExplicit e f
    Left b -> TxFeeImplicit b -- error "unreachable"

mkValidityUpperBound :: forall era .
     IsShelleyBasedEra era
  => SlotNo
  -> TxValidityUpperBound era
mkValidityUpperBound ttl = case validityUpperBoundSupportedInEra (cardanoEra @ era) of
  Just p -> TxValidityUpperBound p ttl
  Nothing -> error "unreachable"

mkTxOutValueAdaOnly :: forall era . IsShelleyBasedEra era => Lovelace -> TxOutValue era
mkTxOutValueAdaOnly l = case multiAssetSupportedInEra (cardanoEra @ era) of
  Right p -> TxOutValue p $ lovelaceToValue l
  Left p -> TxOutAdaOnly p l

txInModeCardano :: forall era . IsShelleyBasedEra era => Tx era -> TxInMode CardanoMode
txInModeCardano tx = case toEraInMode (cardanoEra @ era) CardanoMode of
  Just t -> TxInMode tx t
  Nothing -> error "txInModeCardano :unreachable"
