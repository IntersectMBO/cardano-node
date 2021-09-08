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
  , mkTransactionGen
  , mkTxOutValueAdaOnly
  , mkValidityUpperBound
  , txOutValueToLovelace
  , txInModeCardano
  )
where

import           Prelude

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
    }
  fees = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxFeeExplicit TxFeesExplicitInShelleyEra fee
    ShelleyBasedEraAllegra -> TxFeeExplicit TxFeesExplicitInAllegraEra fee
    ShelleyBasedEraMary    -> TxFeeExplicit TxFeesExplicitInMaryEra fee
    ShelleyBasedEraAlonzo  -> TxFeeExplicit TxFeesExplicitInAlonzoEra fee
  validityUpperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
    ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
    ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl
    ShelleyBasedEraAlonzo  -> TxValidityUpperBound ValidityUpperBoundInAlonzoEra ttl

mkTransaction :: forall era .
     IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> TxMetadataInEra era
  -> SlotNo
  -> Lovelace
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> Tx era
mkTransaction key metadata ttl fee txins txouts
  = case makeTransactionBody txBodyContent of
    Right b -> signShelleyTransaction b [WitnessPaymentKey key]
    Left err -> error $ show err
 where
  txBodyContent = TxBodyContent {
      txIns = zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending
    , txInsCollateral = TxInsCollateralNone
    , txOuts = txouts
    , txFee = mkFee fee
    , txValidityRange = (TxValidityNoLowerBound, mkValidityUpperBound ttl)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }

mkFee :: forall era .
     IsShelleyBasedEra era
  => Lovelace
  -> TxFee era
mkFee f = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxFeeExplicit TxFeesExplicitInShelleyEra f
  ShelleyBasedEraAllegra -> TxFeeExplicit TxFeesExplicitInAllegraEra f
  ShelleyBasedEraMary    -> TxFeeExplicit TxFeesExplicitInMaryEra f
  ShelleyBasedEraAlonzo  -> TxFeeExplicit TxFeesExplicitInAlonzoEra f

mkValidityUpperBound :: forall era .
     IsShelleyBasedEra era
  => SlotNo
  -> TxValidityUpperBound era
mkValidityUpperBound ttl = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
  ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
  ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl
  ShelleyBasedEraAlonzo  -> TxValidityUpperBound ValidityUpperBoundInAlonzoEra ttl

mkTransactionGen :: forall era .
     IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> NonEmpty Fund
  -> AddressInEra era
  -> [(Int, TxOut CtxTx era)]
  -- ^ Each recipient and their payment details
  -> TxMetadataInEra era
  -- ^ Optional size of additional binary blob in transaction (as 'txAttributes')
  -> Lovelace
  -- ^ Tx fee.
  -> ( Maybe (TxIx, Lovelace)   -- The 'change' index and value (if any)
     , Lovelace                 -- The associated fees
     , Map Int TxIx             -- The offset map in the transaction below
     , Tx era
     )
mkTransactionGen signingKey inputs address payments metadata fee =
  (mChange, fee, offsetMap, tx)
 where
  tx = mkTransaction signingKey metadata (SlotNo 10000000)
         fee
         (NonEmpty.toList $ fundTxIn <$> inputs)
         (NonEmpty.toList txOutputs)

  payTxOuts     = map snd payments

  totalInpValue = sum $ fundAdaValue <$> inputs
  totalOutValue = txOutSum payTxOuts
  changeValue = totalInpValue - totalOutValue - fee
      -- change the order of comparisons first check emptyness of txouts AND remove appendr after

  (txOutputs, mChange) = case compare changeValue 0 of
    GT ->
      let changeTxOut   = TxOut address (mkTxOutValueAdaOnly changeValue) TxOutDatumNone
          changeIndex   = TxIx $ fromIntegral $ length payTxOuts -- 0-based index
      in
          (appendr payTxOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    EQ ->
      case payTxOuts of
        []                 -> error "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)
    LT -> error "Bad transaction: insufficient funds"

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, TxIx index))
                                     payments
                                     [0..]
  txOutSum :: [ TxOut CtxTx era ] -> Lovelace
  txOutSum l = sum $ map toVal l

  toVal (TxOut _ val _) = txOutValueToLovelace val

  -- | Append a non-empty list to a list.
  -- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
  appendr :: [a] -> NonEmpty a -> NonEmpty a
  appendr l nel = foldr NonEmpty.cons nel l

mkTxOutValueAdaOnly :: forall era . IsShelleyBasedEra era => Lovelace -> TxOutValue era
mkTxOutValueAdaOnly l = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxOutAdaOnly AdaOnlyInShelleyEra l
  ShelleyBasedEraAllegra -> TxOutAdaOnly AdaOnlyInAllegraEra l
  ShelleyBasedEraMary    -> TxOutValue MultiAssetInMaryEra $ lovelaceToValue l
  ShelleyBasedEraAlonzo  -> TxOutValue MultiAssetInAlonzoEra $ lovelaceToValue l

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
