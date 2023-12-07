{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use map" -}

-- | This module provides means to secure funds that are given in genesis.
--   To secure a fund, the key locking the transaction ouput in genesis has to be provided.
module Cardano.TxGenerator.Genesis
  ( genesisInitialFunds
  , genesisInitialFundForKey
  , genesisTxInput
  , genesisExpenditure
  , genesisSecureInitialFund
  , genesisValidate
  )
where

import           Data.Bifunctor (bimap, second)
import           Data.Function ((&))
import           Data.List (find)
import qualified Data.ListMap as ListMap (toList)

import           Cardano.Api
import           Cardano.Api.Shelley (ReferenceScript (..), fromShelleyLovelace,
                   fromShelleyPaymentCredential, fromShelleyStakeReference)
import           Cardano.Ledger.Shelley.API (Addr (..), sgInitialFunds)
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils


genesisValidate ::  ShelleyGenesis -> Either String ()
genesisValidate
  = validateGenesis

genesisSecureInitialFund :: forall era. IsShelleyBasedEra era =>
     NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> SigningKey PaymentKey
  -> TxGenTxParams
  -> Either TxGenError (Tx era, Fund)
genesisSecureInitialFund networkId genesis srcKey destKey TxGenTxParams{txParamFee, txParamTTL}
  = case genesisInitialFundForKey @era networkId genesis srcKey of
      Nothing             -> Left $ TxGenError "genesisSecureInitialFund: no fund found for given key in genesis"
      Just (_, lovelace)  ->
        let
          txOutValue :: TxOutValue era
          txOutValue = lovelaceToTxOutValue (shelleyBasedEra @era) $ lovelace - txParamFee
        in genesisExpenditure networkId srcKey destAddr txOutValue txParamFee txParamTTL destKey
  where
    destAddr = keyAddress @era networkId destKey

genesisInitialFunds :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> [(AddressInEra era, Lovelace)]
genesisInitialFunds networkId g
 = [ ( shelleyAddressInEra (shelleyBasedEra @era) $
          makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
     , fromShelleyLovelace coin
     )
     | (Addr _ pcr stref, coin) <- ListMap.toList $ sgInitialFunds g
   ]

genesisInitialFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra era, Lovelace)
genesisInitialFundForKey networkId genesis key
  = find (isTxOutForKey . fst) (genesisInitialFunds networkId genesis)
 where
  isTxOutForKey = (keyAddress networkId key ==)

genesisTxInput ::
     NetworkId
  -> SigningKey PaymentKey
  -> TxIn
genesisTxInput networkId
 = genesisUTxOPseudoTxIn networkId
    . verificationKeyHash
    . getVerificationKey
    . castKey

genesisExpenditure ::
     IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> TxOutValue era
  -> Lovelace
  -> SlotNo
  -> SigningKey PaymentKey
  -> Either TxGenError (Tx era, Fund)
genesisExpenditure networkId inputKey addr value fee ttl outputKey
  = second (\tx -> (tx, Fund $ InAnyCardanoEra cardanoEra $ fund tx)) eTx
 where
  eTx         = mkGenesisTransaction (castKey inputKey) ttl fee [pseudoTxIn] [txout]
  txout       = TxOut addr value TxOutDatumNone ReferenceScriptNone
  pseudoTxIn  = genesisTxInput networkId inputKey

  fund tx = FundInEra {
    _fundTxIn = TxIn (getTxId $ getTxBody tx) (TxIx 0)
  , _fundWitness = KeyWitness KeyWitnessForSpending
  , _fundVal  = value
  , _fundSigningKey = Just outputKey
  }

mkGenesisTransaction :: forall era .
     IsShelleyBasedEra era
  => SigningKey GenesisUTxOKey
  -> SlotNo
  -> Lovelace
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> Either TxGenError (Tx era)
mkGenesisTransaction key ttl fee txins txouts
  = bimap
      ApiError
      (\b -> signShelleyTransaction (shelleyBasedEra @era) b [WitnessGenesisUTxOKey key])
      (createAndValidateTransactionBody (shelleyBasedEra @era) txBodyContent)
 where
  txBodyContent = defaultTxBodyContent shelleyBasedEra
    & setTxIns (zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending)
    & setTxOuts txouts
    & setTxFee (mkTxFee fee)
    & setTxValidityLowerBound TxValidityNoLowerBound
    & setTxValidityUpperBound (mkTxValidityUpperBound ttl)

castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey
