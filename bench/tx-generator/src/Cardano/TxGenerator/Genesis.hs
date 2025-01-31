{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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

import           Cardano.Api
import           Cardano.Api.Shelley (ReferenceScript (..), fromShelleyPaymentCredential,
                   fromShelleyStakeReference)

import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Shelley.API (Addr (..), sgInitialFunds)
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Data.Bifunctor (bimap, second)
import           Data.Function ((&))
import           Data.List (find)
import qualified Data.ListMap as ListMap (toList)


genesisValidate ::  ShelleyGenesis -> Either String ()
genesisValidate
  = validateGenesis

genesisSecureInitialFund :: ()
  => ShelleyBasedEra era
  -> NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> SigningKey PaymentKey
  -> TxGenTxParams
  -> Either TxGenError (Tx era, Fund)
genesisSecureInitialFund sbe networkId genesis srcKey destKey TxGenTxParams{txParamFee, txParamTTL}
  = case genesisInitialFundForKey sbe networkId genesis srcKey of
      Nothing             -> Left $ TxGenError "genesisSecureInitialFund: no fund found for given key in genesis"
      Just (_, lovelace)  ->
        let txOutValue = lovelaceToTxOutValue sbe $ lovelace - txParamFee
        in genesisExpenditure sbe networkId srcKey destAddr txOutValue txParamFee txParamTTL destKey
  where
    destAddr = keyAddress sbe networkId destKey

genesisInitialFunds :: ()
  => ShelleyBasedEra era
  -> NetworkId
  -> ShelleyGenesis
  -> [(AddressInEra era, L.Coin)]
genesisInitialFunds sbe networkId g
 = [ ( shelleyAddressInEra sbe $
          makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
     , coin
     )
     | (Addr _ pcr stref, coin) <- ListMap.toList $ sgInitialFunds g
   ]

genesisInitialFundForKey :: ()
  => ShelleyBasedEra era
  -> NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra era, L.Coin)
genesisInitialFundForKey sbe networkId genesis key
  = find isTxOutForKey $ genesisInitialFunds sbe networkId genesis
 where
  isTxOutForKey = (keyAddress sbe networkId key ==) . fst

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
     ShelleyBasedEra era
  -> NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> TxOutValue era
  -> L.Coin
  -> SlotNo
  -> SigningKey PaymentKey
  -> Either TxGenError (Tx era, Fund)
genesisExpenditure sbe networkId inputKey addr value fee ttl outputKey
  = second (\tx -> (tx, shelleyBasedEraConstraints sbe $ Fund $ InAnyCardanoEra (toCardanoEra sbe) $ fund tx)) eTx
 where
  eTx         = mkGenesisTransaction sbe (castKey inputKey) ttl fee [pseudoTxIn] [txout]
  txout       = TxOut addr value TxOutDatumNone ReferenceScriptNone
  pseudoTxIn  = genesisTxInput networkId inputKey

  fund tx = FundInEra {
    _fundTxIn = TxIn (getTxId $ getTxBody tx) (TxIx 0)
  , _fundWitness = KeyWitness KeyWitnessForSpending
  , _fundVal  = value
  , _fundSigningKey = Just outputKey
  }

mkGenesisTransaction ::
     ShelleyBasedEra era
  -> SigningKey GenesisUTxOKey
  -> SlotNo
  -> L.Coin
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> Either TxGenError (Tx era)
mkGenesisTransaction sbe key ttl fee txins txouts
  = bimap
      ApiError
      (\b -> signShelleyTransaction sbe b [WitnessGenesisUTxOKey key])
      (createAndValidateTransactionBody sbe txBodyContent)
 where
  txBodyContent = defaultTxBodyContent sbe
    & setTxIns (map (, BuildTxWith $ KeyWitness KeyWitnessForSpending) txins)
    & setTxOuts txouts
    & setTxFee (mkTxFee sbe fee)
    & setTxValidityLowerBound TxValidityNoLowerBound
    & setTxValidityUpperBound (mkTxValidityUpperBound sbe ttl)

castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey
