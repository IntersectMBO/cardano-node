{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.GeneratorTx.Genesis
  ( genesisFundForKey
  , genesisExpenditure
  )
where

import           Cardano.Prelude hiding (TypeError, filter)
import           Prelude (error, filter)
import qualified Data.Map.Strict as Map

import           Control.Arrow ((***))
import           Cardano.Api
import           Cardano.Api.Shelley (fromShelleyLovelace, fromShelleyStakeReference, fromShelleyPaymentCredential)

import           Cardano.Benchmarking.GeneratorTx.Tx

import           Cardano.Ledger.Shelley.API (Addr(..), ShelleyGenesis, sgInitialFunds)
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)

genesisFunds :: forall era. IsShelleyBasedEra era
  => NetworkId -> ShelleyGenesis StandardShelley -> [(AddressInEra era, Lovelace)]
genesisFunds networkId g
 = map (castAddr *** fromShelleyLovelace)
     $ Map.toList
     $ sgInitialFunds g
 where
  castAddr (Addr _ pcr stref)
    = shelleyAddressInEra $ makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
  castAddr _ = error "castAddr:  unhandled Shelley.Addr case"

genesisFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis StandardShelley
  -> SigningKey PaymentKey
  -> (AddressInEra era, Lovelace)
genesisFundForKey networkId genesis key
  = fromMaybe (error "No genesis funds for signing key.")
    . head
    . filter (isTxOutForKey . fst)
    $ genesisFunds networkId genesis
 where
  isTxOutForKey addr = keyAddress networkId key == addr

genesisExpenditure ::
     IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Lovelace
  -> Lovelace
  -> SlotNo
  -> (Tx era, Fund)
genesisExpenditure networkId key addr coin fee ttl = (tx, fund)
 where
  tx = mkGenesisTransaction (castKey key) 0 ttl fee [ pseudoTxIn ] [ txout ]

  value = mkTxOutValueAdaOnly $ coin - fee
  txout = TxOut addr value TxOutDatumHashNone

  pseudoTxIn = genesisUTxOPseudoTxIn networkId
                 (verificationKeyHash $ getVerificationKey $ castKey key)

  castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
  castKey(PaymentSigningKey skey) = GenesisUTxOSigningKey skey

  fund = mkFund (TxIn (getTxId $ getTxBody tx) (TxIx 0)) value
