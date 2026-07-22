{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


{- HLINT ignore "Use map with tuple-section" -}

-- | This module provides means to secure funds that are given in genesis.
--   To secure a fund, the key locking the transaction output in genesis has to be provided.
module Cardano.TxGenerator.Genesis
  ( genesisInitialFunds
  , genesisInitialFundForKey
  , genesisTxInput
  , genesisExpenditure
  , genesisSecureInitialFund
  , genesisValidate
  )
where

import           Cardano.Api hiding (ShelleyGenesis)

import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Keys.WitVKey (WitVKey (WitVKey))
import           Cardano.Ledger.Shelley.API (Addr (..))
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Data.Bifunctor (second)
import           Data.Function ((&))
import           Data.List (find)
import qualified Data.ListMap as ListMap (toList)
import qualified Data.Set as Set
import           Lens.Micro ((.~), (^.))


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
  -> [(AddressInEra era, L.Coin)]
genesisInitialFunds networkId g
 = [ ( shelleyAddressInEra (shelleyBasedEra @era) $
          makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
     , coin
     )
     | (Addr _ pcr stref, coin) <- ListMap.toList $ sgInitialFunds g
   ]

genesisInitialFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra era, L.Coin)
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
  -> L.Coin
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
    _fundTxIn = TxIn (txIdFromTx tx) (TxIx 0)
  , _fundWitness = KeyWitness KeyWitnessForSpending
  , _fundVal  = value
  , _fundSigningKey = Just outputKey
  }

  txIdFromTx :: Tx era -> TxId
  txIdFromTx (ShelleyTx sbe' tx') =
    shelleyBasedEraConstraints sbe' $ fromShelleyTxId $ Ledger.txIdTxBody (tx' ^. Ledger.bodyTxL)

mkGenesisTransaction :: forall era .
     IsShelleyBasedEra era
  => SigningKey GenesisUTxOKey
  -> SlotNo
  -> L.Coin
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> Either TxGenError (Tx era)
mkGenesisTransaction key ttl fee txins txouts =
  shelleyBasedEraConstraints sbe $
    let txInputs = zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending
        ledgerTxBody =
          mkCommonTxBody sbe txInputs txouts (mkTxFee fee) TxWithdrawalsNone Nothing
            & invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe (mkTxValidityUpperBound ttl)
        rawBody = ledgerTxBody ^. txBodyL
        unsignedLedgerTx = Ledger.mkBasicTx rawBody
        txHash = Ledger.extractHash $ Ledger.hashAnnotated rawBody
        shelleySigningKey = toShelleySigningKey (WitnessGenesisUTxOKey key)
        witVKey = WitVKey
          (getShelleyKeyWitnessVerificationKey shelleySigningKey)
          (makeShelleySignature txHash shelleySigningKey)
        signedLedgerTx = unsignedLedgerTx
          & Ledger.witsTxL .~ (Ledger.mkBasicTxWits & Ledger.addrTxWitsL .~ Set.singleton witVKey)
    in Right $ ShelleyTx sbe signedLedgerTx
 where
  sbe = shelleyBasedEra @era

castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey
