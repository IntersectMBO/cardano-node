{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.TxGenerator.Genesis
  ( genesisInitialFunds
  , genesisInitialFundForKey
  , genesisExpenditure
  , genesisSecureFund
  )
where

-- import           Cardano.Prelude hiding (TypeError, filter)
import qualified Data.ListMap as ListMap (toList)
import           Data.List (find)

import           Cardano.Api
import           Cardano.Api.Shelley (ReferenceScript (..), fromShelleyLovelace,
                   fromShelleyPaymentCredential, fromShelleyStakeReference)
-- import           Control.Arrow ((***))

import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Utils

import           Cardano.Ledger.Shelley.API (Addr (..), ShelleyGenesis, sgInitialFunds)
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)


genesisSecureFund :: -- forall era. IsShelleyBasedEra era => 
     NetworkId
  -> (Tx era, Fund)
genesisSecureFund _networkId = error "genesisSecureFund: TODO" -- TODO
{-
  = genesisExpenditure networkId genesisKey outAddr lovelace fee ttl destKey
  where
      outAddr = keyAddress @ era networkId destKey
      (_, lovelace) = genesisFundForKey @ era networkId genesis genesisKey
-}

genesisInitialFunds :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis StandardShelley 
  -> [(AddressInEra era, Lovelace)]
genesisInitialFunds networkId g
 = [ ( shelleyAddressInEra $ makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
     , fromShelleyLovelace coin
     )
     | (Addr _ pcr stref, coin) <- ListMap.toList $ sgInitialFunds g
   ] 

genesisInitialFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis StandardShelley
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra era, Lovelace)
genesisInitialFundForKey networkId genesis key
  = find (isTxOutForKey . fst) (genesisInitialFunds networkId genesis)
 where
  isTxOutForKey = (keyAddress networkId key ==)

genesisExpenditure ::
     IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Lovelace
  -> Lovelace
  -> SlotNo
  -> SigningKey PaymentKey
  -> (Tx era, Fund)
genesisExpenditure networkId inputKey addr coin fee ttl outputKey
  = (tx, Fund $ InAnyCardanoEra cardanoEra fund)
 where
  tx = mkGenesisTransaction (castKey inputKey) ttl fee [ pseudoTxIn ] [ txout ]

  value = mkTxOutValueAdaOnly $ coin - fee
  txout = TxOut addr value TxOutDatumNone ReferenceScriptNone

  pseudoTxIn = genesisUTxOPseudoTxIn networkId
                 (verificationKeyHash $ getVerificationKey $ castKey inputKey)

  castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
  castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey

  fund = FundInEra {
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
  -> Tx era
mkGenesisTransaction key ttl fee txins txouts
  = case makeTransactionBody txBodyContent of
    Right b -> signShelleyTransaction b [WitnessGenesisUTxOKey key]
    Left err -> error $ show err
 where
  txBodyContent = TxBodyContent {
      txIns = zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending
    , txInsCollateral = TxInsCollateralNone
    , txInsReference = TxInsReferenceNone
    , txOuts = txouts
    , txFee = mkTxFee fee
    , txValidityRange = (TxValidityNoLowerBound, mkTxValidityUpperBound ttl)
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
