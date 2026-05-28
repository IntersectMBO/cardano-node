{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import           Cardano.Api.Experimental (AnyWitness (..), Era, IsEra (useEra), LedgerEra,
                   SignedTx (..), makeKeyWitness, makeUnsignedTx, obtainCommonConstraints, signTx)
import qualified Cardano.Api.Experimental.Tx as Exp

import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Core (mkCoinTxOut)
import           Cardano.Ledger.Shelley.API (Addr (..))
import           Cardano.Ledger.Shelley.Genesis (InjectionData (..), ShelleyExtraConfig (..))
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Data.Bifunctor (first, second)
import           Data.Function ((&))
import           Data.List (find)
import qualified Data.ListMap as ListMap (toList)


genesisValidate ::  ShelleyGenesis -> Either String ()
genesisValidate
  = validateGenesis

genesisSecureInitialFund :: forall era. IsEra era =>
     NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> SigningKey PaymentKey
  -> TxGenTxParams
  -> Either TxGenError (SignedTx era, Fund)
genesisSecureInitialFund networkId genesis srcKey destKey TxGenTxParams{txParamFee, txParamTTL} =
  obtainCommonConstraints (useEra @era) $ do
    mFund <- genesisInitialFundForKey @era networkId genesis srcKey
    case mFund of
      Nothing             -> Left $ TxGenError "genesisSecureInitialFund: no fund found for given key in genesis"
      Just (_, lovelace)  ->
        genesisExpenditure networkId srcKey destAddr (lovelace - txParamFee) txParamFee txParamTTL destKey
  where
    destAddr = keyAddress @era networkId destKey

genesisInitialFunds :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> Either TxGenError [(AddressInEra era, L.Coin)]
genesisInitialFunds networkId g = do
  funds <- embeddedInitialFunds g
  pure
    [ ( shelleyAddressInEra (shelleyBasedEra @era) $
          makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
      , coin
      )
    | (Addr _ pcr stref, coin) <- funds
    ]

-- | 'embeddedInitialFunds' resolves the genesis initial funds, accepting both the
-- legacy top-level @initialFunds@ and the newer @extraConfig.initialFunds@
-- injection that @cardano-cli genesis create-testnet-data@ emits.
embeddedInitialFunds :: ShelleyGenesis -> Either TxGenError [(Addr, L.Coin)]
embeddedInitialFunds g =
  case sgExtraConfig g of
    SNothing -> Right legacy
    SJust extraConfig -> case secInitialFunds extraConfig of
      NoInjection          -> Right legacy
      _ | not (null legacy) ->
          Left $ TxGenError "genesisInitialFunds: both initialFunds and extraConfig.initialFunds are populated; please use only one source"
      EmbeddedInjection lm -> Right (ListMap.toList lm)
      InjectionFromFile{}  ->
        Left $ TxGenError "genesisInitialFunds: file-based initial-funds injection is unsupported; expected embedded funds in extraConfig.initialFunds or initialFunds"
 where
  legacy = ListMap.toList $ sgInitialFunds g

genesisInitialFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> Either TxGenError (Maybe (AddressInEra era, L.Coin))
genesisInitialFundForKey networkId genesis key
  = find (isTxOutForKey . fst) <$> genesisInitialFunds networkId genesis
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

genesisExpenditure :: forall era.
     IsEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> L.Coin
  -> L.Coin
  -> SlotNo
  -> SigningKey PaymentKey
  -> Either TxGenError (SignedTx era, Fund)
genesisExpenditure networkId inputKey addr value fee ttl outputKey
  = obtainCommonConstraints era $
      second (\tx -> (tx, Fund $ InAnyCardanoEra cardanoEra $ fund (lovelaceToTxOutValue (shelleyBasedEra @era) value) tx))
        $ mkGenesisTransaction era (castKey inputKey) ttl fee [pseudoTxIn]
            [Exp.TxOut $ mkCoinTxOut (toShelleyAddr addr) value]
 where
  era = useEra @era
  pseudoTxIn  = genesisTxInput networkId inputKey

  fund txOutValue tx = FundInEra {
    _fundTxIn = TxIn (txIdFromSignedTx tx) (TxIx 0)
  , _fundWitness = AnyKeyWitnessPlaceholder
  , _fundVal  = txOutValue
  , _fundSigningKey = Just outputKey
  }

mkGenesisTransaction ::
     Era era
  -> SigningKey GenesisUTxOKey
  -> SlotNo
  -> L.Coin
  -> [TxIn]
  -> [Exp.TxOut (LedgerEra era)]
  -> Either TxGenError (SignedTx era)
mkGenesisTransaction era key ttl fee txins txouts
  = obtainCommonConstraints era $ do
      let expInputs = map (,AnyKeyWitnessPlaceholder) txins
          txBodyContent = Exp.defaultTxBodyContent
            & Exp.setTxIns expInputs
            & Exp.setTxOuts txouts
            & Exp.setTxFee fee
            & Exp.setTxValidityUpperBound ttl
      unsignedTx <- first (\err -> TxGenError $ "mkGenesisTransaction: " ++ show err) $ makeUnsignedTx era txBodyContent
      let witVKey = makeKeyWitness era unsignedTx (WitnessGenesisUTxOKey key)
      Right $ signTx era [] [witVKey] unsignedTx

castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey
