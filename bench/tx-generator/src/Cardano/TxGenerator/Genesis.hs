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
import           Cardano.Api.Compatible (CompatibleTxBodyContent (..), CompatibleTxError,
                   createCompatibleTx, defaultCompatibleTxBodyContent)
import qualified Cardano.Api.Compatible as Compatible (addWitnesses)
import           Cardano.Api.Experimental (AnyWitness (..), SignedTx (..))
import qualified Cardano.Api.Experimental.Tx as Exp

import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Core (mkCoinTxOut)
import           Cardano.Ledger.Shelley.API (Addr (..))
import           Cardano.Ledger.Shelley.Genesis (InjectionData (..), ShelleyExtraConfig (..))
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Data.Bifunctor (first, second)
import           Data.List (find)
import qualified Data.ListMap as ListMap (toList)
import           Lens.Micro ((^.))


genesisValidate ::  ShelleyGenesis -> Either String ()
genesisValidate
  = validateGenesis

genesisSecureInitialFund :: forall era. IsShelleyBasedEra era =>
     NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> SigningKey PaymentKey
  -> TxGenTxParams
  -> Either TxGenError (SignedTx era, Fund)
genesisSecureInitialFund networkId genesis srcKey destKey TxGenTxParams{txParamFee, txParamTTL} = do
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
     IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> L.Coin
  -> L.Coin
  -> SlotNo
  -> SigningKey PaymentKey
  -> Either TxGenError (SignedTx era, Fund)
genesisExpenditure networkId inputKey addr value fee ttl outputKey
  = shelleyBasedEraConstraints (shelleyBasedEra @era) $
      second (\tx -> (tx, Fund $ InAnyCardanoEra cardanoEra $ fund (lovelaceToTxOutValue (shelleyBasedEra @era) value) tx))
        $ mkGenesisTransaction (shelleyBasedEra @era) (castKey inputKey) ttl fee [pseudoTxIn]
            [Exp.TxOut $ mkCoinTxOut (toShelleyAddr addr) value]
 where
  pseudoTxIn  = genesisTxInput networkId inputKey

  fund txOutValue tx = FundInEra {
    _fundTxIn = TxIn (txIdFromSignedTx tx) (TxIx 0)
  , _fundWitness = AnyKeyWitnessPlaceholder
  , _fundVal  = txOutValue
  , _fundSigningKey = Just outputKey
  }

-- | Builds and signs the genesis-import transaction, for every era, with
-- 'Cardano.Api.Compatible.Tx.createCompatibleTx'.
-- The TTL is passed through as 'compatibleTxValidityUpperBound'.
mkGenesisTransaction ::
     ShelleyBasedEra era
  -> SigningKey GenesisUTxOKey
  -> SlotNo
  -> L.Coin
  -> [TxIn]
  -> [Exp.TxOut (ShelleyLedgerEra era)]
  -> Either TxGenError (SignedTx era)
mkGenesisTransaction sbe key ttl fee txins txouts =
  shelleyBasedEraConstraints sbe $ do
    let expInputs = map (,AnyKeyWitnessPlaceholder) txins
        bodyContent = (defaultCompatibleTxBodyContent sbe)
          { compatibleTxIns = expInputs
          , compatibleTxOuts = txouts
          , compatibleTxFee = fee
          , compatibleTxValidityUpperBound = Just ttl
          }
    unsignedTx@(ShelleyTx _ unsignedLedgerTx) <-
      first (\err -> TxGenError $ "mkGenesisTransaction: " ++ show (err :: CompatibleTxError)) $
        createCompatibleTx sbe bodyContent
    let ledgerBody = unsignedLedgerTx ^. Ledger.bodyTxL
        witVKey = makeShelleyKeyWitness' sbe ledgerBody (WitnessGenesisUTxOKey key)
        ShelleyTx _ signedLedgerTx = Compatible.addWitnesses [witVKey] unsignedTx
    Right $ SignedTx signedLedgerTx

castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey
