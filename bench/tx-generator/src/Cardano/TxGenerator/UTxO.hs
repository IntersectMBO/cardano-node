{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Cardano.TxGenerator.UTxO
Description : The outputs transactions create, and the funds they become.

An 'Output' is decided before the transaction that creates it is built, and
becomes a 'Fund' once the transaction's id is known: output @i@ of transaction
@txId@ is spent as @TxIn txId (TxIx i)@.
-}
module  Cardano.TxGenerator.UTxO
        (module Cardano.TxGenerator.UTxO)
        where

import           Cardano.Api hiding (txId)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Api as L (datumTxOutL, setMinCoinTxOut)
import qualified Cardano.Ledger.Plutus.Data as L (hashData)
import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..))
import           Cardano.TxGenerator.Types (TxGenError (..))
import           Cardano.TxGenerator.Utils (keyAddress)

import           Data.Maybe.Strict (StrictMaybe (SJust))
import           Lens.Micro ((&), (.~), (^.))

-- | An output for a transaction to create, together with how the fund it
-- becomes is spent.
data Output era = Output
  { outputTxOut   :: !(Exp.TxOut (ShelleyLedgerEra era))
  , outputSpender :: !(Spender era)
  }

-- | How a fund is spent: what a 'Fund' holds besides its 'TxIn' and value.
data Spender era = Spender
  { spenderWitness :: !(Exp.AnyWitness (ShelleyLedgerEra era))
  , spenderKey     :: !(Maybe (SigningKey PaymentKey))
  }

-- | The fund that an output becomes as output @txIx@ of transaction @txId@.
outputFund :: forall era. Exp.EraCommonConstraints era => TxId -> TxIx -> Output era -> Fund
outputFund txId txIx (Output (Exp.TxOut txOut) Spender{..})
  = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = spenderWitness
    , _fundVal = txOut ^. L.coinTxOutL
    , _fundSigningKey = spenderKey
    }

-- | The funds that the outputs of transaction @txId@ become, given in the
-- order the transaction creates them.
outputFunds :: Exp.EraCommonConstraints era => TxId -> [Output era] -> [Fund]
outputFunds txId = zipWith (outputFund txId) [TxIx 0 ..]

-- | An output paying @value@ to the key's address, spent with the key.
keyOutput :: forall era. Exp.EraCommonConstraints era
  => NetworkId
  -> SigningKey PaymentKey
  -> L.Coin
  -> Output era
keyOutput networkId key value = Output
  { outputTxOut = mkTxOutToAddress (keyAddress @era networkId key) value
  , outputSpender = Spender Exp.AnyKeyWitnessPlaceholder (Just key)
  }

-- | An output paying @value@ to the address of a Plutus script, with the hash
-- of @datum@, spent with @witness@.
scriptOutput :: forall era. Exp.EraCommonConstraints era
  => NetworkId
  -> (ScriptInAnyLang, ScriptData)
  -> Exp.AnyWitness (ShelleyLedgerEra era)
  -> L.Coin
  -> Output era
scriptOutput networkId (script, datum) witness value = Output
  { outputTxOut = case mkTxOutToAddress plutusScriptAddr value of
      Exp.TxOut out -> Exp.TxOut $ out & L.datumTxOutL .~ L.DatumHash datumHash
  , outputSpender = Spender witness Nothing
  }
 where
  plutusScriptAddr = case script of
    ScriptInAnyLang lang script' ->
      case scriptLanguageSupportedInEra (shelleyBasedEra @era) lang of
        Nothing -> error "scriptOutput: scriptLanguageSupportedInEra==Nothing"
        Just{} -> makeShelleyAddressInEra
                       (shelleyBasedEra @era)
                       networkId
                       (PaymentCredentialByScript $ hashScript script')
                       NoStakeAddress

  datumHash = L.hashData $ toAlonzoData @(ShelleyLedgerEra era) $ unsafeHashableScriptData datum

-- | An output paying @value@ to the key's address, without datum or reference script.
mkTxOutToAddress :: forall era. Exp.EraCommonConstraints era
  => AddressInEra era
  -> L.Coin
  -> Exp.TxOut (ShelleyLedgerEra era)
mkTxOutToAddress addr value
  = Exp.TxOut $ L.mkBasicTxOut (toShelleyAddr addr) (L.inject value)

-- | An output carrying @script@ as a reference script, holding the minimum
-- value the ledger requires of it.
mkReferenceScriptTxOut :: forall era. Exp.EraCommonConstraints era
  => L.PParams (ShelleyLedgerEra era)
  -> AddressInEra era
  -> ScriptInAnyLang
  -> Either TxGenError (Exp.TxOut (ShelleyLedgerEra era))
mkReferenceScriptTxOut pparams addr script
  = case toScriptInEra (shelleyBasedEra @era) script of
      Nothing -> Left $ TxGenError $
        "mkReferenceScriptTxOut: script language not supported in era " ++ show (cardanoEra @era)
      Just scriptInEra -> Right $ Exp.TxOut $ L.setMinCoinTxOut pparams $
        L.mkBasicTxOut (toShelleyAddr addr) mempty
          & L.referenceScriptTxOutL .~ SJust (toShelleyScript scriptInEra)
