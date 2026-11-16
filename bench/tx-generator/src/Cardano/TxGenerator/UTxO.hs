{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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

import qualified Cardano.Ledger.Api as L (dataHashTxOutL, datumTxOutL, setMinCoinTxOut)
import           Cardano.Ledger.Hashes (DataHash)
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
outputFund :: forall era. IsShelleyBasedEra era => TxId -> TxIx -> Output era -> Fund
outputFund txId txIx (Output (Exp.TxOut txOut) Spender{..})
  = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = spenderWitness
    , _fundVal = shelleyBasedEraConstraints (shelleyBasedEra @era) $ txOut ^. L.coinTxOutL
    , _fundSigningKey = spenderKey
    }

-- | The funds that the outputs of transaction @txId@ become, given in the
-- order the transaction creates them.
outputFunds :: IsShelleyBasedEra era => TxId -> [Output era] -> [Fund]
outputFunds txId = zipWith (outputFund txId) [TxIx 0 ..]

-- | An output paying @value@ to the key's address, spent with the key.
keyOutput :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> L.Coin
  -> Output era
keyOutput networkId key value = Output
  { outputTxOut = mkTxOutToAddress (keyAddress @era networkId key) value
  , outputSpender = Spender Exp.AnyKeyWitnessPlaceholder (Just key)
  }

-- | Outputs paying a value to the address of a Plutus script, with the hash
-- of @datum@, spent with @witness@. Plutus scripts are supported from Alonzo
-- onwards.
scriptOutput :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> (ScriptInAnyLang, ScriptData)
  -> Exp.AnyWitness (ShelleyLedgerEra era)
  -> Either TxGenError (L.Coin -> Output era)
scriptOutput networkId (ScriptInAnyLang lang script, datum) witness =
  shelleyBasedEraConstraints sbe $ do
    _ <- maybe (Left $ TxGenError $ "scriptOutput: " ++ show lang ++ " is not supported in " ++ show sbe) Right $
      scriptLanguageSupportedInEra sbe lang
    -- Babbage onwards has the richer 'L.Datum' field ('L.datumTxOutL'),
    -- Alonzo only a datum hash ('L.dataHashTxOutL').
    setDatum <- case sbe of
      ShelleyBasedEraShelley  -> notSupported
      ShelleyBasedEraAllegra  -> notSupported
      ShelleyBasedEraMary     -> notSupported
      ShelleyBasedEraAlonzo   -> pure $ L.dataHashTxOutL .~ SJust datumHash
      ShelleyBasedEraBabbage  -> pure $ L.datumTxOutL .~ L.DatumHash datumHash
      ShelleyBasedEraConway   -> pure $ L.datumTxOutL .~ L.DatumHash datumHash
      ShelleyBasedEraDijkstra -> pure $ L.datumTxOutL .~ L.DatumHash datumHash
    pure $ \value -> Output
      { outputTxOut = case mkTxOutToAddress plutusScriptAddr value of
          Exp.TxOut out -> Exp.TxOut $ out & setDatum
      , outputSpender = Spender witness Nothing
      }
 where
  sbe = shelleyBasedEra @era
  notSupported = Left $ TxGenError "scriptOutput: Plutus scripts are not supported before Alonzo"
  plutusScriptAddr = makeShelleyAddressInEra sbe networkId (PaymentCredentialByScript $ hashScript script) NoStakeAddress
  datumHash :: DataHash
  datumHash = shelleyBasedEraConstraints sbe $ L.hashData $ toAlonzoData @(ShelleyLedgerEra era) $ unsafeHashableScriptData datum

-- | An output paying @value@ to the key's address, without datum or reference script.
mkTxOutToAddress :: forall era. IsShelleyBasedEra era
  => AddressInEra era
  -> L.Coin
  -> Exp.TxOut (ShelleyLedgerEra era)
mkTxOutToAddress addr value = shelleyBasedEraConstraints (shelleyBasedEra @era) $
  Exp.TxOut $ L.mkBasicTxOut (toShelleyAddr addr) (L.inject value)

-- | An output carrying @script@ as a reference script, holding the minimum
-- value the ledger requires of it. Reference scripts are supported from
-- Babbage onwards.
mkReferenceScriptTxOut :: forall era. IsShelleyBasedEra era
  => L.PParams (ShelleyLedgerEra era)
  -> AddressInEra era
  -> ScriptInAnyLang
  -> Either TxGenError (Exp.TxOut (ShelleyLedgerEra era))
mkReferenceScriptTxOut pparams addr script =
  case (forEraMaybeEon @BabbageEraOnwards (cardanoEra @era), toScriptInEra (shelleyBasedEra @era) script) of
    (Nothing, _) -> Left $ TxGenError $
      "mkReferenceScriptTxOut: reference scripts are not supported in era " ++ show (cardanoEra @era)
    (_, Nothing) -> Left $ TxGenError $
      "mkReferenceScriptTxOut: script language not supported in era " ++ show (cardanoEra @era)
    (Just babbageOnwards, Just scriptInEra) -> babbageEraOnwardsConstraints babbageOnwards $
      Right $ Exp.TxOut $ L.setMinCoinTxOut pparams $
        L.mkBasicTxOut (toShelleyAddr addr) mempty
          & L.referenceScriptTxOutL .~ SJust (toShelleyScript scriptInEra)
