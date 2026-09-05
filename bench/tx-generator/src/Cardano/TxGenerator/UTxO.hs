{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.UTxO
        (module Cardano.TxGenerator.UTxO)
        where

import           Cardano.Api hiding (txId)
import           Cardano.Api.Experimental (AnyWitness (..))
import qualified Cardano.Api.Experimental.Tx as Exp

import           Cardano.Ledger.Api.Tx.Out (dataHashTxOutL, datumTxOutL)
import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Core (mkCoinTxOut)
import qualified Cardano.Ledger.Core as Ledger (TxOut)
import qualified Cardano.Ledger.Plutus.Data as Plutus
import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..))
import           Cardano.TxGenerator.Utils (keyAddress)

import           Lens.Micro ((&), (.~))

type ToUTxO era = L.Coin -> (Exp.TxOut (ShelleyLedgerEra era), TxIx -> TxId -> Fund)
type ToUTxOList era split = split -> ([Exp.TxOut (ShelleyLedgerEra era)], TxId -> [Fund])


makeToUTxOList :: [ ToUTxO era ] -> ToUTxOList era [ L.Coin ]
makeToUTxOList fkts values
  = (outs, \txId -> map (\f -> f txId) fs)
  where
    (outs, fs) =unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
         in  (o, f idx)

mkUTxOVariant :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> ToUTxO era
mkUTxOVariant networkId key value = shelleyBasedEraConstraints (shelleyBasedEra @era) $
  let
    mkTxOut v = Exp.TxOut $ mkCoinTxOut (toShelleyAddr $ keyAddress @era networkId key) v
    mkNewFund :: L.Coin -> TxIx -> TxId -> Fund
    mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
        _fundTxIn = TxIn txId txIx
      , _fundWitness = AnyKeyWitnessPlaceholder
      , _fundVal = lovelaceToTxOutValue (shelleyBasedEra @era) val
      , _fundSigningKey = Just key
      }
  in (mkTxOut value, mkNewFund value)

-- to be merged with mkUTxOVariant
-- | Plutus-locked UTxOs need at least Alonzo.
-- Alonzo 'TxOut' only has a bare datum hash field ('dataHashTxOutL').
-- Babbage onwards has the richer 'Datum' field ('datumTxOutL'), which also
-- supports inline datums.
-- 'setDatum' below picks the right lens for the concrete era.
mkUTxOScript :: forall era.
     AlonzoEraOnwards era
  -> NetworkId
  -> (ScriptInAnyLang, ScriptData)
  -> AnyWitness (ShelleyLedgerEra era)
  -> ToUTxO era
mkUTxOScript alonzoOnwards networkId (script, txOutDatum) witness value = alonzoEraOnwardsConstraints alonzoOnwards $
  let
    plutusScriptAddr = case script of
      ScriptInAnyLang lang script' ->
        case scriptLanguageSupportedInEra (shelleyBasedEra @era) lang of
          Nothing -> error "mkUtxOScript: scriptLanguageSupportedInEra==Nothing"
          Just{} -> makeShelleyAddressInEra
                         (shelleyBasedEra @era)
                         networkId
                         (PaymentCredentialByScript $ hashScript script')
                         NoStakeAddress
    dataHash = Plutus.hashData $ toAlonzoData @(ShelleyLedgerEra era) $ unsafeHashableScriptData txOutDatum
    setDatum :: Ledger.TxOut (ShelleyLedgerEra era) -> Ledger.TxOut (ShelleyLedgerEra era)
    setDatum = case alonzoOnwards of
      AlonzoEraOnwardsAlonzo   -> dataHashTxOutL .~ SJust dataHash
      AlonzoEraOnwardsBabbage  -> datumTxOutL .~ Plutus.DatumHash dataHash
      AlonzoEraOnwardsConway   -> datumTxOutL .~ Plutus.DatumHash dataHash
      AlonzoEraOnwardsDijkstra -> datumTxOutL .~ Plutus.DatumHash dataHash
    mkTxOut v = Exp.TxOut $
      mkCoinTxOut (toShelleyAddr plutusScriptAddr) v & setDatum
    mkNewFund :: L.Coin -> TxIx -> TxId -> Fund
    mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
        _fundTxIn = TxIn txId txIx
      , _fundWitness = witness
      , _fundVal = lovelaceToTxOutValue (shelleyBasedEra @era) val
      , _fundSigningKey = Nothing
      }
  in (mkTxOut value, mkNewFund value)
