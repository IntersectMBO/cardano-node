{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.UTxO
        (module Cardano.TxGenerator.UTxO)
        where

import           Cardano.Api hiding (txId)
import           Cardano.Api.Experimental (AnyWitness (..), IsEra, LedgerEra,
                   obtainCommonConstraints, useEra)
import qualified Cardano.Api.Experimental.Tx as Exp

import           Cardano.Ledger.Api.Tx.Out (datumTxOutL)
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Core (mkCoinTxOut)
import qualified Cardano.Ledger.Plutus.Data as Plutus
import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..))
import           Cardano.TxGenerator.Utils (keyAddress)

import           Lens.Micro ((&), (.~))

type ToUTxO era = L.Coin -> (Exp.TxOut (LedgerEra era), TxIx -> TxId -> Fund)
type ToUTxOList era split = split -> ([Exp.TxOut (LedgerEra era)], TxId -> [Fund])


makeToUTxOList :: [ ToUTxO era ] -> ToUTxOList era [ L.Coin ]
makeToUTxOList fkts values
  = (outs, \txId -> map (\f -> f txId) fs)
  where
    (outs, fs) =unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
         in  (o, f idx)

mkUTxOVariant :: forall era. IsEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> ToUTxO era
mkUTxOVariant networkId key value = obtainCommonConstraints (useEra @era) $
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
mkUTxOScript :: forall era.
     IsEra era
  => NetworkId
  -> (ScriptInAnyLang, ScriptData)
  -> AnyWitness era
  -> ToUTxO era
mkUTxOScript networkId (script, txOutDatum) witness value = obtainCommonConstraints (useEra @era) $
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
    datumHash :: Plutus.Datum (LedgerEra era)
    datumHash = Plutus.DatumHash $ Plutus.hashData $ toAlonzoData @(LedgerEra era) $ unsafeHashableScriptData txOutDatum
    mkTxOut v = Exp.TxOut $
      mkCoinTxOut (toShelleyAddr plutusScriptAddr) v & datumTxOutL .~ datumHash
    mkNewFund :: L.Coin -> TxIx -> TxId -> Fund
    mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
        _fundTxIn = TxIn txId txIx
      , _fundWitness = witness
      , _fundVal = lovelaceToTxOutValue (shelleyBasedEra @era) val
      , _fundSigningKey = Nothing
      }
  in (mkTxOut value, mkNewFund value)
