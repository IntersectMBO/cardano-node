{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

mkUTxOVariant :: forall era. Exp.EraCommonConstraints era
  => NetworkId
  -> SigningKey PaymentKey
  -> ToUTxO era
mkUTxOVariant networkId key value
  = ( mkTxOutToAddress (keyAddress @era networkId key) value
    , mkNewFund value
    )
 where
  mkNewFund :: L.Coin -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = Exp.AnyKeyWitnessPlaceholder
    , _fundVal = val
    , _fundSigningKey = Just key
    }

-- to be merged with mkUTxOVariant
mkUTxOScript :: forall era.
     Exp.EraCommonConstraints era
  => NetworkId
  -> (ScriptInAnyLang, ScriptData)
  -> Exp.AnyWitness (ShelleyLedgerEra era)
  -> ToUTxO era
mkUTxOScript networkId (script, txOutDatum) witness value
  = ( mkTxOut value
    , mkNewFund value
    )
 where
  plutusScriptAddr = case script of
    ScriptInAnyLang lang script' ->
      case scriptLanguageSupportedInEra (shelleyBasedEra @era) lang of
        Nothing -> error "mkUtxOScript: scriptLanguageSupportedInEra==Nothing"
        Just{} -> makeShelleyAddressInEra
                       (shelleyBasedEra @era)
                       networkId
                       (PaymentCredentialByScript $ hashScript script')
                       NoStakeAddress

  datumHash = L.hashData $ toAlonzoData @(ShelleyLedgerEra era) $ unsafeHashableScriptData txOutDatum

  mkTxOut :: L.Coin -> Exp.TxOut (ShelleyLedgerEra era)
  mkTxOut v = case mkTxOutToAddress plutusScriptAddr v of
    Exp.TxOut out -> Exp.TxOut $ out & L.datumTxOutL .~ L.DatumHash datumHash

  mkNewFund :: L.Coin -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = witness
    , _fundVal = val
    , _fundSigningKey = Nothing
    }
