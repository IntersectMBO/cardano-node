{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.UTxO
        (module Cardano.TxGenerator.UTxO)
        where

import           Cardano.Api
import           Cardano.Api.Shelley (ReferenceScript (..))

import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..))
import           Cardano.TxGenerator.Utils (keyAddress)

type ToUTxO era = Lovelace -> (TxOut CtxTx era, TxIx -> TxId -> Fund)
type ToUTxOList era split = split -> ([TxOut CtxTx era], TxId -> [Fund])


makeToUTxOList :: [ ToUTxO era ] -> ToUTxOList era [ Lovelace ]
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
mkUTxOVariant networkId key value
  = ( mkTxOut value
    , mkNewFund value
    )
 where
  mkTxOut v = TxOut (keyAddress @era networkId key) (lovelaceToTxOutValue (shelleyBasedEra @era) v) TxOutDatumNone ReferenceScriptNone

  mkNewFund :: Lovelace -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = KeyWitness KeyWitnessForSpending
    , _fundVal = lovelaceToTxOutValue (shelleyBasedEra @era ) val
    , _fundSigningKey = Just key
    }

-- to be merged with mkUTxOVariant
mkUTxOScript :: forall era.
     IsShelleyBasedEra era
  => NetworkId
  -> (ScriptInAnyLang, ScriptData)
  -> Witness WitCtxTxIn era
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

  mkTxOut v = case forEraMaybeEon (cardanoEra @era) of
    Nothing -> error "mkUtxOScript: scriptDataSupportedInEra==Nothing"
    Just tag -> TxOut
                  plutusScriptAddr
                  (lovelaceToTxOutValue (shelleyBasedEra @era) v)
                  (TxOutDatumHash tag $ hashScriptDataBytes $ unsafeHashableScriptData txOutDatum)
                  ReferenceScriptNone

  mkNewFund :: Lovelace -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = witness
    , _fundVal = lovelaceToTxOutValue (shelleyBasedEra @era) val
    , _fundSigningKey = Nothing
    }
