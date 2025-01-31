{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module  Cardano.TxGenerator.UTxO
        (module Cardano.TxGenerator.UTxO)
        where

import           Cardano.Api
import           Cardano.Api.Shelley (ReferenceScript (..))

import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..))
import           Cardano.TxGenerator.Utils (keyAddress)

type ToUTxO era = L.Coin -> (TxOut CtxTx era, TxIx -> TxId -> Fund)
type ToUTxOList era split = split -> ([TxOut CtxTx era], TxId -> [Fund])


makeToUTxOList :: [ ToUTxO era ] -> ToUTxOList era [ L.Coin ]
makeToUTxOList fkts values
  = (outs, \txId -> map (\f -> f txId) fs)
  where
    (outs, fs) =unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
         in  (o, f idx)

mkUTxOVariant :: ()
  => ShelleyBasedEra era
  -> NetworkId
  -> SigningKey PaymentKey
  -> ToUTxO era
mkUTxOVariant sbe networkId key value
  = ( mkTxOut value
    , mkNewFund value
    )
 where
  mkTxOut v = TxOut (keyAddress sbe networkId key) (lovelaceToTxOutValue sbe v) TxOutDatumNone ReferenceScriptNone

  mkNewFund :: L.Coin -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = shelleyBasedEraConstraints sbe $ Fund $ InAnyCardanoEra (toCardanoEra sbe) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = KeyWitness KeyWitnessForSpending
    , _fundVal = lovelaceToTxOutValue sbe val
    , _fundSigningKey = Just key
    }

-- to be merged with mkUTxOVariant
mkUTxOScript ::
     ShelleyBasedEra era
  -> NetworkId
  -> (ScriptInAnyLang, ScriptData)
  -> Witness WitCtxTxIn era
  -> ToUTxO era
mkUTxOScript sbe networkId (script, txOutDatum) witness value
  = ( mkTxOut value
    , mkNewFund value
    )
 where
  plutusScriptAddr = case script of
    ScriptInAnyLang lang script' ->
      case scriptLanguageSupportedInEra sbe lang of
        Nothing -> error "mkUtxOScript: scriptLanguageSupportedInEra==Nothing"
        Just{} -> makeShelleyAddressInEra
                       sbe
                       networkId
                       (PaymentCredentialByScript $ hashScript script')
                       NoStakeAddress

  mkTxOut v = case forShelleyBasedEraMaybeEon sbe of
    Nothing -> error "mkUtxOScript: scriptDataSupportedInEra==Nothing"
    Just tag -> TxOut
                  plutusScriptAddr
                  (lovelaceToTxOutValue sbe v)
                  (TxOutDatumHash tag $ hashScriptDataBytes $ unsafeHashableScriptData txOutDatum)
                  ReferenceScriptNone

  mkNewFund :: L.Coin -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = shelleyBasedEraConstraints sbe $ Fund $ InAnyCardanoEra (toCardanoEra sbe) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = witness
    , _fundVal = lovelaceToTxOutValue sbe val
    , _fundSigningKey = Nothing
    }
