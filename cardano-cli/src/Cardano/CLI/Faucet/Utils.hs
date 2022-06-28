{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Faucet.Utils where

import Cardano.Prelude
import Cardano.Api (TxIn, TxOut(TxOut), CtxUTxO, Lovelace, CardanoEra, TxFee, txFeesExplicitInEra, TxFee(TxFeeImplicit, TxFeeExplicit), anyCardanoEra, TxValidityLowerBound(TxValidityNoLowerBound), TxValidityUpperBound(TxValidityNoUpperBound), validityNoUpperBoundSupportedInEra)
import Cardano.CLI.Faucet.Types
import Cardano.CLI.Faucet.Misc
import Control.Concurrent.STM (TMVar, takeTMVar, putTMVar)
import Data.Map.Strict qualified as Map
import Control.Monad.Trans.Except.Extra (left)
import Cardano.CLI.Shelley.Run.Transaction

takeOneUtxo :: TMVar (Map TxIn (TxOut ctx era)) -> Lovelace -> STM (Maybe (TxIn, TxOut ctx era))
takeOneUtxo utxoTMVar lovelace = do
  utxo <- takeTMVar utxoTMVar
  let
    unwrap (TxOut _ value _ _) = getValue value
    utxoOfRightSize = Map.filter (\out -> unwrap out == lovelace) utxo
    mTxin = head $ Map.toList $ Map.take 1 utxoOfRightSize
  case mTxin of
    Just (txin, txout) -> do
      let
        trimmedUtxo = Map.delete txin utxo
      putTMVar utxoTMVar trimmedUtxo
      pure $ Just (txin, txout)
    Nothing -> do
      putTMVar utxoTMVar utxo
      pure Nothing

findUtxoOfSize :: TMVar (Map TxIn (TxOut CtxUTxO era)) -> Lovelace -> ExceptT FaucetError IO (TxIn, TxOut CtxUTxO era)
findUtxoOfSize utxoTMVar lovelace = do
  mTxinout <- liftIO $ atomically $ takeOneUtxo utxoTMVar lovelace
  case mTxinout of
    Just txinout -> pure txinout
    Nothing -> left $ FaucetErrorUtxoNotFound

validateTxFee ::
     CardanoEra era
  -> Maybe Lovelace
  -> ExceptT FaucetError IO (TxFee era)
validateTxFee era mfee = case (txFeesExplicitInEra era, mfee) of
  (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
  (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)
  (Right _, Nothing) -> txFeatureMismatch era TxFeatureImplicitFees
  (Left  _, Just _)  -> txFeatureMismatch era TxFeatureExplicitFees

txFeatureMismatch ::
     CardanoEra era
  -> TxFeature
  -> ExceptT FaucetError IO a
txFeatureMismatch era feature = left (FaucetErrorFeatureMismatch (anyCardanoEra era) feature)

noBoundsIfSupported ::
     CardanoEra era
  -> ExceptT FaucetError IO (TxValidityLowerBound era, TxValidityUpperBound era)
noBoundsIfSupported era = (,)
  <$> pure TxValidityNoLowerBound
  <*> noUpperBoundIfSupported era

noUpperBoundIfSupported ::
     CardanoEra era
  -> ExceptT FaucetError IO (TxValidityUpperBound era)
noUpperBoundIfSupported era = case validityNoUpperBoundSupportedInEra era of
  Nothing -> txFeatureMismatch era TxFeatureValidityNoUpperBound
  Just supported -> return (TxValidityNoUpperBound supported)
