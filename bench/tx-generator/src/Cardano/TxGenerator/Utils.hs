{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Utils
        (module Cardano.TxGenerator.Utils)
        where

import           Data.Maybe (fromJust)

import           Cardano.Api as Api

import           Cardano.TxGenerator.Types


liftAnyEra :: ( forall era. IsCardanoEra era => f1 era -> f2 era ) -> InAnyCardanoEra f1 -> InAnyCardanoEra f2
liftAnyEra f x = case x of
  InAnyCardanoEra ByronEra a   ->   InAnyCardanoEra ByronEra $ f a
  InAnyCardanoEra ShelleyEra a ->   InAnyCardanoEra ShelleyEra $ f a
  InAnyCardanoEra AllegraEra a ->   InAnyCardanoEra AllegraEra $ f a
  InAnyCardanoEra MaryEra a    ->   InAnyCardanoEra MaryEra $ f a
  InAnyCardanoEra AlonzoEra a  ->   InAnyCardanoEra AlonzoEra $ f a
  InAnyCardanoEra BabbageEra a  ->  InAnyCardanoEra BabbageEra $ f a

keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

-- TODO: check sufficient funds and minimumValuePerUtxo
inputsToOutputsWithFee :: Lovelace -> Int -> [Lovelace] -> [Lovelace]
inputsToOutputsWithFee fee count inputs = map (quantityToLovelace . Quantity) outputs
  where
    (Quantity totalAvailable) = lovelaceToQuantity $ sum inputs - fee
    (out, rest) = divMod totalAvailable (fromIntegral count)
    outputs = (out + rest) : replicate (count-1) out

includeChange :: Lovelace -> [Lovelace] -> [Lovelace] -> PayWithChange
includeChange fee spend have = case compare changeValue 0 of
  GT -> PayWithChange changeValue spend
  EQ -> PayExact spend
  LT -> error "includeChange: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend - fee


-- some convenience constructors
mkTxFee :: forall era. IsCardanoEra era => Lovelace -> TxFee era
mkTxFee f = either
  TxFeeImplicit
  (`TxFeeExplicit` f)
  (txFeesExplicitInEra (cardanoEra @era))

mkTxValidityUpperBound :: forall era. IsShelleyBasedEra era => SlotNo -> TxValidityUpperBound era
mkTxValidityUpperBound =
  TxValidityUpperBound (fromJust $ validityUpperBoundSupportedInEra (cardanoEra @era))

mkTxOutValueAdaOnly :: forall era . IsShelleyBasedEra era => Lovelace -> TxOutValue era
mkTxOutValueAdaOnly l = either 
  (`TxOutAdaOnly` l)
  (\p -> TxOutValue p $ lovelaceToValue l)
  (multiAssetSupportedInEra (cardanoEra @era))

mkTxInModeCardano :: forall era . IsShelleyBasedEra era => Tx era -> TxInMode CardanoMode
mkTxInModeCardano tx =
  TxInMode tx (fromJust $ toEraInMode (cardanoEra @era) CardanoMode)
