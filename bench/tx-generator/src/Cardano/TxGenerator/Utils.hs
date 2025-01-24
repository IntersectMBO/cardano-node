{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Module      : Cardano.TxGenerator.Utils
Description : Utility functions used across the transaction generator.
-}
module  Cardano.TxGenerator.Utils
        (module Cardano.TxGenerator.Utils)
        where

import           Cardano.Api as Api

import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.Types

import           Data.Maybe (fromJust)


-- | `liftAnyEra` applies a function to the value in `InAnyCardanoEra`
-- regardless of which particular era.
liftAnyEra :: ( forall era. IsCardanoEra era => f1 era -> f2 era ) -> InAnyCardanoEra f1 -> InAnyCardanoEra f2
liftAnyEra f x = case x of
  InAnyCardanoEra ByronEra a   ->   InAnyCardanoEra ByronEra $ f a
  InAnyCardanoEra ShelleyEra a ->   InAnyCardanoEra ShelleyEra $ f a
  InAnyCardanoEra AllegraEra a ->   InAnyCardanoEra AllegraEra $ f a
  InAnyCardanoEra MaryEra a    ->   InAnyCardanoEra MaryEra $ f a
  InAnyCardanoEra AlonzoEra a  ->   InAnyCardanoEra AlonzoEra $ f a
  InAnyCardanoEra BabbageEra a ->   InAnyCardanoEra BabbageEra $ f a
  InAnyCardanoEra ConwayEra a  ->   InAnyCardanoEra ConwayEra $ f a

-- | `keyAddress` determines an address for the relevant era.
keyAddress :: ShelleyBasedEra era -> NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress sbe networkId k
  = makeShelleyAddressInEra
      sbe
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

-- TODO: check sufficient funds and minimumValuePerUtxo
inputsToOutputsWithFee :: L.Coin -> Int -> [L.Coin] -> [L.Coin]
inputsToOutputsWithFee fee count inputs = map (quantityToLovelace . Quantity) outputs
  where
    (Quantity totalAvailable) = lovelaceToQuantity $ sum inputs - fee
    (out, rest) = divMod totalAvailable (fromIntegral count)
    outputs = (out + rest) : replicate (count-1) out

-- | 'includeChange' gets use made of it as a value splitter in
-- 'Cardano.TxGenerator.Tx.sourceToStoreTransactionNew' by
-- 'Cardano.Benchmarking.Script.Core.evalGenerator'.
includeChange :: L.Coin -> [L.Coin] -> [L.Coin] -> PayWithChange
includeChange fee spend have = case compare changeValue 0 of
  GT -> PayWithChange changeValue spend
  EQ -> PayExact spend
  LT -> error $ "includeChange: Bad transaction: insufficient funds" ++
                "\n   have: " ++ show have ++
                "\n  spend: " ++ show spend ++
                "\n    fee: " ++ show fee
  where changeValue = sum have - sum spend - fee


-- some convenience constructors

-- | `mkTxFee` reinterprets the `Either` returned by
-- `txFeesExplicitInEra` with `TxFee` constructors.
mkTxFee :: ShelleyBasedEra era -> L.Coin -> TxFee era
mkTxFee = TxFeeExplicit

-- | `mkTxValidityUpperBound` rules out needing the
-- `TxValidityNoUpperBound` with the `ShelleyBasedEra` witness.
mkTxValidityUpperBound :: ShelleyBasedEra era -> SlotNo -> TxValidityUpperBound era
mkTxValidityUpperBound sbe slotNo =
  TxValidityUpperBound (fromJust $ forShelleyBasedEraMaybeEon sbe) (Just slotNo)
