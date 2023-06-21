{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-
Module      : Cardano.TxGenerator.Utils
Description : Utility functions used across the transaction generator.
-}
module  Cardano.TxGenerator.Utils
        (module Cardano.TxGenerator.Utils)
        where

import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Maybe (fromJust)

import           Cardano.Api as Api

import           Cardano.TxGenerator.Types


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
keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

-- TODO: old TODO comment also said to check minimumValuePerUtxo
-- A different variable may need to be consulted now.
-- | 'inputsToOutputsWithFee' is what actually does the division
-- for 'Cardano.Benchmarking.Script.Env.SplitN', but the result
-- may not be positive if the fee exceeds the total. If so, an
-- exception is thrown, but the normal exception types risk
-- circular imports, so this ended up using a string and expects
-- callers to use 'Control.Monad.Trans.Except.withExceptT' to put
-- the strings inside of tagged data structures.
inputsToOutputsWithFee :: Monad m => Lovelace -> Int -> [Lovelace] -> ExceptT String m [Lovelace]
inputsToOutputsWithFee fee count inputs =
  if total < fee then throwE $ "inputsToOutputs: insufficient funds "
                               ++ show total ++ " < " ++ show fee
                 else return . map (quantityToLovelace . Quantity)
                             $ (out + rest) : replicate (count-1) out
  where
    total = sum inputs
    Quantity totalAvailable = lovelaceToQuantity $ total - fee
    (out, rest) = divMod totalAvailable (fromIntegral count)

-- | 'includeChange' gets use made of it as a value splitter in
-- 'Cardano.TxGenerator.Tx.sourceToStoreTransactionNew' by
-- 'Cardano.Benchmarking.Script.Core.evalGenerator'.
includeChange :: Lovelace -> [Lovelace] -> [Lovelace] -> PayWithChange
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
mkTxFee :: forall era. IsCardanoEra era => Lovelace -> TxFee era
mkTxFee f = either
  TxFeeImplicit
  (`TxFeeExplicit` f)
  (txFeesExplicitInEra (cardanoEra @era))

-- | `mkTxValidityUpperBound` rules out needing the
-- `TxValidityNoUpperBound` with the constraint of `IsShelleyBasedEra`.
mkTxValidityUpperBound :: forall era. IsShelleyBasedEra era => SlotNo -> TxValidityUpperBound era
mkTxValidityUpperBound =
  TxValidityUpperBound (fromJust $ validityUpperBoundSupportedInEra (cardanoEra @era))

-- | `mkTxOutValueAdaOnly` reinterprets the `Either` returned by
-- `multiAssetSupportedInEra` with `TxOutValue` constructors.
mkTxOutValueAdaOnly :: forall era . IsShelleyBasedEra era => Lovelace -> TxOutValue era
mkTxOutValueAdaOnly l = either
  (`TxOutAdaOnly` l)
  (\p -> TxOutValue p $ lovelaceToValue l)
  (multiAssetSupportedInEra (cardanoEra @era))

-- | `mkTxInModeCardano` never uses the `TxInByronSpecial` constructor
-- because its type enforces it being a Shelley-based era.
mkTxInModeCardano :: forall era . IsShelleyBasedEra era => Tx era -> TxInMode CardanoMode
mkTxInModeCardano tx =
  TxInMode tx (fromJust $ toEraInMode (cardanoEra @era) CardanoMode)
