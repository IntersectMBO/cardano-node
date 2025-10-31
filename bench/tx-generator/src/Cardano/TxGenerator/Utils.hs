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

import           Cardano.Api as Api
import qualified Cardano.Api.Parser.Text as P

import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.Types

import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           GHC.Stack


-- | `liftAnyEra` applies a function to the value in `InAnyCardanoEra`
-- regardless of which particular era.
liftAnyEra :: ( forall era. IsCardanoEra era => f1 era -> f2 era ) -> InAnyCardanoEra f1 -> InAnyCardanoEra f2
liftAnyEra f x = case x of
  InAnyCardanoEra ByronEra a     ->   InAnyCardanoEra ByronEra $ f a
  InAnyCardanoEra ShelleyEra a   ->   InAnyCardanoEra ShelleyEra $ f a
  InAnyCardanoEra AllegraEra a   ->   InAnyCardanoEra AllegraEra $ f a
  InAnyCardanoEra MaryEra a      ->   InAnyCardanoEra MaryEra $ f a
  InAnyCardanoEra AlonzoEra a    ->   InAnyCardanoEra AlonzoEra $ f a
  InAnyCardanoEra BabbageEra a   ->   InAnyCardanoEra BabbageEra $ f a
  InAnyCardanoEra ConwayEra a    ->   InAnyCardanoEra ConwayEra $ f a
  InAnyCardanoEra DijkstraEra a  ->   InAnyCardanoEra DijkstraEra $ f a

-- | `keyAddress` determines an address for the relevant era.
keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      (shelleyBasedEra @era)
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
mkTxFee :: IsShelleyBasedEra era => L.Coin -> TxFee era
mkTxFee = TxFeeExplicit shelleyBasedEra

-- | `mkTxValidityUpperBound` rules out needing the
-- `TxValidityNoUpperBound` with the constraint of `IsShelleyBasedEra`.
mkTxValidityUpperBound :: forall era. IsShelleyBasedEra era => SlotNo -> TxValidityUpperBound era
mkTxValidityUpperBound slotNo =
  TxValidityUpperBound (fromJust $ forEraMaybeEon (cardanoEra @era)) (Just slotNo)

-- | `mkTxInModeCardano` never uses the `TxInByronSpecial` constructor
-- because its type enforces it being a Shelley-based era.
mkTxInModeCardano :: IsShelleyBasedEra era => Tx era -> TxInMode
mkTxInModeCardano = TxInMode shelleyBasedEra

-- | Convert text representation of a txin "hash#txid" to a TxIn e.g. "dbaff4e270cfb55612d9e2ac4658a27c79da4a5271c6f90853042d1403733810#0"
-- Partial. Useful in tests.
mkTxIn :: HasCallStack => Text -> TxIn
mkTxIn = either error id . P.runParser parseTxIn
