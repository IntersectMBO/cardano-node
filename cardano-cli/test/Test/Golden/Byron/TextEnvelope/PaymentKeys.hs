{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.TextEnvelope.PaymentKeys
  ( golden_byronPaymentKeys
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)


-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_byronPaymentKeys :: Property
golden_byronPaymentKeys = panic "TODO"
