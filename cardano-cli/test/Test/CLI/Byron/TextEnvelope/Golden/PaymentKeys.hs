{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Byron.TextEnvelope.Golden.PaymentKeys
  ( golden_byronPaymentKeys
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)



-- | 1. We generate a key pair
--   2. We check for the existence of the key pair
--   3. We check the TextEnvelope serialization format has not changed.
golden_byronPaymentKeys :: Property
golden_byronPaymentKeys = panic "TODO"
