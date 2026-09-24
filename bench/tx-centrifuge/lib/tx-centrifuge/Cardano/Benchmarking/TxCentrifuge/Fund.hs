{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.Fund
  ( Fund (..)
  , readSigningKey
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Bifunctor (first)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api

--------------------------------------------------------------------------------

-- | A spendable fund: a UTxO reference, its Lovelace value, and the signing key
-- required to spend it.
data Fund = Fund
  { fundTxIn :: !Api.TxIn
    -- | Lovelace amount.
  , fundValue :: !Integer
    -- | Key to spend this UTxO.
  , fundSignKey :: !(Api.SigningKey Api.PaymentKey)
  }

--------------------------------------------------------------------------------

-- | Read a signing key from a text envelope file.
-- Accepts both @PaymentSigningKey_ed25519@ and
-- @GenesisUTxOSigningKey_ed25519@ key types.
-- Genesis UTxO keys are cast to payment keys.
readSigningKey :: FilePath -> IO (Either String (Api.SigningKey Api.PaymentKey))
readSigningKey fp = do
  result <- Api.readFileTextEnvelopeAnyOf
    [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) id
    , Api.FromSomeType
        (Api.AsSigningKey Api.AsGenesisUTxOKey)
        Api.castSigningKey
    ]
    (Api.File fp)
  pure $ first show result
