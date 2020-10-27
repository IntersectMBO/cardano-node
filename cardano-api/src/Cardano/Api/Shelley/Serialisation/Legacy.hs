{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Support for legacy serialisation formats that are no longer available in
-- @cardano-ledger-specs@.
module Cardano.Api.Shelley.Serialisation.Legacy
  ( WrappedMultiSig (..)
  ) where

import           Cardano.Prelude

import           Cardano.Binary
import           Cardano.Ledger.Era (Era)
import           NoThunks.Class (NoThunks (..))
import           Shelley.Spec.Ledger.BaseTypes (invalidKey)
import           Shelley.Spec.Ledger.Scripts (MultiSig)
import           Shelley.Spec.Ledger.Serialization (decodeRecordSum)


-- | Wrapper type for a 'MultiSig' script.
--
-- Used to support the old @Script@ binary serialization format from
-- @cardano-ledger-specs@.
newtype WrappedMultiSig era = WrappedMultiSig
  { unWrappedMultiSig :: MultiSig era }
  deriving newtype (Eq, Ord, Show, NoThunks)
  deriving stock (Generic)

instance (Era era, Typeable era) => FromCBOR (Annotator (WrappedMultiSig era)) where
  fromCBOR = decodeRecordSum "WrappedMultiSig" $
    \case
      0 -> do
        s <- fromCBOR
        pure (2, WrappedMultiSig <$> s)
      k -> invalidKey k

instance Typeable era => ToCBOR (WrappedMultiSig era) where
  toCBOR ms =
    encodeListLen 2
      <> toCBOR (0 :: Word8)
      <> toCBOR (unWrappedMultiSig ms)
