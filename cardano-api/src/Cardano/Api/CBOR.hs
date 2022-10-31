{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Helpers for retaining user supplied CBOR
module Cardano.Api.CBOR
  ( AsType(..)
  , WithCBOR
  , getCBOR
  , getCBORShort
  , withoutCBOR
  , withCBORViaRoundtrip
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import           Data.ByteString.Short (ShortByteString, toShort)

import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash, originalBytes)

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR

-- | A value, paired with its representation. To generate something of
-- type 'WithCBOR', either call 'deserialiseFromCBOR' if you already have
-- CBOR, or call 'withCBORViaRoundtrip' if you need to pair a value with the
-- CBOR from serialising it.
data WithCBOR a = WithCBOR !ByteString !a
    deriving (Eq, Show)

instance Ord a => Ord (WithCBOR a) where
    compare (WithCBOR bx x) (WithCBOR by y) = case compare x y of
      EQ -> compare bx by -- We need to match the derived Eq
      res -> res

-- | Extract the CBOR from a 'WithCBOR'
getCBOR :: WithCBOR a -> ByteString
getCBOR (WithCBOR bs _ ) = bs

-- | Extract the CBOR as a short bytestring
getCBORShort :: WithCBOR a -> ShortByteString
getCBORShort = toShort . getCBOR

-- | Extract the value from a 'WithCBOR'
withoutCBOR :: WithCBOR a -> a
withoutCBOR (WithCBOR _ x) = x

instance HasTypeProxy a => HasTypeProxy (WithCBOR a) where
    data AsType (WithCBOR a) = AsWithCBOR (AsType a)
    proxyToAsType _ = AsWithCBOR (proxyToAsType (Proxy @a))

instance SerialiseAsCBOR a => SerialiseAsCBOR (WithCBOR a) where
    serialiseToCBOR (WithCBOR bs _) = bs
    deserialiseFromCBOR (AsWithCBOR p) bs = WithCBOR bs <$> deserialiseFromCBOR p bs

instance SafeToHash (WithCBOR a) where
    originalBytes = getCBOR

instance HashAnnotated (WithCBOR a) a StandardCrypto

-- | Create a value of type 'WithCBOR' by first serialising it and then deserialising it.
-- Note that the value stored here may not be the original value of serialisation doesn't
-- roundtrip, and this will throw an error if we can't deserialise from the generated
-- CBOR.
withCBORViaRoundtrip :: forall a . SerialiseAsCBOR a => a -> WithCBOR a
withCBORViaRoundtrip x = case deserialiseFromCBOR (AsWithCBOR (proxyToAsType $ Proxy @a)) (serialiseToCBOR x) of
    Left err -> error $ "withCBORViaRoundtrip: Does not round trip " <> show err
    Right x' -> x'
