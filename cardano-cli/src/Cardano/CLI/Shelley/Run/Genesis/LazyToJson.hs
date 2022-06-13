{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Shelley.Run.Genesis.LazyToJson
  ( Aeson(..)
  , LazyToJson(..)
  ) where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Crypto (StandardCrypto)
import Data.Aeson (Value, ToJSON)
import Data.Functor ((<$>))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Prelude ((.))

import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as B
import qualified Data.List as L
import qualified Data.Vector as DV

class LazyToJson a where
  lazyToJson :: a -> B.Builder

instance LazyToJson Value where
  lazyToJson = \case
    J.Array v -> lazyToJson (DV.toList v)
    J.Object kvs -> "{" <> mconcat (L.intersperse "," (entryLazyToJson <$> KM.toList kvs)) <> "}"
    J.String s -> B.lazyByteString (J.encode (J.String s))
    J.Number n -> B.lazyByteString (J.encode (J.Number n))
    J.Bool b -> if b then "true" else "false"
    J.Null -> "null"
    where entryLazyToJson :: (J.Key, Value) -> B.Builder
          entryLazyToJson (k, v) = B.lazyByteString (J.encode k) <> ":" <> lazyToJson v

instance LazyToJson a => LazyToJson [a] where
  lazyToJson as = "[" <> mconcat (L.intersperse "," (lazyToJson <$> as)) <> "]"

instance LazyToJson (Addr StandardCrypto) where
  lazyToJson = B.lazyByteString . J.encode

newtype Aeson a = Aeson a

instance ToJSON a => LazyToJson (Aeson a) where
  lazyToJson (Aeson a) = B.lazyByteString (J.encode a)
