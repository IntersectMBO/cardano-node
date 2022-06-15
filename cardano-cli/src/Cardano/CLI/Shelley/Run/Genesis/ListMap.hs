{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Genesis.ListMap
  ( ListMap(..)
  ) where

import Data.Aeson (Value(..), ToJSON(..), ToJSON1(..), ToJSON2(..), ToJSONKey(..), ToJSONKeyFunction(..))
import Data.Aeson.Encoding ( dict )
import Data.Aeson.Types (listValue)
import Data.Eq (Eq(..))
import Data.Function ((.), id)
import Prelude (uncurry)
import Text.Show (Show(..))

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List as L
import qualified Data.Vector as V

newtype ListMap k v = ListMap
  { unListMap :: [(k, v)]
  } deriving (Eq, Show)

instance ToJSONKey k => ToJSON1 (ListMap k) where
  liftToJSON g _ = case toJSONKey of
    ToJSONKeyText  f _ -> Object . KM.fromList . unListMap . mapKeyValO f g
    ToJSONKeyValue f _ -> Array . V.fromList . L.map (toJSONPair f g) . unListMap
    where mapKeyValO :: (k1 -> k2) -> (v1 -> v2) -> ListMap k1 v1 -> ListMap k2 v2
          mapKeyValO fk kv = ListMap . foldrWithKey (\(k, v) -> ((fk k, kv v):)) []
          toJSONPair :: (a -> Value) -> (b -> Value) -> (a, b) -> Value
          toJSONPair a b = liftToJSON2 a (listValue a) b (listValue b)

  liftToEncoding g _ = case toJSONKey of
    ToJSONKeyText  _ f -> dict f g (foldrWithKey . uncurry)
    ToJSONKeyValue _ f -> E.list (pairEncoding f) . unListMap
    where pairEncoding f (a, b) = E.list id [f a, g b]

instance (ToJSON v, ToJSONKey k) => ToJSON (ListMap k v) where
    toJSON = J.toJSON1

    toEncoding = J.liftToEncoding J.toEncoding J.toEncodingList

foldrWithKey :: ((k, a) -> b -> b) -> b -> ListMap k a -> b
foldrWithKey f z = L.foldr f z . unListMap
