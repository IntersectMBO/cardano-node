{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.Timeseries.Util(isSubsetOf, isEqual, toMaybe, maybeToEither, safeToWord64, safeToDouble, head, range) where

import           Prelude hiding (head)

import           Data.List (nub)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)

isSubsetOf :: forall a. (Eq a) => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) (nub xs)

isEqual :: forall a. (Eq a) => [a] -> [a] -> Bool
isEqual xs ys = isSubsetOf xs ys && isSubsetOf ys xs

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True x = Just x

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just x) = Right x
maybeToEither err Nothing = Left err

-- | Geometric center of two 1D points
center :: Word64 -> Word64 -> Word64
center a b | a <= b = a + (b - a) `div` 2
center a b = b + (a - b) `div` 2

safeToWord64 :: Integer -> Maybe Word64
safeToWord64 x
  | x >= 0 && x <= fromIntegral (maxBound :: Word64) = Just (fromIntegral x)
  | otherwise = Nothing

safeToDouble :: Integer -> Maybe Double
safeToDouble x
  | x >= 0 && x <= 2^(53 :: Integer) = Just (fromIntegral x)
  | otherwise = Nothing

head :: [a] -> Maybe a
head (x : _) = Just x
head [] = Nothing

-- | Return submap containing only keys in (lo, hi).
-- | Complexity: O(log(n)).
range :: Ord k => k -> k -> Map k v -> Map k v
range lo hi m =
    let (_, m1) = Map.split lo m  -- drop all < lo
        (m2, _) = Map.split hi m1 -- drop all > hi
    in m2

