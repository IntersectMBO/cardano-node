{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.Timeseries.Util(isSubsetOf, isEqual, toMaybe, maybeToEither, safeToWord64, safeToDouble, head) where

import           Prelude hiding (head)

import           Data.List (nub)
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
