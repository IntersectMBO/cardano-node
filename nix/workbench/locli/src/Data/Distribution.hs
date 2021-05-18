{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Distribution
  ( ToRealFrac(..)
  , Distribution(..)
  , computeDistribution
  , zeroDistribution
  , PercSpec(..)
  , renderPercSpec
  , Percentile(..)
  , pctFrac
  -- Aux
  , spans
  ) where

import           Prelude (String)
import           Cardano.Prelude

import           Control.Arrow
import           Data.Aeson (ToJSON(..))
import qualified Data.Foldable as F
import           Data.List (span)
import qualified Data.Sequence as Seq
import           Data.Sequence (index)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Text.Printf (PrintfArg, printf)

data Distribution a b =
  Distribution
  { dAverage      :: a
  , dCount        :: Int
  , dPercentiles  :: [Percentile a b]
  }
  deriving (Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (Distribution a b)

newtype PercSpec a = Perc { psFrac :: a } deriving (Generic, Show)

renderPercSpec :: PrintfArg a => Int -> PercSpec a -> String
renderPercSpec width = \case
  Perc x    -> printf ("%0."<>show (width-2)<>"f") x

data Percentile a b =
  Percentile
  { pctSpec        :: !(PercSpec a)
  , pctSampleIndex :: !Int
  , pctSamplePrev  :: !Int
  , pctSample      :: !b
  }
  deriving (Generic, Show)

pctFrac :: Percentile a b -> a
pctFrac = psFrac . pctSpec

instance (ToJSON a) => ToJSON (PercSpec a)
instance (ToJSON a, ToJSON b) => ToJSON (Percentile a b)

zeroDistribution :: Num a => Distribution a b
zeroDistribution =
  Distribution
  { dAverage     = 0
  , dCount       = 0
  , dPercentiles = mempty
  }

countSeq :: Eq a => a -> Seq a -> Int
countSeq x = foldl' (\n e -> if e == x then n + 1 else n) 0

computeDistribution :: (RealFrac a, Real v, ToRealFrac v a) => [PercSpec a] -> Seq v -> Distribution a v
computeDistribution percentiles (Seq.sort -> sorted) =
  Distribution
  { dAverage     = toRealFrac (F.sum sorted) / fromIntegral (size `max` 1)
  , dCount       = size
  , dPercentiles =
    (Percentile     (Perc 0)   size (countSeq mini sorted) mini:) .
    (<> [Percentile (Perc 1.0) 1    (countSeq maxi sorted) maxi]) $
    percentiles <&>
      \spec ->
        let (sampleIndex :: Int, sample) =
              if size == 0
              then (0, fromInteger 0)
              else floor (fromIntegral (size - 1) * psFrac spec) &
                   ((\x->x) &&& Seq.index sorted)
        in Percentile
             spec
             (size - sampleIndex)
             (countSeq sample sorted)
             sample
  }
  where size   = Seq.length sorted
        (,) mini maxi =
          if size == 0
          then (0, fromInteger 0)
          else (index sorted 0, index sorted $ size - 1)

class RealFrac b => ToRealFrac a b where
  toRealFrac :: a -> b

instance RealFrac b => ToRealFrac Int b where
  toRealFrac = fromIntegral

instance {-# OVERLAPPABLE #-} (RealFrac b, Real a) => ToRealFrac a b where
  toRealFrac = realToFrac

spans :: forall a. (a -> Bool) -> [a] -> [Vector a]
spans f = go []
 where
   go :: [Vector a] -> [a] -> [Vector a]
   go acc [] = reverse acc
   go acc xs =
     case span f $ dropWhile (not . f) xs of
       ([], rest) -> go acc rest
       (ac, rest) ->
         go (Vec.fromList ac:acc) rest
