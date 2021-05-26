{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

module Data.Distribution
  ( ToRealFrac(..)
  , Distribution(..)
  , computeDistribution
  , computeDistributionStats
  , mapToDistribution
  , zeroDistribution
  , PercSpec(..)
  , renderPercSpec
  , Percentile(..)
  , pctFrac
  , stdPercentiles
  -- Aux
  , spans
  ) where

import           Prelude (String, (!!), head, last, id)
import           Cardano.Prelude hiding (head)

import           Control.Arrow
import           Data.Aeson (ToJSON(..))
import           Data.List (span)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Statistics.Sample as Stat
import           Text.Printf (PrintfArg, printf)

data Distribution a b =
  Distribution
  { dSize         :: Int
  , dMean         :: a
  , dPercentiles  :: [Percentile a b]
  }
  deriving (Functor, Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (Distribution a b)

newtype PercSpec a = Perc { psFrac :: a } deriving (Generic, Show)

renderPercSpec :: PrintfArg a => Int -> PercSpec a -> String
renderPercSpec width = \case
  Perc x    -> printf ("%0."<>show (width-2)<>"f") x

data Percentile a b =
  Percentile
  { pctSpec        :: !(PercSpec a)
  , pctSample      :: !b
  }
  deriving (Functor, Generic, Show)

pctFrac :: Percentile a b -> a
pctFrac = psFrac . pctSpec

stdPercentiles :: [PercSpec Float]
stdPercentiles =
  [ Perc 0.01, Perc 0.05
  , Perc 0.1, Perc 0.2, Perc 0.3, Perc 0.4
  , Perc 0.5, Perc 0.6
  , Perc 0.7, Perc 0.75
  , Perc 0.8, Perc 0.85, Perc 0.875
  , Perc 0.9, Perc 0.925, Perc 0.95, Perc 0.97, Perc 0.98, Perc 0.99
  , Perc 0.995, Perc 0.997, Perc 0.998, Perc 0.999
  , Perc 0.9995, Perc 0.9997, Perc 0.9998, Perc 0.9999
  ]

instance (ToJSON a) => ToJSON (PercSpec a)
instance (ToJSON a, ToJSON b) => ToJSON (Percentile a b)

zeroDistribution :: Num a => Distribution a b
zeroDistribution =
  Distribution
  { dSize        = 0
  , dMean        = 0
  , dPercentiles = mempty
  }

-- | For a list of distributions, compute a distribution of averages and rel stddev
-- (aka. coefficient of variance).
computeDistributionStats ::
    forall a v
  . ( v ~ Double -- 'v' is fixed by Stat.stdDev
    -- , RealFrac a, Real v, Fractional v, ToRealFrac v a
    , Num a
    )
  => [Distribution a v]
  -> Either String (Distribution a v, Distribution a v)
computeDistributionStats xs = do
  let distPcts    = dPercentiles <$> xs
      pctDistVals = transpose distPcts
  unless (all (pctLen ==) (length <$> distPcts)) $
    Left ("Distributions with different percentile counts: " <> show (length <$> distPcts))
  pure $ (join (***) (Distribution (length xs) 0)
          :: ([Percentile a v], [Percentile a v]) -> (Distribution a v, Distribution a v))
       $ unzip (pctsMeanCoV <$> pctDistVals)
 where
   pctLen = length . dPercentiles $ head xs

   pctsMeanCoV :: [Percentile a v] -> (Percentile a v, Percentile a v)
   pctsMeanCoV xs' = (join (***) (Percentile . pctSpec $ head xs'))
     (mean, Stat.stdDev vec / mean)
    where
      vec = Vec.fromList $ pctSample <$> xs'
      mean = Stat.mean vec

mapToDistribution :: (Real v, ToRealFrac v a) => (b -> v) -> [PercSpec a] -> [b] -> Distribution a v
mapToDistribution f pspecs xs = computeDistribution pspecs (f <$> xs)

computeDistribution :: (Real v, ToRealFrac v a) => [PercSpec a] -> [v] -> Distribution a v
computeDistribution percentiles (sort -> sorted) =
  Distribution
  { dSize        = size
  , dMean        = toRealFrac $ sorted !! indexAtFrac 0.5
  , dPercentiles =
    (Percentile     (Perc 0)   mini:) .
    (<> [Percentile (Perc 1.0) maxi]) $
    percentiles <&>
      \spec ->
        let sample = if size == 0
                     then 0
                     else sorted !! (indexAtFrac (psFrac spec))
        in Percentile spec sample
  }
 where size = length sorted
       indexAtFrac f = floor (fromIntegral (size - 1) * f)
       (,) mini maxi =
         if size == 0
         then (0,           0)
         else (head sorted, last sorted)

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
