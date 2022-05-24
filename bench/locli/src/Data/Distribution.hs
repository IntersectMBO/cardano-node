{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

module Data.Distribution
  ( ToRealFrac(..)
  , Distribution(..)
  , mapDistributionIx
  , computeDistribution
  , computeDistributionStats
  , mapToDistribution
  , subsetDistribution
  , zeroDistribution
  , dPercIx
  , dPercSpec
  , dPercSpec'
  , PercSpec(..)
  , renderPercSpec
  , Percentile(..)
  , pctFrac
  , distribPercSpecs
  , briefPercSpecs
  , stdPercSpecs
  -- Aux
  , spans
  ) where

import Prelude (String, (!!), error, head, show)
import Cardano.Prelude hiding (head, show)

import Control.Arrow
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.List (span)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Statistics.Sample qualified as Stat
import Text.Printf (PrintfArg, printf)

data Distribution a b =
  Distribution
  { dSize         :: Int
  , dAverage      :: a
  , dStddev       :: a
  , dRange        :: (b, b)
  , dPercentiles  :: [Percentile a b]
  }
  deriving (Functor, Generic, Show)
  deriving anyclass NFData

instance (FromJSON a, FromJSON b) => FromJSON (Distribution a b)
instance (  ToJSON a,   ToJSON b) => ToJSON   (Distribution a b)

newtype PercSpec a =
  Perc { psFrac :: a }
  deriving (Eq, Generic, Show)
  deriving anyclass NFData

instance (FromJSON a) => FromJSON (PercSpec a)
instance (  ToJSON a) => ToJSON   (PercSpec a)

mapDistributionIx :: (a -> b) -> Distribution a c -> Distribution b c
mapDistributionIx f Distribution{..} =
  Distribution
  { dAverage     = f dAverage
  , dStddev      = f dStddev
  , dPercentiles = mapPercentileIx f <$> dPercentiles
  , ..
  }

dPercIx :: Int -> Distribution a b -> b
dPercIx i d = pctSample $ dPercentiles d !! i

dPercSpec :: Eq (PercSpec a) => PercSpec a -> Distribution a b -> Maybe b
dPercSpec p = fmap pctSample . find ((== p) . pctSpec) . dPercentiles

dPercSpec' :: (Show a, Eq (PercSpec a)) => String -> PercSpec a -> Distribution a b -> b
dPercSpec' desc p =
  maybe (error er) pctSample . find ((== p) . pctSpec) . dPercentiles
 where
   er = printf "Missing centile %f in %s" (show $ psFrac p) desc

renderPercSpec :: PrintfArg a => Int -> PercSpec a -> String
renderPercSpec width = \case
  Perc x    -> printf ("%0."<>show (width-2)<>"f") x

data Percentile a b =
  Percentile
  { pctSpec        :: !(PercSpec a)
  , pctSample      :: !b
  }
  deriving (Functor, Generic, Show)
  deriving anyclass NFData

instance (FromJSON a, FromJSON b) => FromJSON (Percentile a b)
instance (  ToJSON a,   ToJSON b) => ToJSON   (Percentile a b)

mapPercentileIx :: (a -> b) -> Percentile a c -> Percentile b c
mapPercentileIx f Percentile{..} = Percentile { pctSpec = Perc (f $ psFrac pctSpec), ..}

pctFrac :: Percentile a b -> a
pctFrac = psFrac . pctSpec

distribPercSpecs :: Distribution a b -> [PercSpec a]
distribPercSpecs = fmap pctSpec . dPercentiles

briefPercSpecs :: [PercSpec Double]
briefPercSpecs =
  [ Perc 0.5, Perc 0.9, Perc 1.0 ]

stdPercSpecs :: [PercSpec Double]
stdPercSpecs =
  [ Perc 0.01, Perc 0.05
  , Perc 0.1, Perc 0.2, Perc 0.3, Perc 0.4
  , Perc 0.5, Perc 0.6
  , Perc 0.7, Perc 0.75
  , Perc 0.8, Perc 0.85, Perc 0.875
  , Perc 0.9, Perc 0.925, Perc 0.95, Perc 0.97, Perc 0.98, Perc 0.99
  , Perc 0.995, Perc 0.997, Perc 0.998, Perc 0.999
  , Perc 0.9995, Perc 0.9997, Perc 0.9998, Perc 0.9999
  ]

zeroDistribution :: (Num a, Num b) => Distribution a b
zeroDistribution =
  Distribution
  { dSize        = 0
  , dAverage     = 0
  , dStddev      = 0
  , dRange       = (0, 0)
  , dPercentiles = mempty
  }

-- | For a list of distributions, compute a distribution of averages and rel stddev
-- (aka. coefficient of variance).
computeDistributionStats ::
    forall a v
  . ( RealFrac a
    , v ~ Double -- 'v' is fixed by Stat.stdDev
    )
  => String -> [Distribution a v]
  -> Either String (Distribution a v, Distribution a v)
computeDistributionStats desc xs = do
  when (null xs) $
    Left $ "Empty list of distributions in " <> desc
  let distPcts    = dPercentiles <$> xs
      pctDistVals = transpose distPcts
      total       = sum $ xs <&> \Distribution{..} -> dAverage * fromIntegral dSize
      samples     = sum $ dSize <$> xs
  unless (all (pctLen ==) (length <$> distPcts)) $
    Left ("Distributions with different percentile counts: " <> show (length <$> distPcts) <> " in " <> desc)
  pure $ (join (***) (Distribution
                      (length xs)
                      (fromRational (toRational total) / fromIntegral samples)
                      0
                      (minimum $ fst . dRange <$> xs, maximum $ snd . dRange <$> xs))
          :: ([Percentile a v], [Percentile a v]) -> (Distribution a v, Distribution a v))
       $ unzip (pctsMeanCoV <$> pctDistVals)
 where
   pctLen = length . dPercentiles $ head xs

   pctsMeanCoV :: [Percentile a v] -> (Percentile a v, Percentile a v)
   pctsMeanCoV xs' = join (***) (Percentile . pctSpec $ head xs')
     (mean, Stat.stdDev vec / mean)
    where
      vec = Vec.fromList $ pctSample <$> xs'
      mean = Stat.mean vec

mapToDistribution :: (Real v, ToRealFrac v a, a ~ Double)
  => (b -> v) -> [PercSpec a] -> [b] -> Distribution a v
mapToDistribution f pspecs xs = computeDistribution pspecs (f <$> xs)

computeDistribution :: (Real v, ToRealFrac v a, a ~ Double)
  => [PercSpec a] -> [v] -> Distribution a v
computeDistribution percentiles (sort -> sorted) =
  Distribution
  { dSize        = size
  , dAverage     = Stat.mean   doubleVec
  , dStddev      = Stat.stdDev doubleVec
  , dRange       = (mini, maxi)
  , dPercentiles =
    (Percentile     (Perc 0)   mini:) .
    (<> [Percentile (Perc 1.0) maxi]) $
    percentiles <&>
      \spec ->
        let sample = if size == 0 then 0
                     else vec Vec.! fracIndex (psFrac spec)
        in Percentile spec sample
  }
 where vec         = Vec.fromList sorted
       size        = length vec
       doubleVec   = fromRational . toRational <$> vec
       fracIndex f = floor (fromIntegral (size - 1) * f)
       (,) mini maxi =
         if size == 0
         then (0,           0)
         else (vec Vec.! 0, Vec.last vec)

subsetDistribution :: Eq a => [PercSpec a] -> Distribution a b -> Distribution a b
subsetDistribution xs d =
  d { dPercentiles = dPercentiles d & filter ((`elem` xs) . pctSpec) }

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
