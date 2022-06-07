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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

module Data.CDF
  ( Centile(..)
  , renderCentile
  , briefCentiles
  , stdCentiles
  , CDFError (..)
  , CDF(..)
  , centilesCDF
  , filterCDF
  , zeroCDF
  , projectCDF
  , projectCDF'
  , indexCDF
  , DirectCDF
  , computeCDF
  , computeCDFStats
  , mapToCDF
  , CDF2
  , cdf2OfCDFs
  --
  , module Data.SOP.Strict
  ) where

import Prelude (String, (!!), error, head, show)
import Cardano.Prelude hiding (head, show)

import Control.Arrow
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.SOP.Strict
import Data.Vector qualified as Vec
import Statistics.Sample qualified as Stat
import Text.Printf (printf)


-- | Centile specifier: a fractional in range of [0; 1].
newtype Centile =
  Centile { unCentile :: Double }
  deriving (Eq, Generic, FromJSON, ToJSON, Show)
  deriving anyclass NFData

renderCentile :: Int -> Centile -> String
renderCentile width = \case
  Centile x    -> printf ("%0."<>show (width-2)<>"f") x

briefCentiles :: [Centile]
briefCentiles =
  [ Centile 0.5, Centile 0.9, Centile 1.0 ]

stdCentiles :: [Centile]
stdCentiles =
  [ Centile 0.01, Centile 0.05
  , Centile 0.1, Centile 0.2, Centile 0.3, Centile 0.4
  , Centile 0.5, Centile 0.6
  , Centile 0.7, Centile 0.75
  , Centile 0.8, Centile 0.85, Centile 0.875
  , Centile 0.9, Centile 0.925, Centile 0.95, Centile 0.97, Centile 0.98, Centile 0.99
  , Centile 0.995, Centile 0.997, Centile 0.998, Centile 0.999
  , Centile 0.9995, Centile 0.9997, Centile 0.9998, Centile 0.9999
  ]

--
-- * Parametric CDF (cumulative distribution function)
--
data CDF p a =
  CDF
  { cdfSize      :: Int
  , cdfAverage   :: Double
  , cdfStddev    :: Double
  , cdfRange     :: (a, a)
  , cdfSamples  :: [(Centile, p a)]
  }
  deriving (Functor, Generic, Show)
  deriving anyclass NFData

instance (FromJSON (p a), FromJSON a) => FromJSON (CDF p a)
instance (  ToJSON (p a),   ToJSON a) => ToJSON   (CDF p a)

centilesCDF :: CDF p a -> [Centile]
centilesCDF = fmap fst . cdfSamples

zeroCDF :: (Num a) => CDF p a
zeroCDF =
  CDF
  { cdfSize    = 0
  , cdfAverage = 0
  , cdfStddev  = 0
  , cdfRange   = (0, 0)
  , cdfSamples = mempty
  }

filterCDF :: ((Centile, p a) -> Bool) -> CDF p a -> CDF p a
filterCDF f d =
  d { cdfSamples = cdfSamples d & filter f }

indexCDF :: Int -> CDF p a -> p a
indexCDF i d = snd $ cdfSamples d !! i

projectCDF :: Centile -> CDF p a -> Maybe (p a)
projectCDF p = fmap snd . find ((== p) . fst) . cdfSamples

projectCDF' :: String -> Centile -> CDF p a -> p a
projectCDF' desc p =
  maybe (error er) snd . find ((== p) . fst) . cdfSamples
 where
   er = printf "Missing centile %f in %s" (show $ unCentile p) desc

--
-- * Trivial instantiation: samples are value points
--
type DirectCDF a = CDF I a

-- | For a list of cdfs, compute a distribution of averages and rel stddev
-- (aka. coefficient of variance).
computeCDFStats ::
    forall a v
  .  (v ~ Double, RealFrac a)
  => String -> [DirectCDF a]
  -> Either String (DirectCDF Double, DirectCDF Double)
computeCDFStats desc xs = do
  when (null xs) $
    Left $ "Empty list of distributions in " <> desc
  let samples    = cdfSamples <$> xs
      sampleDistVals = transpose samples
      total       = sum $ xs <&> \CDF{..} -> cdfAverage * fromIntegral cdfSize
      nSamples    = sum $ xs <&> cdfSize
  unless (all (sampleLen ==) (length <$> samples)) $
    Left ("CDFs with different centile counts: " <> show (length <$> samples) <> " in " <> desc)
  pure $ (join (***) (CDF
                      (length xs)
                      (fromRational (toRational total) / fromIntegral nSamples)
                      0
                      (minimum $ fromRational . toRational . fst . cdfRange <$> xs,
                       maximum $ fromRational . toRational . snd . cdfRange <$> xs))
          :: ([(Centile, I v)], [(Centile, I v)])
          -> (DirectCDF Double,   DirectCDF Double))
       $ unzip (samplesMeanCoV <$> sampleDistVals)
 where
   sampleLen = length . cdfSamples $ head xs

   samplesMeanCoV :: [(c, I a)] -> ((c, I Double), (c, I Double))
   samplesMeanCoV xs' = --join (***) (fst $ head xs',)
     ((fst $ head xs', I mean),
      (fst $ head xs', I $ Stat.stdDev vec / mean))
    where
      -- vec :: Vector Double
      vec  = Vec.fromList $ fromRational . toRational . unI . snd <$> xs'
      mean = Stat.mean vec

computeCDF :: Real v => [Centile] -> [v] -> DirectCDF v
computeCDF centiles (sort -> sorted) =
  CDF
  { cdfSize        = size
  , cdfAverage     = Stat.mean   doubleVec
  , cdfStddev      = Stat.stdDev doubleVec
  , cdfRange       = (mini, maxi)
  , cdfSamples =
    (    (Centile   0, I mini) :) .
    (<> [(Centile 1.0, I maxi) ]) $
    centiles <&>
      \spec ->
        let sample = if size == 0 then 0
                     else vec Vec.! fracIndex (unCentile spec)
        in (,) spec (I sample)
  }
 where vec         = Vec.fromList sorted
       size        = length vec
       doubleVec   = fromRational . toRational <$> vec
       fracIndex f = floor (fromIntegral (size - 1) * f)
       (,) mini maxi =
         if size == 0
         then (0,           0)
         else (vec Vec.! 0, Vec.last vec)

mapToCDF :: Real a => (b -> a) -> [Centile] -> [b] -> DirectCDF a
mapToCDF f pspecs xs = computeCDF pspecs (f <$> xs)

type CDF2 a = CDF (CDF I) a

data CDFError
  = CDFIncoherentSamplingLengths  [Int]
  | CDFIncoherentSamplingCentiles [[Centile]]
  | CDFEmptyDataset

-- cdf2OfCDFs :: forall a. Real a => ([a] -> DirectCDF a) -> [DirectCDF a] -> Either CDFError (CDF2 a)

cdf2OfCDFs :: forall a p. Real a => ([p a] -> CDF I a) -> [CDF p a] -> Either CDFError (CDF (CDF I) a)
cdf2OfCDFs _ [] = Left CDFEmptyDataset
cdf2OfCDFs cdfy xs = do
  unless (all (head lengths ==) lengths) $
    Left $ CDFIncoherentSamplingLengths lengths
  unless (null incoherent) $
    Left $ CDFIncoherentSamplingCentiles (fmap fst <$> incoherent)
  pure CDF
    { cdfSize    = totalSize
    , cdfAverage = totalArea / fromIntegral totalSize
    , cdfRange   = (,) (minimum $ fst <$> ranges)
                       (maximum $ snd <$> ranges)
    , cdfStddev  = maximum $ cdfStddev <$> xs
    , cdfSamples = fmap cdfy <$> coherent
    }
 where
   totalSize      = sum sizes
   sizes          = cdfSize <$> xs
   ranges         = cdfRange <$> xs
   samples        = cdfSamples <$> xs
   lengths        = length <$> samples
   totalArea      = sum $ xs <&> \CDF{..} -> cdfAverage * fromIntegral cdfSize

   centileOrdered :: [[(Centile, p a)]]
   centileOrdered = transpose samples

   (incoherent, coherent) = partitionEithers $ centileOrdered <&>
     \case
       [] -> error "cdfOfCDFs:  empty list of centiles, hands down."
       xxs@((c, _):(fmap fst -> cs)) -> if any (/= c) cs
                                        then Left xxs
                                        else Right (c, snd <$> xxs)
