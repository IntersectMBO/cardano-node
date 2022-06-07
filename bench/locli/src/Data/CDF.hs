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
  ( ToRealFrac(..)
  , CDF(..)
  , DirectCDF
  , computeCDF
  , computeCDFStats
  , mapToCDF
  , subsetCDF
  , zeroCDF
  , dCentiIx
  , dCentiSpec
  , dCentiSpec'
  , CentiSpec(..)
  , renderCentiSpec
  , distribCentiSpecs
  , briefCentiSpecs
  , stdCentiSpecs
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
newtype CentiSpec =
  Centi { unCentiSpec :: Double }
  deriving (Eq, Generic, FromJSON, ToJSON, Show)
  deriving anyclass NFData

renderCentiSpec :: Int -> CentiSpec -> String
renderCentiSpec width = \case
  Centi x    -> printf ("%0."<>show (width-2)<>"f") x

briefCentiSpecs :: [CentiSpec]
briefCentiSpecs =
  [ Centi 0.5, Centi 0.9, Centi 1.0 ]

stdCentiSpecs :: [CentiSpec]
stdCentiSpecs =
  [ Centi 0.01, Centi 0.05
  , Centi 0.1, Centi 0.2, Centi 0.3, Centi 0.4
  , Centi 0.5, Centi 0.6
  , Centi 0.7, Centi 0.75
  , Centi 0.8, Centi 0.85, Centi 0.875
  , Centi 0.9, Centi 0.925, Centi 0.95, Centi 0.97, Centi 0.98, Centi 0.99
  , Centi 0.995, Centi 0.997, Centi 0.998, Centi 0.999
  , Centi 0.9995, Centi 0.9997, Centi 0.9998, Centi 0.9999
  ]

--
-- CDF
--
data CDF p a =
  CDF
  { dSize      :: Int
  , dAverage   :: Double
  , dStddev    :: Double
  , dRange     :: (a, a)
  , dCentiles  :: [(CentiSpec, p a)]
  }
  deriving (Functor, Generic, Show)
  deriving anyclass NFData

type DirectCDF a = CDF I a

instance (FromJSON (p a), FromJSON a) => FromJSON (CDF p a)
instance (  ToJSON (p a),   ToJSON a) => ToJSON   (CDF p a)

dCentiIx :: Int -> DirectCDF a -> a
dCentiIx i d = unI . snd $ dCentiles d !! i

dCentiSpec :: CentiSpec -> DirectCDF a -> Maybe a
dCentiSpec p = fmap (unI . snd) . find ((== p) . fst) . dCentiles

dCentiSpec' :: String -> CentiSpec -> DirectCDF a -> a
dCentiSpec' desc p =
  maybe (error er) (unI . snd) . find ((== p) . fst) . dCentiles
 where
   er = printf "Missing centile %f in %s" (show $ unCentiSpec p) desc

distribCentiSpecs :: DirectCDF a -> [CentiSpec]
distribCentiSpecs = fmap fst . dCentiles

zeroCDF :: (Num a) => CDF p a
zeroCDF =
  CDF
  { dSize     = 0
  , dAverage  = 0
  , dStddev   = 0
  , dRange    = (0, 0)
  , dCentiles = mempty
  }

-- | For a list of distributions, compute a distribution of averages and rel stddev
-- (aka. coefficient of variance).
computeCDFStats ::
    forall a v
  .  (v ~ Double, RealFrac a)
  => String -> [DirectCDF a]
  -> Either String (DirectCDF Double, DirectCDF Double)
computeCDFStats desc xs = do
  when (null xs) $
    Left $ "Empty list of distributions in " <> desc
  let distPcts    = dCentiles <$> xs
      pctDistVals = transpose distPcts
      total       = sum $ xs <&> \CDF{..} -> dAverage * fromIntegral dSize
      nSamples    = sum $ xs <&> dSize
  unless (all (pctLen ==) (length <$> distPcts)) $
    Left ("CDFs with different percentile counts: " <> show (length <$> distPcts) <> " in " <> desc)
  pure $ (join (***) (CDF
                      (length xs)
                      (fromRational (toRational total) / fromIntegral nSamples)
                      0
                      (minimum $ fromRational . toRational . fst . dRange <$> xs,
                       maximum $ fromRational . toRational . snd . dRange <$> xs))
          :: ([(CentiSpec, I v)], [(CentiSpec, I v)])
          -> (DirectCDF Double,   DirectCDF Double))
       $ unzip (pctsMeanCoV <$> pctDistVals)
 where
   pctLen = length . dCentiles $ head xs

   pctsMeanCoV :: [(c, I a)] -> ((c, I Double), (c, I Double))
   pctsMeanCoV xs' = --join (***) (fst $ head xs',)
     ((fst $ head xs', I mean),
      (fst $ head xs', I $ Stat.stdDev vec / mean))
    where
      -- vec :: Vector Double
      vec  = Vec.fromList $ fromRational . toRational . unI . snd <$> xs'
      mean = Stat.mean vec

mapToCDF :: Real a
  => (b -> a) -> [CentiSpec] -> [b] -> DirectCDF a
mapToCDF f pspecs xs = computeCDF pspecs (f <$> xs)

computeCDF :: Real v
  => [CentiSpec] -> [v] -> DirectCDF v
computeCDF percentiles (sort -> sorted) =
  CDF
  { dSize        = size
  , dAverage     = Stat.mean   doubleVec
  , dStddev      = Stat.stdDev doubleVec
  , dRange       = (mini, maxi)
  , dCentiles =
    (    (Centi   0, I mini) :) .
    (<> [(Centi 1.0, I maxi) ]) $
    percentiles <&>
      \spec ->
        let sample = if size == 0 then 0
                     else vec Vec.! fracIndex (unCentiSpec spec)
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

subsetCDF :: [CentiSpec] -> DirectCDF a -> DirectCDF a
subsetCDF xs d =
  d { dCentiles = dCentiles d & filter ((`elem` xs) . fst) }

class RealFrac b => ToRealFrac a b where
  toRealFrac :: a -> b

instance RealFrac b => ToRealFrac Int b where
  toRealFrac = fromIntegral

instance {-# OVERLAPPABLE #-} (RealFrac b, Real a) => ToRealFrac a b where
  toRealFrac = realToFrac
