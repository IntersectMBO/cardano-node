{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

{- HLINT ignore "Use infix" -}

module Data.CDF
  ( Centile(..)
  , renderCentile
  , briefCentiles
  , stdCentiles
  , nEquicentiles
  , Divisible (..)
  , weightedAverage
  , averageDouble
  , CDFError (..)
  , CDF(..)
  , cdf
  , cdfZ
  , cdfAverageVal
  , centilesCDF
  , filterCDF
  , subsetCDF
  , zeroCDF
  , projectCDF
  , projectCDF'
  , indexCDF
  , CDFIx (..)
  , KnownCDF (..)
  , CDFList
  , liftCDFVal
  , unliftCDFVal
  , unliftCDFValExtra
  , arity
  , cdfArity
  , cdfArity'
  , arityProj
  , mapCDFCentiles
  , Combine (..)
  , stdCombine1
  , stdCombine2
  , CDF2
  , collapseCDF
  , collapseCDFs
  , cdf2OfCDFs
  --
  , module Data.SOP.Strict
  ) where

import Prelude ((!!), show)
import Cardano.Prelude hiding (head, show)

import Data.SOP.Strict
import Data.Tuple.Extra (both)
import Data.Vector qualified as Vec
import Statistics.Sample qualified as Stat

import Cardano.Util


-- | Centile specifier: a fractional in range of [0; 1].
newtype Centile =
  Centile { unCentile :: Double }
  deriving (Eq, Show)
  deriving newtype (FromJSON, ToJSON, NFData)

renderCentile :: Int -> Centile -> String
renderCentile width = \case
  Centile x    -> printf ("%0."<>show (width-2)<>"f") x

briefCentiles :: [Centile]
briefCentiles =
  [ Centile 0.5, Centile 0.9, Centile 1.0 ]

stdCentiles :: [Centile]
stdCentiles =
  [ Centile 0.0
  , Centile 0.01, Centile 0.05
  , Centile 0.1, Centile 0.2, Centile 0.3, Centile 0.4
  , Centile 0.5, Centile 0.6
  , Centile 0.7, Centile 0.75
  , Centile 0.8, Centile 0.85, Centile 0.875
  , Centile 0.9, Centile 0.925, Centile 0.95, Centile 0.97, Centile 0.98, Centile 0.99
  , Centile 0.995, Centile 0.997, Centile 0.998, Centile 0.999
  , Centile 0.9995, Centile 0.9997, Centile 0.9998, Centile 0.9999
  , Centile 1.0
  ]

nEquicentiles :: Int -> [Centile]
nEquicentiles n =
  if reindices == indices
  then Centile <$> centiles
  else error $ printf "centilesForN:  reindices for %d: %s, indices: %s" n (show reindices) (show indices)
 where
   reindices = centiles <&> runCentile n
   centiles  = [ step * (fromIntegral i + 0.5) | i <- indices ]
   indices   = [0 .. n - 1]
   step :: Double
   step = 1.0 / fromIntegral n

-- | Given a centile of N-large population, produce index of the population element referred by centile.
{-# INLINE runCentile #-}
runCentile :: Int -> Double -> Int
runCentile n centile = floor (fromIntegral n * centile)
                       & min (n - 1)

{-# INLINE vecCentile #-}
vecCentile :: Vec.Vector a -> Int -> Centile -> a
vecCentile vec n (Centile c) = vec Vec.! runCentile n c

--
-- | Avoiding `Fractional`
--
class Real a => Divisible a where
  divide     :: a -> Double -> a
  fromDouble :: Double -> a

instance Divisible Double where
  divide = (/)
  fromDouble = identity

instance Divisible Int where
  divide x by = round $ fromIntegral x / by
  fromDouble = ceiling

instance Divisible Integer where
  divide x by = round $ fromIntegral x / by
  fromDouble = ceiling

instance Divisible Word32 where
  divide x by = round $ fromIntegral x / by
  fromDouble = ceiling

instance Divisible Word64 where
  divide x by = round $ fromIntegral x / by
  fromDouble = ceiling

instance Divisible NominalDiffTime where
  divide x by = x / secondsToNominalDiffTime by
  fromDouble = secondsToNominalDiffTime

deriving newtype instance Divisible RUTCTime

weightedAverage :: forall b. (Divisible b) => [(Int, b)] -> b
weightedAverage xs =
  (`divide` (fromIntegral . sum $ fst <$> xs)) . sum $
  xs <&> \(size, avg) -> fromIntegral size * avg

averageDouble :: Divisible a => [a] -> Double
averageDouble xs = toDouble (sum xs) / fromIntegral (length xs)

--
-- * Parametric CDF (cumulative distribution function)
--
data CDF p a =
  CDF
  { cdfSize      :: Int
  , cdfAverage   :: p Double
  , cdfMedian    :: a
  , cdfStddev    :: Double
  , cdfRange     :: Interval a
  , cdfSamples   :: [(Centile, p a)]
  }
  deriving (Functor, Generic)

deriving instance (Eq     a, Eq     (p a), Eq     (p Double)) => Eq     (CDF p a)
deriving instance (Show   a, Show   (p a), Show   (p Double)) => Show   (CDF p a)
deriving instance (NFData a, NFData (p a), NFData (p Double)) => NFData (CDF p a)

instance (FromJSON (p a), FromJSON (p Double), FromJSON a) => FromJSON (CDF p a)
instance (  ToJSON (p a),   ToJSON (p Double),   ToJSON a) => ToJSON   (CDF p a)

cdfAverageVal :: (KnownCDF p) => CDF p a -> Double
cdfAverageVal =
  cdfArity
    (toDouble . unI . cdfAverage)
    \x ->
      let sizes = cdfSize . snd <$> cdfSamples x
      in
        weightedAverage (zip sizes $ fmap (toDouble . unI . snd) . cdfSamples $ cdfAverage x)

centilesCDF :: CDF p a -> [Centile]
centilesCDF = fmap fst . cdfSamples

filterCDF :: ((Centile, p a) -> Bool) -> CDF p a -> CDF p a
filterCDF f d =
  d { cdfSamples = cdfSamples d & filter f }

subsetCDF :: [Centile] ->  CDF p b -> CDF p b
subsetCDF = filterCDF . \cs c -> elem (fst c) cs

indexCDF :: Int -> CDF p a -> p a
indexCDF i d = snd $ cdfSamples d !! i

projectCDF :: Centile -> CDF p a -> Maybe (p a)
projectCDF p = fmap snd . find ((== p) . fst) . cdfSamples

projectCDF' :: String -> Centile -> CDF p a -> p a
projectCDF' desc p x =
  maybe (error er) snd . find ((== p) . fst) $ cdfSamples x
 where
   er = printf "Missing centile %f in %s (samples %s)" (unCentile p) desc (show $ fst <$> cdfSamples x)

zeroCDF :: (Real a, KnownCDF p) => CDF p a
zeroCDF =
  CDF
  { cdfSize    = 0
  , cdfAverage = liftCDFVal 0 cdfIx
  , cdfMedian  = 0
  , cdfStddev  = 0
  , cdfRange   = Interval 0 0
  , cdfSamples = mempty
  }

-- | Simple, monomorphic, first-order CDF.
cdf :: forall a. Divisible a => [Centile] -> [a] -> CDF I a
cdf centiles (sort -> sorted) =
  CDF
  { cdfSize        = size
  , cdfAverage     = I . fromDouble $ Stat.mean doubleVec
  , cdfMedian      = vecCentile vec size (Centile 0.5)
  , cdfStddev      = Stat.stdDev doubleVec
  , cdfRange       = Interval mini maxi
  , cdfSamples =
    centiles <&>
      \spec ->
        let sample = if size == 0 then 0
                     else vecCentile vec size spec
        in (,) spec (I sample)
  }
 where vec         = Vec.fromList sorted
       size        = length vec
       doubleVec   = fromRational . toRational <$> vec
       (,) mini maxi =
         if size == 0
         then (0,           0)
         else (vec Vec.! 0, Vec.last vec)

cdfZ :: forall a. Divisible a => [Centile] -> [a] -> CDF I a
cdfZ cs [] = zeroCDF { cdfSamples = fmap (,I 0) cs }
cdfZ cs xs = cdf cs xs

-- * Arity dispatch
--
-- Dealing with polymorphism over:
--  - I
--  - CDF I
--  - CDF (CDF I)
--
-- This toolkit isn't exhaustive, only covering what's actually used.
type CDF2 a = CDF (CDF I) a

data CDFIx p where
  CDFI :: CDFIx I
  CDF2 :: CDFIx (CDF I)

class KnownCDF a where
  cdfIx :: CDFIx a

instance KnownCDF      I  where cdfIx = CDFI
instance KnownCDF (CDF I) where cdfIx = CDF2

type family CDFList (f :: Type -> Type) (t :: Type) :: Type where
  CDFList I       t = t
  CDFList (CDF I) t = [t]

liftCDFVal :: forall a p. Real a => a -> CDFIx p -> p a
liftCDFVal x = \case
  CDFI -> I x
  CDF2 -> CDF { cdfSize    = 1
              , cdfAverage = I $ toDouble x
              , cdfMedian  = x
              , cdfStddev  = 0
              , cdfRange   = point x
              , cdfSamples = []
              , .. }

unliftCDFVal :: forall a p. Divisible a => CDFIx p -> p a -> a
unliftCDFVal CDFI (I x) = x
unliftCDFVal CDF2 CDF{cdfMedian} = (1 :: a) `divide` (1 / toDouble cdfMedian)

unliftCDFValExtra :: forall a p. Divisible a => CDFIx p -> p a -> [a]
unliftCDFValExtra CDFI (I x) = [x]
unliftCDFValExtra i@CDF2 c@CDF{cdfRange=Interval mi ma, ..} = [ mean
                                                              , mi
                                                              , ma
                                                              , mean - stddev
                                                              , mean + stddev
                                                              ]
 where mean   = unliftCDFVal i c
       stddev = (1 :: a) `divide` (1 / cdfStddev)

arity :: forall p a b. KnownCDF p => (I a -> b) -> (CDF I a -> b) -> p a -> b
arity fi fcdf x =
  case cdfIx @p of
    CDFI -> fi   x
    CDF2 -> fcdf x

cdfArity :: forall p a b. KnownCDF p => (CDF I a -> b) -> (CDF (CDF I) a -> b) -> CDF p a -> b
cdfArity fcdf fcdf2 x =
  case cdfIx @p of
    CDFI -> fcdf  x
    CDF2 -> fcdf2 x

cdfArity' :: forall p a. KnownCDF p => (CDF I a -> I a) -> (CDF (CDF I) a -> CDF I a) -> CDF p a -> p a
cdfArity' fi fcdf x =
  case cdfIx @p of
    CDFI -> fi   x
    CDF2 -> fcdf x

mapCDFCentiles :: (Centile -> p a -> b) -> CDF p a -> [b]
mapCDFCentiles f CDF{..} = fmap (uncurry f) cdfSamples

arityProj :: forall p a. KnownCDF p => (CDF I a -> a) -> p a -> a
arityProj = arity unI

data CDFError
  = CDFIncoherentSamplingLengths  [Int]
  | CDFIncoherentSamplingCentiles [[Centile]]
  | CDFEmptyDataset
  deriving Show

-- * Combining population stats
data Combine p a
  = Combine
    { cWeightedAverages :: !([(Int, Double)] -> Double)
    , cStddevs          :: !([Double] -> Double)
    , cRanges           :: !([Interval a] -> Interval a)
    , cWeightedSamples  :: !([(Int, a)] -> a)
    , cCDF              :: !([p a] -> Either CDFError (CDF I a))
    }

stdCombine1 :: forall a. (Divisible a) => [Centile] -> Combine I a
stdCombine1 cs =
  Combine
  { cWeightedAverages = weightedAverage
  , cRanges           = unionIntv
  , cStddevs          = maximum          -- it's an approximation
  , cWeightedSamples  = weightedAverage
  , cCDF              = Right . cdf cs . fmap unI
  }

stdCombine2 :: Divisible a => [Centile] -> Combine (CDF I) a
stdCombine2 cs =
  let c@Combine{..} = stdCombine1 cs in
  Combine
  { cCDF = collapseCDFs c
  , ..
  }

collapseCDF :: ([a] -> b) -> CDF (CDF I) a -> CDF I b
collapseCDF avg c =
  CDF
  { cdfSize    = cdfSize c
  , cdfAverage = I $ cdfAverageVal c
  , cdfMedian  = avg [cdfMedian c]
  , cdfRange   = cdfRange c
                 & low &&& high
                 & both (avg . (:[]))
                 & uncurry Interval
  , cdfStddev  = cdfStddev c
  , cdfSamples = zip (cdfSamples c <&> fst)
                     (cdfSamples c <&>
                      I
                      . avg               -- :: a
                      . fmap  (unI . snd) -- :: [(Int, a)]
                      . cdfSamples . snd) -- :: [(Centile a)]
  }

listMedian :: Ord a => [a] -> a
listMedian ms = sort ms !! div (length ms) 2

-- | Collapse basic CDFs.
collapseCDFs :: forall a. Ord a => Combine I a -> [CDF I a] -> Either CDFError (CDF I a)
collapseCDFs _ [] = Left CDFEmptyDataset
collapseCDFs Combine{..} xs = do
  unless (all (head lengths ==) lengths) $
    Left $ CDFIncoherentSamplingLengths lengths
  unless (null incoherent) $
    Left $ CDFIncoherentSamplingCentiles (fmap fst <$> incoherent)
  pure CDF
    { cdfSize    = sum sizes
    , cdfAverage = I . fromDouble . cWeightedAverages $ zip sizes avgs
    , cdfMedian  = listMedian $ xs <&> cdfMedian
    , cdfRange   = xs <&> cdfRange   & cRanges
    , cdfStddev  = xs <&> cdfStddev  & cStddevs
    , cdfSamples = coherent <&>
                   fmap (I . cWeightedSamples . zip sizes . fmap unI)
    }
 where
   sizes   = xs <&> cdfSize
   avgs    = xs <&> toDouble . unI . cdfAverage
   samples = xs <&> cdfSamples
   lengths = samples <&> length

   centileOrdered :: [[(Centile, I a)]] -- Each sublist must (checked) have the same Centile.
   centileOrdered = transpose samples

   coherent :: [(Centile, [I a])]
   (incoherent, coherent) = partitionEithers $ centileOrdered <&>
     \case
       [] -> error "cdfOfCDFs:  empty list of centiles, hands down."
       xxs@((c, _):(fmap fst -> cs)) -> if any (/= c) cs
                                        then Left xxs
                                        else Right (c, snd <$> xxs)

-- | Polymorphic, but practically speaking, intended for either:
--    1. given a ([I]     -> CDF I) function, and a list of (CDF I),       produce a CDF (CDF I), or
--    2. given a ([CDF I] -> CDF I) function, and a list of (CDF (CDF I)), produce a CDF (CDF I)
cdf2OfCDFs :: forall a p. (KnownCDF p, Ord a)
           => Combine p a -> [CDF p a] -> Either CDFError (CDF (CDF I) a)
cdf2OfCDFs _ [] = Left CDFEmptyDataset
cdf2OfCDFs Combine{..} xs = do
  unless (all (head lengths ==) lengths) $
    Left $ CDFIncoherentSamplingLengths lengths
  unless (null incoherent) $
    Left $ CDFIncoherentSamplingCentiles (fmap fst <$> incoherent)

  cdfSamples <- mapM sequence -- ..to  Either CDFError [(Centile, CDF I a)]
                  (coherent <&> fmap cCDF :: [(Centile, Either CDFError (CDF I a))])

  pure CDF
    { cdfSize    = sum sizes
    , cdfRange   = xs <&> cdfRange   & cRanges
    , cdfStddev  = xs <&> cdfStddev  & cStddevs
    , cdfAverage = cdf (nEquicentiles nCDFs) averages -- XXX: unweighted
    , cdfMedian  = listMedian $ xs <&> cdfMedian
    , ..
    }
 where
   nCDFs    = length xs
   averages :: [Double]
   averages = xs <&> unI . cdfArity cdfAverage (cdfAverage . cdfAverage)

   sizes    = xs <&> cdfSize
   samples  = xs <&> cdfSamples
   lengths  = length <$> samples

   centileOrdered :: [[(Centile, p a)]]
   centileOrdered = transpose samples

   (incoherent, coherent) = partitionEithers $ centileOrdered <&>
     \case
       [] -> error "cdfOfCDFs:  empty list of centiles, hands down."
       xxs@((c, _):(fmap fst -> cs)) -> if any (/= c) cs
                                        then Left xxs
                                        else Right (c, snd <$> xxs)
