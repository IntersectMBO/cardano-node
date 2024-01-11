{-# LANGUAGE DerivingVia #-}

module  Cardano.Beacon.CDF (module Cardano.Beacon.CDF) where

import           Control.Monad (forM_)
import           Data.Function ((&))
import           Data.List (sort)
import           Data.Maybe (fromJust)
import           Data.Ratio
import           GHC.Real (Ratio ((:%)))
import           Numeric (readFloat)

import           Data.IntervalMap.FingerTree as Intv (Interval (..), bounds, union, singleton)
import qualified Data.Vector.Unboxed as VU
import qualified Statistics.Function as Stat
import qualified Statistics.Quantile as Stat
import qualified Statistics.Sample as Stat
import           Statistics.Sample (Sample)


-- the Quantile type should be read shorthand as "the k_th q-quantile" for a Ratio of k % q
newtype Quantile = Q {unQ :: Ratio Int}
        deriving Show

-- "smart" constructor
mkQuantile :: String -> Quantile
mkQuantile x =
  case readFloat x of
    ((n, []) : _) -> Q n
    _ -> error "Invalid quantile"

briefQuantiles :: [Quantile]
briefQuantiles =
  map mkQuantile
    [ "0.5"
    , "0.9"
    , "1.0"
    ]

data CDF =
  CDF
  { cdfSize      :: Int
  , cdfAverage   :: Double
  , cdfMedian    :: Double
  , cdfStddev    :: Double
  , cdfMinMax    :: Interval Double
  , cdfRange     :: Double
  , cdfSamples   :: [(Quantile, Double)]
  , cdfSamples2  :: [(Quantile, Double)]
  }
  deriving Show

cdf :: Stat.ContParam -> [Quantile] -> Sample -> CDF
cdf contParam quantiles unsorted =
  CDF
  { cdfSize        = size
  , cdfAverage     = Stat.mean sorted
  , cdfMedian      = Stat.median contParam sorted
  , cdfStddev      = Stat.stdDev sorted
  , cdfMinMax      = Interval imin imax
  , cdfRange       = imax - imin
  , cdfSamples     = zip quantiles (Stat.quantiles contParam noms denom sorted)
  , cdfSamples2    = zip quantiles (map (sorted `elemClosestTo`) quantiles)
  }
  where
    (noms, denom) = expandToCommonDenom quantiles
    size          = VU.length sorted
    sorted        = Stat.sort unsorted
    imin          = VU.unsafeHead sorted
    imax          = VU.unsafeLast sorted

expandToCommonDenom :: [Quantile] -> ([Int], Int)
expandToCommonDenom qs =
  ( map expandNom qs, commonDenom )
  where
    expandNom (Q (n :% d))  = (commonDenom `div` d) * n
    commonDenom             = foldr1 lcm (map (denominator . unQ) qs)

-- | Given a sorted sample, produce population element closest to specified quantile
elemClosestTo :: Sample -> Quantile -> Double
elemClosestTo vec (Q (n :% d)) =
  vec `VU.unsafeIndex` closestIx
  where
    position :: Double
    position  = fromIntegral (size * n) / (fromIntegral d)
    closestIx = min (size - 1) $ floor position
    size      = VU.length vec

combineSamples :: [Sample] -> CDF
combineSamples =
  cdf Stat.medianUnbiased briefQuantiles . VU.concat

-- WIP!
combineCDFs :: [CDF] -> CDF
combineCDFs cdfs =
  CDF
  { cdfSize     = sum $ cdfSize <$> cdfs
  , cdfAverage  = 0
  , cdfStddev   = maximum $ cdfStddev <$> cdfs    -- approximating
  , cdfMedian   = 0
  , cdfMinMax   = Interval imin imax
  , cdfRange    = imax - imin
  , cdfSamples  = []
  , cdfSamples2 = []
  }
  where
    imin = 0
    imax = 0


---
--- testing / development
---

contParams :: [Stat.ContParam]
contParams =
  [ Stat.cadpw
  , Stat.hazen
  , Stat.spss
  , Stat.s
  , Stat.medianUnbiased
  , Stat.normalUnbiased
  ]

testList :: [Double]
testList = [1, 2, 3, 4, 1, 5, 1, 5, 9, 10, 6, 1, 4, 7, 0, 0, 6]

test :: [Double] -> IO ()
test ns = do
  putStrLn $ "median element: " ++ show (vec `elemClosestTo` Q (1 % 2))
  forM_ contParams $ \contParam -> do
    putStrLn $ " --> for ContParam: " ++ show contParam ++ ": "
    print $ cdf contParam briefQuantiles vec
  where
    vec = Stat.sort $ VU.fromList ns
