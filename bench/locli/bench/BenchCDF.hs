{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Criterion benchmarks comparing the old sort-and-index CDF construction
--   (inlined from master as 'cdfOld') with the new t-digest\/Welford-based
--   implementation exported by "Data.CDF".
module Main (main) where

import           Prelude

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Control.Monad (forM)
import           Data.CDF (CDF (..), Centile (..), addCdfSample, briefCentiles, cdf, emptyCdfAccum,
                   finaliseCdfAccum, stdCentiles)
import           Data.IntervalMap.FingerTree (Interval (..))
import           Data.List (foldl', sort)
import           Data.SOP (I (..))
import qualified Data.Vector as Vec

import           Criterion.Main


-- -------------------------------------------------------------------
-- Baseline: old sort-and-index implementation from master.
--
-- Pasted verbatim (with trivial mean/stddev replacing Statistics.Sample)
-- so we can A/B within a single build without branch switching.
-- -------------------------------------------------------------------

{-# INLINE runCentileOld #-}
runCentileOld :: Int -> Double -> Int
runCentileOld n centile = floor (fromIntegral n * centile)
                          & min (n - 1)
  where (&) = flip ($)

{-# INLINE vecCentileOld #-}
vecCentileOld :: Vec.Vector a -> Int -> Centile -> a
vecCentileOld vec n (Centile c) = vec Vec.! runCentileOld n c

naiveMean :: Vec.Vector Double -> Double
naiveMean v
  | Vec.null v = 0
  | otherwise  = Vec.sum v / fromIntegral (Vec.length v)

naiveStdDev :: Vec.Vector Double -> Double
naiveStdDev v
  | Vec.length v < 2 = 0
  | otherwise =
      let n  = fromIntegral (Vec.length v) :: Double
          m  = naiveMean v
          ss = Vec.foldl' (\acc x -> acc + (x - m) * (x - m)) 0 v
      in sqrt (ss / (n - 1))

-- | The old CDF construction: sort the input list, build a Vector, compute
--   mean\/stddev from a second Vector of Doubles, then index centiles.
cdfOld :: [Centile] -> [Double] -> CDF I Double
cdfOld centiles (sort -> sorted) =
  CDF
  { cdfSize    = size
  , cdfAverage = I (naiveMean doubleVec)
  , cdfMedian  = vecCentileOld vec size (Centile 0.5)
  , cdfStddev  = naiveStdDev doubleVec
  , cdfRange   = Interval mini maxi
  , cdfSamples =
      centiles <&> \spec ->
        let sample = if size == 0 then 0
                     else vecCentileOld vec size spec
        in (spec, I sample)
  }
 where
  vec  = Vec.fromList sorted
  size = Vec.length vec
  -- In master this was @fromRational . toRational <$> vec@; identity for Double.
  doubleVec = vec
  (mini, maxi)
    | size == 0 = (0, 0)
    | otherwise = (vec Vec.! 0, Vec.last vec)
  (<&>) = flip fmap


-- -------------------------------------------------------------------
-- Deterministic data generators (no IO, reproducible)
-- -------------------------------------------------------------------

-- | Monotone ramp: @[0.0, 1.0, ..., n-1]@.  Best case for timsort
--   (already sorted), useful for revealing sort overhead on random input.
mkLinear :: Int -> [Double]
mkLinear n = [fromIntegral i | i <- [0 .. n - 1]]

-- | Pseudo-random via a simple LCG.  Deterministic and dependency-free.
--   Values are in @[0, 1)@.
mkPseudoRandom :: Int -> [Double]
mkPseudoRandom n = take n $ iterate lcg 0.042
  where
    lcg x = let y = x * 1103515245 + 12345
                z = y - fromIntegral (floor (y / 1e9) :: Int) * 1e9
            in z / 1e9


-- -------------------------------------------------------------------
-- Benchmarks
-- -------------------------------------------------------------------

main :: IO ()
main = do
  -- Pre-force all input data to exclude allocation cost from timings.
  let sizes = [100, 1000, 10000, 100000]
  inputs <- forM sizes $ \n -> do
    lin <- evaluate (force (mkLinear n))
    rnd <- evaluate (force (mkPseudoRandom n))
    pure (n, lin, rnd)

  defaultMain
    [ bgroup "cdf/stdCentiles"
      [ bgroup (show n)
        [ bench "old/linear"  $ nf (cdfOld stdCentiles) lin
        , bench "new/linear"  $ nf (cdf    stdCentiles) lin
        , bench "old/random"  $ nf (cdfOld stdCentiles) rnd
        , bench "new/random"  $ nf (cdf    stdCentiles) rnd
        ]
      | (n, lin, rnd) <- inputs
      ]
    , bgroup "cdf/briefCentiles"
      [ bgroup (show n)
        [ bench "old/random"  $ nf (cdfOld briefCentiles) rnd
        , bench "new/random"  $ nf (cdf    briefCentiles) rnd
        ]
      | (n, _lin, rnd) <- inputs
      ]
    , bgroup "accumulator/foldl'"
      [ bgroup (show n)
        [ bench "addCdfSample" $ whnf (foldl' addCdfSample emptyCdfAccum) rnd
        ]
      | (n, _, rnd) <- inputs
      ]
    , bgroup "finalise/stdCentiles"
      -- Use env to build the accumulator once, then benchmark only finalisation.
      [ env (evaluate $! foldl' addCdfSample emptyCdfAccum (mkPseudoRandom 10000))
          $ \acc -> bench "10000"  $ nf (finaliseCdfAccum @Double stdCentiles) acc
      , env (evaluate $! foldl' addCdfSample emptyCdfAccum (mkPseudoRandom 100000))
          $ \acc -> bench "100000" $ nf (finaliseCdfAccum @Double stdCentiles) acc
      ]
    ]
