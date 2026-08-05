{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CDFs
--
-- This module should be imported qualified.
--
module Cardano.Node.Tracing.Cdf
  ( Counter (..)
  , Config (..)
  , State
  , size
  , null
  , empty
  , minPriority
  , defaultConfig
  , processDataPoint
  ) where

import           Prelude hiding (null)
import           Data.Int (Int64)
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as Pq
import           Data.Time (NominalDiffTime)

data Counter = Counter {
    limit   :: !Double
  , counter :: !Int64
}

decCdf :: Double -> Counter -> Counter
decCdf v cdf@Counter{..}
  | v < limit = cdf {counter = counter - 1}
  | otherwise = cdf

incCdf :: Double -> Counter -> Counter
incCdf v cdf@Counter{..}
  | v < limit = cdf {counter = counter + 1}
  | otherwise = cdf


-- | We keep the results in a priority queue, to be able to evict the oldest
-- entry when the data sets become larger than `numOfDataPoints`.
--
newtype State p = State { cdfState :: IntPSQ p NominalDiffTime }

empty :: State p
empty = State Pq.empty

null :: State p -> Bool
null = Pq.null . cdfState

size :: State p -> Int
size = Pq.size  . cdfState

minPriority :: Ord p => State p -> Maybe p
minPriority State { cdfState } = case Pq.minView cdfState of
    Nothing -> Nothing
    Just (_, p, _, _) -> Just p

newtype Config = Config { numOfDataPoints :: Int }

-- | Default `Config` keeps `k/2` data points.
defaultConfig :: Config
defaultConfig = Config { numOfDataPoints = 1080 }


processDataPoint
  :: forall f p.
     ( Ord p
     , Functor f
     )
  => Config
  -> (Int, p, NominalDiffTime)
  -- ^ index, priority, value
  -> State p
  -> f Counter
  -> Maybe (f Counter, State p)
processDataPoint Config { numOfDataPoints } (idx, p, delay) (State m) cdfs
    | idx `Pq.member` m
    = Nothing

    | otherwise
    = if Pq.size m' > numOfDataPoints
      then
        case Pq.minView m' of
          Nothing -> Nothing
          Just (_, minVal, minDelay, m'')
            | minVal == p
            -> Nothing

            | otherwise
            -> Just (adjustCdf (realToFrac minDelay) <$> cdfs, State m'')
      else
        Just (updateCdf <$> cdfs, State m')
  where
    m' = Pq.insert idx p delay m

    updateCdf :: Counter -> Counter
    updateCdf = incCdf (realToFrac delay)

    adjustCdf :: Double -> Counter -> Counter
    adjustCdf d = updateCdf . decCdf d
