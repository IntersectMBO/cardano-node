-- | Dunning t-digest for online quantile estimation.
-- Merging digest variant with K_1 (arcsine) scale function.
-- Pure functional implementation using only base libraries.

module Data.TDigest
  ( TDigest(..)
  , Centroid(..)
  , empty
  , emptyWith
  , add
  , addWeighted
  , compress
  , quantile
  , cdf
  , merge
  , totalWeight
  , centroidCount
  ) where

import           Control.DeepSeq (NFData (..))
import           Data.List (foldl', sortBy)
import           Data.Ord (comparing)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data Centroid = Centroid
  { cMean   :: {-# UNPACK #-} !Double
  , cWeight :: {-# UNPACK #-} !Double
  } deriving (Show)

instance NFData Centroid where
  rnf (Centroid m w) = m `seq` w `seq` ()

data TDigest = TDigest
  { tdCentroids   :: ![Centroid]    -- sorted by mean
  , tdBuffer      :: ![Centroid]    -- unsorted buffered additions
  , tdBufSize     :: {-# UNPACK #-} !Int  -- buffer length (O(1) check)
  , tdTotalWeight :: !Double
  , tdMin         :: !Double
  , tdMax         :: !Double
  , tdDelta       :: !Double        -- compression parameter
  , tdBufferCap   :: !Int           -- ceiling (delta * 5)
  } deriving (Show)

instance NFData TDigest where
  rnf (TDigest cs buf _ tw mn mx d bc) =
    rnf cs `seq` rnf buf `seq` tw `seq` mn `seq` mx `seq` d `seq` bc `seq` ()

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Create an empty t-digest with default delta = 100.
empty :: TDigest
empty = emptyWith 100

-- | Create an empty t-digest with a given compression parameter.
emptyWith :: Double -> TDigest
emptyWith delta = TDigest
  { tdCentroids   = []
  , tdBuffer      = []
  , tdBufSize     = 0
  , tdTotalWeight = 0
  , tdMin         = 1/0    -- +Infinity
  , tdMax         = -(1/0) -- -Infinity
  , tdDelta       = delta
  , tdBufferCap   = ceiling (delta * 5)
  }

-- ---------------------------------------------------------------------------
-- Scale function K_1
-- ---------------------------------------------------------------------------

-- | K_1 scale function: k(q, delta) = (delta / (2*pi)) * asin(2*q - 1)
kScale :: Double -> Double -> Double
kScale delta q = (delta / (2 * pi)) * asin (2 * q - 1)

-- ---------------------------------------------------------------------------
-- Adding values
-- ---------------------------------------------------------------------------

-- | Add a single value with weight 1.
add :: Double -> TDigest -> TDigest
add x = addWeighted x 1

-- | Add a value with a given weight.
addWeighted :: Double -> Double -> TDigest -> TDigest
addWeighted x w td =
  let newSize = tdBufSize td + 1
      td' = td
        { tdBuffer      = Centroid x w : tdBuffer td
        , tdBufSize     = newSize
        , tdTotalWeight = tdTotalWeight td + w
        , tdMin         = min x (tdMin td)
        , tdMax         = max x (tdMax td)
        }
  in if newSize >= tdBufferCap td
     then compress td'
     else td'

-- ---------------------------------------------------------------------------
-- Compression (greedy merge)
-- ---------------------------------------------------------------------------

-- | Compress the digest by merging the buffer into the centroid list.
compress :: TDigest -> TDigest
compress td
  | tdBufSize td == 0 && length (tdCentroids td) <= 1 = td
  | otherwise =
    let allItems = tdCentroids td ++ tdBuffer td
        sorted   = sortBy (comparing cMean) allItems
        n        = tdTotalWeight td
        delta    = tdDelta td
        merged   = greedyMerge delta n sorted
    in td { tdCentroids = merged
          , tdBuffer    = []
          , tdBufSize   = 0
          }

-- | Greedy merge pass: walk sorted centroids and merge adjacent ones
-- when the scale function constraint allows it.
greedyMerge :: Double -> Double -> [Centroid] -> [Centroid]
greedyMerge _     _ []     = []
greedyMerge delta n (c:cs) = go 0 c cs
  where
    k = kScale delta

    go :: Double -> Centroid -> [Centroid] -> [Centroid]
    go _          current []     = [current]
    go weightSoFar current (item:rest)
      -- Always allow merging when the proposed weight is tiny (single count).
      | proposed <= 1 && not (null rest) =
          go weightSoFar (mergeCentroid current item) rest
      | k q1 - k q0 <= 1.0 =
          go weightSoFar (mergeCentroid current item) rest
      | otherwise =
          current : go (weightSoFar + cWeight current) item rest
      where
        proposed = cWeight current + cWeight item
        q0       = weightSoFar / n
        q1       = (weightSoFar + proposed) / n

-- | Merge a centroid into another using weighted mean.
mergeCentroid :: Centroid -> Centroid -> Centroid
mergeCentroid a b =
  let w = cWeight a + cWeight b
      m = (cMean a * cWeight a + cMean b * cWeight b) / w
  in Centroid m w

-- ---------------------------------------------------------------------------
-- Quantile estimation
-- ---------------------------------------------------------------------------

-- | Estimate the value at quantile q (0 <= q <= 1).
quantile :: Double -> TDigest -> Maybe Double
quantile q td0
  | null cs   = Nothing
  | length cs == 1 = Just (cMean (head cs))
  | otherwise = Just (walkQuantile (clamp 0 1 q) cs)
  where
    td = if tdBufSize td0 == 0 then td0 else compress td0
    cs = tdCentroids td
    n  = tdTotalWeight td
    mn = tdMin td
    mx = tdMax td

    walkQuantile :: Double -> [Centroid] -> Double
    walkQuantile q' centroids = go 0 0 centroids
      where
        target       = q' * n
        numCentroids = length centroids
        lastIdx      = numCentroids - 1

        go :: Int -> Double -> [Centroid] -> Double
        go _ _ [] = mx  -- fallback
        go i cumulative (c:rest) =
          let mid = cumulative + cWeight c / 2
          in
          -- Left boundary: interpolate between min and first centroid
          if i == 0 && target < cWeight c / 2
          then
            if cWeight c == 1
            then mn
            else mn + (cMean c - mn) * (target / (cWeight c / 2))
          -- Right boundary: interpolate between last centroid and max
          else if i == lastIdx
          then
            if target > n - cWeight c / 2
            then
              if cWeight c == 1
              then mx
              else
                let remaining = n - cWeight c / 2
                in cMean c + (mx - cMean c) * ((target - remaining) / (cWeight c / 2))
            else cMean c
          -- Middle: interpolate between adjacent centroid midpoints
          else
            let nextC   = head rest
                nextMid = cumulative + cWeight c + cWeight nextC / 2
            in if target <= nextMid
               then
                 let frac = if nextMid == mid
                            then 0.5
                            else (target - mid) / (nextMid - mid)
                 in cMean c + frac * (cMean nextC - cMean c)
               else go (i + 1) (cumulative + cWeight c) rest

-- ---------------------------------------------------------------------------
-- CDF estimation
-- ---------------------------------------------------------------------------

-- | Estimate the cumulative distribution function at value x.
cdf :: Double -> TDigest -> Maybe Double
cdf x td0
  | null cs   = Nothing
  | x <= mn   = Just 0
  | x >= mx   = Just 1
  | otherwise = Just (walkCdf x cs)
  where
    td = if tdBufSize td0 == 0 then td0 else compress td0
    cs = tdCentroids td
    n  = tdTotalWeight td
    mn = tdMin td
    mx = tdMax td

    walkCdf :: Double -> [Centroid] -> Double
    walkCdf x' centroids = go 0 0 centroids
      where
        numCentroids = length centroids
        lastIdx      = numCentroids - 1

        go :: Int -> Double -> [Centroid] -> Double
        go _ _ [] = 1.0  -- fallback
        go i cumulative (c:rest)
          -- First centroid: left boundary
          | i == 0 && x' < cMean c =
              let innerW = cWeight c / 2
                  frac   = if cMean c == mn then 1.0
                           else (x' - mn) / (cMean c - mn)
              in (innerW * frac) / n
          | i == 0 && x' == cMean c =
              (cWeight c / 2) / n
          -- Last centroid: right boundary
          | i == lastIdx && x' > cMean c =
              let halfW  = cWeight c / 2
                  rightW = n - cumulative - halfW
                  frac   = if mx == cMean c then 0.0
                           else (x' - cMean c) / (mx - cMean c)
              in (cumulative + halfW + rightW * frac) / n
          | i == lastIdx =
              (cumulative + cWeight c / 2) / n
          -- Middle: interpolate between centroid midpoints
          | otherwise =
              let mid            = cumulative + cWeight c / 2
                  nextC          = head rest
                  nextCumulative = cumulative + cWeight c
                  nextMid        = nextCumulative + cWeight nextC / 2
              in if x' < cMean nextC
                 then
                   let frac = if cMean c == cMean nextC
                              then 0.5
                              else (x' - cMean c) / (cMean nextC - cMean c)
                   in (mid + frac * (nextMid - mid)) / n
                 else go (i + 1) (cumulative + cWeight c) rest

-- ---------------------------------------------------------------------------
-- Merge
-- ---------------------------------------------------------------------------

-- | Merge two t-digests. All centroids from the second are added as
-- buffered values into the first, then compressed.
merge :: TDigest -> TDigest -> TDigest
merge td other =
  let otherTd   = if tdBufSize other == 0 then other else compress other
      otherCs   = tdCentroids otherTd
      combined  = foldl' (\d c -> addWeighted (cMean c) (cWeight c) d) td otherCs
  in compress combined

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

totalWeight :: TDigest -> Double
totalWeight = tdTotalWeight

-- | Number of centroids (after compressing any pending buffer).
centroidCount :: TDigest -> Int
centroidCount td =
  let td' = if tdBufSize td == 0 then td else compress td
  in length (tdCentroids td')

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

clamp :: Double -> Double -> Double -> Double
clamp lo hi x
  | x < lo   = lo
  | x > hi   = hi
  | otherwise = x
