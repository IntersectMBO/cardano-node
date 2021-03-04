{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Examples.Aggregation where

import qualified Data.Aeson as A
import           GHC.Generics (Generic)
import           Katip
import           Katip.Scribes.Handle (ioLogEnv)

import           Cardano.Logging

data BaseStats = BaseStats {
    bsMeasure :: Double,
    bsMin     :: Double,
    bsMax     :: Double,
    bsCount   :: Int,
    bsSum     :: Double
    } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON BaseStats where
    toEncoding = A.genericToEncoding A.defaultOptions

instance LogFormatting BaseStats where
  asMetrics BaseStats {..} =
    [ DoubleM (Just "measure") bsMeasure
    , DoubleM (Just "sum") bsSum]

baseStatsDocumented :: Documented Double
baseStatsDocumented =
  Documented [DocMsg 0.0 "Measure" "Measure", DocMsg 0.0 "Sum" "Sum"]

emptyStats :: BaseStats
emptyStats = BaseStats 0.0 100000000.0 (-100000000.0) 0 0.0

calculate :: BaseStats -> Double -> BaseStats
calculate BaseStats{..} val =
   BaseStats  val
              (min bsMin val)
              (max bsMax val)
              (1 + bsCount)
              (bsSum + val)

testAggregation :: IO ()
testAggregation = do
    simpleTracer  <- stdoutObjectKatipTracer
    tracer <- foldTraceM calculate emptyStats simpleTracer
    configureTracers emptyTraceConfig baseStatsDocumented [tracer]
    traceWith tracer 1.0
    traceWith tracer 2.0
    traceWith tracer 0.5


{-}
newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
      where
        (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
            let (cir1', b) = cir1 a
                (cir2', c) = cir2 b
            in  (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(b, d) ->
        let (cir', c) = cir b
        in  (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _   []     = []
runCircuit cir (x:xs) =
    let (cir',x') = unCircuit cir x
    in  x' : runCircuit cir' xs

-- | Accumulator that outputs a value determined by the supplied function.
accum :: (acc -> a -> (b, acc)) -> acc -> Circuit a b
accum f acc = Circuit $ \input ->
    let (output, acc') = f acc input
    in  (accum f acc', output)

-- | Accumulator that outputs the accumulator value.
accum' :: (b -> a -> b) -> b -> Circuit a b
accum' f = accum (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' (+) 0

type RecTrace m a b = Circuit (Trace m a) (Trace m b)

-- -- | Folds the function with state b over messages a in the trace.
-- foldTrace :: forall a acc m
--   .  (acc -> a -> acc)
--   -> acc
--   -> Trace m a
--   -> Trace m acc
-- foldTrace f acc trace =
--   T.Tracer $ TA.compute
--     (\(lc,v) ->
--       let accNext = f v acc
--       in foldTrace f accNext trace)

-- *Main> runCircuit total [1,0,1,0,0,2]
-}
