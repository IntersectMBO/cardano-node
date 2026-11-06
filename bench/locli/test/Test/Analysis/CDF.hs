{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.Analysis.CDF where

import Cardano.Prelude hiding (handle, head)

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.CDF
import Cardano.Util


handle :: Either CDFError b -> b
handle = either (error . show) identity

sho :: Show a => a -> String
sho = show

out :: String -> PropertyT IO ()
out = liftIO . putStrLn

-- | The unifying idea behind all samples is that the population average is always 1.0
samples2x2, samples3x3, samples3x3shifted :: [[Double]]
samples2x2 =
  [ [0..1]
  , [0..1]
  ]
samples3x3 =
  [ [0..2]
  , [0..2]
  , [0..2]
  ]
samples3x3shifted =
  [ [(-1)..1]
  , [0..2]
  , [1..3]
  ]
samples3x3x3shifted :: [[[Double]]]
samples3x3x3shifted =
  [ samples3x3shifted <&> fmap (\x -> x - 2)
  , samples3x3shifted <&> fmap (+ 0)
  , samples3x3shifted <&> fmap (+ 2)
  ]

centi2x2, centi3x3 :: [Centile]
centi2x2 = nEquicentiles 2
centi3x3 = nEquicentiles 3

cdfI_2x2, cdfI_3x3 :: CDF I Double
cdfI_2x2 = cdf centi2x2 (head samples2x2)
cdfI_3x3 = cdf centi3x3 (head samples3x3)

cdf2_2x2, cdf2_3x3, cdf2_3x3sh, cdf2_3x3x3sh :: CDF (CDF I) Double
cdf2_2x2   = cdf2OfCDFs (stdCombine1 centi2x2) (cdf centi2x2 <$> samples2x2)
  & handle
cdf2_3x3   = cdf2OfCDFs (stdCombine1 centi3x3) (cdf centi3x3 <$> samples3x3)
  & handle
cdf2_3x3sh = cdf2OfCDFs (stdCombine1 centi3x3) (cdf centi3x3 <$> samples3x3shifted)
  & handle
cdf2_3x3x3sh =
  handle . cdf2OfCDFs (stdCombine1 centi3x3) . (cdf centi3x3 <$>) <$> samples3x3x3shifted
  & cdf2OfCDFs (stdCombine2 centi3x3)
  & handle

prop_CDF_I_2x2 = property $ cdfI_2x2 ===
  CDF
  { cdfSize = 2
  , cdfMedian = 1
  , cdfAverage = I 0.5
  , cdfStddev = 0.7071067811865476
  , cdfRange = Interval 0.0 1.0
  , cdfSamples =
    [(Centile 0.25,I 0.0)
    ,(Centile 0.75,I 1.0)]}

prop_CDF_CDF_I_3x3 = property $ cdf2_3x3 ===
  CDF
  { cdfSize = 9
  , cdfMedian = 1
  , cdfAverage =
    CDF
    { cdfSize = 3
    , cdfMedian = 1
    , cdfAverage = I 1.0
    , cdfStddev = 0.0
    , cdfRange = Interval 1.0 1.0
    , cdfSamples =
      [(Centile 0.16666666666666666, I 1.0)
      ,(Centile 0.5,                 I 1.0)
      ,(Centile 0.8333333333333333,  I 1.0)]}
  , cdfStddev = 1.0
  , cdfRange = Interval 0.0 2.0
  , cdfSamples =
    [(Centile 0.16666666666666666
     ,CDF
      { cdfSize = 3
      , cdfMedian = 0
      , cdfAverage = I 0.0
      , cdfStddev = 0.0
      , cdfRange = Interval 0.0 0.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I 0.0)
        ,(Centile 0.5,                 I 0.0)
        ,(Centile 0.8333333333333333,  I 0.0)]})
    ,(Centile 0.5
     ,CDF
      { cdfSize = 3
      , cdfMedian = 1
      , cdfAverage = I 1.0
      , cdfStddev = 0.0
      , cdfRange = Interval 1.0 1.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I 1.0)
        ,(Centile 0.5,                 I 1.0)
        ,(Centile 0.8333333333333333,  I 1.0)]})
    ,(Centile 0.8333333333333333
     ,CDF
      { cdfSize = 3
      , cdfMedian = 2
      , cdfAverage = I 2.0
      , cdfStddev = 0.0
      , cdfRange = Interval 2.0 2.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I 2.0)
        ,(Centile 0.5,                 I 2.0)
        ,(Centile 0.8333333333333333,  I 2.0)]})]}

prop_CDF_CDF_I_3x3_shifted = property $ cdf2_3x3sh ===
  CDF
  { cdfSize = 9
  , cdfMedian = 1
  , cdfAverage =
    CDF
    { cdfSize = 3
    , cdfMedian = 1
    , cdfAverage = I 1.0
    , cdfStddev = 1.0
    , cdfRange = Interval 0.0 2.0
    , cdfSamples =
      [(Centile 0.16666666666666666, I 0.0)
      ,(Centile 0.5,                 I 1.0)
      ,(Centile 0.8333333333333333,  I 2.0)]}
  , cdfStddev = 1.0
  , cdfRange = Interval (-1.0) 3.0
  , cdfSamples =
    [(Centile 0.16666666666666666
     ,CDF
      { cdfSize = 3
      , cdfMedian = 0
      , cdfAverage = I 0.0
      , cdfStddev = 1.0
      , cdfRange = Interval (-1.0) 1.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I (-1.0))
        ,(Centile 0.5,                 I 0.0)
        ,(Centile 0.8333333333333333,  I 1.0)]})
    ,(Centile 0.5
     ,CDF
      { cdfSize = 3
      , cdfMedian = 1
      , cdfAverage = I 1.0
      , cdfStddev = 1.0
      , cdfRange = Interval 0.0 2.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I 0.0)
        ,(Centile 0.5,                 I 1.0)
        ,(Centile 0.8333333333333333,  I 2.0)]})
    ,(Centile 0.8333333333333333
     ,CDF
      { cdfSize = 3
      , cdfMedian = 2
      , cdfAverage = I 2.0
      , cdfStddev = 1.0
      , cdfRange = Interval 1.0 3.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I 1.0)
        ,(Centile 0.5,                 I 2.0)
        ,(Centile 0.8333333333333333,  I 3.0)]})]}

prop_CDF_CDF_I_3x3x3_collapsed_shifted = property $ cdf2_3x3x3sh ===
  CDF
  { cdfSize = 27
  , cdfMedian = 1
  , cdfAverage =
    CDF
    { cdfSize = 3
    , cdfMedian = 1
    , cdfAverage = I 1.0
    , cdfStddev = 2.0
    , cdfRange = Interval (-1.0) 3.0
    , cdfSamples =
      [(Centile 0.16666666666666666, I (-1.0))
      ,(Centile 0.5,                 I 1.0)
      ,(Centile 0.8333333333333333,  I 3.0)]}
  , cdfStddev = 1.0
  , cdfRange = Interval (-3.0) 5.0
  , cdfSamples =
    [(Centile 0.16666666666666666
     ,CDF
      { cdfSize = 9
      , cdfMedian = 0
      , cdfAverage = I 0.0
      , cdfStddev = 1.0
      , cdfRange = Interval (-3.0) 3.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I (-1.0))
        ,(Centile 0.5,                 I 0.0)
        ,(Centile 0.8333333333333333,  I 1.0)]})
    ,(Centile 0.5
     ,CDF
      { cdfSize = 9
      , cdfMedian = 1
      , cdfAverage = I 1.0
      , cdfStddev = 1.0
      , cdfRange = Interval (-2.0) 4.0
      , cdfSamples =
        [(Centile 0.16666666666666666, I 0.0)
        ,(Centile 0.5,                 I 1.0)
        ,(Centile 0.8333333333333333,  I 2.0)]})
    ,(Centile 0.8333333333333333
     ,CDF
      { cdfSize = 9
      , cdfMedian = 2
      , cdfAverage = I 2.0
      , cdfStddev = 1.0
      , cdfRange = Interval (-1.0) 5.0
        , cdfSamples =
        [(Centile 0.16666666666666666, I 1.0)
        ,(Centile 0.5,                 I 2.0)
        ,(Centile 0.8333333333333333,  I 3.0)]})]}

-- | Combining CDFs of one sample each (e.g. a per-node count): the spread is that of
--   the samples themselves, as the CDFs' own stddevs are undefined.
prop_CDF_CDF_I_singletons_stddev = property $
  cdfStddev cdf2_singletons === 1.0
 where
   cdf2_singletons :: CDF (CDF I) Double
   cdf2_singletons =
     cdf2OfCDFs (stdCombine1 centi3x3) (cdf centi3x3 . pure <$> [1, 2, 3])
     & handle

-- | The average of a CDF of CDFs is the average over all its inputs' averages, also
--   with more inputs (hosts) than centiles (as with 52 hosts and 'stdCentiles').
prop_CDF_CDF_I_average_52_hosts = withTests 1 $ property $
  cdfAverageVal (hosts (pure <$> [1..52])) === 26.5

prop_CDF_CDF_I_average_any_hosts = property $ do
  xss <- forAll $ Gen.list (Range.linear 1 100) $
           Gen.list (Range.linear 1 20) (Gen.double $ Range.linearFrac 0 1000)
  let mean xs = sum xs / fromIntegral (length xs)
  annotateShow (length xss)
  assert $ abs (cdfAverageVal (hosts xss) - mean (mean <$> xss)) < 1e-9

-- | A CDF of no samples is total: median and samples are 0.
prop_CDF_I_empty = withTests 1 $ property $ do
  let c = cdf stdCentiles ([] :: [Double])
  (cdfSize c, cdfMedian c) === (0, 0)
  (snd <$> cdfSamples c) === (I 0 <$ stdCentiles)

hosts :: [[Double]] -> CDF (CDF I) Double
hosts xss = cdf2OfCDFs (stdCombine1 stdCentiles) (cdf stdCentiles <$> xss) & handle

tests :: IO Bool
tests =
  checkSequential $$discover
