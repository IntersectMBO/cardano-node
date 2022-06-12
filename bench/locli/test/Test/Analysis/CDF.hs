{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Analysis.CDF where

import Prelude (error, head)
import Cardano.Prelude hiding (handle, head)

import Hedgehog

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

prop_CDF_I_2x2                         = property $ cdfI_2x2     === cdfI_2x2_golden
prop_CDF_CDF_I_3x3                     = property $ cdf2_3x3     === cdf2_3x3_golden
prop_CDF_CDF_I_3x3_shifted             = property $ cdf2_3x3sh   === cdf2_3x3sh_golden
prop_CDF_CDF_I_3x3x3_collapsed_shifted = property $ cdf2_3x3x3sh === cdf2_3x3x3sh_golden

tests :: IO Bool
tests =
  checkSequential $$discover

--
-- * Golden values
--
cdfI_2x2_golden :: CDF I Double
cdf2_3x3_golden, cdf2_3x3sh_golden, cdf2_3x3x3sh_golden :: CDF (CDF I) Double

cdfI_2x2_golden =
  CDF
  { cdfSize = 2
  , cdfAverage = 0.5
  , cdfStddev = 0.7071067811865476
  , cdfRange = (0.0,1.0)
  , cdfSamples =
    [(Centile 0.0,I 0.0),(Centile 1.0,I 1.0)]}

cdf2_3x3_golden =
  CDF
  { cdfSize = 9
  , cdfAverage = 1.0
  , cdfStddev = 1.0
  , cdfRange = (0.0,2.0)
  , cdfSamples =
    [(Centile 0.0
     ,CDF
      { cdfSize = 3
      , cdfAverage = 0.0
      , cdfStddev = 0.0
      , cdfRange = (0.0,0.0)
      , cdfSamples =
        [(Centile 0.0,I 0.0)
        ,(Centile 0.5,I 0.0)
        ,(Centile 1.0,I 0.0)]})
    ,(Centile 0.5
     ,CDF
      { cdfSize = 3
      , cdfAverage = 1.0
      , cdfStddev = 0.0
      , cdfRange = (1.0,1.0)
      , cdfSamples =
        [(Centile 0.0,I 1.0)
        ,(Centile 0.5,I 1.0)
        ,(Centile 1.0,I 1.0)]})
    ,(Centile 1.0
     ,CDF
      { cdfSize = 3
      , cdfAverage = 2.0
      , cdfStddev = 0.0
      , cdfRange = (2.0,2.0)
      , cdfSamples =
        [(Centile 0.0,I 2.0)
        ,(Centile 0.5,I 2.0)
        ,(Centile 1.0,I 2.0)]})]}

cdf2_3x3sh_golden =
  CDF
  { cdfSize = 9
  , cdfAverage = 1.0
  , cdfStddev = 1.0
  , cdfRange = (-1.0,3.0)
  , cdfSamples =
    [(Centile 0.0
     ,CDF
      { cdfSize = 3
      , cdfAverage = 0.0
      , cdfStddev = 1.0
      , cdfRange = (-1.0,1.0)
      , cdfSamples =
        [(Centile 0.0,I (-1.0))
        ,(Centile 0.5,I 0.0)
        ,(Centile 1.0,I 1.0)]})
    ,(Centile 0.5
     ,CDF
      { cdfSize = 3
      , cdfAverage = 1.0
      , cdfStddev = 1.0
      , cdfRange = (0.0,2.0)
      , cdfSamples =
        [(Centile 0.0,I 0.0)
        ,(Centile 0.5,I 1.0)
        ,(Centile 1.0,I 2.0)]})
    ,(Centile 1.0
     ,CDF
      { cdfSize = 3
      , cdfAverage = 2.0
      , cdfStddev = 1.0
      , cdfRange = (1.0,3.0)
      , cdfSamples =
        [(Centile 0.0,I 1.0)
        ,(Centile 0.5,I 2.0)
        ,(Centile 1.0,I 3.0)]})]}

cdf2_3x3x3sh_golden =
  CDF
  { cdfSize = 27
  , cdfAverage = 1.0
  , cdfStddev = 1.0
  , cdfRange = (-3.0,5.0)
  , cdfSamples =
    [(Centile 0.0
     ,CDF
      { cdfSize = 9
      , cdfAverage = 0.0
      , cdfStddev = 1.0
      , cdfRange = (-3.0,3.0)
      , cdfSamples =
        [(Centile 0.0,I (-1.0))
        ,(Centile 0.5,I 0.0)
        ,(Centile 1.0,I 1.0)]})
    ,(Centile 0.5
     ,CDF
      { cdfSize = 9
      , cdfAverage = 1.0
      , cdfStddev = 1.0
      , cdfRange = (-2.0,4.0)
      , cdfSamples =
        [(Centile 0.0,I 0.0)
        ,(Centile 0.5,I 1.0)
        ,(Centile 1.0,I 2.0)]})
    ,(Centile 1.0
     ,CDF
      { cdfSize = 9
      , cdfAverage = 2.0
      , cdfStddev = 1.0
      , cdfRange = (-1.0,5.0)
      , cdfSamples =
        [(Centile 0.0,I 1.0)
        ,(Centile 0.5,I 2.0)
        ,(Centile 1.0,I 3.0)]})]}
