{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Data.Ratio ((%))
import Data.Word (Word64)
import Control.DeepSeq (deepseq)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific

import Criterion.Main

--------------------------------------------------------------------------------

-- Baseline
------------

calcDensityBase :: (Word64,Word64) -> Rational
calcDensityBase (bl,sl) = toRational bl / toRational sl

intermediateBase :: Rational -> Double
intermediateBase = fromRational

toJSONBase :: Double -> Aeson.Value
toJSONBase tsChainDensity = Aeson.object
  [
    ("chainDensity", Aeson.Number (fromRational (toRational tsChainDensity)) )
  ]

base :: (Word64,Word64) -> Aeson.Value
base (bl,sl) = toJSONBase $ intermediateBase $ calcDensityBase (bl,sl)

-- Proposed change 1
---------------------

calcDensityDouble :: (Word64,Word64) -> Double
calcDensityDouble (bl,sl) = fromIntegral bl / fromIntegral sl

toJSONDouble :: Double -> Aeson.Value
toJSONDouble tsChainDensity = Aeson.object
  [
    ("chainDensity", Aeson.toJSON tsChainDensity)
  ]

double :: (Word64,Word64) -> Aeson.Value
double (bl,sl) = toJSONDouble $ calcDensityDouble (bl,sl)

-- Proposed change 2
---------------------

calcDensityRatio :: (Word64,Word64) -> Rational
calcDensityRatio (bl,sl) = toInteger bl % toInteger sl

toJSONRatio :: Rational -> Aeson.Value
toJSONRatio tsChainDensity = Aeson.object
  [
    ("chainDensity", Aeson.Number $
      case Scientific.fromRationalRepetendLimited 10 tsChainDensity of
        (Left  (sc, _)) -> sc
        (Right (sc, _)) -> sc
    )
  ]

ratio :: (Word64,Word64) -> Aeson.Value
ratio (bl,sl) = toJSONRatio $ calcDensityRatio (bl,sl)

-- Bench
--------------------------------------------------------------------------------

main :: IO ()
main = do
  defaultMain
    [ bgroup "simple" [
      bench "double" $ nf
        (\(bl,sl) ->
          Scientific.fromFloatDigits
            ((fromIntegral (bl::Word64) / fromIntegral (sl::Word64) :: Double))
        )
        (17,230)
    , bench "ratio"  $ nf
        (\(bl,sl) ->
          let ratio = toInteger (bl::Word64) % toInteger (sl::Word64)
          in case Scientific.fromRationalRepetendLimited 10 ratio of
            (Left  (sc, _)) -> sc
            (Right (sc, _)) -> sc
        )
        (17,230)
    ]]
  print $ base   (17,230)
  print $ double (17,230)
  print $ ratio  (17,230)
  let inputs =
                -- Block: 11783017
                -- Epoch / Slot: 554 / 76433
                -- Absolute Slot: 154041233
                -- https://cardanoscan.io/block/11783017
                map
                  (\i -> (11783017 + i, 154041233 + i * 20))
                  [1..1000000]
  deepseq inputs (return ())
  print $ base   $ head inputs
  print $ double $ head inputs
  print $ ratio  $ head inputs
  defaultMain
    [ bgroup "real" [
        bench "base"   $ nf (map base)   inputs
      , bench "double" $ nf (map double) inputs
      , bench "ratio"  $ nf (map ratio)  inputs
    ]]

{--
  forM_
    [0..50000]
}
--  (\n -> print $ toJSONRatio  $ intermediateRatio  $ calcDensityRatio  (7 + n) (238 + n))
    (\n -> print $ toJSONDouble $ intermediateDouble $ calcDensityDouble (7 + n) (238 + n))
--}
