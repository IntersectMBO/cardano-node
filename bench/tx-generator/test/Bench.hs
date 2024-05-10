{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
module Main (main) where

import           Cardano.Benchmarking.Script.Env (mkNewEnv)
import           Cardano.Benchmarking.Script.Selftest

import           Prelude

import           Control.Monad.STM (atomically)

import           Criterion.Main hiding (env)

main :: IO ()
main = defaultMain [
  bgroup "cardano-tx-generator-integration" [
    bench "tx-gen" $ whnfIO $ do
        env <- atomically mkNewEnv
        runSelftest env (error "noIOManager") Nothing >>= \case
          Right _  -> pure ()
          Left err -> error $ show err
    ]
  ]

