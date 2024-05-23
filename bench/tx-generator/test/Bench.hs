{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
module Main (main) where

import           Cardano.Benchmarking.Script.Env (emptyEnv, newEnvConsts)
import           Cardano.Benchmarking.Script.Selftest

import           Prelude

import           Control.Monad.STM (atomically)

import           Criterion.Main

main :: IO ()
main = defaultMain [
  bgroup "cardano-tx-generator-integration" [
    bench "tx-gen" $ whnfIO do
        envConsts <- atomically do
          newEnvConsts (error "No IOManager!") Nothing
        runSelftest emptyEnv envConsts Nothing >>= \case
          Right _  -> pure ()
          Left err -> error $ show err
    ]
  ]

