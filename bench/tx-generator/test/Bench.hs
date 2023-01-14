{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
module Main (main) where

import           Prelude
import           Criterion.Main
import           Cardano.Benchmarking.Script.Selftest

main :: IO ()
main = defaultMain [
  bgroup "cardano-tx-generator-integration" [
    bench "tx-gen" $ whnfIO $ do
        runSelftest (error "noIOManager") Nothing >>= \case
          Right _ -> return ()
          Left err -> error $ show err
    ]
  ]

