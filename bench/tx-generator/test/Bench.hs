{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
module Main (main) where

import           Cardano.Benchmarking.Script.Selftest
import           Criterion.Main
import           Prelude

main :: IO ()
main = defaultMain [
  bgroup "cardano-tx-generator-integration" [
    bench "tx-gen" $ whnfIO $ do
        runSelftest (error "noIOManager") Nothing >>= \case
          Right _ -> return ()
          Left err -> error $ show err
    ]
  ]

