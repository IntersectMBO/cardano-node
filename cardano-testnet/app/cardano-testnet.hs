module Main where

import qualified Cardano.Crypto.Init as Crypto

import qualified Options.Applicative as Opt

import           Parsers.Run (opts, pref, runTestnetCmd)

main :: IO ()
main = do
  Crypto.cryptoInit

  tNetCmd <- Opt.customExecParser pref opts
  runTestnetCmd tNetCmd
