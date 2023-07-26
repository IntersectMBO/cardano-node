module Main where

import           Cardano.CLI.Environment (getEnvCli)
import qualified Cardano.Crypto.Init as Crypto

import qualified Options.Applicative as Opt

import           Parsers.Run (opts, pref, runTestnetCmd)

main :: IO ()
main = do
  Crypto.cryptoInit

  envCli <- getEnvCli
  tNetCmd <- Opt.customExecParser pref (opts envCli)
  runTestnetCmd tNetCmd
