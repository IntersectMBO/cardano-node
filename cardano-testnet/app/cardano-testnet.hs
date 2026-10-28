module Main where

import           Cardano.CLI.Environment (getEnvCli)
import qualified Cardano.Crypto.Init as Crypto

import qualified Options.Applicative as Opt
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

import           Parsers.Run (opts, pref, runTestnetCmd)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Crypto.cryptoInit

  envCli <- getEnvCli
  tNetCmd <- Opt.customExecParser pref (opts envCli)
  runTestnetCmd tNetCmd
