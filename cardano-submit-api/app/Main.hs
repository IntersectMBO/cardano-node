module Main where

import           Cardano.CLI.Environment (getEnvCli)
import qualified Cardano.Crypto.Init as Crypto
import           Cardano.TxSubmit (opts, runTxSubmitWebapi)

import qualified Options.Applicative as Opt

main :: IO ()
main = do
  Crypto.cryptoInit

  envCli <- getEnvCli

  runTxSubmitWebapi =<< Opt.execParser (opts envCli)
