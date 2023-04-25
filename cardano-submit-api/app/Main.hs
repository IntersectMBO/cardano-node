module Main where

import           Cardano.CLI.Environment (getEnvCli)
import           Cardano.TxSubmit (opts, runTxSubmitWebapi)

import qualified Options.Applicative as Opt

main :: IO ()
main = do
  envCli <- getEnvCli
  runTxSubmitWebapi =<< Opt.execParser (opts envCli)
