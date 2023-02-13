module Main where

import           Cardano.TxSubmit (opts, runTxSubmitWebapi)

import qualified Options.Applicative as Opt

main :: IO ()
main = runTxSubmitWebapi =<< Opt.execParser opts
