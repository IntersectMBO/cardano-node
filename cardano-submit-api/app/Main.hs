module Main where

import           Cardano.CLI.Environment (getEnvNetworkId)
import           Cardano.TxSubmit (opts, runTxSubmitWebapi)

import qualified Options.Applicative as Opt

main :: IO ()
main = do
  mNetworkId <- getEnvNetworkId
  runTxSubmitWebapi =<< Opt.execParser (opts mNetworkId)
