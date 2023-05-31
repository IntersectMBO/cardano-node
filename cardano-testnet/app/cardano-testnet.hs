module Main where

import qualified Cardano.Crypto.Init as Crypto

import           Options.Applicative
import           Testnet.Parsers

main :: IO ()
main = do
  Crypto.cryptoInit

  tNetCmd <- customExecParser
               (prefs $ showHelpOnEmpty <> showHelpOnError)
               (info (commands <**> helper) idm)
  runTestnetCmd tNetCmd
