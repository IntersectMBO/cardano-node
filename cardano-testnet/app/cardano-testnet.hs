module Main where

import           Options.Applicative
import           Testnet.Parsers

main :: IO ()
main = do
  tNetCmd <- customExecParser
               (prefs $ showHelpOnEmpty <> showHelpOnError)
               (info (commands <**> helper) idm)
  runTestnetCmd tNetCmd
