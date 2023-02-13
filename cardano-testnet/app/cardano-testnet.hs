module Main where

import           Control.Monad
import           Options.Applicative
import           Testnet.Parsers (commands)

main :: IO ()
main = join $ customExecParser
  (prefs $ showHelpOnEmpty <> showHelpOnError)
  (info (commands <**> helper) idm)
