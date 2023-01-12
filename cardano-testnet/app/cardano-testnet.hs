module Main where

import           Control.Monad
import           Data.Function
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Parsers (commands)

main :: IO ()
main = join $ customExecParser
  (prefs $ showHelpOnEmpty <> showHelpOnError)
  (info (commands <**> helper) idm)
