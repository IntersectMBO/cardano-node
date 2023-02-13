module Main where

import           Cardano.Chairman.Commands
import           Control.Monad
import           Options.Applicative

main :: IO ()
main = join
  . customExecParser
    ( prefs (showHelpOnEmpty <> showHelpOnError)
    )
  $ info (commands <**> helper)
    (  fullDesc
    <> progDesc "Chairman checks Cardano clusters for progress and consensus."
    <> header "Chairman sits in a room full of Shelley nodes, and checks \
              \if they are all behaving ..."
    )
