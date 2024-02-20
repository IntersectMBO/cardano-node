module Main where

import           Cardano.Chairman.Commands
import qualified Cardano.Crypto.Init as Crypto

import           Control.Monad
import           Options.Applicative

main :: IO ()
main = do
  Crypto.cryptoInit

  join
    . customExecParser (prefs (showHelpOnEmpty <> showHelpOnError))
    $ info (commands <**> helper) $ mconcat
      [ fullDesc
      , progDesc "Chairman checks Cardano clusters for progress and consensus."
      , header "Chairman sits in a room full of Shelley nodes, and checks \
                \if they are all behaving ..."
      ]
