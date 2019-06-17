module Main (main) where

import           Data.Semigroup ((<>))

import           CLI
import           Run


main :: IO ()
main = runNode =<< execParser opts
  where
    opts = info (parseCLI <**> helper)
      ( fullDesc
     <> progDesc "Run a node with the chain-following protocol hooked in."
     )

