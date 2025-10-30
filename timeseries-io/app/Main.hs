module Main where

import           Data.Attoparsec.Text (parseOnly)
import           Data.Query.Parser
import           Data.Text (pack)
import           System.Exit (die)

-- name -> labels ⨯ timestamp ⨯ value

-- (blocks_forged, node1, 15/10/2025 12:00:00, 2)
-- (blocks_forged, node1, 15/10/2025 12:00:01, 4)
-- (blocks_forged, node3, 15/10/2025 12:00:03, 1)
-- ...

main :: IO ()
main = do
  putStrLn "This is a prototype!"
  string <- readFile "expression.txt"
  let result = parseOnly expr (pack string)
  case result of
    Left err -> die err
    Right x -> print x

