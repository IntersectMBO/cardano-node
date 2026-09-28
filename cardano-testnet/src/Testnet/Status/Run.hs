module Testnet.Status.Run
  ( runCheckStatusOptions
  ) where

import           Cardano.Prelude (forM_, transpose)

import           Prelude

import           Testnet.Status.Types (CheckStatusOptions)

runCheckStatusOptions :: CheckStatusOptions -> IO ()
runCheckStatusOptions _ =
  printTable []


printTable :: [[String]] -> IO ()
printTable rows = do
  forM_ rows $ \row -> do
    putStrLn $ concat ("+":["-" ++ replicate width '-' ++ "-+" | width <- widths])
    putStrLn $ concat ("|":[ " " ++ pad width cell ++ " |" | (cell, width)  <- zip row widths])
  putStrLn $ concat ("+":["-" ++replicate width '-' ++ "-+" | width <- widths])
  where
    widths :: [Int]
    widths = map (maximum . map length) (transpose rows)

    pad :: Int -> String -> [Char]
    pad w s = s ++ replicate (w - length s) ' '
