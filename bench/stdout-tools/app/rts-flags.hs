module Main (main) where

import GHC.RTS.Flags

main :: IO ()
main = do
        putStrLn "RTSFlags"
        getRTSFlags >>= print
        putStrLn "GCFlags"
        getGCFlags >>= print
