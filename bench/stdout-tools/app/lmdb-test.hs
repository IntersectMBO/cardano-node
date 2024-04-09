{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (when, forM, forM_, foldM)
import System.IO (readFile)
import System.Environment (getArgs)
import Foreign.Storable (sizeOf)
import Database.LMDB.Simple

main = do
  readProcBlockInOut
  ------------------
  (filePath:nStr:mode:args) <- getArgs
  let n = (read nStr :: Int)
  let kvSize  = sum [ sizeOf 'C' * (1 + (length . show) i) * 2 | i <- [1..n] ]
  -- If "small" `mapSize`: "MDB_MAP_FULL: Environment mapsize limit reached".
  env <- openReadWriteEnvironment filePath (defaultLimits {mapSize = kvSize})
  db <- readOnlyTransaction env $ getDatabase Nothing :: IO (Database String Int)
  let !nList = if any ("--partition" ==) args
               then    [  (n `quot` 4      + 1) .. (  n `quot` 2     ) ]
                    ++ [                     1  .. (  n `quot` 4     ) ]
                    ++ [ ((n `quot` 4) * 3 + 1) .. (  n              ) ]
                    ++ [  (n `quot` 2      + 1) .. ( (n `quot` 4) * 3) ]
               else [1..n]
  when (any (mode ==) ["w","rw","wr"]) $ {-# SCC "write" #-} do
    putStrLn "put all ..."
    if any ("--single" ==) args
    -- As a single tx:
    then {-# SCC "tx_rw" #-} readWriteTransaction env $ {-# SCC "forM_" #-} forM_
      nList
      ({-# SCC "put" #-} (\i -> put db (show i) (Just i)))
    -- As many txs:
    else {-# SCC "forM_" #-} forM_
      nList
      (\i -> {-# SCC "tx_rw" #-} readWriteTransaction env $
        {-# SCC "put" #-} (put db (show i) (Just i))
      )
  when (any (mode ==) ["r","rw","wr"]) $ {-# SCC "read" #-} do
    putStrLn "sum all ..."
    print =<< if any ("--single" ==) args
    -- As a single tx:
    then {-# SCC "tx_ro" #-} readOnlyTransaction env $
      {-# SCC "foldM" #-} foldM
        (\ !s k -> do
          acc <- {-# SCC "get" #-} (get db $ show k)
          return $ case acc of
            Nothing -> s
            (Just v) -> s + v
        )
        0
        nList
    else {-# SCC "foldM" #-} foldM
        (\ !s k -> do
          acc <- {-# SCC "tx_ro" #-} readOnlyTransaction env $
            {-# SCC "get" #-} (get db $ show k)
          return $ case acc of
            Nothing -> s
            (Just v) -> s + v
        )
        0
        nList
    putStrLn "check ..."
    print $ n * (n + 1) `quot` 2
  ------------------
  readProcBlockInOut

-- https://github.com/IntersectMBO/cardano-node/blob/0e241dbfc8df303b75f9fdb4bb93b470a9c4a5ed/trace-resources/src/Cardano/Logging/Resources/Linux.hs#L73-L79
readProcBlockInOut :: IO ()
readProcBlockInOut = do
    -- We're only interested in 'read_bytes' & 'write_bytes':
    fields <- readFile "/proc/self/io"
    print $ (take 4 . drop 8) (words fields)
