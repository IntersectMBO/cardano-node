module Main where

import Control.Concurrent
import Control.Monad
import Cardano.Logging.Resources
import System.Random (randomRIO)


main :: IO ()
main = do
  writeCentiCpuOnChange
  forever $ do
    n <- randomRIO (1000, 10000)
    putStrLn $ show n ++ " isPrime ? " ++ show (isPrime n)
    threadDelay (1000 * 1000)

-- just burn some CPU cycles
isPrime :: Int -> Bool  
isPrime x = x `elem` primes x
  where
    isqrt = floor . sqrt . fromIntegral
    primes 2 = [2]  
    primes n = 2:[i | i <- [3,5..n], all ((/= 0) . mod i) (primes (isqrt i))]

writeCentiCpuOnChange :: IO ()
writeCentiCpuOnChange =
  void $ forkIO $ resourceThread 0
  where
    delayMilliseconds :: Double
    delayMilliseconds = 750
    
    resourceThread oldVal = do
        threadDelay (round $ delayMilliseconds * 1000)
        maybeStats <- readResourceStats
        case maybeStats of
            Just Resources{rCentiCpu = cputicks}
              | cputicks /= oldVal -> putStrLn ("new CentiCpu: " ++ show cputicks) >> resourceThread cputicks
            _ -> resourceThread oldVal
