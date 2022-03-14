module Cardano.Node.Output
  ( hOut
  , outPutStrLn
  ) where

import Control.Monad (Monad(..))
import Data.Function (($))
import Data.String (String)
import System.IO (Handle, IO)
import System.IO.Unsafe (unsafePerformIO)

import qualified System.IO as IO

hOut :: Handle
hOut = unsafePerformIO $ IO.openFile "cardano-node.out" IO.WriteMode
{-# NOINLINE hOut #-}

outPutStrLn :: String -> IO ()
outPutStrLn s = IO.hPutStrLn hOut s >> IO.hFlush hOut
