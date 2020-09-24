{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Control.Monad
import           Data.Bool
import           Data.Function
import           Hedgehog
import           System.IO (IO)

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM as STM

import qualified Test.Cardano.Node.Chairman.Shelley

main :: IO ()
main = do
  tvDone <- STM.newTVarIO @Bool False

  void . check $ Test.Cardano.Node.Chairman.Shelley.prepropChairman tvDone

  void . forever $ IO.threadDelay 100000000

  return ()
