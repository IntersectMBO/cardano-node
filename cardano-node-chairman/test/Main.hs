{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bool
import           Data.Function
import           Hedgehog
import           System.IO (IO)

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM as STM
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.IO as IO

main :: IO ()
main = do
  tvDone <- STM.newTVarIO @Bool False

  void . check $ H.propertyOnce . H.workspace "chairman" $ \_ -> do
    void . register . liftIO $ IO.appendFile "logs.txt" "Cleanup\n"

    void . liftResourceT . resourceForkIO $ do
      liftIO $ IO.appendFile "logs.txt" "Forked\n"
      void . forever . liftIO . STM.atomically $ do
        done <- STM.readTVar tvDone
        unless done STM.retry
      liftIO $ IO.appendFile "logs.txt" "Thread done\n"

      liftIO $ IO.appendFile "logs.txt" "Done\n"
      return ()

    H.success

  void . forever $ IO.threadDelay 100000000

  return ()
