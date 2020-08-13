{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Common.NetworkSpec
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, (===))
import           Network.Socket (Socket)

import qualified Control.Monad.Trans.Resource as IO
import qualified Hedgehog as H
import qualified Network.Socket as IO
import qualified System.Random as IO
import qualified Test.Common.Base as H
import qualified Test.Common.Network as IO
import qualified UnliftIO.Exception as IO

prop_isPortOpen_False :: Property
prop_isPortOpen_False = H.propertyOnce . H.workspace "temp/network" $ \_ -> do
  -- Check multiple random ports and assert that one is closed.
  -- Multiple random ports are checked because there is a remote possibility a random
  -- port is actually open by another program
  ports <- H.evalM . liftIO $ fmap (take 10 . IO.randomRs @Int (5000, 9000)) IO.getStdGen
  results <- forM ports $ \port -> do
    H.evalM . liftIO $ IO.isPortOpen port
  H.assert (False `elem` results)

prop_isPortOpen_True :: Property
prop_isPortOpen_True = H.propertyOnce . H.workspace "temp/network" $ \_ -> do
  -- Check first random port from multiple possible ports to be successfully bound is open
  -- Multiple random ports are checked because there is a remote possibility a random
  -- port is actually open by another program
  ports <- H.evalM . liftIO $ fmap (take 10 . IO.randomRs @Int (5000, 9000)) IO.getStdGen
  (socket, port) <- liftIO $ openOnePortFrom ports
  void $ IO.register $ IO.close socket
  result <- H.evalM . liftIO $ IO.isPortOpen port
  result === True
  where openOnePortFrom :: [Int] -> IO (Socket, Int)
        openOnePortFrom ports = case ports of
          [] -> panic "Could not open any ports"
          (n:ns) -> do
            socketResult <- IO.try . liftIO $ IO.listenOn n
            case socketResult of
              Right socket -> return (socket, n)
              Left (_ :: IOException) -> openOnePortFrom ns

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
