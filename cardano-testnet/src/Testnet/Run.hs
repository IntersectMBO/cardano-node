module Testnet.Run
  ( runTestnet
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..))

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM as STM
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as IO
import qualified System.IO as IO
import qualified Testnet.Conf as H
import qualified Testnet.Util.Base as H

testnetProperty :: Maybe Int -> (H.Conf -> H.Integration ()) -> H.Property
testnetProperty maybeTestnetMagic tn = H.integrationRetryWorkspace 2 "testnet" $ \workspaceDir -> do
  conf <- H.mkConf Nothing workspaceDir maybeTestnetMagic

  -- Fork a thread to keep alive indefinitely any resources allocated by testnet.
  void . H.evalM . liftResourceT . resourceForkIO . forever . liftIO $ IO.threadDelay 10000000

  void $ tn conf

  H.failure -- Intentional failure to force failure report

runTestnet :: Maybe Int -> (H.Conf -> H.Integration a) -> IO ()
runTestnet maybeTestnetMagic tn = do
  tvRunning <- STM.newTVarIO False

  void . H.check $ testnetProperty maybeTestnetMagic $ \c -> do
    void $ tn c
    H.evalIO . STM.atomically $ STM.writeTVar tvRunning True

  running <- STM.readTVarIO tvRunning

  if running
    then do
      ANSI.setSGR [SetColor Foreground Vivid Green]
      IO.putStr "Testnet is running.  Type CTRL-C to exit."
      ANSI.setSGR [Reset]
      IO.putStrLn ""
      void . forever $ IO.threadDelay 10000000
    else do
      ANSI.setSGR [SetColor Foreground Vivid Red]
      IO.putStr "Failed to start testnet."
      ANSI.setSGR [Reset]
      IO.putStrLn ""
      IO.exitFailure
