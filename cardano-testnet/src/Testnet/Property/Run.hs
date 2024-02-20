module Testnet.Property.Run
  ( runTestnet
  -- Ignore tests on various OSs
  , ignoreOn
  , ignoreOnWindows
  , ignoreOnMac
  , ignoreOnMacAndWindows
  , disabled
  ) where

import           Prelude

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM as STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bool (bool)
import           Data.String (IsString (..))
import qualified System.Console.ANSI as ANSI
import           System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..))
import qualified System.Exit as IO
import qualified System.Info as SYS
import qualified System.IO as IO

import qualified Testnet.Property.Utils as H
import           Testnet.Start.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.OS (isWin32)
import qualified Hedgehog.Extras.Test.Base as H
import           Test.Tasty.ExpectedFailure (wrapTest)
import qualified Test.Tasty.Hedgehog as H
import           Test.Tasty.Providers (testPassed)
import           Test.Tasty.Runners (Result (resultShortDescription), TestTree)

runTestnet :: (Conf -> H.Integration a) -> IO ()
runTestnet tn = do
  tvRunning <- STM.newTVarIO False

  void . H.check $ testnetProperty $ \c -> do
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


testnetProperty :: (Conf -> H.Integration ()) -> H.Property
testnetProperty tn = H.integrationWorkspace "testnet" $ \workspaceDir -> do
  conf <- mkConf workspaceDir

  -- Fork a thread to keep alive indefinitely any resources allocated by testnet.
  void . H.evalM . liftResourceT . resourceForkIO . forever . liftIO $ IO.threadDelay 10000000

  void $ tn conf

  H.failure -- Intentional failure to force failure report


-- Ignore properties on various OSs

type Os = String

ignoreOnWindows :: String -> Property -> TestTree
ignoreOnWindows pName prop =
  bool id (ignoreOn "Windows") isWin32 $ H.testPropertyNamed pName (fromString pName) prop

ignoreOnMac :: String -> Property -> TestTree
ignoreOnMac pName prop =
  bool id (ignoreOn "MacOS") isMacOS $ H.testPropertyNamed pName (fromString pName) prop

ignoreOnMacAndWindows :: String -> Property -> TestTree
ignoreOnMacAndWindows pName prop =
  bool id (ignoreOn "MacOS and Windows") (isMacOS || isWin32) $ H.testPropertyNamed pName (fromString pName) prop

isMacOS :: Bool
isMacOS = SYS.os == "darwin"

ignoreOn :: Os -> TestTree -> TestTree
ignoreOn os = wrapTest $ const $ return $
  (testPassed ("IGNORED on " <> os))
    { resultShortDescription = "IGNORED on " <> os
    }

disabled :: String -> Property -> TestTree
disabled pName prop = ignoreOn "Disabled" $ H.testPropertyNamed pName (fromString pName) prop
