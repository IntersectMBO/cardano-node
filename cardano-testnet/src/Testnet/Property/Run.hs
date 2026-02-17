{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Testnet.Property.Run
  ( runTestnet
  -- Ignore tests on various OSs
  , ignoreOn
  , ignoreOnWindows
  , ignoreOnMac
  , ignoreOnMacAndWindows
  , disabled
  ) where

import           Cardano.Api.IO (unFile)

import           Prelude

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM as STM
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Bool (bool)
import           Data.String (IsString (..))
import qualified System.Console.ANSI as ANSI
import           System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..))
import           System.Directory
import qualified System.Exit as IO
import           System.FilePath (normalise, (</>))
import qualified System.Info as SYS
import qualified System.IO as IO
import           Text.Printf (printf)

import           Testnet.Process.RunIO
import           Testnet.Property.Util (integration, integrationWorkspace)
import           Testnet.Start.Types (Conf, UserProvidedEnv (..), mkConf)
import           Testnet.Types (TestnetNode (..), TestnetRuntime (..), spoNodes)

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.OS (isWin32)
import qualified Hedgehog.Extras.Test.Base as H
import           Test.Tasty.ExpectedFailure (wrapTest)
import qualified Test.Tasty.Hedgehog as H
import           Test.Tasty.Providers (testPassed)
import           Test.Tasty.Runners (Result (resultShortDescription), TestTree)

runTestnet :: UserProvidedEnv -> (Conf -> H.Integration TestnetRuntime) -> IO ()
runTestnet env tn = do
  tvRunning <- STM.newTVarIO Nothing

  void . H.check $ testnetProperty env $ \c -> do
    runtime <- tn c
    H.evalIO . STM.atomically $ STM.writeTVar tvRunning (Just runtime)

  STM.readTVarIO tvRunning >>= \case
    Just runtime@TestnetRuntime
      { configurationFile
      , testnetMagic
      } -> do
      ANSI.setSGR [SetColor Foreground Vivid Green]
      IO.putStrLn $ printf
        "Please disregard the message above implying a failure.\n\
        \\n\
        \Testnet is running with config file %s"
        (unFile configurationFile)
      case spoNodes runtime of
        TestnetNode
          { nodeSprocket=Sprocket{sprocketBase, sprocketName}
          , nodeStdout
          }:_ -> putStrLn $ printf
            "Logs of the SPO node can be found at %s\n\
            \\n\
            \To interact with the testnet using cardano-cli, you might want to set:\n\
            \\n\
            \  export CARDANO_NODE_SOCKET_PATH=%s\n\
            \  export CARDANO_NODE_NETWORK_ID=%d\n"
            nodeStdout
            (normalise $ sprocketBase </> sprocketName)
            testnetMagic
        [] -> do
          ANSI.setSGR [SetColor Foreground Vivid Yellow]
          IO.putStrLn "\nFailed to find any SPO node in the testnet\n"
          ANSI.setSGR [SetColor Foreground Vivid Green]
      IO.putStrLn "Type CTRL-C to exit."

      ANSI.setSGR [Reset]
      void . forever $ IO.threadDelay 10000000
    Nothing -> do
      ANSI.setSGR [SetColor Foreground Vivid Red]
      IO.putStrLn "Failed to start testnet."
      ANSI.setSGR [Reset]
      IO.exitFailure


testnetProperty :: UserProvidedEnv -> (Conf -> H.Integration ()) -> H.Property
testnetProperty env runTn =
  case env of
      NoUserProvidedEnv -> do
        integrationWorkspace "testnet" $ \workspaceDir -> do
          mkConf workspaceDir >>= forkAndRunTestnet
      UserProvidedEnv userOutputDir ->
        integration $ do
          absUserOutputDir <- H.evalIO $ makeAbsolute userOutputDir
          dirExists <- H.evalIO $ doesDirectoryExist absUserOutputDir
          if dirExists then
            -- Happens when the environment has previously been created by the user
            H.note_ $ "Reusing " <> absUserOutputDir
          else do
            liftIOAnnotated $ createDirectory absUserOutputDir
            H.note_ $ "Created " <> absUserOutputDir
          conf <- mkConf absUserOutputDir
          forkAndRunTestnet conf
  where
    forkAndRunTestnet conf = do
      -- Fork a thread to keep alive indefinitely any resources allocated by testnet.
      void $ H.evalM . liftResourceT . resourceForkIO . forever . liftIOAnnotated $ IO.threadDelay 10000000
      void $ runTn conf
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
