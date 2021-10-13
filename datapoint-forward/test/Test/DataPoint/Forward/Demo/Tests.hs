{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.DataPoint.Forward.Demo.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.STM.TVar (modifyTVar')
import qualified Data.Aeson as A
import           GHC.Conc
import           System.Directory (getTemporaryDirectory)
#if defined(mingw32_HOST_OS)
import           System.FilePath ((</>), dropDrive)
import qualified Data.Text as T
#else
import           System.FilePath ((</>))
#endif
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Time.Extra (sleep)

import           Ouroboros.Network.IOManager (withIOManager)

import           DataPoint.Forward.Acceptor
import           DataPoint.Forward.Configuration
import           DataPoint.Forward.Forwarder
import           DataPoint.Forward.Utils

import           Test.DataPoint.Forward.Demo.Configs
import           Test.DataPoint.Forward.Protocol.Codec ()
import           Test.DataPoint.Forward.Types

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "DataPoint.Forward.Demo"
  [ testProperty "Pass DataPoints" prop_Demo
  ]

ni :: NodeInfo
ni = NodeInfo
  { niName     = "core-1"
  , niVersion  = "1.30.1"
  , niCommit   = "abcdefg"
  , niProtocol = "Shelley"
  }

bs :: BlockchainStatus
bs = BlockchainStatus
  { bsEpoch = 124
  , bsSlot  = 6785
  }

prop_Demo :: Property
prop_Demo = ioProperty . withIOManager $ \iomgr -> do
  ep <- LocalPipe <$> mkLocalPipePath
  weAreDone <- newTVarIO False
  dpStore <- initDataPointStore
  dpAsker <- initDataPointAsker

  dpValues <- withAsync (runDataPointAcceptor iomgr (mkAcceptorConfig ep weAreDone) dpAsker) $ \_ -> do
    sleep 0.5
    withAsync (runDataPointForwarder iomgr (mkForwarderConfig ep) dpStore) $ \_ -> do
      sleep 0.5
      writeToStore dpStore "nodeInfo"     $ DataPoint ni
      writeToStore dpStore "bchainStatus" $ DataPoint bs
      sleep 0.5
      values <- askForDataPoints dpAsker ["nodeInfo", "bchainStatus", "wrongName"]
      sleep 1.0
      atomically $ modifyTVar' weAreDone $ const True
      sleep 1.0
      return values

  p1 <- case lookup "nodeInfo" dpValues of
    Just (Just v) -> case A.decode v of
                       Nothing -> false "nodeInfo value is invalid!"
                       Just (ni' :: NodeInfo) -> return $ ni === ni'
    _ -> false "No expected nodeInfo value!"

  p2 <- case lookup "bchainStatus" dpValues of
    Just (Just v) -> case A.decode v of
                       Nothing -> false "bchainStatus value is invalid!"
                       Just (bs' :: BlockchainStatus) -> return $ bs === bs'
    _ -> false "No expected bchainStatus value!"

  p3 <- case lookup "wrongName" dpValues of
    Just Nothing -> return $ property True
    _ -> false "No expected wrongName value!"

  return $ p1 .&&. p2 .&&. p3

mkLocalPipePath :: IO FilePath
mkLocalPipePath = do
  tmpDir <- getTemporaryDirectory
#if defined(mingw32_HOST_OS)
  return $ "\\\\.\\pipe\\" <> (T.unpack . T.replace "\\" "-" . T.pack) (dropDrive tmpDir)
                           <> "_" </> "trace-forward-test"
#else
  return $ tmpDir </> "trace-forward-test.sock"
#endif

false :: String -> IO Property
false msg = return . counterexample msg $ property False
