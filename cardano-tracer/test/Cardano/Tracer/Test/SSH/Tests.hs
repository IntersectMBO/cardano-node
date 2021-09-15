{-# LANGUAGE CPP #-}

module Cardano.Tracer.Test.SSH.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import           Control.Monad.Extra (ifM)
import qualified Data.List.NonEmpty as NE
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Process
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Run

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

data NetworkMode = Initiator | Responder

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Network"
  [ testProperty "SSH forwarding, initiator" $
      propRunInLogsStructure2 (propSSHForward Initiator)
  , testProperty "SSH forwarding, responder" $
      propRunInLogsStructure2 (propSSHForward Responder)
  ]

propSSHForward
  :: NetworkMode
  -> FilePath
  -> FilePath
  -> FilePath
  -> IO Property
propSSHForward netMode rootDir localSock1 localSock2 = do
  withCreateProcess (sshForward localSock1 localSock2) $ \_ _ _ _ -> do
    sleep 0.5
    -- Please note that "node" and tracer use different local sockets
    -- in this test, so they cannot establish the connection.
    -- The only way to do it is an SSH forwarding between these local sockets.
    stopEKG <- newTVarIO False
    stopTF  <- newTVarIO False
    let brakes = NE.fromList [(stopEKG, stopTF)]
    withAsync (runCardanoTracerWithConfigBrakes config brakes) $ \_ ->
      withAsync (launchForwardersSimple localSock2 1000 10000) $ \_ -> do
        sleep 4.0 -- Wait till some work is done.
        atomically $ do
          modifyTVar' stopEKG . const $ True
          modifyTVar' stopTF  . const $ True
        sleep 1.0

  -- Check if the root directory isn't empty, which means that the connection
  -- between parts was established. It proves that SSH tunnel works.
  ifM (doesDirectoryEmpty rootDir)
    (false "root dir is empty")
    (return $ property True)
 where
  config = TracerConfig
    { network        = net
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = NE.fromList [LoggingParams rootDir FileMode ForMachine]
    , rotation       = Nothing
    }

  net = case netMode of
          Initiator -> ConnectTo $ NE.fromList [LocalSocket localSock1]
          Responder -> AcceptAt $ LocalSocket localSock1

sshForward :: FilePath -> FilePath -> CreateProcess
sshForward tracerSock forwarderSock = proc "ssh"
  [ "-nNT"
  , "-L"
  , tracerSock
  , ":"
  , forwarderSock
  , "-o"
  , "\"ExitOnForwardFailure yes\""
  , "denis@127.0.0.1"
  ]
