{-# LANGUAGE CPP #-}

module Cardano.Tracer.Test.Network.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (asyncBound, uninterruptibleCancel)
import           Control.Monad.Extra (ifM)
import qualified Data.List.NonEmpty as NE
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Run

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

data SideToRestart = First | Second

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Network"
  [ testProperty "restart forwarder" $ propRunInLogsStructure (propNetwork First)
  , testProperty "restart acceptor"  $ propRunInLogsStructure (propNetwork Second)
  ]

propNetwork :: SideToRestart -> FilePath -> FilePath -> IO Property
propNetwork whichSide rootDir localSock = do
  case whichSide of
    First ->
      propNetwork'
        rootDir
        ( runCardanoTracerWithConfig (config rootDir localSock)
        , launchForwardersSimple localSock 1000 10000
        )
    Second ->
      propNetwork'
        rootDir
        ( launchForwardersSimple localSock 1000 10000
        , runCardanoTracerWithConfig (config rootDir localSock)
        )

propNetwork' :: FilePath -> (IO (), IO ()) -> IO Property
propNetwork' rootDir (fstSide, sndSide) = do
  f <- asyncBound fstSide
  s <- asyncBound sndSide
  -- Now sides should be connected and do some work.
  sleep 3.0
  -- Forcibly stop the first side (like killing the process in the real world).
  uninterruptibleCancel f
  -- Now the second side is working without the first one, and tries to re-connect.
  sleep 4.0
  removeDirectoryContent rootDir -- To check it later.
  -- Restart the first side, now the connection should be re-established.
  f' <- asyncBound fstSide
  -- Now it should be connected to the second side again,
  -- and, if so, the root dir should be re-created.
  sleep 3.0
  -- Forcibly kill both sides.
  uninterruptibleCancel s
  uninterruptibleCancel f'
  -- Check if the root directory isn't empty, which means that the connection
  -- between parts was re-established and some work was performed.
  ifM (doesDirectoryEmpty rootDir)
    (false "root dir is empty")
    (return $ property True)

config :: FilePath -> FilePath -> TracerConfig
config root p = TracerConfig
  { network        = ConnectTo $ NE.fromList [LocalSocket p]
  , loRequestNum   = Just 1
  , ekgRequestFreq = Just 1.0
  , hasEKG         = Nothing
  , hasPrometheus  = Nothing
  , logging        = NE.fromList [LoggingParams root FileMode ForMachine]
  , rotation       = Nothing
  }
