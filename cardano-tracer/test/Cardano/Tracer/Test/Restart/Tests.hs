{-# OPTIONS_GHC -Wno-partial-fields -Wno-unused-local-binds -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Test.Restart.Tests
  ( tests
  ) where

import           Cardano.Logging (Trace (..))
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Run
import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils
import           Cardano.Tracer.Utils
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Control.Concurrent.Async (asyncBound, uninterruptibleCancel)
import           Control.Monad (forM_)
import           Control.Monad.Extra (ifM)
import qualified Data.List.NonEmpty as NE
import           System.Directory (removePathForcibly)
import           System.Directory.Extra (listDirectories)
import           System.FilePath ((</>))
import qualified System.IO as Sys
import           System.Time.Extra (sleep)

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestSetup Identity -> TestTree
tests ts = localOption (QuickCheckTests 1) $ testGroup "Test.Restart"
  [ testProperty "forwarder" $ propRunInLogsStructure ts (propNetworkForwarder ts)
  ]

propNetworkForwarder :: TestSetup Identity -> FilePath -> FilePath -> IO Property
propNetworkForwarder ts rootDir localSock = do
  let config = mkConfig ts rootDir localSock
  brake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  propNetwork' ts rootDir
    ( launchForwardersSimple ts Initiator localSock 1000 10000
    , doRunCardanoTracer config (Just $ rootDir <> "/../state") stderrShowTracer brake dpRequestors
    )

propNetwork'
  :: TestSetup Identity
  -> FilePath
  -> (IO (), IO ())
  -> IO Property
propNetwork' _ rootDir (fstSide, sndSide) = do
  f <- asyncBound fstSide
  sleep 1.0
  s <- asyncBound sndSide
  -- Now sides should be connected and do some work.
  sleep 5.0
  -- Check if the root dir contains subdir, which is a proof that interaction
  -- between sides already occurred.
  ifM (isDirectoryEmpty rootDir)
    (false $ "root dir is empty after the first start: " <> rootDir)
    $ do
      -- Take current subdirs (it corresponds to the connection).
      subDirs <- listDirectories rootDir
      -- Forcibly stop the first side (like killing the process in the real world).
      uninterruptibleCancel f
      -- Now the second side is working without the first one, and is trying to re-connect.
      sleep 6.0
      -- Remove previous subdirs to make sure the connection will be re-established.
      forM_ subDirs $ \sd -> removePathForcibly $ rootDir </> sd
      -- Start the first side again, soon the connection should be re-established.
      f' <- asyncBound fstSide
      -- Now it should be connected to the second side again.
      sleep 5.0
      -- Forcibly kill both sides.
      uninterruptibleCancel s
      uninterruptibleCancel f'
      -- Take current subdirs again, they must exist now.
      listDirectories rootDir >>= \case
        [] -> false "No re-connect occurred!"
        _ ->  return $ property True

mkConfig
  :: TestSetup Identity
  -> FilePath
  -> FilePath
  -> TracerConfig
mkConfig TestSetup{..} rootDir p = TracerConfig
  { networkMagic   = fromIntegral . unNetworkMagic $ unI tsNetworkMagic
  , network        = AcceptAt $ LocalSocket p
  , loRequestNum   = Just 1
  , ekgRequestFreq = Just 1.0
  , hasEKG         = Nothing
  , hasPrometheus  = Nothing
  , hasRTView      = Nothing
  , logging        = NE.fromList [LoggingParams rootDir FileMode ForMachine]
  , rotation       = Nothing
  , verbosity      = Just Minimum
  , metricsComp    = Nothing
  , hasForwarding  = Nothing
  , resourceFreq   = Nothing
  }
