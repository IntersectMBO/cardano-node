{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.Test.Restart.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (asyncBound, uninterruptibleCancel)
import           Control.Monad (forM_)
import           Control.Monad.Extra (ifM)
import qualified Data.List.NonEmpty as NE
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.FilePath ((</>))
import           System.Directory (removePathForcibly)
import           System.Directory.Extra (listDirectories)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Run
import           Cardano.Tracer.Utils

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Restart"
  [ testProperty "forwarder" $ propRunInLogsStructure propNetworkForwarder
  ]

propNetworkForwarder :: FilePath -> FilePath -> IO Property
propNetworkForwarder rootDir localSock =
  propNetwork' rootDir
    ( launchForwardersSimple Initiator localSock 1000 10000
    , lift3M doRunCardanoTracer (return $ mkConfig rootDir localSock)
                                initProtocolsBrake
                                initDataPointRequestors
    )

propNetwork'
  :: FilePath
  -> (IO (), IO ())
  -> IO Property
propNetwork' rootDir (fstSide, sndSide) = do
  f <- asyncBound fstSide
  sleep 1.0
  s <- asyncBound sndSide
  -- Now sides should be connected and do some work.
  sleep 5.0
  -- Check if the root dir contains subdir, which is a proof that interaction
  -- between sides already occurred.
  ifM (doesDirectoryEmpty rootDir)
    (false "root dir is empty after the first start")
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
  :: FilePath
  -> FilePath
  -> TracerConfig
mkConfig root p = TracerConfig
  { networkMagic   = 764824073
  , network        = AcceptAt $ LocalSocket p
  , loRequestNum   = Just 1
  , ekgRequestFreq = Just 1.0
  , hasEKG         = Nothing
  , hasPrometheus  = Nothing
  , logging        = NE.fromList [LoggingParams root FileMode ForMachine]
  , rotation       = Nothing
  , verbosity      = Just Minimum
  }
