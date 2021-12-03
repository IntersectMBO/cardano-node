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
import           Cardano.Tracer.Utils

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

data Mode = Initiate | Response

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Network"
  [ testProperty "restart tracer"    $ propRunInLogsStructure propNetworkTracer
  , testProperty "restart forwarder" $ propRunInLogsStructure propNetworkForwarder
  ]

propNetworkTracer, propNetworkForwarder :: FilePath -> FilePath -> IO Property
propNetworkTracer rootDir localSock =
  propNetwork' rootDir
    ( doRunCardanoTracer (mkConfig Initiate rootDir localSock) =<< initProtocolsBrake
    , launchForwardersSimple Responder localSock 1000 10000
    )
propNetworkForwarder rootDir localSock =
  propNetwork' rootDir
    ( launchForwardersSimple Initiator localSock 1000 10000
    , doRunCardanoTracer (mkConfig Response rootDir localSock) =<< initProtocolsBrake
    )

propNetwork' :: FilePath -> (IO (), IO ()) -> IO Property
propNetwork' rootDir (fstSide, sndSide) = do
  f <- asyncBound fstSide
  s <- asyncBound sndSide
  -- Now sides should be connected and do some work.
  sleep 3.0
  -- Check if the root dir contains subdir, which is a proof that interaction
  -- between sides already occurred.
  ifM (doesDirectoryEmpty rootDir)
    (false "root dir is empty after the first start")
    $ do
      -- Forcibly stop the first side (like killing the process in the real world).
      uninterruptibleCancel f
      -- Now the second side is working without the first one, and tries to re-connect.
      sleep 3.0
      -- Remove rootDir's content, to make sure it will be re-created later.
      removeDirectoryContent rootDir
      -- Start the first side again, soon the connection should be re-established.
      f' <- asyncBound fstSide
      -- Now it should be connected to the second side again,
      -- and, if so, the subdir in the rootDir should be re-created.
      sleep 3.0
      -- Forcibly kill both sides.
      uninterruptibleCancel s
      uninterruptibleCancel f'
      -- Check if the root directory isn't empty, which means that the connection
      -- between sides was re-established and some work was performed again.
      ifM (doesDirectoryEmpty rootDir)
        (false "root dir is empty after restart")
        (return $ property True)

mkConfig
  :: Mode
  -> FilePath
  -> FilePath
  -> TracerConfig
mkConfig mode root p = TracerConfig
  { network        = case mode of
                       Initiate -> ConnectTo $ NE.fromList [LocalSocket p]
                       Response -> AcceptAt $ LocalSocket p
  , loRequestNum   = Just 1
  , ekgRequestFreq = Just 1.0
  , hasEKG         = Nothing
  , hasPrometheus  = Nothing
  , hasRTView      = Nothing
  , logging        = NE.fromList [LoggingParams root FileMode ForMachine]
  , rotation       = Nothing
  , verbosity      = Just Minimum
  }
