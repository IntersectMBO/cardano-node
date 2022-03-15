module Cardano.Tracer.Test.Restart.Tests
  ( tests
  ) where

import           Control.Concurrent.Async (asyncBound, uninterruptibleCancel)
import           Control.Monad.Extra (ifM)
import qualified Data.List.NonEmpty as NE
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory (getFileSize)
import           System.Directory.Extra (listDirectories, listFiles)
import           System.FilePath ((</>))
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Run
import           Cardano.Tracer.Utils

import           Cardano.Tracer.Test.Forwarder
import           Cardano.Tracer.Test.Utils

data Mode = Initiate | Response

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Restart"
  [ testProperty "forwarder" $ propRunInLogsStructure propNetworkForwarder
  ]

propNetworkForwarder :: FilePath -> FilePath -> IO Property
propNetworkForwarder rootDir localSock =
  propNetwork' Initiator rootDir
    ( launchForwardersSimple Initiator localSock 1000 10000
    , lift3M doRunCardanoTracer (return $ mkConfig Response rootDir localSock)
                                initProtocolsBrake
                                initDataPointRequestors
    )

propNetwork'
  :: ForwardersMode
  -> FilePath
  -> (IO (), IO ())
  -> IO Property
propNetwork' fmode rootDir (fstSide, sndSide) = do
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
      subDirs1 <- listDirectories rootDir
      log1Size <- getLogSize subDirs1
      -- Forcibly stop the first side (like killing the process in the real world).
      uninterruptibleCancel f
      -- Now the second side is working without the first one, and is trying to re-connect.
      sleep 5.0
      -- Start the first side again, soon the connection should be re-established.
      f' <- asyncBound fstSide
      -- Now it should be connected to the second side again.
      sleep 3.0
      -- Forcibly kill both sides.
      uninterruptibleCancel s
      uninterruptibleCancel f'
      -- Take current subdirs again.
      subDirs2 <- listDirectories rootDir
      case fmode of
        Responder -> do
          log2Size <- getLogSize subDirs2
          if log2Size > log1Size
            then return $ property True
            else false "No re-connected occurred (no items were added in the log)!"
        Initiator ->
          if subDirs1 == subDirs2
            then false "No re-connect occurred!"
            else return $ property True
 where
  getLogSize subDirs = do
    let subDir = head subDirs -- It's safe because we already checked it's not empty.
    fs <- listFiles (rootDir </> subDir)
    case fs of
      [f,_l] -> getFileSize f
      _ -> return 0

mkConfig
  :: Mode
  -> FilePath
  -> FilePath
  -> TracerConfig
mkConfig mode root p = TracerConfig
  { networkMagic   = 764824073
  , network        = case mode of
                       Initiate -> ConnectTo $ NE.fromList [LocalSocket p]
                       Response -> AcceptAt $ LocalSocket p
  , loRequestNum   = Just 1
  , ekgRequestFreq = Just 1.0
  , hasEKG         = Nothing
  , hasPrometheus  = Nothing
  , logging        = NE.fromList [LoggingParams root FileMode ForMachine]
  , rotation       = Nothing
  , verbosity      = Just Minimum
  }
