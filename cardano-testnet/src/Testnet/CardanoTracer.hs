{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.CardanoTracer
  ( CardanoTracerConf (..)
  , CardanoTracerRuntime (..)
  , startCardanoTracer
  ) where


import           Testnet.Filepath
import           Cardano.Node.Testnet.Paths (defaultSocketName)
import           Cardano.Tracer.Configuration

import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket, sprocketArgumentName)

import           Prelude

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Aeson (encodeFile)
import           Data.List.NonEmpty (NonEmpty(..))
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Network.Socket (PortNumber)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as IO
import           System.Process (ProcessHandle)

import qualified Testnet.Ping as Ping
import           Testnet.Process.RunIO (procFlex, liftIOAnnotated)
import           Testnet.Process.Run (initiateProcess)

import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO

import           RIO (runRIO, throwString, unless)

data CardanoTracerConf = CardanoTracerConf
  { tempAbsPath :: FilePath
  , testnetMagic :: Int
  , logFormat :: LogFormat
  } deriving (Eq, Show)

mkConfig :: CardanoTracerConf -> Int -> FilePath ->  Sprocket -> TracerConfig
mkConfig CardanoTracerConf { testnetMagic, logFormat } port logFile sprocket = TracerConfig
  { networkMagic = fromIntegral testnetMagic
  , network = AcceptAt $ LocalPipe $ sprocketArgumentName sprocket
  , loRequestNum = Nothing
  , ekgRequestFreq = Nothing
  , hasEKG = Nothing
  , hasPrometheus = Just $ Endpoint "127.0.0.1" port $ Just False
  , hasTimeseries = Nothing
  , tlsCertificate = Nothing
  , hasForwarding = Nothing
  , logging = LoggingParams logFile FileMode logFormat :| []
  , rotation = Just $ RotationParams
      { rpFrequencySecs = 60
      , rpLogLimitBytes = 50_000_000
      , rpMaxAgeMinutes = 3 * 24 * 60
      , rpKeepFilesNum = 10
      }
  , verbosity = Nothing
  , metricsNoSuffix = Nothing
  , metricsHelp = Nothing
  , resourceFreq = Nothing
  , ekgRequestFull = Nothing
  , prometheusLabels = Nothing
  }

data CardanoTracerRuntime = CardanoTracerRuntime
  { tracerSprocket :: Sprocket
  , tracerHandle :: ProcessHandle
  , prometheusPort :: PortNumber
  }

-- | Start a @cardano-tracer@ process, returning the (working-directory relative)
-- path to the socket that testnet nodes should connect to, together with the
-- process handle of the spawned tracer.
startCardanoTracer
  :: HasCallStack
  => MonadFail m
  => MonadResource m
  => MonadCatch m
  => CardanoTracerConf
  -> m CardanoTracerRuntime
startCardanoTracer conf@CardanoTracerConf{tempAbsPath} = GHC.withFrozenCallStack $ do
  let tmpPath = TmpAbsolutePath tempAbsPath
      logDir = makeLogDir tmpPath
      tempBaseAbsPath = makeTmpBaseAbsPath tmpPath

  liftIO $ do
    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True $ tempAbsPath </> makeSocketDir tmpPath

  let nodeStdoutFile = logDir </> "cardano-tracer.stdout.log"
      nodeStderrFile = logDir </> "cardano-tracer.stderr.log"
      sprocket = makeSprocket tmpPath defaultSocketName
      configFile = tempAbsPath </> "cardano-tracer-config.json"

  hNodeStdout <- liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- liftIO $ IO.openFile nodeStderrFile IO.WriteMode

  [prometheusPortNo] <- liftIO $ IO.allocateRandomPorts 1
  let prometheusPort = fromIntegral prometheusPortNo
  -- The port number if it is obtained using 'H.randomPort', it is firstly bound to and then closed. The closing
  -- and release in the operating system is done asynchronously and can be slow. Here we wait until the port

  let portWaitTimeout = 45
  isClosed <- liftIOAnnotated $ Ping.waitForPortClosed portWaitTimeout 0.1 prometheusPort
  unless isClosed $
    throwString $ "Port is still in use after " ++ show portWaitTimeout ++ " seconds before starting tracer: " <> show prometheusPortNo
  liftIO $ encodeFile configFile $ mkConfig conf prometheusPortNo logDir sprocket

  cp <- runRIO () $ procFlex "cardano-tracer" "CARDANO_TRACER"
    [ "--config", configFile
    ]

  eResult <- runExceptT . initiateProcess $ cp
    { IO.std_in = IO.CreatePipe
    , IO.std_out = IO.UseHandle hNodeStdout
    , IO.std_err = IO.UseHandle hNodeStderr
    , IO.cwd = Just tempBaseAbsPath
    }
  hProcess <- case eResult of
    Left err -> throwString $ "Could not start cardano-tracer: " <> show err
    Right (_, _, _, hProcess, _) -> pure hProcess

  ePortResult <- liftIOAnnotated $ Ping.waitForTcpPort portWaitTimeout 0.1 "127.0.0.1" prometheusPort
  case ePortResult of
    Left err -> throwString $ "Prometheus didn't start: " <> show err
    Right _ -> pure ()

  pure $ CardanoTracerRuntime sprocket hProcess prometheusPort
