{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.CardanoTracer
  ( CardanoTracerConf (..)
  , startCardanoTracer
  ) where


import           Testnet.Filepath
import           Cardano.Node.Testnet.Paths (defaultSocketName)
import           Cardano.Tracer.Configuration

import           Prelude

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Aeson (encodeFile)
import           Data.List.NonEmpty (NonEmpty(..))
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as IO
import           System.Process (ProcessHandle)

import           Testnet.Process.RunIO (procFlex)
import           Testnet.Process.Run (initiateProcess)

import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO

import           RIO (runRIO, throwString)

data CardanoTracerConf = CardanoTracerConf
  { tempAbsPath :: FilePath
  , testnetMagic :: Int
  , logFormat :: LogFormat
  } deriving (Eq, Show)

mkConfig :: CardanoTracerConf -> Int -> FilePath ->  FilePath -> TracerConfig
mkConfig CardanoTracerConf { testnetMagic, logFormat } port logFile socketFile = TracerConfig
  { networkMagic = fromIntegral testnetMagic
  , network = AcceptAt $ LocalPipe socketFile
  , loRequestNum = Nothing
  , ekgRequestFreq = Nothing
  , hasEKG = Nothing
  , hasPrometheus = Just $ Endpoint "127.0.0.1" port $ Just False
  , hasTimeseries = Nothing
  , tlsCertificate = Nothing
  , hasForwarding = Nothing
  , logging = LoggingParams logFile FileMode logFormat :| []
  , rotation = Nothing
  , verbosity = Nothing
  , metricsNoSuffix = Nothing
  , metricsHelp = Nothing
  , resourceFreq = Nothing
  , ekgRequestFull = Nothing
  , prometheusLabels = Nothing
  }

-- | Start a @cardano-tracer@ process, returning the (working-directory relative)
-- path to the socket that testnet nodes should connect to, together with the
-- process handle of the spawned tracer.
startCardanoTracer
  :: HasCallStack
  => MonadResource m
  => MonadCatch m
  => CardanoTracerConf
  -> m (FilePath, ProcessHandle)
startCardanoTracer conf@CardanoTracerConf{tempAbsPath} = GHC.withFrozenCallStack $ do
  let tmpPath = TmpAbsolutePath tempAbsPath
      logDir = makeLogDir tmpPath
      tempBaseAbsPath = makeTmpBaseAbsPath tmpPath

  liftIO $ do
    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True $ tempAbsPath </> makeSocketDir tmpPath

  let nodeStdoutFile = logDir </> "cardano-tracer.stdout.log"
      nodeStderrFile = logDir </> "cardano-tracer.stderr.log"
      -- The socket path is relative to the working directory shared by the
      -- tracer and the nodes ('tempBaseAbsPath').
      socketFile = makeSocketDir tmpPath </> defaultSocketName
      configFile = tempAbsPath </> "cardano-tracer-config.json"

  hNodeStdout <- liftIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- liftIO $ IO.openFile nodeStderrFile IO.WriteMode

  prometheusPort <- fmap head $ liftIO $ IO.allocateRandomPorts 1
  liftIO $ encodeFile configFile $ mkConfig conf prometheusPort logDir socketFile

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

  liftIO $ putStrLn $ "Prometheus is running at http://localhost:" <> show prometheusPort
  pure (socketFile, hProcess)
