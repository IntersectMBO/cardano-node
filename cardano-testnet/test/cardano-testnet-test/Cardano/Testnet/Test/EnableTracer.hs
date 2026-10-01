{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.EnableTracer
  ( hprop_enable_tracer
  ) where

import           Cardano.Testnet (TestnetRuntimeOptions (..), TraceSupport (..), createAndRunTestnet, mkConf)

import           Data.Default.Class (def)
import           System.FilePath ((</>))

import qualified Testnet.Ping as Ping
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types (TestnetRuntime (..))

import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Enable Tracer/"'@
hprop_enable_tracer :: H.Property
hprop_enable_tracer = integrationRetryWorkspace 2 "enable-tracer" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let creationOptions = def
      runtimeOptions = def { runtimeEnableTracer = TraceEnabled }

  conf <- mkConf tmpDir
  runtime <- createAndRunTestnet creationOptions runtimeOptions conf

  -- The tracer writes its configuration to the testnet directory and its
  -- stdout/stderr to the logs directory. Their presence confirms the tracer
  -- was started.
  H.assertFilesExist
    [ tmpDir </> "cardano-tracer-config.json"
    , tmpDir </> "logs" </> "cardano-tracer.stdout.log"
    , tmpDir </> "logs" </> "cardano-tracer.stderr.log"
    ]

  -- The tracer exposes a Prometheus endpoint. Being able to connect to it
  -- confirms that the tracer is up and serving metrics.
  port <- H.nothingFail $ prometheusPort runtime
  H.evalIO (Ping.waitForTcpPort 45 0.1 "127.0.0.1" port) >>= \case
    Left err -> failWith Nothing $ "Prometheus endpoint did not respond: " <> show err
    Right () -> pure ()
