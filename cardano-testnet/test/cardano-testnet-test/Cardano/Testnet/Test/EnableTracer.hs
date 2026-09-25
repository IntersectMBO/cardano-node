{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.EnableTracer
  ( hprop_enable_tracer
  ) where

import           Cardano.Testnet (TestnetRuntimeOptions (..), TraceSupport (..), createAndRunTestnet, mkConf)

import           Data.Default.Class (def)
import           System.FilePath ((</>))

import           Testnet.Property.Util (integrationRetryWorkspace)

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Enable Tracer/"'@
hprop_enable_tracer :: H.Property
hprop_enable_tracer = integrationRetryWorkspace 2 "enable-tracer" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let creationOptions = def
      runtimeOptions = def { runtimeEnableTracer = TraceEnabled }

  conf <- mkConf tmpDir
  _runtime <- createAndRunTestnet creationOptions runtimeOptions conf

  -- The tracer writes its configuration to the testnet directory and its
  -- stdout/stderr to the logs directory. Their presence confirms the tracer
  -- was started.
  H.assertFilesExist
    [ tmpDir </> "cardano-tracer-config.json"
    , tmpDir </> "logs" </> "cardano-tracer.stdout.log"
    , tmpDir </> "logs" </> "cardano-tracer.stderr.log"
    ]
