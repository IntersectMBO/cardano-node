{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.EnableTracer
  ( hprop_enable_tracer
  ) where

import           Cardano.Testnet (TestnetRuntimeOptions (..), TraceSupport (..), createAndRunTestnet, mkConf)

import           Control.Monad (forM_)
import           Data.Default.Class (def)
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Network.HTTP.Simple
import           System.FilePath ((</>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types (TestnetRuntime (..))

import           Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Enable Tracer/"'@
hprop_enable_tracer :: H.Property
hprop_enable_tracer = integrationRetryWorkspace 2 "enable-tracer" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let creationOptions = def
      runtimeOptions = def { runtimeEnableTracer = TraceEnabled "127.0.0.1" Nothing }

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

  -- The tracer exposes a Prometheus endpoint whose root lists one route per
  -- connected node. A non-empty listing proves the nodes forward to the tracer.
  port <- H.nothingFail $ prometheusPort runtime
  let prometheusUrl = "http://127.0.0.1:" <> show port
  routes <- H.byDurationM 1 45 "Prometheus lists at least one node" $ do
    request <- H.evalM $ parseRequest prometheusUrl
      <&> setRequestHeader "Accept" ["application/json"]
    response <- H.evalM $ httpLbs request
    getResponseStatusCode response === 200
    routes :: Map Text Text <- H.leftFail . Aeson.eitherDecode $ getResponseBody response
    H.assertWith routes $ not . Map.null
    pure routes

  -- Each listed route serves that node's metrics page.
  H.byDurationM 1 45 "Every listed node serves metrics" $
    forM_ (Map.elems routes) $ \route -> do
      request <- H.evalM . parseRequest $ prometheusUrl <> Text.unpack route
      response <- H.evalM $ httpLbs request
      getResponseStatusCode response === 200
      H.assertWith (getResponseBody response) $ not . LBS.null
