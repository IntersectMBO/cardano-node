{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Status
  ( hprop_status
  , hprop_status_no_testnet
  ) where

import           Cardano.Api (CardanoEra (..))

import           Cardano.Testnet (TestnetNode (..), TestnetRuntime (..), createAndRunTestnet,
                   defaultManifestFile, isTestnetNodeSpo, mkConf)

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class (def)
import           Data.Either (isLeft)
import           Data.Foldable (toList)
import           Data.List (find)
import           Data.Maybe (isJust)
import           System.FilePath ((</>))
import qualified System.Process as Process

import           Testnet.Defaults (defaultGenesisFilepath)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Signal (hardKillProcess)
import           Testnet.Status.Run (checkStatus)
import           Testnet.Status.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

import           RIO (tryAny)

-- | Integration test: a fresh testnet is RUNNING. After one relay is
-- killed, it is DEGRADED, and the killed relay is down.
--
-- Execute with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Status reports network health/"'@
hprop_status :: Property
hprop_status = integrationRetryWorkspace 2 "status" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tmpDir
  TestnetRuntime{testnetNodes} <- createAndRunTestnet def def conf

  running <- H.evalIO $ checkStatus tmpDir 5
  reportStatus running === NetworkRunning
  map probeNodeState (reportNodes running) === map (const NodeOk) (toList testnetNodes)

  relay <- H.evalMaybe $ find (not . isTestnetNodeSpo) testnetNodes
  H.evalIO $ do
    hardKillProcess (nodeProcessHandle relay)
    -- Wait for the process, so that its pid is gone and status can tell "down".
    void . Process.waitForProcess $ nodeProcessHandle relay

  degraded <- H.evalIO $ checkStatus tmpDir 5
  reportStatus degraded === NetworkDegraded
  killed <- H.evalMaybe $ find ((== nodeName relay) . probeNodeName) (reportNodes degraded)
  probeNodeState killed === NodeDown
  H.assert . isJust $ probeError killed

-- | 'checkStatus' without a running testnet: a directory without a manifest
-- is NO-TESTNET, with a hint about why, and a broken manifest is an error.
--
-- Execute with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Status without a testnet/"'@
hprop_status_no_testnet :: Property
hprop_status_no_testnet = H.propertyOnce . H.workspace "status-no-testnet" $ \dir -> do
  let missingDir = dir </> "missing"
  missing <- H.evalIO $ checkStatus missingDir 1
  reportStatus missing === NoTestnet
  reportProblemExplanation missing === Just ("The directory " <> missingDir <> " does not exist.")

  notTestnet <- H.evalIO $ checkStatus dir 1
  reportStatus notTestnet === NoTestnet
  reportProblemExplanation notTestnet === Just (dir <> " is not a testnet output directory.")

  -- The genesis files are written before the manifest, while the testnet starts.
  H.writeFile (dir </> defaultGenesisFilepath ShelleyEra) "{}"
  starting <- H.evalIO $ checkStatus dir 1
  reportStatus starting === NoTestnet
  reportProblemExplanation starting
    === Just "Genesis files exist, but no manifest: the testnet is still starting or it never became ready."

  H.writeFile (dir </> defaultManifestFile) "{"
  broken <- H.evalIO . tryAny $ checkStatus dir 1
  H.assertWith broken isLeft
