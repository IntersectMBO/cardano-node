{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Manifest
  ( hprop_manifest
  , hprop_manifest_windows_pipe_path
  ) where

import           Cardano.Testnet (createAndRunTestnet, defaultManifestFile, mkConf)

import           Prelude

import           Data.Default.Class (def)
import           System.FilePath (normalise, (</>))
import qualified System.FilePath.Windows as FilePath.Windows

import           Testnet.Manifest
import           Testnet.Property.Util (integrationRetryWorkspace)

import           Hedgehog (Property, (===))
import qualified Hedgehog.Extras as H

-- | Integration test: start a default testnet, then verify that
-- @manifest.json@ exists and can be decoded into the 'Manifest' type,
-- and that basic counts match expectations (3 nodes, 3 wallets, magic 42).
--
-- Execute with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Manifest/"'@
hprop_manifest :: Property
hprop_manifest = integrationRetryWorkspace 2 "manifest" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tmpDir
  _runtime <- createAndRunTestnet def def conf

  manifest <- H.readJsonFileOk @Manifest $ tmpDir </> defaultManifestFile

  -- The default cluster has 3 nodes (1 SPO + 2 relays)
  length (manifestNodes manifest) === 3
  -- The default cluster creates 3 funded wallets
  length (manifestWallets manifest) === 3
  -- The default testnet magic is 42
  mnMagic (manifestNetwork manifest) === 42

-- | Pure property: Windows named-pipe paths pass through
-- 'makeManifestRelPath' unchanged ('splitDrive' treats @\\.\@ as a drive).
--
-- Execute with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Manifest Windows pipe path/"'@
hprop_manifest_windows_pipe_path :: Property
hprop_manifest_windows_pipe_path = H.propertyOnce $ do
  let outputDir = "C:\\Users\\test\\testnet"
      pipePath  = "\\\\.\\pipe\\cardanotestnet-node1"

  -- (1) Under Windows-flavour filepath: pipe path passes through unchanged
  FilePath.Windows.normalise (FilePath.Windows.makeRelative outputDir pipePath)
    === pipePath

  -- (2) Under the real (platform-native) makeManifestRelPath: pipe path unchanged
  makeManifestRelPath outputDir pipePath === pipePath

  -- (3) Sanity happy-path: a child path is made relative and normalised.
  --     We compare against 'normalise' of the expected value because the
  --     platform separator may differ (forward slash on Posix, backslash
  --     on Windows).
  makeManifestRelPath "/tmp/out" "/tmp/out/socket/node1/sock"
    === normalise "socket/node1/sock"
