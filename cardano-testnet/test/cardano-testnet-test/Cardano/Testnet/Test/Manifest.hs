{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Manifest
  ( hprop_manifest
  ) where

import           Cardano.Testnet (createAndRunTestnet, defaultManifestFile, mkConf)

import           Prelude

import           Data.Default.Class (def)
import           System.FilePath ((</>))

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
