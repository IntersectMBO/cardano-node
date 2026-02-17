{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.RunTestnet
  ( hprop_run_testnet
  ) where

import           Cardano.Testnet (createAndRunTestnet, mkConf)
import           Cardano.Testnet.Test.Utils (nodesProduceBlocks)

import           Prelude

import           Data.Default.Class (def)

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (GenesisOptions (..))

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Produces blocks/"'@
hprop_run_testnet :: H.Property
hprop_run_testnet = integrationRetryWorkspace 2 "run-testnet" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let shelleyOptions = def { genesisEpochLength = 200 }
      testnetOptions = def

  conf <- mkConf tmpDir
  runtime <- createAndRunTestnet testnetOptions shelleyOptions conf

  nodesProduceBlocks tmpDir runtime
