{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.P2PTopology
  ( hprop_p2p_topology
  ) where

import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Testnet (CardanoTestnetOptions (..), cardanoTestnet, createTestnetEnv,
                   mkConf)
import           Cardano.Testnet.Test.Utils (nodesProduceBlocks)

import           Prelude

import           Data.Default.Class (def)
import           System.FilePath ((</>))

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types (GenesisOptions (..), NodeId, UserProvidedEnv (..))

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- TODO we're not supporting non-p2p topology, does this test make any sense now?

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Can be started with P2P topology file/"'@
hprop_p2p_topology :: H.Property
hprop_p2p_topology = integrationRetryWorkspace 2 "p2p-topology" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let testnetOptions = def { cardanoOutputDir = UserProvidedEnv tmpDir }
      genesisOptions = def { genesisEpochLength = 200 }
      createEnvOptions = def
      someTopologyFile = tmpDir </> "node-data" </> "node1" </> "topology.json"

  -- Generate the sandbox
  conf <- mkConf tmpDir
  createTestnetEnv testnetOptions genesisOptions createEnvOptions conf

  -- Check that the topology is indeed P2P
  eTopology <- H.readJsonFile someTopologyFile
  (_topology :: P2P.NetworkTopology NodeId) <- H.leftFail eTopology

  -- Run testnet with generated config
  runtime <- cardanoTestnet testnetOptions conf

  nodesProduceBlocks tmpDir runtime
