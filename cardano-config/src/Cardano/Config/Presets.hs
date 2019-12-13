{-# LANGUAGE OverloadedStrings   #-}

module Cardano.Config.Presets
  ( mainnetConfiguration
  , devConfiguration
  ) where

import           Cardano.Prelude

import qualified Cardano.Chain.Update as Update
import           Ouroboros.Consensus.NodeId

import           Cardano.Config.Defaults (traceOptionsDefault)
import           Cardano.Config.Partial ( PartialCardanoConfiguration (..)
                                        , PartialCore (..)
                                        , PartialLastKnownBlockVersion (..)
                                        , PartialNode (..)
                                        , PartialUpdate (..)
                                        )
import           Cardano.Config.Topology (NodeAddress(..), NodeHostAddress(..),
                                          TopologyInfo(..))
import           Cardano.Config.Types (Protocol(..), ViewMode(..))
import           Cardano.Crypto (RequiresNetworkMagic(..))

--------------------------------------------------------------------------------
-- Cardano Mainnet Configuration
--------------------------------------------------------------------------------

mainnetConfiguration :: PartialCardanoConfiguration
mainnetConfiguration =
  PartialCardanoConfiguration
    { pccLogPath = pure "./logs/"
    , pccLogConfig = pure "./configuration/log-configuration.yaml"
    , pccDBPath = pure "./db/"
    , pccApplicationLockFile = pure ""
    , pccTopologyInfo = pure $ TopologyInfo (CoreId 0) "./configuration/simple-topology.json"
    , pccNodeAddress = pure $ NodeAddress (NodeHostAddress Nothing) 7000
    , pccProtocol = pure ByronLegacy
    , pccViewMode = pure LiveView
    , pccLogMetrics = pure True
    , pccTraceOptions = pure traceOptionsDefault
    , pccSocketDir = pure "./socket/"
    , pccCore =
        PartialCore
          { pcoGenesisFile = pure "mainnet-genesis.json"
          , pcoGenesisHash = pure "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
          , pcoNodeId = mempty
          , pcoNumCoreNodes = mempty
          , pcoStaticKeySigningKeyFile = mempty
          , pcoStaticKeyDlgCertFile = mempty
          , pcoRequiresNetworkMagic = pure RequiresNoMagic
          , pcoPBftSigThd = mempty
          }
    , pccUpdate =
        PartialUpdate
          { pupApplicationName = pure (Update.ApplicationName "cardano-sl")
          , pupApplicationVersion = pure 1
          , pupLastKnownBlockVersion =
              PartialLastKnownBlockVersion
                  { plkbvMajor = pure 0
                  , plkbvMinor = pure 2
                  , plkbvAlt = pure 0
                  }
          }
    , pccNode =
        PartialNode
          { pnoSlotLength = pure 20
          , pnoNetworkConnectionTimeout = pure 15000
          , pnoHandshakeTimeout = pure 30000
          }
    }

--------------------------------------------------------------------------------
-- Cardano Dev Configuration
--------------------------------------------------------------------------------

devConfiguration :: PartialCardanoConfiguration
devConfiguration =
  PartialCardanoConfiguration
    { pccLogPath = pure "./logs/"
    , pccDBPath = pure "./db/"
    , pccLogConfig = pure "./log-config.yaml"
    , pccSocketDir = pure "./socket/"
    , pccApplicationLockFile = pure ""
    , pccTopologyInfo = pure $ TopologyInfo (CoreId 0) "./configuration/simple-topology.json"
    , pccNodeAddress = pure $ NodeAddress (NodeHostAddress Nothing) 7000
    , pccProtocol = pure ByronLegacy
    , pccViewMode = pure LiveView
    , pccLogMetrics = pure True
    , pccTraceOptions = pure traceOptionsDefault
    , pccCore =
        PartialCore
          { pcoGenesisFile = pure "testnet-genesis.json"
          , pcoGenesisHash = pure "7f141ea26e189c9cb09e2473f6499561011d5d3c90dd642fde859ce02282a3ae"
          , pcoNodeId = mempty
          , pcoNumCoreNodes = mempty
          , pcoStaticKeySigningKeyFile = mempty
          , pcoStaticKeyDlgCertFile = mempty
          , pcoRequiresNetworkMagic = pure RequiresMagic
          , pcoPBftSigThd = mempty
          }
    , pccUpdate =
        PartialUpdate
          { pupApplicationName = pure (Update.ApplicationName "cardano-sl")
          , pupApplicationVersion = pure 0
          , pupLastKnownBlockVersion =
              PartialLastKnownBlockVersion
                { plkbvMajor = pure 0
                , plkbvMinor = pure 0
                , plkbvAlt = pure 0
                }
          }
    , pccNode =
        PartialNode
          { pnoSlotLength = pure 2
          , pnoNetworkConnectionTimeout = pure 15000
          , pnoHandshakeTimeout = pure 30000
          }
    }
