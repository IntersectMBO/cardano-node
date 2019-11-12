{-# LANGUAGE OverloadedStrings   #-}

module Cardano.Config.Presets
  ( mainnetConfiguration
  , devConfiguration
  ) where

import           Cardano.Prelude

import           Ouroboros.Consensus.NodeId

import           Cardano.Config.Defaults (traceOptionsDefault)
import           Cardano.Config.Partial ( NodeProtocol (..)
                                        , PartialBlock (..)
                                        , PartialCardanoConfiguration (..)
                                        , PartialCertificate (..)
                                        , PartialCore (..)
                                        , PartialDLG (..)
                                        , PartialLastKnownBlockVersion (..)
                                        , PartialNTP (..)
                                        , PartialNode (..)
                                        , PartialTLS (..)
                                        , PartialTXP (..)
                                        , PartialUpdate (..)
                                        , PartialWallet (..)
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
          , pcoNodeProtocol = pure BFTProtocol
          , pcoStaticKeySigningKeyFile = mempty
          , pcoStaticKeyDlgCertFile = mempty
          , pcoRequiresNetworkMagic = pure RequiresNoMagic
          , pcoPBftSigThd = mempty
          }
    , pccNTP =
        PartialNTP
          { pntpResponseTimeout = pure 30000000
          , pntpPollDelay = pure 1800000000
          , pntpServers = pure
              [ "0.pool.ntp.org"
              , "2.pool.ntp.org"
              , "3.pool.ntp.org"
              ]
          }
    , pccUpdate =
        PartialUpdate
          { pupApplicationName = pure "cardano-sl"
          , pupApplicationVersion = pure 1
          , pupLastKnownBlockVersion =
              PartialLastKnownBlockVersion
                  { plkbvMajor = pure 0
                  , plkbvMinor = pure 2
                  , plkbvAlt = pure 0
                  }
          }
    , pccTXP =
        PartialTXP
          { ptxpMemPoolLimitTx = pure 200
          , ptxpAssetLockedSrcAddress = pure []
          }
    , pccDLG =
        PartialDLG
          { pdlgCacheParam = pure 500
          , pdlgMessageCacheTimeout = pure 30
          }
    , pccBlock =
        PartialBlock
          { pblNetworkDiameter = pure 18
          , pblRecoveryHeadersMessage = pure 2200
          , pblStreamWindow = pure 2048
          , pblNonCriticalCQBootstrap = pure 0.95
          , pblNonCriticalCQ = pure 0.8
          , pblCriticalCQBootstrap = pure 0.8888
          , pblCriticalCQ = pure 0.654321
          , pblCriticalForkThreshold = pure 3
          , pblFixedTimeCQ = pure 3600
          }
    , pccNode =
        PartialNode
          { pnoSlotLength = pure 20
          , pnoNetworkConnectionTimeout = pure 15000
          , pnoHandshakeTimeout = pure 30000
          }
    , pccTLS =
        PartialTLS
          { ptlsCA =
              PartialCertificate
                { pcertOrganization = pure "Input Output HK"
                , pcertCommonName = pure "Cardano SL Self-Signed Root CA"
                , pcertExpiryDays = pure 3600
                , pcertAltDNS = pure []
                }
          , ptlsServer =
              PartialCertificate
                { pcertOrganization = pure "Input Output HK"
                , pcertCommonName = pure "Cardano SL Server Node"
                , pcertExpiryDays = pure 3600
                , pcertAltDNS = pure
                    [ "localhost"
                    , "localhost.localdomain"
                    , "127.0.0.1"
                    , "::1"
                    ]
                }
          , ptlsClients =
              PartialCertificate
                { pcertOrganization = pure "Input Output HK"
                , pcertCommonName = pure "Daedalus Wallet"
                , pcertExpiryDays = pure 3600
                , pcertAltDNS = pure []
                }
          }
    , pccWallet =
        PartialWallet
          { pthEnabled = pure False
          , pthRate = pure 0
          , pthPeriod = pure ""
          , pthBurst = pure 0
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
          , pcoNodeProtocol = pure BFTProtocol
          , pcoStaticKeySigningKeyFile = mempty
          , pcoStaticKeyDlgCertFile = mempty
          , pcoRequiresNetworkMagic = pure RequiresMagic
          , pcoPBftSigThd = mempty
          }
    , pccNTP =
        PartialNTP
          { pntpResponseTimeout = pure 30000000
          , pntpPollDelay = pure 1800000000
          , pntpServers = pure
              [ "0.pool.ntp.org"
              , "2.pool.ntp.org"
              , "3.pool.ntp.org"
              ]
          }
    , pccUpdate =
        PartialUpdate
          { pupApplicationName = pure "cardano-sl"
          , pupApplicationVersion = pure 0
          , pupLastKnownBlockVersion =
              PartialLastKnownBlockVersion
                { plkbvMajor = pure 0
                , plkbvMinor = pure 0
                , plkbvAlt = pure 0
                }
          }
    , pccTXP =
        PartialTXP
          { ptxpMemPoolLimitTx = pure 200
          , ptxpAssetLockedSrcAddress = pure []
          }
    , pccDLG =
        PartialDLG
          { pdlgCacheParam = pure 500
          , pdlgMessageCacheTimeout = pure 30
          }
    , pccBlock =
        PartialBlock
          { pblNetworkDiameter = pure 3
          , pblRecoveryHeadersMessage = pure 20
          , pblStreamWindow = pure 2048
          , pblNonCriticalCQBootstrap = pure 0.95
          , pblNonCriticalCQ = pure 0.8
          , pblCriticalCQBootstrap = pure 0.8888
          , pblCriticalCQ = pure 0.654321
          , pblCriticalForkThreshold = pure 2
          , pblFixedTimeCQ = pure 10
          }
    , pccNode =
        PartialNode
          { pnoSlotLength = pure 2
          , pnoNetworkConnectionTimeout = pure 15000
          , pnoHandshakeTimeout = pure 30000
          }
    , pccTLS =
        PartialTLS
          { ptlsCA =
              PartialCertificate
                { pcertOrganization = pure "Input Output HK"
                , pcertCommonName = pure "Cardano SL Self-Signed Root CA"
                , pcertExpiryDays = pure 3650
                , pcertAltDNS = pure []
                }
          , ptlsServer =
              PartialCertificate
                { pcertOrganization = pure "Input Output HK"
                , pcertCommonName = pure "Cardano SL Server Node"
                , pcertExpiryDays = pure 365
                , pcertAltDNS = pure
                    [ "localhost"
                    , "localhost.localdomain"
                    , "127.0.0.1"
                    , "::1"
                    ]
                }
          , ptlsClients =
              PartialCertificate
                { pcertOrganization = pure "Input Output HK"
                , pcertCommonName = pure "Daedalus Wallet"
                , pcertExpiryDays = pure 365
                , pcertAltDNS = pure []
                }
          }
    , pccWallet =
        PartialWallet
          { pthEnabled = pure False
          , pthRate = pure 0
          , pthPeriod = pure ""
          , pthBurst = pure 0
          }
    }
