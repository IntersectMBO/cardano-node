{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module Testnet.Components.Configuration
  ( createConfigYaml
  , mkTopologyConfig
  ) where

import           Cardano.Api.Shelley hiding (cardanoEra)

import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Node.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.String
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath.Posix ((</>))

import           Hedgehog

import           Testnet.Defaults
import           Testnet.Filepath
import           Testnet.Property.Utils

createConfigYaml
  :: (MonadTest m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> m LBS.ByteString
createConfigYaml tempAbsPath anyCardanoEra' = GHC.withFrozenCallStack $ do
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  -- TODO: These genesis filepaths should not be hardcoded. Using the cli as a library
  -- rather as an executable will allow us to get the genesis files paths in a more
  -- direct fashion.

  byronGenesisHash <- getByronGenesisHash $ tempAbsPath' </> "byron/genesis.json"
  shelleyGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.json") "ShelleyGenesisHash"
  alonzoGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.alonzo.json") "AlonzoGenesisHash"
  conwayGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.conway.json") "ConwayGenesisHash"


  return . Aeson.encode . Aeson.Object
    $ mconcat [ byronGenesisHash
              , shelleyGenesisHash
              , alonzoGenesisHash
              , conwayGenesisHash
              , defaultYamlHardforkViaConfig anyCardanoEra'
              ]


ifaceAddress :: String
ifaceAddress = "127.0.0.1"

-- TODO: Reconcile all other mkTopologyConfig functions. NB: We only intend
-- to support current era on mainnet and the upcoming era.
mkTopologyConfig :: Int -> [Int] -> Int -> Bool -> LBS.ByteString
mkTopologyConfig numNodes allPorts port False = Aeson.encode topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        [ NonP2P.RemoteAddress (fromString ifaceAddress)
                               (fromIntegral peerPort)
                               (numNodes - 1)
        | peerPort <- allPorts List.\\ [port]
        ]
mkTopologyConfig numNodes allPorts port True = Aeson.encode topologyP2P
  where
    rootConfig :: P2P.RootConfig
    rootConfig =
      P2P.RootConfig
        [ RelayAccessAddress (fromString ifaceAddress)
                             (fromIntegral peerPort)
        | peerPort <- allPorts List.\\ [port]
        ]
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (HotValency (numNodes - 1))
                                  (WarmValency (numNodes - 1))
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        (UseLedger DontUseLedger)
