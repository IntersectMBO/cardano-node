{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.Configuration
  ( createConfigYaml
  , createSPOGenesisAndFiles
  , mkTopologyConfig
  , numSeededUTxOKeys
  ) where

import           Cardano.Api.Shelley hiding (cardanoEra)

import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Cardano.Node.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.String
import           Data.Time
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath.Posix ((</>))

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Aeson as J
import qualified Hedgehog.Extras.Stock.Time as DTC
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

import           Testnet.Defaults
import           Testnet.Filepath
import           Testnet.Process.Run (execCli_)
import           Testnet.Property.Utils
import           Testnet.Start.Types


createConfigYaml
  :: (MonadTest m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> m LBS.ByteString
createConfigYaml (TmpAbsolutePath tempAbsPath') anyCardanoEra' = GHC.withFrozenCallStack $ do
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


numSeededUTxOKeys :: Int
numSeededUTxOKeys = 3

-- | Adjust a value at a specific key. When the key is not a member of the map, the original map is returned
adjustKM :: (v -> v) -> KM.Key -> KM.KeyMap v -> KM.KeyMap v
adjustKM f k m =
  case KM.lookup k m of
    Nothing -> m
    Just v -> KM.insert k (f v) m

createSPOGenesisAndFiles
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => CardanoTestnetOptions
  -> UTCTime -- ^ Start time
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles testnetOptions startTime (TmpAbsolutePath tempAbsPath') = do
  let testnetMagic = cardanoTestnetMagic testnetOptions
      numPoolNodes = length $ cardanoNodes testnetOptions
      -- TODO: Even this is cumbersome. You need to know where to put the initial
      -- shelley genesis for create-staked to use.
      createStakedInitialGenesisFile = tempAbsPath' </> "genesis.spec.json"

  -- TODO: We need to read the genesis files into Haskell and modify them
  -- based on cardano-testnet's cli parameters

  -- We create the initial genesis file to avoid having to re-write the genesis file later
  -- with the parameters we want. The user must provide genesis files or we will use a default.
  -- We should *never* be modifying the genesis file after cardano-testnet is run because this
  -- is sure to be a source of confusion if users provide genesis files and we are mutating them
  -- without their knowledge.
  let shelleyGenesis :: LBS.ByteString
      shelleyGenesis = encode $ defaultShelleyGenesis startTime testnetOptions

  H.evalIO $ LBS.writeFile createStakedInitialGenesisFile shelleyGenesis

  -- TODO: Remove this rewrite.
 -- 50 second epochs
 -- Epoch length should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
  H.rewriteJsonFile createStakedInitialGenesisFile $ J.rewriteObject
      ( KM.insert "securityParam"          (toJSON @Int 5)    -- TODO: USE config p arameter
      . adjustKM
          (J.rewriteObject
              $ adjustKM
                (J.rewriteObject (KM.insert "major" (toJSON @Int 8)))
                "protocolVersion"
          )   "protocolParams"
      . KM.insert "rho"                    (toJSON @Double 0.1)
      . KM.insert "tau"                    (toJSON @Double 0.1)
      . KM.insert "updateQuorum"           (toJSON @Int 2)
      )

  execCli_
    [ "genesis", "create-staked"
    , "--genesis-dir", tempAbsPath'
    , "--testnet-magic", show @Int testnetMagic
    , "--gen-pools", show @Int numPoolNodes
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--gen-stake-delegs", "3"
    , "--gen-utxo-keys", show numSeededUTxOKeys
    , "--start-time", DTC.formatIso8601 startTime
    ]

  -- Here we move all of the keys etc generated by create-staked
  -- for the nodes to use

  -- Move all genesis related files

  genesisByronDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "byron"
  genesisShelleyDir <- H.createDirectoryIfMissing $ tempAbsPath' </> "shelley"

  files <- H.listDirectory tempAbsPath'
  forM_ files $ \file -> do
    H.note file

  H.renameFile (tempAbsPath' </> "byron-gen-command/genesis.json") (genesisByronDir </> "genesis.json")
  H.renameFile (tempAbsPath' </> "genesis.alonzo.json") (genesisShelleyDir </> "genesis.alonzo.json")
  H.renameFile (tempAbsPath' </> "genesis.conway.json") (genesisShelleyDir </> "genesis.conway.json")
  H.renameFile (tempAbsPath' </> "genesis.json") (genesisShelleyDir </> "genesis.json")

  return genesisShelleyDir

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
