{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.Configuration
  ( anyEraToString
  , createConfigYaml
  , createSPOGenesisAndFiles
  , eraToString
  , mkTopologyConfig
  , numSeededUTxOKeys
  , NumPools
  , numPools
  ) where

import           Cardano.Api.Ledger (StandardCrypto)
import           Cardano.Api.Shelley hiding (Value, cardanoEra)

import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis)
import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Ouroboros.Network.PeerSelection.Bootstrap
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.PeerTrustable
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Lens as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.String
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Lens.Micro
import           System.FilePath.Posix (takeDirectory, (</>))

import           Testnet.Defaults
import           Testnet.Filepath
import           Testnet.Process.Run (execCli_)
import           Testnet.Property.Utils
import           Testnet.Start.Types (CardanoTestnetOptions (..), anyEraToString, eraToString)

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Time as DTC
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

-- | Returns JSON encoded hashes of the era, as well as the hard fork configuration toggle.
createConfigYaml :: ()
  => (MonadTest m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra -- ^ The era used for generating the hard fork configuration toggle
  -> m LBS.ByteString
createConfigYaml (TmpAbsolutePath tempAbsPath) era = GHC.withFrozenCallStack $ do
  byronGenesisHash <- getByronGenesisHash $ tempAbsPath </> "byron/genesis.json"
  shelleyGenesisHash <- getHash ShelleyEra "ShelleyGenesisHash"
  alonzoGenesisHash  <- getHash AlonzoEra  "AlonzoGenesisHash"
  conwayGenesisHash  <- getHash ConwayEra  "ConwayGenesisHash"

  return . Aeson.encode . Aeson.Object
    $ mconcat [ byronGenesisHash
              , shelleyGenesisHash
              , alonzoGenesisHash
              , conwayGenesisHash
              , defaultYamlHardforkViaConfig era
              ]
   where
    getHash :: (MonadTest m, MonadIO m) => CardanoEra a -> Text.Text -> m (Aeson.KeyMap Aeson.Value)
    getHash e = getShelleyGenesisHash (tempAbsPath </> defaultGenesisFilepath e)

numSeededUTxOKeys :: Int
numSeededUTxOKeys = 3

newtype NumPools = NumPools Int

numPools :: CardanoTestnetOptions -> NumPools
numPools CardanoTestnetOptions { cardanoNodes } = NumPools $ length cardanoNodes

createSPOGenesisAndFiles
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => NumPools -- ^ The number of pools to make
  -> AnyCardanoEra -- ^ The era to use
  -> ShelleyGenesis StandardCrypto -- ^ The shelley genesis to use.
  -> AlonzoGenesis -- ^ The alonzo genesis to use, for example 'getDefaultAlonzoGenesis' from this module.
  -> ConwayGenesis StandardCrypto -- ^ The conway genesis to use, for example 'Defaults.defaultConwayGenesis'.
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles (NumPools numPoolNodes) era shelleyGenesis alonzoGenesis conwayGenesis (TmpAbsolutePath tempAbsPath) = do
  let genesisShelleyFpAbs = tempAbsPath </> defaultGenesisFilepath ShelleyEra
      genesisAlonzoFpAbs = tempAbsPath </> defaultGenesisFilepath AlonzoEra
      genesisConwayFpAbs = tempAbsPath </> defaultGenesisFilepath ConwayEra
      genesisShelleyDirAbs = takeDirectory genesisShelleyFpAbs
  genesisShelleyDir <- H.createDirectoryIfMissing genesisShelleyDirAbs
  let testnetMagic = sgNetworkMagic shelleyGenesis
      numStakeDelegators = 3 :: Int
      startTime = sgSystemStart shelleyGenesis

  -- We create the initial genesis file to avoid having to re-write the genesis file later
  -- with the parameters we want. The user must provide genesis files or we will use a default.
  -- We should *never* be modifying the genesis file after @cardanoTestnet@ is run because this
  -- is sure to be a source of confusion if users provide genesis files and we are mutating them
  -- without their knowledge.
  H.evalIO $ do
    LBS.writeFile genesisShelleyFpAbs $ encode shelleyGenesis
    LBS.writeFile genesisAlonzoFpAbs  $ encode alonzoGenesis
    LBS.writeFile genesisConwayFpAbs  $ encode conwayGenesis

  -- TODO: Remove this rewrite.
 -- 50 second epochs
 -- Epoch length should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
  H.rewriteJsonFile @Value genesisShelleyFpAbs $ \o -> o
    -- TODO: remove rho and tau adjustment after https://github.com/IntersectMBO/cardano-api/pull/425 gets
    -- integrated with newer cardano-api into node
    & L.key "protocolParams" .  L.key "rho" . L._Number  .~ 0.1
    & L.key "protocolParams" .  L.key "tau" . L._Number  .~ 0.1
    & L.key "securityParam" . L._Integer .~ 5
    & L.key "updateQuorum" . L._Integer .~ 2

  -- TODO: create-testnet-data should have arguments for
  -- Alonzo and Conway genesis that are optional and if not
  -- supplised the users get a default
  H.note_ $ "Number of pools: " <> show numPoolNodes
  H.note_ $ "Number of stake delegators: " <> show numPoolNodes
  H.note_ $ "Number of seeded UTxO keys: " <> show numSeededUTxOKeys

  execCli_
    [ anyEraToString era, "genesis", "create-testnet-data"
    , "--spec-shelley", genesisShelleyFpAbs
    , "--spec-alonzo",  genesisAlonzoFpAbs
    , "--spec-conway",  genesisConwayFpAbs
    , "--testnet-magic", show testnetMagic
    , "--pools", show numPoolNodes
    , "--total-supply",     show @Int 2_000_000_000_000
    , "--delegated-supply", show @Int 1_000_000_000_000
    , "--stake-delegators", show numStakeDelegators
    , "--utxo-keys", show numSeededUTxOKeys
    , "--drep-keys", "3"
    , "--start-time", DTC.formatIso8601 startTime
    , "--out-dir", tempAbsPath
    ]

  -- Here we move all of the keys etc generated by create-testnet-data
  -- for the nodes to use

  -- Move all genesis related files

  genesisByronDir <- H.createDirectoryIfMissing $ tempAbsPath </> "byron"

  files <- H.listDirectory tempAbsPath
  forM_ files $ \file -> do
    H.note file

  H.renameFile (tempAbsPath </> "byron-gen-command" </> "genesis.json") (genesisByronDir </> "genesis.json")
  H.renameFile (tempAbsPath </> "shelley-genesis.json") (genesisShelleyDir </> "genesis.shelley.json")

  -- For some reason when setting "--total-supply 10E16" in create-testnet-data, we're getting negative
  -- treasury. TODO: This should be fixed by https://github.com/IntersectMBO/cardano-cli/pull/644
  -- So this can be removed when cardano-cli is upgraded above 8.20.3.0.
  H.rewriteJsonFile @Value (genesisShelleyDir </> "genesis.shelley.json") $ \o -> o
    & L.key "maxLovelaceSupply" . L._Integer .~ 10_000_000_000_000_000

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
                                  IsNotTrustable
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        DontUseLedgerPeers
        DontUseBootstrapPeers
