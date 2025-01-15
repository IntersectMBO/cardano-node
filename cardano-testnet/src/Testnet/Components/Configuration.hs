{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testnet.Components.Configuration
  ( createConfigJson
  , createSPOGenesisAndFiles
  , mkTopologyConfig
  , numSeededUTxOKeys

  , getByronGenesisHash
  , getShelleyGenesisHash

  , anyEraToString
  , eraToString
  ) where

import           Cardano.Api.Ledger (StandardCrypto)
import           Cardano.Api.Shelley hiding (Value, cardanoEra)

import           Cardano.Chain.Genesis (GenesisHash (unGenesisHash), readGenesisData)
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis)
import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.Bootstrap
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.PeerTrustable
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Aeson.Key hiding (fromString)
import           Data.Aeson.KeyMap hiding (map)
import qualified Data.Aeson.Lens as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Lens.Micro
import qualified System.Directory as System
import           System.FilePath.Posix (takeDirectory, (</>))

import           Testnet.Defaults
import           Testnet.Filepath
import           Testnet.Process.Run (execCli_)
import           Testnet.Start.Types (NumDReps (..), NumPools (..), anyEraToString,
                   anyShelleyBasedEraToString, eraToString)

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.Time as DTC
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

-- | Returns JSON encoded hashes of the era, as well as the hard fork configuration toggle.
createConfigJson :: ()
  => (MonadTest m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> ShelleyBasedEra era -- ^ The era used for generating the hard fork configuration toggle
  -> m LBS.ByteString
createConfigJson (TmpAbsolutePath tempAbsPath) sbe = GHC.withFrozenCallStack $ do
  byronGenesisHash <- getByronGenesisHash $ tempAbsPath </> "byron/genesis.json"
  shelleyGenesisHash <- getHash ShelleyEra "ShelleyGenesisHash"
  alonzoGenesisHash  <- getHash AlonzoEra  "AlonzoGenesisHash"
  conwayGenesisHash  <- getHash ConwayEra  "ConwayGenesisHash"

  pure . A.encodePretty . Object
    $ mconcat [ byronGenesisHash
              , shelleyGenesisHash
              , alonzoGenesisHash
              , conwayGenesisHash
              , defaultYamlHardforkViaConfig sbe
              ]
   where
    getHash :: (MonadTest m, MonadIO m) => CardanoEra a -> Text.Text -> m (KeyMap Value)
    getHash e = getShelleyGenesisHash (tempAbsPath </> defaultGenesisFilepath e)


-- Generate hashes for genesis.json files

getByronGenesisHash
  :: (H.MonadTest m, MonadIO m)
  => FilePath
  -> m (KeyMap Aeson.Value)
getByronGenesisHash path = do
  e <- runExceptT $ readGenesisData path
  (_, genesisHash) <- H.leftFail e
  let genesisHash' = unGenesisHash genesisHash
  pure . singleton "ByronGenesisHash" $ toJSON genesisHash'

getShelleyGenesisHash
  :: (H.MonadTest m, MonadIO m)
  => FilePath
  -> Text
  -> m (KeyMap Aeson.Value)
getShelleyGenesisHash path key = do
  content <- H.evalIO  $ BS.readFile path
  let genesisHash = Crypto.hashWith id content :: Crypto.Hash Crypto.Blake2b_256 BS.ByteString
  pure . singleton (fromText key) $ toJSON genesisHash

numSeededUTxOKeys :: Int
numSeededUTxOKeys = 3

createSPOGenesisAndFiles
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => NumPools -- ^ The number of pools to make
  -> NumDReps -- ^ The number of pools to make
  -> Word64 -- ^ The maximum supply
  -> AnyShelleyBasedEra -- ^ The era to use
  -> ShelleyGenesis StandardCrypto -- ^ The shelley genesis to use.
  -> AlonzoGenesis -- ^ The alonzo genesis to use, for example 'getDefaultAlonzoGenesis' from this module.
  -> ConwayGenesis StandardCrypto -- ^ The conway genesis to use, for example 'Defaults.defaultConwayGenesis'.
  -> TmpAbsolutePath
  -> m FilePath -- ^ Shelley genesis directory
createSPOGenesisAndFiles nPoolNodes nDelReps maxSupply sbe shelleyGenesis
                         alonzoGenesis conwayGenesis (TmpAbsolutePath tempAbsPath) = GHC.withFrozenCallStack $ do
  let inputGenesisShelleyFp = tempAbsPath </> genesisInputFilepath ShelleyEra
      inputGenesisAlonzoFp  = tempAbsPath </> genesisInputFilepath AlonzoEra
      inputGenesisConwayFp  = tempAbsPath </> genesisInputFilepath ConwayEra

  -- We write the genesis files to disk, to pass them to create-testnet-data.
  -- Then, create-testnet-data will output (possibly augmented/modified) versions
  -- and we remove those input files (see below), to avoid confusion.
  H.evalIO $ do
    LBS.writeFile inputGenesisShelleyFp $ A.encodePretty shelleyGenesis
    LBS.writeFile inputGenesisAlonzoFp  $ A.encodePretty alonzoGenesis
    LBS.writeFile inputGenesisConwayFp  $ A.encodePretty conwayGenesis

  let genesisShelleyDirAbs = takeDirectory inputGenesisShelleyFp
  genesisShelleyDir <- H.createDirectoryIfMissing genesisShelleyDirAbs
  let testnetMagic = sgNetworkMagic shelleyGenesis
      -- At least there should be a delegator per DRep
      -- otherwise some won't be representing anybody
      numStakeDelegators = max 3 (fromIntegral nDelReps) :: Int
      startTime = sgSystemStart shelleyGenesis

 -- TODO: Remove this rewrite.
 -- 50 second epochs
 -- Epoch length should be "10 * k / f" where "k = securityParam, f = activeSlotsCoeff"
  H.rewriteJsonFile @Value inputGenesisShelleyFp $ \o -> o
    & L.key "securityParam" . L._Integer .~ 5
    & L.key "updateQuorum" . L._Integer .~ 2

  -- TODO: create-testnet-data should have arguments for
  -- Alonzo and Conway genesis that are optional and if not
  -- supplised the users get a default
  H.note_ $ "Number of pools: " <> show nPoolNodes
  H.note_ $ "Number of stake delegators: " <> show nPoolNodes
  H.note_ $ "Number of seeded UTxO keys: " <> show numSeededUTxOKeys

  execCli_
    [ anyShelleyBasedEraToString sbe, "genesis", "create-testnet-data"
    , "--spec-shelley", inputGenesisShelleyFp
    , "--spec-alonzo",  inputGenesisAlonzoFp
    , "--spec-conway",  inputGenesisConwayFp
    , "--testnet-magic", show testnetMagic
    , "--pools", show nPoolNodes
    , "--total-supply",     show maxSupply -- Half of this will be delegated, see https://github.com/IntersectMBO/cardano-cli/pull/874
    , "--stake-delegators", show numStakeDelegators
    , "--utxo-keys", show numSeededUTxOKeys
    , "--drep-keys", show nDelReps
    , "--start-time", DTC.formatIso8601 startTime
    , "--out-dir", tempAbsPath
    ]

  -- Remove the input files. We don't need them anymore, since create-testnet-data wrote new versions.
  forM_ [inputGenesisShelleyFp, inputGenesisAlonzoFp, inputGenesisConwayFp] (liftIO . System.removeFile)

  -- Move all genesis related files
  genesisByronDir <- H.createDirectoryIfMissing $ tempAbsPath </> "byron"

  files <- H.listDirectory tempAbsPath
  forM_ files H.note

  H.renameFile (tempAbsPath </> "byron-gen-command" </> "genesis.json") (genesisByronDir </> "genesis.json")

  return genesisShelleyDir
  where
    genesisInputFilepath e = "genesis-input." <> anyEraToString (AnyCardanoEra e) <> ".json"

ifaceAddress :: String
ifaceAddress = "127.0.0.1"

-- TODO: Reconcile all other mkTopologyConfig functions. NB: We only intend
-- to support current era on mainnet and the upcoming era.
mkTopologyConfig :: Int -> [Int] -> Int -> Bool -> LBS.ByteString
mkTopologyConfig numNodes allPorts port False = A.encodePretty topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        [ NonP2P.RemoteAddress (fromString ifaceAddress)
                               (fromIntegral peerPort)
                               (numNodes - 1)
        | peerPort <- allPorts List.\\ [port]
        ]
mkTopologyConfig numNodes allPorts port True = A.encodePretty topologyP2P
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
                                  InitiatorAndResponderDiffusionMode
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        DontUseLedgerPeers
        DontUseBootstrapPeers
