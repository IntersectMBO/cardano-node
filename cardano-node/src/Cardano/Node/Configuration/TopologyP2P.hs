{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Node.Configuration.TopologyP2P
  ( TopologyError(..)
  , NetworkTopology(..)
  , PublicRootPeers(..)
  , LocalRootPeersGroup(..)
  , LocalRootPeersGroups(..)
  , RootConfig(..)
  , NodeHostIPAddress(..)
  , NodeHostIPv4Address(..)
  , NodeHostIPv6Address(..)
  , NodeSetup(..)
  , PeerAdvertise(..)
  , nodeAddressToSockAddr
  , readTopologyFile
  , readPeerSnapshotFile
  , readTopologyFileOrError
  , rootConfigToRelayAccessPoint
  )
where

import           Cardano.Api (handleIOExceptionsLiftWith, liftEither, runExceptT, throwError)

import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Configuration.Topology (TopologyError (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot (..),
                   UseLedgerPeers (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   WarmValency (..))

import           Control.Applicative (Alternative (..))
import           Control.Exception.Safe (Exception (..), IOException, handleAny)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified "contra-tracer" Control.Tracer as CT
import           Data.Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           System.FilePath (takeDirectory, (</>))

data NodeSetup a = NodeSetup
  { nodeId          :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers       :: ![RootConfig a]
  , useLedger       :: !UseLedgerPeers
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (NodeSetup a) where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .:  "nodeId"
                  <*> o .:  "nodeIPv4Address"
                  <*> o .:  "nodeIPv6Address"
                  <*> o .:  "producers"
                  <*> o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers

instance ToJSON a => ToJSON (NodeSetup a) where
  toJSON ns =
    object
      [ "nodeId"             .= nodeId ns
      , "nodeIPv4Address"    .= nodeIPv4Address ns
      , "nodeIPv6Address"    .= nodeIPv6Address ns
      , "producers"          .= producers ns
      , "useLedgerAfterSlot" .= useLedger ns
      ]


-- | Each root peer consists of a list of access points and a shared
-- 'PeerAdvertise' field.
--
data RootConfig a = RootConfig
  { rootAccessPoints :: [a]
    -- ^ a list of relay access points, each of which is either an ip address
    -- or domain name and a port number.
  , rootAdvertise    :: PeerAdvertise
    -- ^ 'advertise' configures whether the root should be advertised through
    -- peer sharing.
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (RootConfig a) where
  parseJSON = withObject "RootConfig" $ \o ->
                RootConfig
                  <$> o .:  "accessPoints"
                  <*> o .:? "advertise" .!= DoNotAdvertisePeer

instance ToJSON a => ToJSON (RootConfig a) where
  toJSON ra =
    object
      [ "accessPoints" .= rootAccessPoints ra
      , "advertise"    .= rootAdvertise ra
      ]

-- | Transforms a 'RootConfig' into a pair of 'RelayAccessPoint' and its
-- corresponding 'PeerAdvertise' value.
--
rootConfigToRelayAccessPoint :: ()
  => forall a. RootConfig a
  -> [(a, PeerAdvertise)]
rootConfigToRelayAccessPoint RootConfig { rootAccessPoints, rootAdvertise  } =
    [ (accessPoint, rootAdvertise) | accessPoint <- rootAccessPoints ]


-- | A local root peers group.  Local roots are treated by the outbound
-- governor in a special way.  The node will make sure that a node has the
-- requested number ('valency'/'hotValency') of connections to the local root peer group.
-- 'warmValency' value is the value of warm/established connections that the node
-- will attempt to maintain. By default this value will be equal to 'hotValency'.
--
data LocalRootPeersGroup a = LocalRootPeersGroup
  { localRoots :: RootConfig a
  , hotValency :: HotValency
  , warmValency :: WarmValency
  , trustable   :: PeerTrustable
    -- ^ 'trustable' configures whether the root should be trusted in fallback
    -- state.
  , rootDiffusionMode :: DiffusionMode
    -- ^ diffusion mode; used for local root peers.
  } deriving (Eq, Show)

-- | Does not use the 'FromJSON' instance of 'RootConfig', so that
-- 'accessPoints', 'advertise', 'valency' and 'warmValency' fields are attached to the
-- same object.
instance FromJSON a => FromJSON (LocalRootPeersGroup a) where
  parseJSON = withObject "LocalRootPeersGroup" $ \o -> do
                hv@(HotValency v) <- o .: "valency"
                                 <|> o .: "hotValency"
                LocalRootPeersGroup
                  <$> parseJSON (Object o)
                  <*> pure hv
                  <*> o .:? "warmValency" .!= WarmValency v
                  <*> o .:? "trustable" .!= IsNotTrustable
                      -- deserialise via NodeDiffusionMode
                  <*> (maybe InitiatorAndResponderDiffusionMode getDiffusionMode
                        <$> o .:? "diffusionMode")

instance ToJSON a => ToJSON (LocalRootPeersGroup a) where
  toJSON lrpg =
    object
      [ "accessPoints" .= rootAccessPoints (localRoots lrpg)
      , "advertise" .= rootAdvertise (localRoots lrpg)
      , "hotValency" .= hotValency lrpg
      , "warmValency" .= warmValency lrpg
      , "trustable" .= trustable lrpg
        -- serialise via NodeDiffusionMode
      , "diffusionMode" .= NodeDiffusionMode (rootDiffusionMode lrpg)
      ]

newtype LocalRootPeersGroups a = LocalRootPeersGroups
  { groups :: [LocalRootPeersGroup a]
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (LocalRootPeersGroups a) where
  parseJSON = fmap LocalRootPeersGroups . parseJSONList

instance ToJSON a => ToJSON (LocalRootPeersGroups a) where
  toJSON = toJSONList . groups

newtype PublicRootPeers a = PublicRootPeers
  { publicRoots :: RootConfig a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (PublicRootPeers a) where
  parseJSON = fmap PublicRootPeers . parseJSON

instance ToJSON a => ToJSON (PublicRootPeers a) where
  toJSON = toJSON . publicRoots

data NetworkTopology a = RealNodeTopology
  { ntLocalRootPeersGroups :: !(LocalRootPeersGroups a)
  , ntPublicRootPeers      :: ![PublicRootPeers a]
  , ntUseLedgerPeers       :: !UseLedgerPeers
  , ntUseBootstrapPeers    :: !UseBootstrapPeers
  , ntPeerSnapshotPath     :: !(Maybe PeerSnapshotFile)
  }
  deriving (Eq, Show)

instance AdjustFilePaths (NetworkTopology a) where
  adjustFilePaths f nt@(RealNodeTopology _ _ _ _ mPeerSnapshotPath) =
    nt{ntPeerSnapshotPath = PeerSnapshotFile . f . unPeerSnapshotFile <$> mPeerSnapshotPath}

instance FromJSON a => FromJSON (NetworkTopology a) where
  parseJSON = withObject "NetworkTopology" $ \o ->
                RealNodeTopology <$> (o .: "localRoots"                                  )
                                 <*> (o .: "publicRoots"                                 )
                                 <*> (o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers  )
                                 <*> (o .:? "bootstrapPeers" .!= DontUseBootstrapPeers   )
                                 <*> (o .:? "peerSnapshotFile")

instance ToJSON a => ToJSON (NetworkTopology a) where
  toJSON top =
    case top of
      RealNodeTopology { ntLocalRootPeersGroups
                       , ntPublicRootPeers
                       , ntUseLedgerPeers
                       , ntUseBootstrapPeers
                       , ntPeerSnapshotPath
                       } -> object [ "localRoots"         .= ntLocalRootPeersGroups
                                   , "publicRoots"        .= ntPublicRootPeers
                                   , "useLedgerAfterSlot" .= ntUseLedgerPeers
                                   , "bootstrapPeers"     .= ntUseBootstrapPeers
                                   , "peerSnapshotFile"   .= ntPeerSnapshotPath
                                   ]

-- | Read the `NetworkTopology` configuration from the specified file.
readTopologyFile :: ()
  => forall a. FromJSON a
  => NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO (Either Text (NetworkTopology a))
readTopologyFile NodeConfiguration{ncTopologyFile=TopologyFile topologyFilePath, ncConsensusMode} tracer = runExceptT $ do
  bs <- handleIOExceptionsLiftWith handler $ BS.readFile topologyFilePath
  topology@RealNodeTopology{ntUseBootstrapPeers} <-
    liftEither . first handlerJSON $
      eitherDecode $ LBS.fromStrict bs

  unless (isValidTrustedPeerConfiguration topology) $
    throwError handlerBootstrap

  -- make all relative paths in the topology file relative to the topology file location
  adjustFilePaths (takeDirectory topologyFilePath </>) <$>
    if isGenesisCompatible ncConsensusMode ntUseBootstrapPeers
       then pure topology
       else do
         liftIO $ CT.traceWith tracer $ NetworkConfigUpdateError genesisIncompatible
         pure $ topology{ntUseBootstrapPeers = DontUseBootstrapPeers}
  where
    handler :: IOException -> Text
    handler e = Text.pack $ "Cardano.Node.Configuration.Topology.readTopologyFile: "
                          ++ displayException e
    handlerJSON :: String -> Text
    handlerJSON err = mconcat
      [ "Is your topology file formatted correctly? "
      , "Expecting P2P Topology file format. "
      , "The port and valency fields should be numerical. "
      , "If you specified the correct topology file "
      , "make sure that you correctly setup EnableP2P "
      , "configuration flag. "
      , Text.pack err
      ]
    genesisIncompatible
      = Text.pack $  "Cardano.Node.Configuration.Topology.readTopologyFile: "
                  <> "Bootstrap peers (field 'bootstrapPeers') are not compatible "
                  <> "with Genesis syncing mode, reverting to 'DontUseBootstrapPeers'. "
                  <> "Big ledger peers will be leveraged for decentralized syncing - it "
                  <> "is recommened to provide an up-to-date big ledger peer snapshot file "
                  <> "(field 'peerSnapshotFile' in topology configuration) to facilitate "
                  <> "this process."
    handlerBootstrap :: Text
    handlerBootstrap = mconcat
      [ "You seem to have not configured any trustable peers. "
      , "This is important in order for the node to make progress "
      , "in bootstrap mode. Make sure you provide at least one bootstrap peer "
      , "source. "
      ]
    isGenesisCompatible GenesisMode UseBootstrapPeers{} = False
    isGenesisCompatible _ _ = True

readTopologyFileOrError :: ()
  => forall a. FromJSON a
  => NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO (NetworkTopology a)
readTopologyFileOrError nc tr =
      readTopologyFile nc tr
  >>= either (\err -> error $ "Cardano.Node.Configuration.TopologyP2P.readTopologyFile: "
                           <> Text.unpack err)
             pure

readPeerSnapshotFile :: PeerSnapshotFile -> IO LedgerPeerSnapshot
readPeerSnapshotFile  (PeerSnapshotFile peerSnapshotFile) =
  handleException $
    either error pure =<< eitherDecodeFileStrict peerSnapshotFile
  where
    handleException = handleAny $ \e -> error $ "Cardano.Node.Configuration.TopologyP2P.readPeerSnapshotFile: " <> displayException e

--
-- Checking for chance of progress in bootstrap phase
--

-- | This function returns false if non-trustable peers are configured
--
isValidTrustedPeerConfiguration :: NetworkTopology a -> Bool
isValidTrustedPeerConfiguration (RealNodeTopology (LocalRootPeersGroups lprgs) _ _ ubp _) =
    case ubp of
      DontUseBootstrapPeers   -> True
      UseBootstrapPeers []    -> anyTrustable
      UseBootstrapPeers (_:_) -> True
  where
    anyTrustable =
      any (\LocalRootPeersGroup {localRoots, trustable} ->
            case trustable of
              IsNotTrustable -> False
              IsTrustable    -> not
                              . null
                              . rootAccessPoints
                              $ localRoots
          ) lprgs
