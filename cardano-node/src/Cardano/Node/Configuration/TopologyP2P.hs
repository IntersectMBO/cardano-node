{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

-- TODO: We need `2a89d89775 orphan-instances: more flexible NetworkTopology
-- JSON encoding` in `ouroboros-network-0.23` for `networkTopologyFromJSON` in
-- `Ouroboros.Network.OrphanInstances` to implement a drop in replacement using
-- `Ouroboros.Network.Diffusion.Topology` API.
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
  , nodeAddressToSockAddr
  , readTopologyFile
  , readPeerSnapshotFile
  , readTopologyFileOrError
  , rootConfigToRelayAccessPoint
  -- * Re-exports
  , DiffusionMode(..)
  , PeerAdvertise(..)
  , PeerTrustable(..)
  , RelayAccessPoint(..)
  , UseBootstrapPeers(..)
  , UseLedgerPeers(..)
  )
where

import           Cardano.Api (handleIOExceptionsLiftWith, liftEither, runExceptT, throwError)

import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot (..),
                   UseLedgerPeers (..), RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   WarmValency (..))

import           Control.Applicative (Alternative (..))
import           Control.Exception.Safe (Exception (..), IOException, try)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified "contra-tracer" Control.Tracer as CT
import           Data.Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe (isJust, isNothing)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           System.FilePath (takeDirectory, (</>))

newtype TopologyError
  = NodeIdNotFoundInToplogyFile FilePath
  deriving Show


data NodeSetup adr = NodeSetup
  { nodeId          :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers       :: ![RootConfig adr]
  , useLedger       :: !UseLedgerPeers
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance FromJSON adr => FromJSON (NodeSetup adr) where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .:  "nodeId"
                  <*> o .:  "nodeIPv4Address"
                  <*> o .:  "nodeIPv6Address"
                  <*> o .:  "producers"
                  <*> o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers

instance ToJSON adr => ToJSON (NodeSetup adr) where
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
data RootConfig adr = RootConfig
  { rootAccessPoints :: [adr]
    -- ^ a list of relay access points, each of which is either an ip address
    -- or domain name and a port number.
  , rootAdvertise    :: PeerAdvertise
    -- ^ 'advertise' configures whether the root should be advertised through
    -- peer sharing.
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance FromJSON adr => FromJSON (RootConfig adr) where
  parseJSON = withObject "RootConfig" $ \o ->
                RootConfig
                  <$> o .:  "accessPoints"
                  <*> o .:? "advertise" .!= DoNotAdvertisePeer

instance ToJSON adr => ToJSON (RootConfig adr) where
  toJSON ra =
    object
      [ "accessPoints" .= rootAccessPoints ra
      , "advertise"    .= rootAdvertise ra
      ]

-- | Transforms a 'RootConfig' into a pair of 'RelayAccessPoint' and its
-- corresponding 'PeerAdvertise' value.
--
rootConfigToRelayAccessPoint :: ()
  => forall adr. RootConfig adr
  -> [(adr, PeerAdvertise)]
rootConfigToRelayAccessPoint RootConfig { rootAccessPoints, rootAdvertise  } =
    [ (accessPoint, rootAdvertise) | accessPoint <- rootAccessPoints ]


-- | A local root peers group.  Local roots are treated by the outbound
-- governor in a special way.  The node will make sure that a node has the
-- requested number ('valency'/'hotValency') of connections to the local root peer group.
-- 'warmValency' value is the value of warm/established connections that the node
-- will attempt to maintain. By default this value will be equal to 'hotValency'.
--
data LocalRootPeersGroup adr = LocalRootPeersGroup
  { localRoots :: RootConfig adr
  , hotValency :: HotValency
  , warmValency :: WarmValency
  , trustable   :: PeerTrustable
    -- ^ 'trustable' configures whether the root should be trusted in fallback
    -- state.
  , rootDiffusionMode :: DiffusionMode
    -- ^ diffusion mode; used for local root peers.
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Does not use the 'FromJSON' instance of 'RootConfig', so that
-- 'accessPoints', 'advertise', 'valency' and 'warmValency' fields are attached to the
-- same object.
instance FromJSON adr => FromJSON (LocalRootPeersGroup adr) where
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

instance ToJSON adr => ToJSON (LocalRootPeersGroup adr) where
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

newtype LocalRootPeersGroups adr = LocalRootPeersGroups
  { groups :: [LocalRootPeersGroup adr]
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance FromJSON adr => FromJSON (LocalRootPeersGroups adr) where
  parseJSON = fmap LocalRootPeersGroups . parseJSONList

instance ToJSON adr => ToJSON (LocalRootPeersGroups adr) where
  toJSON = toJSONList . groups

newtype PublicRootPeers adr = PublicRootPeers
  { publicRoots :: RootConfig adr
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance FromJSON adr => FromJSON (PublicRootPeers adr) where
  parseJSON = fmap PublicRootPeers . parseJSON

instance ToJSON adr => ToJSON (PublicRootPeers adr) where
  toJSON = toJSON . publicRoots

-- | Describes the P2P topology of a node. Whenever the node actually runs,
-- the type parameter `adr` should be `RelayAccessPoint`. However, we might want to
-- use and serialize this type with `adr` being `NodeId`, or another placeholder
-- type, if we want the user to be able to edit the topology without knowing the
-- actual addresses of the nodes: those might only be knowable at runtime.
data NetworkTopology adr = RealNodeTopology
  { ntLocalRootPeersGroups :: !(LocalRootPeersGroups adr)
  , ntPublicRootPeers      :: ![PublicRootPeers adr]
  , ntUseLedgerPeers       :: !UseLedgerPeers
  , ntUseBootstrapPeers    :: !UseBootstrapPeers
  , ntPeerSnapshotPath     :: !(Maybe PeerSnapshotFile)
  }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance AdjustFilePaths (NetworkTopology adr) where
  adjustFilePaths f nt@(RealNodeTopology _ _ _ _ mPeerSnapshotPath) =
    nt{ntPeerSnapshotPath = PeerSnapshotFile . f . unPeerSnapshotFile <$> mPeerSnapshotPath}

instance FromJSON adr => FromJSON (NetworkTopology adr) where
  parseJSON = withObject "NetworkTopology" $ \o ->
                RealNodeTopology <$> (o .: "localRoots"                                  )
                                 <*> (o .: "publicRoots"                                 )
                                 <*> (o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers  )
                                 <*> (o .:? "bootstrapPeers" .!= DontUseBootstrapPeers   )
                                 <*> (o .:? "peerSnapshotFile")

instance ToJSON adr => ToJSON (NetworkTopology adr) where
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
  => forall adr. FromJSON adr
  => NodeConfiguration
  -> CT.Tracer IO (StartupTrace blk) -> IO (Either Text (NetworkTopology adr))
readTopologyFile NodeConfiguration{ncTopologyFile=TopologyFile topologyFilePath, ncConsensusMode, ncProtocolFiles} tracer = runExceptT $ do
  bs <- handleIOExceptionsLiftWith handler $ BS.readFile topologyFilePath
  topology@RealNodeTopology{ntUseLedgerPeers, ntUseBootstrapPeers, ntPeerSnapshotPath} <-
    liftEither . first handlerJSON $
      eitherDecode $ LBS.fromStrict bs

  unless (isValidTrustedPeerConfiguration topology) $
    throwError handlerBootstrap

  when (isBlockProducer && useLedgerPeers ntUseLedgerPeers) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg "Use of ledger peers is not recommended for BP operation"

  when (isJust ntPeerSnapshotPath && not (useLedgerPeers ntUseLedgerPeers) && isBlockProducer) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateInfo
           $ createMsg "Ledger peers and peer snapshot, although specified in the topology file, are disabled in line with recommended BP operation"

  when (inPraosMode && isJust ntPeerSnapshotPath &&  not (useLedgerPeers ntUseLedgerPeers)) $
    if isBlockProducer
    then liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg
           $  "Peer snapshot file specified but topology disables ledger peers - ignoring file. "
           <> "To turn off this message remove peerSnapshotFile from the topology file."
    else liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg
           $  "Peer snapshot file specified but topology disables ledger peers - ignoring file. "
           <> "To turn off this message enable the use of ledger peers or remove peerSnapshotFile from the topology file."


  when (inGenesisMode && not (useLedgerPeers ntUseLedgerPeers) && not isBlockProducer) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg "It is recommended to use ledger peers and peer snapshot file for relay operations in Genesis mode"

  when (inGenesisMode && isNothing ntPeerSnapshotPath && useLedgerPeers ntUseLedgerPeers && not isBlockProducer) $
    liftIO $ CT.traceWith tracer
           $ NetworkConfigUpdateWarning
           $ createMsg
           $  "It is recommended to specify an up-to-date ledger peer snapshot file for relay operations in Genesis mode "
           <> "when the use of ledger peers is enabled."

  -- make all relative paths in the topology file relative to the topology file location
  adjustFilePaths (takeDirectory topologyFilePath </>) <$>
    if isGenesisCompatible ncConsensusMode ntUseBootstrapPeers
       then pure topology
       else do
         liftIO $ CT.traceWith tracer $ NetworkConfigUpdateWarning genesisIncompatible
         pure $ topology{ntUseBootstrapPeers = DontUseBootstrapPeers}
  where
    createMsg msg =
      "Cardano.Node.Configuration.Topology.readTopologyFile: " <> msg
    handler :: IOException -> Text
    handler = Text.pack . createMsg . displayException
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
      = Text.pack . createMsg $
                     "Bootstrap peers (field 'bootstrapPeers') are not compatible "
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
    useLedgerPeers DontUseLedgerPeers = False
    useLedgerPeers _ = True
    isGenesisCompatible GenesisMode UseBootstrapPeers{} = False
    isGenesisCompatible _ _ = True
    inPraosMode = ncConsensusMode == PraosMode
    inGenesisMode = ncConsensusMode == GenesisMode
    isBlockProducer = hasProtocolFile ncProtocolFiles

readTopologyFileOrError :: ()
  => forall adr. FromJSON adr
  => NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO (NetworkTopology adr)
readTopologyFileOrError nc tr =
      readTopologyFile nc tr
  >>= either (\err -> error $ "Cardano.Node.Configuration.TopologyP2P.readTopologyFile: "
                           <> Text.unpack err)
             pure

readPeerSnapshotFile :: PeerSnapshotFile -> IO (Either Text LedgerPeerSnapshot)
readPeerSnapshotFile (PeerSnapshotFile file) = do
  content <- first renderException <$> try (BS.readFile file)
  return $ first handler $ content >>= eitherDecodeStrict
  where
    renderException :: IOException -> String
    renderException = displayException

    handler :: String -> Text
    handler msg =
      Text.pack
        $ "Cardano.Node.Configuration.TopologyP2P.readPeerSnapshotFile: " <> msg

--
-- Checking for chance of progress in bootstrap phase
--

-- | This function returns false if non-trustable peers are configured
--
isValidTrustedPeerConfiguration :: NetworkTopology adr -> Bool
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
