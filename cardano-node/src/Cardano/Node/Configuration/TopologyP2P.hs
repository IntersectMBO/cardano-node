{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Configuration.Topology (TopologyError (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Types
import           Cardano.Logging (traceWith)
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Network.ConsensusMode
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot (..),
                   UseLedgerPeers (..))
import           Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   WarmValency (..))

import           Control.Applicative (Alternative (..))
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Exception.Base (Exception (..))
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

data NodeSetup = NodeSetup
  { nodeId          :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers       :: ![RootConfig]
  , useLedger       :: !UseLedgerPeers
  } deriving (Eq, Show)

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .:  "nodeId"
                  <*> o .:  "nodeIPv4Address"
                  <*> o .:  "nodeIPv6Address"
                  <*> o .:  "producers"
                  <*> o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers

instance ToJSON NodeSetup where
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
data RootConfig = RootConfig
  { rootAccessPoints :: [RelayAccessPoint]
    -- ^ a list of relay access points, each of which is either an ip address
    -- or domain name and a port number.
  , rootAdvertise    :: PeerAdvertise
    -- ^ 'advertise' configures whether the root should be advertised through
    -- peer sharing.
  } deriving (Eq, Show)

instance FromJSON RootConfig where
  parseJSON = withObject "RootConfig" $ \o ->
                RootConfig
                  <$> o .:  "accessPoints"
                  <*> o .:? "advertise" .!= DoNotAdvertisePeer

instance ToJSON RootConfig where
  toJSON ra =
    object
      [ "accessPoints" .= rootAccessPoints ra
      , "advertise"    .= rootAdvertise ra
      ]

-- | Transforms a 'RootConfig' into a pair of 'RelayAccessPoint' and its
-- corresponding 'PeerAdvertise' value.
--
rootConfigToRelayAccessPoint
  :: RootConfig
  -> [(RelayAccessPoint, PeerAdvertise)]
rootConfigToRelayAccessPoint RootConfig { rootAccessPoints, rootAdvertise  } =
    [ (ap, rootAdvertise) | ap <- rootAccessPoints ]


-- | A local root peers group.  Local roots are treated by the outbound
-- governor in a special way.  The node will make sure that a node has the
-- requested number ('valency'/'hotValency') of connections to the local root peer group.
-- 'warmValency' value is the value of warm/established connections that the node
-- will attempt to maintain. By default this value will be equal to 'hotValency'.
--
data LocalRootPeersGroup = LocalRootPeersGroup
  { localRoots :: RootConfig
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
instance FromJSON LocalRootPeersGroup where
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

instance ToJSON LocalRootPeersGroup where
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

newtype LocalRootPeersGroups = LocalRootPeersGroups
  { groups :: [LocalRootPeersGroup]
  } deriving (Eq, Show)

instance FromJSON LocalRootPeersGroups where
  parseJSON = fmap LocalRootPeersGroups . parseJSONList

instance ToJSON LocalRootPeersGroups where
  toJSON = toJSONList . groups

newtype PublicRootPeers = PublicRootPeers
  { publicRoots :: RootConfig
  } deriving (Eq, Show)

instance FromJSON PublicRootPeers where
  parseJSON = fmap PublicRootPeers . parseJSON

instance ToJSON PublicRootPeers where
  toJSON = toJSON . publicRoots

data NetworkTopology = RealNodeTopology { ntLocalRootPeersGroups :: !LocalRootPeersGroups
                                        , ntPublicRootPeers      :: ![PublicRootPeers]
                                        , ntUseLedgerPeers       :: !UseLedgerPeers
                                        , ntUseBootstrapPeers    :: !UseBootstrapPeers
                                        , ntPeerSnapshotPath     :: !(Maybe PeerSnapshotFile)
                                        }
  deriving (Eq, Show)

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o ->
                RealNodeTopology <$> (o .: "localRoots"                                  )
                                 <*> (o .: "publicRoots"                                 )
                                 <*> (o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers  )
                                 <*> (o .:? "bootstrapPeers" .!= DontUseBootstrapPeers   )
                                 <*> (o .:? "peerSnapshotFile")

instance ToJSON NetworkTopology where
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
--
readTopologyFile :: NodeConfiguration -> IO (Either Text NetworkTopology)
readTopologyFile nc = do
  eBs <- Exception.try $ BS.readFile (unTopology $ ncTopologyFile nc)

  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
        return $ case eitherDecode bs' of
          Left err -> Left (handlerJSON err)
          Right t
            | isValidTrustedPeerConfiguration t ->
                if isGenesisCompatible (ncConsensusMode nc) (ntUseBootstrapPeers t)
                  then return (Right t)
                  else do
                    traceWith (ncTraceConfig nc) $
                      NetworkConfigUpdateError genesisIncompatible
                    return . Right $ t { ntUseBootstrapPeers = DontUseBootstrapPeers }
            | otherwise ->
                Left handlerBootstrap
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
                  <> "Bootstrap peers are not used in Genesis consensus mode."
    handlerBootstrap :: Text
    handlerBootstrap = mconcat
      [ "You seem to have not configured any trustable peers. "
      , "This is important in order for the node to make progress "
      , "in bootstrap mode. Make sure you provide at least one bootstrap peer "
      , "source. "
      ]
    isGenesisCompatible GenesisMode (UseBootstrapPeers{}) = False
    isGenesisCompatible _ _ = True

readTopologyFileOrError :: NodeConfiguration -> IO NetworkTopology
readTopologyFileOrError nc =
      readTopologyFile nc
  >>= either (\err -> error $ "Cardano.Node.Configuration.TopologyP2P.readTopologyFile: "
                           <> Text.unpack err)
             pure

readPeerSnapshotFile :: PeerSnapshotFile -> IO LedgerPeerSnapshot
readPeerSnapshotFile (PeerSnapshotFile psf) = either error pure =<< runExceptT
  (handleLeftT (left . ("Cardano.Node.Configuration.TopologyP2P.readPeerSnapshotFile: " <>)) $ do
    bs <- BS.readFile psf `catchIOExceptT` displayException
    hoistEither (eitherDecode . LBS.fromStrict $ bs))

--
-- Checking for chance of progress in bootstrap phase
--

-- | This function returns false if non-trustable peers are configured
--
isValidTrustedPeerConfiguration :: NetworkTopology -> Bool
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
