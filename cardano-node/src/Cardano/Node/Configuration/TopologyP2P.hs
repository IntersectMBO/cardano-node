{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

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
  , readTopologyFileOrError
  , rootConfigToRelayAccessPoint
  )
where

import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Configuration.Topology (TopologyError (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Network.NodeToNode (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (UseLedgerPeers (..))
import           Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   WarmValency (..))

import           Control.Applicative (Alternative (..))
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Exception.Base (Exception (..))
import           "contra-tracer" Control.Tracer (Tracer, traceWith)
import           Data.Aeson
import           Data.Bifunctor (Bifunctor (..))
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
rootConfigToRelayAccessPoint RootConfig { rootAccessPoints, rootAdvertise } =
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

instance ToJSON LocalRootPeersGroup where
  toJSON lrpg =
    object
      [ "accessPoints" .= rootAccessPoints (localRoots lrpg)
      , "advertise" .= rootAdvertise (localRoots lrpg)
      , "hotValency" .= hotValency lrpg
      , "warmValency" .= warmValency lrpg
      , "trustable" .= trustable lrpg
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
                                        }
  deriving (Eq, Show)

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o ->
                RealNodeTopology <$> (o .: "localRoots"                                  )
                                 <*> (o .: "publicRoots"                                 )
                                 <*> (o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers  )
                                 <*> (o .:? "bootstrapPeers" .!= DontUseBootstrapPeers)

instance ToJSON NetworkTopology where
  toJSON top =
    case top of
      RealNodeTopology { ntLocalRootPeersGroups
                       , ntPublicRootPeers
                       , ntUseLedgerPeers
                       , ntUseBootstrapPeers
                       } -> object [ "localRoots"         .= ntLocalRootPeersGroups
                                   , "publicRoots"        .= ntPublicRootPeers
                                   , "useLedgerAfterSlot" .= ntUseLedgerPeers
                                   , "bootstrapPeers"     .= ntUseBootstrapPeers
                                   ]

--
-- Legacy p2p topology file format
--

-- | A newtype wrapper which provides legacy 'FromJSON' instances.
--
newtype Legacy a = Legacy { getLegacy :: a }

instance FromJSON (Legacy a) => FromJSON (Legacy [a]) where
  parseJSON = fmap (Legacy . map getLegacy) . parseJSONList

instance FromJSON (Legacy LocalRootPeersGroup) where
  parseJSON = withObject "LocalRootPeersGroup" $ \o -> do
                hv@(HotValency v) <- o .: "hotValency"
                wv <- o .:? "warmValency" .!= WarmValency v
                fmap Legacy $ LocalRootPeersGroup
                  <$> o .: "localRoots"
                  <*> pure hv
                  <*> pure wv
                  <*> o .: "trustable"

instance FromJSON (Legacy LocalRootPeersGroups) where
  parseJSON = withObject "LocalRootPeersGroups" $ \o ->
                Legacy . LocalRootPeersGroups . getLegacy
                  <$> o .: "groups"

instance FromJSON (Legacy PublicRootPeers) where
  parseJSON = withObject "PublicRootPeers" $ \o ->
                Legacy . PublicRootPeers
                  <$> o .: "publicRoots"

instance FromJSON (Legacy NetworkTopology) where
  parseJSON = fmap Legacy
            . withObject "NetworkTopology" (\o ->
              RealNodeTopology <$> fmap getLegacy (o .: "LocalRoots")
                               <*> fmap getLegacy (o .: "PublicRoots")
                               <*> (o .:? "useLedgerAfterSlot" .!= DontUseLedgerPeers)
                               <*> pure DontUseBootstrapPeers)

-- | Read the `NetworkTopology` configuration from the specified file.
--
readTopologyFile :: Tracer IO (StartupTrace blk)
                 -> NodeConfiguration -> IO (Either Text NetworkTopology)
readTopologyFile tr nc = do
  eBs <- Exception.try $ BS.readFile (unTopology $ ncTopologyFile nc)

  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
      (case eitherDecode bs' of
        Left err -> Left (handlerJSON err)
        Right t
          | isValidTrustedPeerConfiguration t -> Right t
          | otherwise                         -> Left handlerBootstrap
      )
      `combine`
      first handlerJSON (eitherDecode bs')

 where
  combine :: Either Text NetworkTopology
          -> Either Text (Legacy NetworkTopology)
          -> IO (Either Text NetworkTopology)
  combine a b = case (a, b) of
    (Right {}, _)     -> return a
    (_, Right {})     -> traceWith tr NetworkConfigLegacy
                           >> return (getLegacy <$> b)
    (Left _, Left _)  -> -- ignore parsing error of legacy format
                         return a

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
  handlerBootstrap :: Text
  handlerBootstrap = mconcat
    [ "You seem to have not configured any trustable peers. "
    , "This is important in order for the node to make progress "
    , "in bootstrap mode. Make sure you provide at least one bootstrap peer "
    , "source. "
    ]

readTopologyFileOrError :: Tracer IO (StartupTrace blk)
                        -> NodeConfiguration -> IO NetworkTopology
readTopologyFileOrError tr nc =
      readTopologyFile tr nc
  >>= either (\err -> error $ "Cardano.Node.Configuration.TopologyP2P.readTopologyFile: "
                           <> Text.unpack err)
             pure

--
-- Checking for chance of progress in bootstrap phase
--

-- | This function returns false if non-trustable peers are configured
--
isValidTrustedPeerConfiguration :: NetworkTopology -> Bool
isValidTrustedPeerConfiguration (RealNodeTopology (LocalRootPeersGroups lprgs) _ _ ubp) =
    case ubp of
      DontUseBootstrapPeers   -> True
      UseBootstrapPeers []    -> anyTrustable
      UseBootstrapPeers (_:_) -> True
  where
    anyTrustable =
      any (\(LocalRootPeersGroup lr _ _ pt) -> case pt of
              IsNotTrustable -> False
              IsTrustable    -> not
                              . null
                              . rootAccessPoints
                              $ lr
          ) lprgs
