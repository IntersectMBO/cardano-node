{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

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


import           Cardano.Logging
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Configuration.Topology (TopologyError (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Tracing.Render (renderHeaderHashForDetails)
import           Cardano.Node.Tracing.Tracers.Startup ()
import           Cardano.Node.Types
import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.ConnMap (LocalAddr (..))
import           Ouroboros.Network.ConnectionManager.State (ConnStateId (..))
import           Ouroboros.Network.ConnectionManager.Types (AbstractState (..),
                   ConnectionManagerCounters (..), OperationResult (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnMgr
import           Ouroboros.Network.ConsensusMode
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import           Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Ouroboros.Network.PeerSelection.Governor (AssociationMode (..),
                   PeerSelectionTargets (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import qualified Ouroboros.Network.PeerSelection.PublicRootPeers as PublicRootPeers
import           Ouroboros.Network.PeerSelection.State.KnownPeers (KnownPeerInfo (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   LocalRootPeers, WarmValency (..), toGroups)
import           Ouroboros.Network.PeerSelection.Types (PeerStatus (..))
import           Ouroboros.Network.Protocol.Handshake (HandshakeException (..),
                   HandshakeProtocolError (..), RefuseReason (..))
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingResult (..))
import           Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Snocket (LocalAddress (..))

import           Control.Applicative (Alternative (..))
import qualified Control.Exception as Exception
import           Control.Exception.Base (Exception (..))
import qualified Control.Monad.Class.MonadTime.SI as SI
import           Control.Monad.Trans.Except.Extra
import qualified "contra-tracer" Control.Tracer as CT
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, object,
                   withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Data (Proxy (..))
import qualified Data.IP as IP
import qualified Data.Map.Strict as Map
import           Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word64)
import           Network.Mux (MiniProtocolNum (..))
import           Network.Socket (SockAddr (..))


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
readTopologyFile :: NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO (Either Text NetworkTopology)
readTopologyFile nc tr = do
  eBs <- Exception.try $ BS.readFile (unTopology $ ncTopologyFile nc)

  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
        case eitherDecode bs' of
          Left err -> return $ Left (handlerJSON err)
          Right t
            | isValidTrustedPeerConfiguration t ->
                if isGenesisCompatible (ncConsensusMode nc) (ntUseBootstrapPeers t)
                  then return (Right t)
                  else do
                    CT.traceWith tr $
                      NetworkConfigUpdateError genesisIncompatible
                    return . Right $ t { ntUseBootstrapPeers = DontUseBootstrapPeers }
            | otherwise ->
                pure $ Left handlerBootstrap
  where
    handler :: Exception.IOException -> Text
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
    isGenesisCompatible GenesisMode (UseBootstrapPeers{}) = False
    isGenesisCompatible _ _ = True

readTopologyFileOrError :: NodeConfiguration -> CT.Tracer IO (StartupTrace blk) -> IO NetworkTopology
readTopologyFileOrError nc tr =
      readTopologyFile nc tr
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



instance ToJSON peerAddr => ToJSON (ConnectionId peerAddr) where
  toJSON ConnectionId { localAddress, remoteAddress } =
    Aeson.object [ "localAddress"  .= toJSON localAddress
                 , "remoteAddress" .= toJSON remoteAddress
                 ]

instance Aeson.ToJSON ConnectionManagerCounters where
  toJSON ConnectionManagerCounters { fullDuplexConns
                                   , duplexConns
                                   , unidirectionalConns
                                   , inboundConns
                                   , outboundConns
                                   } =
    Aeson.object [ "fullDuplex"     .= toJSON fullDuplexConns
                 , "duplex"         .= toJSON duplexConns
                 , "unidirectional" .= toJSON unidirectionalConns
                 , "inbound"        .= inboundConns
                 , "outbound"       .= outboundConns
                 ]


instance ToJSON LocalAddress where
    toJSON (LocalAddress path) = String (pack path)

instance Aeson.ToJSONKey LocalAddress where

instance ConvertRawHash header
      => ToJSON (Point header) where
  toJSON GenesisPoint = String "GenesisPoint"
  toJSON (BlockPoint (SlotNo slotNo) hash) =
    -- it is unlikely that there will be two short hashes in the same slot
    String $ renderHeaderHashForDetails
               (Proxy @header)
                DMinimal
                hash
          <> "@"
          <> pack (show slotNo)

newtype Verbose a = Verbose a

instance ConvertRawHash header
      => ToJSON (Verbose (Point header)) where
  toJSON (Verbose GenesisPoint) = String "GenesisPoint"
  toJSON (Verbose (BlockPoint (SlotNo slotNo) hash)) =
    -- it is unlikely that there will be two short hashes in the same slot
    String $ renderHeaderHashForDetails
               (Proxy @header)
                DMaximum
                hash
          <> "@"
          <> pack (show slotNo)

instance ToJSON PeerGSV where
  toJSON PeerGSV { outboundGSV = GSV outboundG _ _
                 , inboundGSV = GSV inboundG _ _
                 } =
    Aeson.object ["G" .= (realToFrac (outboundG + inboundG) :: Double)]

instance (ToJSON peer, ToJSON point)
      => ToJSON (TraceLabelPeer peer (FetchDecision [point])) where
  toJSON (TraceLabelPeer peer decision) =
    Aeson.object
      [ "peer" .= toJSON peer
      , "decision" .= toJSON (FetchDecisionToJSON decision)
      ]

instance (ToJSON peer, ToJSON (Verbose point))
    => ToJSON (Verbose (TraceLabelPeer peer (FetchDecision [point]))) where
              toJSON (Verbose (TraceLabelPeer peer decision)) =
                Aeson.object
                [ "peer" .= toJSON peer
                , "decision" .= toJSON (FetchDecisionToJSON $ map Verbose <$> decision)
                ]

newtype FetchDecisionToJSON point =
    FetchDecisionToJSON (FetchDecision [point])

instance ToJSON point
      => ToJSON (FetchDecisionToJSON point) where
  toJSON (FetchDecisionToJSON (Left decline)) =
    Aeson.object [ "declined" .= String (pack . show $ decline) ]
  toJSON (FetchDecisionToJSON (Right points)) =
    toJSON points

instance Aeson.ToJSONKey SockAddr where

instance Aeson.ToJSON SockAddr where
    toJSON (SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        Aeson.object [ "address" .= toJSON ip
                     , "port" .= show port
                     ]
    toJSON (SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        Aeson.object [ "address" .= toJSON ip
                     , "port" .= show port
                     ]
    toJSON (SockAddrUnix path) =
        Aeson.object [ "socketPath" .= show path ]


instance Aeson.ToJSONKey DomainAccessPoint where
  toJSONKey = Aeson.toJSONKeyText render
    where
      render da = mconcat
        [ Text.decodeUtf8 (dapDomain da)
        , ":"
        , Text.pack $ show @Int (fromIntegral (dapPortNumber da))
        ]

instance ToJSON IP where
  toJSON ip = String (pack . show $ ip)

instance ToJSON KnownPeerInfo where
  toJSON (KnownPeerInfo
            nKnownPeerFailCount
            nKnownPeerTepid
            nKnownPeerSharing
            nKnownPeerAdvertise
            nKnownSuccessfulConnection
         ) =
    Aeson.object [ "kind" .= String "KnownPeerInfo"
                 , "failCount" .= nKnownPeerFailCount
                 , "tepid" .= nKnownPeerTepid
                 , "peerSharing" .= nKnownPeerSharing
                 , "peerAdvertise" .= nKnownPeerAdvertise
                 , "successfulConnection" .= nKnownSuccessfulConnection
                 ]

instance ToJSON PeerStatus where
  toJSON = String . pack . show

instance (Aeson.ToJSONKey peerAddr, ToJSON peerAddr, Ord peerAddr, Show peerAddr)
  => ToJSON (LocalRootPeers peerAddr) where
  toJSON lrp =
    Aeson.object [ "kind" .= String "LocalRootPeers"
                 , "groups" .= Aeson.toJSONList (toGroups lrp)
                 ]

instance ToJSON PeerSelectionTargets where
  toJSON (PeerSelectionTargets
            nRootLedgerPeers
            nKnownLedgerPeers
            nEstablishedLedgerPeers
            nActiveLedgerPeers
            nKnownBigLedgerPeers
            nEstablishedBigLedgerPeers
            nActiveBigLedgerPeers
         ) =
    Aeson.object [ "kind" .= String "PeerSelectionTargets"
                 , "targetRootLedgerPeers" .= nRootLedgerPeers
                 , "targetKnownLedgerPeers" .= nKnownLedgerPeers
                 , "targetEstablishedLedgerPeers" .= nEstablishedLedgerPeers
                 , "targetActiveLedgerPeers" .= nActiveLedgerPeers

                 , "targetKnownBigLedgerPeers" .= nKnownBigLedgerPeers
                 , "targetEstablishedBigLedgerPeers" .= nEstablishedBigLedgerPeers
                 , "targetActiveBigLedgerPeers" .= nActiveBigLedgerPeers
                 ]

instance ToJSON RepromoteDelay where
  toJSON = toJSON . repromoteDelay

instance ToJSON addr => ToJSON (PeerSharingResult addr) where
  toJSON (PeerSharingResult addrs) = Aeson.toJSONList addrs
  toJSON PeerSharingNotRegisteredYet = String "PeerSharingNotRegisteredYet"

-- Connection manager abstract state.  For explanation of each state see
-- <https://hydra.iohk.io/job/Cardano/ouroboros-network/native.network-docs.x86_64-linux/latest/download/2>
instance Aeson.ToJSON AbstractState where
    toJSON UnknownConnectionSt =
      Aeson.object [ "kind" .= String "UnknownConnectionSt" ]
    toJSON ReservedOutboundSt =
      Aeson.object [ "kind" .= String "ReservedOutboundSt" ]
    toJSON (UnnegotiatedSt provenance) =
      Aeson.object [ "kind" .= String "UnnegotiatedSt"
                   , "provenance" .= String (pack . show $ provenance)
                   ]
    toJSON (InboundIdleSt dataFlow) =
      Aeson.object [ "kind" .= String "InboundIdleSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON (InboundSt dataFlow) =
      Aeson.object [ "kind" .= String "InboundSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON OutboundUniSt =
      Aeson.object [ "kind" .= String "OutboundUniSt" ]
    toJSON (OutboundDupSt timeoutExpired) =
      Aeson.object [ "kind" .= String "OutboundDupSt"
                   , "timeoutState" .= String (pack . show $ timeoutExpired)
                   ]
    toJSON (OutboundIdleSt dataFlow) =
      Aeson.object [ "kind" .= String "OutboundIdleSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON DuplexSt =
      Aeson.object [ "kind" .= String "DuplexSt" ]
    toJSON WaitRemoteIdleSt =
      Aeson.object [ "kind" .= String "WaitRemoteIdleSt" ]
    toJSON TerminatingSt =
      Aeson.object [ "kind" .= String "TerminatingSt" ]
    toJSON TerminatedSt =
      Aeson.object [ "kind" .= String "TerminatedSt" ]

instance ToJSON ProtocolLimitFailure where
  toJSON (ExceededSizeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= show tok
                 ]
  toJSON (ExceededTimeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= show tok
                 ]

instance Show vNumber => ToJSON (RefuseReason vNumber) where
  toJSON (VersionMismatch vNumber tags) =
    Aeson.object [ "kind" .= String "VersionMismatch"
                 , "versionNumber" .= show vNumber
                 , "tags" .= Aeson.toJSONList tags
                 ]
  toJSON (HandshakeDecodeError vNumber t) =
    Aeson.object [ "kind" .= String "HandshakeDecodeError"
                 , "versionNumber" .= show vNumber
                 , "text" .= String (pack $ show t)
                 ]
  toJSON (Refused vNumber t) =
    Aeson.object [ "kind" .= String "Refused"
                 , "versionNumber" .= show vNumber
                 , "text" .= String (pack $ show t)
                 ]

instance Show vNumber => ToJSON (HandshakeProtocolError vNumber) where
  toJSON (HandshakeError rvNumber) =
    Aeson.object [ "kind" .= String "HandshakeError"
                 , "reason" .= toJSON rvNumber
                 ]
  toJSON (NotRecognisedVersion vNumber) =
    Aeson.object [ "kind" .= String "NotRecognisedVersion"
                 , "versionNumber" .= show vNumber
                 ]
  toJSON (InvalidServerSelection vNumber t) =
    Aeson.object [ "kind" .= String "InvalidServerSelection"
                 , "versionNumber" .= show vNumber
                 , "reason" .= String (pack $ show t)
                 ]
  toJSON QueryNotSupported =
    Aeson.object [ "kind" .= String "QueryNotSupported"
                 ]

instance Show vNumber => ToJSON (HandshakeException vNumber) where
  toJSON (HandshakeProtocolLimit plf) =
    Aeson.object [ "kind" .= String "HandshakeProtocolLimit"
                 , "handshakeProtocolLimit" .= toJSON plf
                 ]
  toJSON (HandshakeProtocolError err) =
    Aeson.object [ "kind" .= String "HandshakeProtocolError"
                 , "reason" .= show err
                 ]

instance ToJSON addr => ToJSON (LocalAddr addr) where
  toJSON (LocalAddr addr) = toJSON addr
  toJSON UnknownLocalAddr = Null

instance ToJSON DiffusionMode where
  toJSON = String . pack . show

instance ToJSON ConnStateId where
  toJSON (ConnStateId connStateId) = toJSON connStateId

instance ToJSON state => ToJSON (ConnMgr.MaybeUnknown state) where
    toJSON (ConnMgr.Known st) =
      Aeson.object
        [ "state" .= toJSON st
        , "type"  .= String "known"
        ]
    toJSON (ConnMgr.Race st) =
      Aeson.object
        [ "state" .= toJSON st
        , "type"  .= String "race"
        ]
    toJSON ConnMgr.Unknown =
      Aeson.object
        [ "type"  .= String "unknown" ]

instance ToJSON MiniProtocolNum where
  toJSON (MiniProtocolNum w) =
    Aeson.object [ "kind" .= String "MiniProtocolNum"
                 , "num" .= w
                 ]

instance ToJSON addr => ToJSON (OperationResult addr) where
  toJSON (UnsupportedState as) =
    Aeson.object [ "kind" .= String "UnsupportedState"
                 , "unsupportedState" .= toJSON as
                 ]
  toJSON (OperationSuccess addr) =
    Aeson.object [ "kind" .= String "OperationSuccess"
                 , "operationSuccess" .= toJSON addr
                 ]
  toJSON (TerminatedConnection as) =
    Aeson.object [ "kind" .= String "TerminatedConnection"
                 , "terminatedConnection" .= toJSON as
                 ]

instance ToJSON RemoteSt where
  toJSON = String . pack . show

instance ToJSON addr => Aeson.ToJSONKey (ConnectionId addr) where

instance ToJSON LedgerStateJudgement where
  toJSON YoungEnough = String "YoungEnough"
  toJSON TooOld      = String "TooOld"

instance FromJSON LedgerStateJudgement where
  parseJSON (String "YoungEnough") = pure YoungEnough
  parseJSON (String "TooOld")      = pure TooOld
  parseJSON _                      = fail "Invalid JSON for LedgerStateJudgement"

instance ToJSON AssociationMode where
  toJSON LocalRootsOnly = String "LocalRootsOnly"
  toJSON Unrestricted   = String "Unrestricted"

instance FromJSON AssociationMode where
  parseJSON (String "LocalRootsOnly") = pure LocalRootsOnly
  parseJSON (String "Unrestricted")   = pure Unrestricted
  parseJSON _                      = fail "Invalid JSON for AssociationMode"

instance ToJSON UseBootstrapPeers where
  toJSON DontUseBootstrapPeers   = Null
  toJSON (UseBootstrapPeers dps) = toJSON dps

instance FromJSON UseBootstrapPeers where
  parseJSON Null = pure DontUseBootstrapPeers
  parseJSON v    = UseBootstrapPeers <$> parseJSON v

instance ToJSON SI.Time where
  toJSON = String . pack . show

instance ToJSON peerAddr => ToJSON (PublicRootPeers.PublicRootPeers peerAddr) where
  toJSON prp =
    Aeson.object [ "kind" .= String "PublicRootPeers"
                 , "bootstrapPeers" .= PublicRootPeers.getBootstrapPeers prp
                 , "ledgerPeers" .= PublicRootPeers.getLedgerPeers prp
                 , "bigLedgerPeers" .= PublicRootPeers.getBigLedgerPeers prp
                 , "publicConfigPeers" .= Map.keysSet (PublicRootPeers.getPublicConfigPeers prp)
                 ]

