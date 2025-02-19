{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
#if __GLASGOW_HASKELL__ < 904
-- Pattern synonym record fields with GHC-8.10 is issuing the `-Wname-shadowing`
-- warning.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif

module Cardano.Node.Tracing.NetworkOrphans
  ( Verbose (..)
  , FetchDecisionToJSON (..)
  ) where
import           Cardano.Logging
import           Cardano.Node.Tracing.Render (renderHeaderHashForDetails)
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
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import           Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
                   NodeToClientVersionData (..))
import           Ouroboros.Network.PeerSelection.Bootstrap
import           Ouroboros.Network.PeerSelection.Governor (AssociationMode (..),
                   DebugPeerSelection (..), DebugPeerSelectionState (..), PeerSelectionCounters,
                   PeerSelectionState (..), PeerSelectionTargets (..), PeerSelectionView (..),
                   TracePeerSelection (..), peerSelectionStateToCounters)
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.PeerSelection.PeerTrustable
import           Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
import           Ouroboros.Network.PeerSelection.State.KnownPeers (KnownPeerInfo (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   LocalRootConfig (..), LocalRootPeers, WarmValency (..), toGroups)
import           Ouroboros.Network.PeerSelection.Types (PeerStatus (..))
import           Ouroboros.Network.Protocol.Handshake (HandshakeException (..),
                   HandshakeProtocolError (..), RefuseReason (..))
import           Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Snocket (LocalAddress (..))

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Data (Proxy (..))
import qualified Data.IP as IP
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Mux (MiniProtocolNum (..))
import           Network.Socket (SockAddr (..))


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


instance Aeson.ToJSONKey RelayAccessPoint where

instance ToJSON HotValency where
  toJSON (HotValency v) = toJSON v
instance ToJSON WarmValency where
  toJSON (WarmValency v) = toJSON v

instance FromJSON HotValency where
  parseJSON v = HotValency <$> parseJSON v

instance FromJSON WarmValency where
  parseJSON v = WarmValency <$> parseJSON v

instance ToJSON LocalRootConfig where
  toJSON LocalRootConfig { peerAdvertise,
                           peerTrustable,
                           diffusionMode } =
    Aeson.object
      [ "peerAdvertise" .= peerAdvertise
      , "peerTrustable" .= peerTrustable
      , "diffusionMode" .= show diffusionMode
      ]


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

instance ToJSON peerAddr => ToJSON (PublicRootPeers peerAddr) where
  toJSON prp =
    Aeson.object [ "kind" .= String "PublicRootPeers"
                 , "bootstrapPeers" .= PublicRootPeers.getBootstrapPeers prp
                 , "ledgerPeers" .= PublicRootPeers.getLedgerPeers prp
                 , "bigLedgerPeers" .= PublicRootPeers.getBigLedgerPeers prp
                 , "publicConfigPeers" .= Map.keysSet (PublicRootPeers.getPublicConfigPeers prp)
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

instance ToJSON NodeToNodeVersion where
  toJSON NodeToNodeV_13  = Number 13
  toJSON NodeToNodeV_14  = Number 14

instance FromJSON NodeToNodeVersion where
  parseJSON (Number 13) = return NodeToNodeV_13
  parseJSON (Number 14) = return NodeToNodeV_14
  parseJSON (Number x) = fail ("FromJSON.NodeToNodeVersion: unsupported node-to-node protocol version " ++ show x)
  parseJSON x          = fail ("FromJSON.NodeToNodeVersion: error parsing NodeToNodeVersion: " ++ show x)

instance ToJSON NodeToClientVersion where
  toJSON NodeToClientV_16 = Number 16
  toJSON NodeToClientV_17 = Number 17
  toJSON NodeToClientV_18 = Number 18
  toJSON NodeToClientV_19 = Number 19
  -- NB: When adding a new version here, update FromJSON below as well!

instance FromJSON NodeToClientVersion where
  parseJSON (Number 16) = return NodeToClientV_16
  parseJSON (Number 17) = return NodeToClientV_17
  parseJSON (Number 18) = return NodeToClientV_18
  parseJSON (Number 19) = return NodeToClientV_19
  parseJSON (Number x) = fail ("FromJSON.NodeToClientVersion: unsupported node-to-client protocol version " ++ show x)
  parseJSON x          = fail ("FromJSON.NodeToClientVersion: error parsing NodeToClientVersion: " ++ show x)

instance ToJSON NodeToNodeVersionData where
  toJSON (NodeToNodeVersionData (NetworkMagic m) dm ps q) =
    Aeson.object [ "networkMagic" .= toJSON m
                 , "diffusionMode" .= show dm
                 , "peerSharing" .= show ps
                 , "query" .= toJSON q
                 ]

instance ToJSON NodeToClientVersionData where
  toJSON (NodeToClientVersionData (NetworkMagic m) q) =
    Aeson.object [ "networkMagic" .= toJSON m
                 , "query" .= toJSON q
                 ]

instance ToJSON addr => ToJSON (LocalAddr addr) where
  toJSON (LocalAddr addr) = toJSON addr
  toJSON UnknownLocalAddr = Null

instance ToJSON NtN.DiffusionMode where
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

instance ToJSON Time where
  toJSON = String . pack . show

instance FromJSON PeerSharing where
  parseJSON = Aeson.withBool "PeerSharing" $ \b ->
    pure $ if b then PeerSharingEnabled
                else PeerSharingDisabled

instance ToJSON PeerSharing where
  toJSON PeerSharingEnabled = Bool True
  toJSON PeerSharingDisabled = Bool False

instance FromJSON UseLedgerPeers where
  parseJSON (Number slot) = return $
    case compare slot 0 of
      GT -> UseLedgerPeers (After (SlotNo (floor slot)))
      EQ -> UseLedgerPeers Always
      LT -> DontUseLedgerPeers
  parseJSON invalid = fail $ "Parsing of slot number failed due to type mismatch. "
                            <> "Encountered: " <> show invalid

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

instance ToJSON UseLedgerPeers where
  toJSON DontUseLedgerPeers                  = Number (-1)
  toJSON (UseLedgerPeers Always)             = Number 0
  toJSON (UseLedgerPeers (After (SlotNo s))) = Number (fromIntegral s)

instance ToJSON UseBootstrapPeers where
  toJSON DontUseBootstrapPeers   = Null
  toJSON (UseBootstrapPeers dps) = toJSON dps

instance FromJSON UseBootstrapPeers where
  parseJSON Null = pure DontUseBootstrapPeers
  parseJSON v    = UseBootstrapPeers <$> parseJSON v

instance FromJSON PeerTrustable where
  parseJSON = Aeson.withBool "PeerTrustable" $ \b ->
    pure $ if b then IsTrustable
                else IsNotTrustable

instance ToJSON PeerTrustable where
  toJSON IsTrustable = Bool True
  toJSON IsNotTrustable = Bool False
