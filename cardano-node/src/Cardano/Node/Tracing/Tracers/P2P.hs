{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.P2P
  (
    namesForLocalRootPeers
  , severityLocalRootPeers
  , docLocalRootPeers

  , namesForPublicRootPeers
  , severityPublicRootPeers
  , docPublicRootPeers

  , namesForPeerSelection
  , severityPeerSelection
  , docPeerSelection

  , namesForDebugPeerSelection
  , severityDebugPeerSelection
  , docDebugPeerSelection

  , namesForPeerSelectionCounters
  , severityPeerSelectionCounters
  , docPeerSelectionCounters

  , namesForPeerSelectionActions
  , severityPeerSelectionActions
  , docPeerSelectionActions

  , namesForConnectionManager
  , severityConnectionManager
  , docConnectionManager

  , namesForConnectionManagerTransition
  , severityConnectionManagerTransition
  , docConnectionManagerTransition

  , namesForServer
  , severityServer
  , docServer

  , namesForInboundGovernor
  , severityInboundGovernor
  , docInboundGovernorLocal
  , docInboundGovernorRemote

  , namesForInboundGovernorTransition
  , severityInboundGovernorTransition
  , docInboundGovernorTransition

  ) where

import           Cardano.Logging
import           Cardano.Prelude hiding (group, show)
import           Data.Aeson (ToJSON, ToJSONKey, Value (..), object, toJSON, toJSONList, (.=))
import           Data.Aeson.Types (listValue)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (pack)
import           Network.Socket (SockAddr (..))
import           Prelude (id, show)

import           Cardano.Node.Configuration.TopologyP2P ()
import           Cardano.Tracing.OrphanInstances.Network ()

import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NonP2P ()

import           Network.Mux (MiniProtocolNum (..))
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types (AbstractState (..),
                   ConnectionManagerCounters (..), ConnectionManagerTrace (..),
                   DemotedToColdRemoteTr (..), OperationResult (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..))
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.InboundGovernor.State (InboundGovernorCounters (..))
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.Governor (DebugPeerSelection (..),
                   PeerSelectionCounters (..), PeerSelectionState (..), PeerSelectionTargets (..),
                   TracePeerSelection (..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.PeerStateActions (PeerSelectionActionsTrace (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import           Ouroboros.Network.PeerSelection.RootPeersDNS (TraceLocalRootPeers (..),
                   TracePublicRootPeers (..))
import           Ouroboros.Network.PeerSelection.Types ()
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (..))
import           Ouroboros.Network.Server2 (ServerTrace (..))
import           Ouroboros.Network.Snocket (LocalAddress (..))

--------------------------------------------------------------------------------
-- LocalRootPeers Tracer
--------------------------------------------------------------------------------

namesForLocalRootPeers :: TraceLocalRootPeers ntnAddr resolverError -> [Text]
namesForLocalRootPeers TraceLocalRootDomains {} = ["LocalRootDomains"]
namesForLocalRootPeers TraceLocalRootWaiting {} = ["LocalRootWaiting"]
namesForLocalRootPeers TraceLocalRootResult {}  = ["LocalRootResult"]
namesForLocalRootPeers TraceLocalRootGroups {}  = ["LocalRootGroups"]
namesForLocalRootPeers TraceLocalRootFailure {} = ["LocalRootFailure"]
namesForLocalRootPeers TraceLocalRootError {}   = ["LocalRootError"]

severityLocalRootPeers :: TraceLocalRootPeers ntnAddr resolverError -> SeverityS
severityLocalRootPeers _ = Info

instance (ToJSONKey ntnAddr, ToJSONKey RelayAccessPoint, Show ntnAddr, Show exception) =>
    LogFormatting (TraceLocalRootPeers ntnAddr exception) where
  forMachine _dtal (TraceLocalRootDomains groups) =
    mconcat [ "kind" .= String "LocalRootDomains"
             , "localRootDomains" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootWaiting d dt) =
    mconcat [ "kind" .= String "LocalRootWaiting"
             , "domainAddress" .= toJSON d
             , "diffTime" .= show dt
             ]
  forMachine _dtal (TraceLocalRootResult d res) =
    mconcat [ "kind" .= String "LocalRootResult"
             , "domainAddress" .= toJSON d
             , "result" .= toJSONList res
             ]
  forMachine _dtal (TraceLocalRootGroups groups) =
    mconcat [ "kind" .= String "LocalRootGroups"
             , "localRootGroups" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootFailure d exception) =
    mconcat [ "kind" .= String "LocalRootFailure"
             , "domainAddress" .= toJSON d
             , "reason" .= show exception
             ]
  forMachine _dtal (TraceLocalRootError d exception) =
    mconcat [ "kind" .= String "LocalRootError"
             , "domainAddress" .= toJSON d
             , "reason" .= show exception
             ]
  forHuman = pack . show

docLocalRootPeers :: Documented (TraceLocalRootPeers ntnAddr resolverError)
docLocalRootPeers = Documented [
    DocMsg
      (TraceLocalRootDomains anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootWaiting anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootResult anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootGroups anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootFailure anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootError anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- PublicRootPeers Tracer
--------------------------------------------------------------------------------

namesForPublicRootPeers :: TracePublicRootPeers -> [Text]
namesForPublicRootPeers TracePublicRootRelayAccessPoint {} = ["PublicRootRelayAccessPoint"]
namesForPublicRootPeers TracePublicRootDomains {} = ["PublicRootDomains"]
namesForPublicRootPeers TracePublicRootResult {}  = ["PublicRootResult"]
namesForPublicRootPeers TracePublicRootFailure {}  = ["PublicRootFailure"]

severityPublicRootPeers :: TracePublicRootPeers -> SeverityS
severityPublicRootPeers _ = Info

instance LogFormatting TracePublicRootPeers where
  forMachine _dtal (TracePublicRootRelayAccessPoint relays) =
    mconcat [ "kind" .= String "PublicRootRelayAddresses"
             , "relayAddresses" .= toJSONList relays
             ]
  forMachine _dtal (TracePublicRootDomains domains) =
    mconcat [ "kind" .= String "PublicRootDomains"
             , "domainAddresses" .= toJSONList domains
             ]
  forMachine _dtal (TracePublicRootResult b res) =
    mconcat [ "kind" .= String "PublicRootResult"
             , "domain" .= show b
             , "result" .= toJSONList res
             ]
  forMachine _dtal (TracePublicRootFailure b d) =
    mconcat [ "kind" .= String "PublicRootFailure"
             , "domain" .= show b
             , "reason" .= show d
             ]
  forHuman = pack . show

docPublicRootPeers :: Documented TracePublicRootPeers
docPublicRootPeers = Documented [
    DocMsg
      (TracePublicRootRelayAccessPoint anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootDomains anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootResult anyProto anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootFailure anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- PeerSelection Tracer
--------------------------------------------------------------------------------

namesForPeerSelection :: TracePeerSelection peeraddr -> [Text]
namesForPeerSelection TraceLocalRootPeersChanged {} = ["LocalRootPeersChanged"]
namesForPeerSelection TraceTargetsChanged {}        = ["TargetsChanged"]
namesForPeerSelection TracePublicRootsRequest {}    = ["ublicRootsRequest"]
namesForPeerSelection TracePublicRootsResults {}    = ["PublicRootsResults"]
namesForPeerSelection TracePublicRootsFailure {}    = ["PublicRootsFailure"]
namesForPeerSelection TraceGossipRequests {}        = ["GossipRequests"]
namesForPeerSelection TraceGossipResults {}         = ["GossipResults"]
namesForPeerSelection TraceForgetColdPeers {}       = ["ForgetColdPeers"]
namesForPeerSelection TracePromoteColdPeers {}      = ["PromoteColdPeers"]
namesForPeerSelection TracePromoteColdLocalPeers {} = ["PromoteColdLocalPeers"]
namesForPeerSelection TracePromoteColdFailed {}     = ["PromoteColdFailed"]
namesForPeerSelection TracePromoteColdDone {}       = ["PromoteColdDone"]
namesForPeerSelection TracePromoteWarmPeers {}      = ["PromoteWarmPeers"]
namesForPeerSelection TracePromoteWarmLocalPeers {} = ["PromoteWarmLocalPeers"]
namesForPeerSelection TracePromoteWarmFailed {}     = ["PromoteWarmFailed"]
namesForPeerSelection TracePromoteWarmDone {}       = ["PromoteWarmDone"]
namesForPeerSelection TracePromoteWarmAborted {}    = ["PromoteWarmAborted"]
namesForPeerSelection TraceDemoteWarmPeers {}       = ["DemoteWarmPeers"]
namesForPeerSelection TraceDemoteWarmFailed {}      = ["DemoteWarmFailed"]
namesForPeerSelection TraceDemoteWarmDone {}        = ["DemoteWarmDone"]
namesForPeerSelection TraceDemoteHotPeers {}        = ["DemoteHotPeers"]
namesForPeerSelection TraceDemoteLocalHotPeers {}   = ["DemoteLocalHotPeers"]
namesForPeerSelection TraceDemoteHotFailed {}       = ["DemoteHotFailed"]
namesForPeerSelection TraceDemoteHotDone {}         = ["DemoteHotDone"]
namesForPeerSelection TraceDemoteAsynchronous {}    = ["DemoteAsynchronous"]
namesForPeerSelection TraceGovernorWakeup {}        = ["GovernorWakeup"]
namesForPeerSelection TraceChurnWait {}             = ["ChurnWait"]
namesForPeerSelection TraceChurnMode {}             = ["ChurnMode"]


severityPeerSelection :: TracePeerSelection peeraddr -> SeverityS
severityPeerSelection TraceLocalRootPeersChanged {} = Notice
severityPeerSelection TraceTargetsChanged        {} = Notice
severityPeerSelection TracePublicRootsRequest    {} = Info
severityPeerSelection TracePublicRootsResults    {} = Info
severityPeerSelection TracePublicRootsFailure    {} = Error
severityPeerSelection TraceGossipRequests        {} = Debug
severityPeerSelection TraceGossipResults         {} = Debug
severityPeerSelection TraceForgetColdPeers       {} = Info
severityPeerSelection TracePromoteColdPeers      {} = Info
severityPeerSelection TracePromoteColdLocalPeers {} = Info
severityPeerSelection TracePromoteColdFailed     {} = Info
severityPeerSelection TracePromoteColdDone       {} = Info
severityPeerSelection TracePromoteWarmPeers      {} = Info
severityPeerSelection TracePromoteWarmLocalPeers {} = Info
severityPeerSelection TracePromoteWarmFailed     {} = Info
severityPeerSelection TracePromoteWarmDone       {} = Info
severityPeerSelection TracePromoteWarmAborted    {} = Info
severityPeerSelection TraceDemoteWarmPeers       {} = Info
severityPeerSelection TraceDemoteWarmFailed      {} = Info
severityPeerSelection TraceDemoteWarmDone        {} = Info
severityPeerSelection TraceDemoteHotPeers        {} = Info
severityPeerSelection TraceDemoteLocalHotPeers   {} = Info
severityPeerSelection TraceDemoteHotFailed       {} = Info
severityPeerSelection TraceDemoteHotDone         {} = Info
severityPeerSelection TraceDemoteAsynchronous    {} = Info
severityPeerSelection TraceGovernorWakeup        {} = Info
severityPeerSelection TraceChurnWait             {} = Info
severityPeerSelection TraceChurnMode             {} = Info

instance LogFormatting (TracePeerSelection SockAddr) where
  forMachine _dtal (TraceLocalRootPeersChanged lrp lrp') =
    mconcat [ "kind" .= String "LocalRootPeersChanged"
             , "previous" .= toJSON lrp
             , "current" .= toJSON lrp'
             ]
  forMachine _dtal (TraceTargetsChanged pst pst') =
    mconcat [ "kind" .= String "TargetsChanged"
             , "previous" .= toJSON pst
             , "current" .= toJSON pst'
             ]
  forMachine _dtal (TracePublicRootsRequest tRootPeers nRootPeers) =
    mconcat [ "kind" .= String "PublicRootsRequest"
             , "targetNumberOfRootPeers" .= tRootPeers
             , "numberOfRootPeers" .= nRootPeers
             ]
  forMachine _dtal (TracePublicRootsResults res group dt) =
    mconcat [ "kind" .= String "PublicRootsResults"
             , "result" .= toJSONList (toList res)
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TracePublicRootsFailure err group dt) =
    mconcat [ "kind" .= String "PublicRootsFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TraceGossipRequests targetKnown actualKnown aps sps) =
    mconcat [ "kind" .= String "GossipRequests"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "availablePeers" .= toJSONList (toList aps)
             , "selectedPeers" .= toJSONList (toList sps)
             ]
  forMachine _dtal (TraceGossipResults res) =
    mconcat [ "kind" .= String "GossipResults"
             , "result" .= toJSONList (map ( bimap show id <$> ) res)
             ]
  forMachine _dtal (TraceForgetColdPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "ForgeColdPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdPeers targetKnown actualKnown sp) =
    mconcat [ "kind" .= String "PromoteColdPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdLocalPeers tLocalEst aLocalEst sp) =
    mconcat [ "kind" .= String "PromoteColdLocalPeers"
             , "targetLocalEstablished" .= tLocalEst
             , "actualLocalEstablished" .= aLocalEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdFailed tEst aEst p d err) =
    mconcat [ "kind" .= String "PromoteColdFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteColdDone tEst aEst p) =
    mconcat [ "kind" .= String "PromoteColdDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmPeers tActive aActive sp) =
    mconcat [ "kind" .= String "PromoteWarmPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmLocalPeers taa sp) =
    mconcat [ "kind" .= String "PromoteWarmLocalPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmFailed tActive aActive p err) =
    mconcat [ "kind" .= String "PromoteWarmFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteWarmDone tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmAborted tActive aActive p) =
    mconcat [ "kind" .= String "PromoteWarmAborted"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteWarmPeers tEst aEst sp) =
    mconcat [ "kind" .= String "DemoteWarmPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteWarmFailed tEst aEst p err) =
    mconcat [ "kind" .= String "DemoteWarmFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteWarmDone tEst aEst p) =
    mconcat [ "kind" .= String "DemoteWarmDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteHotPeers tActive aActive sp) =
    mconcat [ "kind" .= String "DemoteHotPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteLocalHotPeers taa sp) =
    mconcat [ "kind" .= String "DemoteLocalHotPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteHotFailed tActive aActive p err) =
    mconcat [ "kind" .= String "DemoteHotFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteHotDone tActive aActive p) =
    mconcat [ "kind" .= String "DemoteHotDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteAsynchronous"
             , "state" .= toJSON msp
             ]
  forMachine _dtal TraceGovernorWakeup =
    mconcat [ "kind" .= String "GovernorWakeup"
             ]
  forMachine _dtal (TraceChurnWait dt) =
    mconcat [ "kind" .= String "ChurnWait"
             , "diffTime" .= toJSON dt
             ]
  forMachine _dtal (TraceChurnMode c) =
    mconcat [ "kind" .= String "ChurnMode"
             , "event" .= show c ]
  forHuman = pack . show

docPeerSelection :: Documented (TracePeerSelection SockAddr)
docPeerSelection = Documented [
    DocMsg
      (TraceLocalRootPeersChanged anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceTargetsChanged anyProto anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootsRequest 1 1)
      []
      ""
  , DocMsg
      (TracePublicRootsResults anyProto 1 anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootsResults anyProto 1 anyProto)
      []
      ""
  , DocMsg
      (TraceGossipRequests 1 1 anyProto anyProto)
      []
      "target known peers, actual known peers, peers available for gossip,\
      \ peers selected for gossip"
  , DocMsg
      (TraceGossipResults [])
      []
      ""
  , DocMsg
      (TraceForgetColdPeers 1 1 anyProto)
      []
      "target known peers, actual known peers, selected peers"
  , DocMsg
      (TracePromoteColdPeers 1 1 anyProto)
      []
      "target established, actual established, selected peers"
  , DocMsg
      (TracePromoteColdLocalPeers 1 1 anyProto)
      []
      "target local established, actual local established, selected peers"
  , DocMsg
      (TracePromoteColdFailed 1 1 anyProto anyProto anyProto)
      []
      "target established, actual established, peer, delay until next\
      \ promotion, reason"
  , DocMsg
      (TracePromoteColdDone 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TracePromoteWarmPeers 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TracePromoteWarmLocalPeers [] anyProto)
      []
      "local per-group (target active, actual active), selected peers"
  , DocMsg
      (TracePromoteWarmFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TracePromoteWarmDone 1 1 anyProto)
      []
      "target active, actual active, peer"
  , DocMsg
      (TraceDemoteWarmPeers 1 1 anyProto)
      []
      "target established, actual established, selected peers"
  , DocMsg
      (TraceDemoteWarmFailed 1 1 anyProto anyProto)
      []
      "target established, actual established, peer, reason"
  , DocMsg
      (TraceDemoteWarmDone 1 1 anyProto)
      []
      "target established, actual established, peer"
  , DocMsg
      (TraceDemoteHotPeers 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TraceDemoteLocalHotPeers [] anyProto)
      []
      "local per-group (target active, actual active), selected peers"
  , DocMsg
      (TraceDemoteHotFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TraceDemoteHotFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TraceDemoteHotDone 1 1 anyProto )
      []
      "target active, actual active, peer"
  , DocMsg
      (TraceDemoteAsynchronous anyProto )
      []
      ""
  , DocMsg
      TraceGovernorWakeup
      []
      ""
  , DocMsg
      (TraceChurnWait anyProto)
      []
      ""
  , DocMsg
      (TraceChurnMode anyProto)
      []
      ""
  ]

peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers } =
    Object $
      mconcat [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               ]

--------------------------------------------------------------------------------
-- DebugPeerSelection Tracer
--------------------------------------------------------------------------------

namesForDebugPeerSelection :: DebugPeerSelection SockAddr peerConn -> [Text]
namesForDebugPeerSelection _ = ["GovernorState"]

severityDebugPeerSelection :: DebugPeerSelection SockAddr peerConn -> SeverityS
severityDebugPeerSelection _ = Debug

instance Show peerConn => LogFormatting (DebugPeerSelection SockAddr peerConn) where
  forMachine DNormal (TraceGovernorState blockedAt wakeupAfter
                   PeerSelectionState { targets, knownPeers, establishedPeers, activePeers }) =
    mconcat [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "targets" .= peerSelectionTargetsToObject targets
             , "numberOfPeers" .=
                 Object (mconcat [ "known" .= KnownPeers.size knownPeers
                                  , "established" .= EstablishedPeers.size establishedPeers
                                  , "active" .= Set.size activePeers
                                  ])
             ]
  forMachine _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mconcat [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "peerSelectionState" .= String (pack $ show ev)
             ]
  forHuman = pack . show

docDebugPeerSelection :: Documented (DebugPeerSelection SockAddr peerConn)
docDebugPeerSelection = Documented
  [  DocMsg
      (TraceGovernorState anyProto anyProto anyProto)
      []
      ""
  ]

namesForPeerSelectionCounters :: PeerSelectionCounters -> [Text]
namesForPeerSelectionCounters _ = []

severityPeerSelectionCounters :: PeerSelectionCounters -> SeverityS
severityPeerSelectionCounters _ = Info

instance LogFormatting PeerSelectionCounters where
  forMachine _dtal ev =
    mconcat [ "kind" .= String "PeerSelectionCounters"
             , "coldPeers" .= coldPeers ev
             , "warmPeers" .= warmPeers ev
             , "hotPeers" .= hotPeers ev
             ]
  forHuman = pack . show
  asMetrics PeerSelectionCounters {..} =
    [ IntM
        "cardano.node.peerSelection.cold"
        (fromIntegral coldPeers)
    , IntM
        "cardano.node.peerSelection.warm"
        (fromIntegral warmPeers)
    , IntM
        "cardano.node.peerSelection.hot"
        (fromIntegral hotPeers)
      ]

docPeerSelectionCounters :: Documented PeerSelectionCounters
docPeerSelectionCounters = Documented
  [  DocMsg
      (PeerSelectionCounters 1 1 1)
      [ ("cardano.node.peerSelection.cold", "Number of cold peers")
      , ("cardano.node.peerSelection.warm", "Number of warm peers")
      , ("cardano.node.peerSelection.hot", "Number of hot peers") ]
      "Counters for cold, warm and hot peers"
  ]

--------------------------------------------------------------------------------
-- PeerSelectionActions Tracer
--------------------------------------------------------------------------------

namesForPeerSelectionActions :: PeerSelectionActionsTrace ntnAddr -> [Text]
namesForPeerSelectionActions PeerStatusChanged   {}     = ["StatusChanged"]
namesForPeerSelectionActions PeerStatusChangeFailure {} = ["StatusChangeFailure"]
namesForPeerSelectionActions PeerMonitoringError {}     = ["MonitoringError"]
namesForPeerSelectionActions PeerMonitoringResult {}    = ["MonitoringResult"]

severityPeerSelectionActions :: PeerSelectionActionsTrace ntnAddr -> SeverityS
severityPeerSelectionActions PeerStatusChanged {}       = Info
severityPeerSelectionActions PeerStatusChangeFailure {} = Error
severityPeerSelectionActions PeerMonitoringError {}     = Error
severityPeerSelectionActions PeerMonitoringResult {}    = Debug

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance LogFormatting (PeerSelectionActionsTrace SockAddr) where
  forMachine _dtal (PeerStatusChanged ps) =
    mconcat [ "kind" .= String "PeerStatusChanged"
             , "peerStatusChangeType" .= show ps
             ]
  forMachine _dtal (PeerStatusChangeFailure ps f) =
    mconcat [ "kind" .= String "PeerStatusChangeFailure"
             , "peerStatusChangeType" .= show ps
             , "reason" .= show f
             ]
  forMachine _dtal (PeerMonitoringError connId s) =
    mconcat [ "kind" .= String "PeerMonitoridngError"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  forMachine _dtal (PeerMonitoringResult connId wf) =
    mconcat [ "kind" .= String "PeerMonitoringResult"
             , "connectionId" .= toJSON connId
             , "withProtocolTemp" .= show wf
             ]
  forHuman = pack . show

docPeerSelectionActions :: Documented (PeerSelectionActionsTrace ntnAddr)
docPeerSelectionActions = Documented
  [  DocMsg
      (PeerStatusChanged anyProto)
      []
      ""
  ,  DocMsg
      (PeerStatusChangeFailure anyProto anyProto)
      []
      ""
  ,  DocMsg
      (PeerMonitoringError anyProto anyProto)
      []
      ""
  ,  DocMsg
      (PeerMonitoringResult anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- Connection Manager Tracer
--------------------------------------------------------------------------------

namesForConnectionManager :: ConnectionManagerTrace ntnAddr cht -> [Text]
namesForConnectionManager TrIncludeConnection {}  = ["IncludeConnection"]
namesForConnectionManager TrUnregisterConnection {} = ["UnregisterConnection"]
namesForConnectionManager TrConnect {}  = ["Connect"]
namesForConnectionManager TrConnectError {} = ["ConnectError"]
namesForConnectionManager TrTerminatingConnection {} = ["TerminatingConnection"]
namesForConnectionManager TrTerminatedConnection {} = ["TerminatedConnection"]
namesForConnectionManager TrConnectionHandler {} = ["ConnectionHandler"]
namesForConnectionManager TrShutdown {} = ["Shutdown"]
namesForConnectionManager TrConnectionExists {} = ["ConnectionExists"]
namesForConnectionManager TrForbiddenConnection {} = ["ForbiddenConnection"]
namesForConnectionManager TrImpossibleConnection {} = ["ImpossibleConnection"]
namesForConnectionManager TrConnectionFailure {} = ["ConnectionFailure"]
namesForConnectionManager TrConnectionNotFound {} = ["ConnectionNotFound"]
namesForConnectionManager TrForbiddenOperation {} = ["ForbiddenOperation"]
namesForConnectionManager TrPruneConnections {}  = ["PruneConnections"]
namesForConnectionManager TrConnectionCleanup {} = ["ConnectionCleanup"]
namesForConnectionManager TrConnectionTimeWait {} = ["ConnectionTimeWait"]
namesForConnectionManager TrConnectionTimeWaitDone {} = ["ConnectionTimeWaitDone"]
namesForConnectionManager TrConnectionManagerCounters {} = ["ConnectionManagerCounters"]
namesForConnectionManager TrState {} = ["State"]
namesForConnectionManager ConnectionManager.TrUnexpectedlyFalseAssertion {} =
                            ["UnexpectedlyFalseAssertion"]
namesForConnectionManager TrUnknownConnection {} = ["UnknownConnection"]

severityConnectionManager ::
  ConnectionManagerTrace addr
    (ConnectionHandlerTrace versionNumber agreedOptions) -> SeverityS
severityConnectionManager TrIncludeConnection {}                  = Debug
severityConnectionManager TrUnregisterConnection {}               = Debug
severityConnectionManager TrConnect {}                            = Debug
severityConnectionManager TrConnectError {}                       = Info
severityConnectionManager TrTerminatingConnection {}              = Debug
severityConnectionManager TrTerminatedConnection {}               = Debug
severityConnectionManager (TrConnectionHandler _ ev')             =
        case ev' of
          TrHandshakeSuccess {}     -> Info
          TrHandshakeClientError {} -> Notice
          TrHandshakeServerError {} -> Info
          TrError _ _ ShutdownNode  -> Critical
          TrError _ _ ShutdownPeer  -> Info

severityConnectionManager TrShutdown                              = Info
severityConnectionManager TrConnectionExists {}                   = Info
severityConnectionManager TrForbiddenConnection {}                = Info
severityConnectionManager TrImpossibleConnection {}               = Info
severityConnectionManager TrConnectionFailure {}                  = Info
severityConnectionManager TrConnectionNotFound {}                 = Debug
severityConnectionManager TrForbiddenOperation {}                 = Info

severityConnectionManager TrPruneConnections {}                   = Notice
severityConnectionManager TrConnectionCleanup {}                  = Debug
severityConnectionManager TrConnectionTimeWait {}                 = Debug
severityConnectionManager TrConnectionTimeWaitDone {}             = Debug
severityConnectionManager TrConnectionManagerCounters {}          = Info
severityConnectionManager TrState {}                              = Info
severityConnectionManager ConnectionManager.TrUnexpectedlyFalseAssertion {} =
                            Error
severityConnectionManager TrUnknownConnection {}                  = Debug

instance (Show addr, Show versionNumber, Show agreedOptions, LogFormatting addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions)
      => LogFormatting (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
    forMachine dtal (TrIncludeConnection prov peerAddr) =
        mconcat $ reverse
          [ "kind" .= String "IncludeConnection"
          , "remoteAddress" .= forMachine dtal peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine dtal (TrUnregisterConnection prov peerAddr) =
        mconcat $ reverse
          [ "kind" .= String "UnregisterConnection"
          , "remoteAddress" .= forMachine dtal peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine _dtal (TrConnect (Just localAddress) remoteAddress) =
        mconcat
          [ "kind" .= String "ConnectTo"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          ]
    forMachine dtal (TrConnect Nothing remoteAddress) =
        mconcat
          [ "kind" .= String "ConnectTo"
          , "remoteAddress" .= forMachine dtal remoteAddress
          ]
    forMachine _dtal (TrConnectError (Just localAddress) remoteAddress err) =
        mconcat
          [ "kind" .= String "ConnectError"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          , "reason" .= String (pack . show $ err)
          ]
    forMachine dtal (TrConnectError Nothing remoteAddress err) =
        mconcat
          [ "kind" .= String "ConnectError"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "reason" .= String (pack . show $ err)
          ]
    forMachine _dtal (TrTerminatingConnection prov connId) =
        mconcat
          [ "kind" .= String "TerminatingConnection"
          , "provenance" .= String (pack . show $ prov)
          , "connectionId" .= toJSON connId
          ]
    forMachine dtal (TrTerminatedConnection prov remoteAddress) =
        mconcat
          [ "kind" .= String "TerminatedConnection"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= forMachine dtal remoteAddress
          ]
    forMachine dtal (TrConnectionHandler connId a) =
        mconcat
          [ "kind" .= String "ConnectionHandler"
          , "connectionId" .= toJSON connId
          , "connectionHandler" .= forMachine dtal a
          ]
    forMachine _dtal TrShutdown =
        mconcat
          [ "kind" .= String "Shutdown"
          ]
    forMachine dtal (TrConnectionExists prov remoteAddress inState) =
        mconcat
          [ "kind" .= String "ConnectionExists"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "state" .= toJSON inState
          ]
    forMachine _dtal (TrForbiddenConnection connId) =
        mconcat
          [ "kind" .= String "ForbiddenConnection"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrImpossibleConnection connId) =
        mconcat
          [ "kind" .= String "ImpossibleConnection"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionFailure connId) =
        mconcat
          [ "kind" .= String "ConnectionFailure"
          , "connectionId" .= toJSON connId
          ]
    forMachine dtal (TrConnectionNotFound prov remoteAddress) =
        mconcat
          [ "kind" .= String "ConnectionNotFound"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine dtal (TrForbiddenOperation remoteAddress connState) =
        mconcat
          [ "kind" .= String "ForbiddenOperation"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "connectionState" .= toJSON connState
          ]
    forMachine dtal (TrPruneConnections pruningSet numberPruned chosenPeers) =
        mconcat
          [ "kind" .= String "PruneConnections"
          , "prunedPeers" .= toJSON pruningSet
          , "numberPrunedPeers" .= toJSON numberPruned
          , "choiceSet" .= toJSON (forMachine dtal `Set.map` chosenPeers)
          ]
    forMachine _dtal (TrConnectionCleanup connId) =
        mconcat
          [ "kind" .= String "ConnectionCleanup"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionTimeWait connId) =
        mconcat
          [ "kind" .= String "ConnectionTimeWait"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionTimeWaitDone connId) =
        mconcat
          [ "kind" .= String "ConnectionTimeWaitDone"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionManagerCounters cmCounters) =
        mconcat
          [ "kind"  .= String "ConnectionManagerCounters"
          , "state" .= toJSON cmCounters
          ]
    forMachine _dtal (TrState cmState) =
        mconcat
          [ "kind"  .= String "ConnectionManagerState"
          , "state" .= listValue (\(addr, connState) ->
                                         object
                                           [ "remoteAddress"   .= toJSON addr
                                           , "connectionState" .= toJSON connState
                                           ])
                                       (Map.toList cmState)
          ]
    forMachine _dtal (ConnectionManager.TrUnexpectedlyFalseAssertion info) =
        mconcat
          [ "kind" .= String "UnexpectedlyFalseAssertion"
          , "info" .= String (pack . show $ info)
          ]
    forMachine _dtal (TrUnknownConnection info) =
      mconcat
        [ "kind" .= String "UnknownConnection"
        , "info" .= String (pack . show $ info)
        ]
    forHuman = pack . show
    asMetrics (TrConnectionManagerCounters ConnectionManagerCounters {..}) =
          [ IntM
              "cardano.node.connectionManager.fullDuplexConns"
              (fromIntegral fullDuplexConns)
          , IntM
              "cardano.node.connectionManager.duplexConns"
              (fromIntegral duplexConns)
          , IntM
              "cardano.node.connectionManager.unidirectionalConns"
              (fromIntegral unidirectionalConns)
          , IntM
              "cardano.node.connectionManager.inboundConns"
              (fromIntegral inboundConns)
          , IntM
              "cardano.node.connectionManager.outboundConns"
              (fromIntegral outboundConns)
            ]
    asMetrics _ = []

instance (Show versionNumber, ToJSON versionNumber, ToJSON agreedOptions)
  => LogFormatting (ConnectionHandlerTrace versionNumber agreedOptions) where
  forMachine _dtal (TrHandshakeSuccess versionNumber agreedOptions) =
    mconcat
      [ "kind" .= String "HandshakeSuccess"
      , "versionNumber" .= toJSON versionNumber
      , "agreedOptions" .= toJSON agreedOptions
      ]
  forMachine _dtal (TrHandshakeClientError err) =
    mconcat
      [ "kind" .= String "HandshakeClientError"
      , "reason" .= toJSON err
      ]
  forMachine _dtal (TrHandshakeServerError err) =
    mconcat
      [ "kind" .= String "HandshakeServerError"
      , "reason" .= toJSON err
      ]
  forMachine _dtal (TrError e err cerr) =
    mconcat
      [ "kind" .= String "Error"
      , "context" .= show e
      , "reason" .= show err
      , "command" .= show cerr
      ]

protoConnectionHandlerTrace :: ConnectionHandlerTrace
  ntnVersion
  ntnVersionData
protoConnectionHandlerTrace = TrHandshakeSuccess anyProto anyProto

docConnectionManager :: Documented
  (ConnectionManagerTrace
    ntnAddr
    (ConnectionHandlerTrace
      ntnVersion
      ntnVersionData))
docConnectionManager = Documented
  [  DocMsg
      (TrIncludeConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrUnregisterConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrConnect Nothing anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectError Nothing anyProto protoSomeException)
      []
      ""
  ,  DocMsg
      (TrTerminatingConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrTerminatedConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionHandler anyProto protoConnectionHandlerTrace)
      []
      ""
  ,  DocMsg
      TrShutdown
      []
      ""
  ,  DocMsg
      (TrConnectionExists anyProto anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrForbiddenConnection anyProto)
      []
      ""
  ,  DocMsg
      (TrImpossibleConnection anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionFailure anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionNotFound anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrForbiddenOperation anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrPruneConnections anyProto anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionCleanup anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionTimeWait anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionTimeWaitDone anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionManagerCounters anyProto)
      [("cardano.node.connectionManager.fullDuplexConns","")
      ,("cardano.node.connectionManager.duplexConns","")
      ,("cardano.node.connectionManager.unidirectionalConns","")
      ,("cardano.node.connectionManager.inboundConns","")
      ,("cardano.node.connectionManager.outboundConns","")
      ]
      ""
  ,  DocMsg
      (TrState anyProto)
      []
      ""
  ,  DocMsg
      (ConnectionManager.TrUnexpectedlyFalseAssertion anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- Connection Manager Transition Tracer
--------------------------------------------------------------------------------

namesForConnectionManagerTransition
    :: ConnectionManager.AbstractTransitionTrace peerAddr -> [Text]
namesForConnectionManagerTransition ConnectionManager.TransitionTrace {} =
    ["ConnectionManagerTransition" ]

severityConnectionManagerTransition
  :: ConnectionManager.AbstractTransitionTrace peerAddr -> SeverityS
severityConnectionManagerTransition _ = Debug

instance (Show peerAddr, ToJSON peerAddr)
      => LogFormatting (ConnectionManager.AbstractTransitionTrace peerAddr) where
    forMachine _dtal (ConnectionManager.TransitionTrace peerAddr tr) =
      mconcat $ reverse
        [ "kind"    .= String "ConnectionManagerTransition"
        , "address" .= toJSON peerAddr
        , "from"    .= toJSON (ConnectionManager.fromState tr)
        , "to"      .= toJSON (ConnectionManager.toState   tr)
        ]
    forHuman = pack . show
    asMetrics _ = []

docConnectionManagerTransition
    :: Documented (ConnectionManager.AbstractTransitionTrace peerAddr)
docConnectionManagerTransition = Documented
  [ DocMsg
      (ConnectionManager.TransitionTrace anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- Server Tracer
--------------------------------------------------------------------------------

namesForServer :: ServerTrace ntnAddr -> [Text]
namesForServer TrAcceptConnection {}  = ["AcceptConnection"]
namesForServer TrAcceptError {}       = ["AcceptError"]
namesForServer TrAcceptPolicyTrace {} = ["AcceptPolicy"]
namesForServer TrServerStarted {}     = ["Started"]
namesForServer TrServerStopped {}     = ["Stopped"]
namesForServer TrServerError {}       = ["Error"]

severityServer ::  ServerTrace ntnAddr -> SeverityS
severityServer TrAcceptConnection {}  = Debug
severityServer TrAcceptError {}       = Error
severityServer TrAcceptPolicyTrace {} = Notice
severityServer TrServerStarted {}     = Notice
severityServer TrServerStopped {}     = Notice
severityServer TrServerError {}       = Critical

instance (Show addr, LogFormatting addr, ToJSON addr)
      => LogFormatting (ServerTrace addr) where
  forMachine dtal (TrAcceptConnection peerAddr)     =
    mconcat [ "kind" .= String "AcceptConnection"
             , "address" .= forMachine dtal peerAddr
             ]
  forMachine _dtal (TrAcceptError exception)         =
    mconcat [ "kind" .= String "AcceptErroor"
             , "reason" .= show exception
             ]
  forMachine dtal (TrAcceptPolicyTrace policyTrace) =
    mconcat [ "kind" .= String "AcceptPolicyTrace"
             , "policy" .= forMachine dtal policyTrace
             ]
  forMachine dtal (TrServerStarted peerAddrs)       =
    mconcat [ "kind" .= String "AcceptPolicyTrace"
             , "addresses" .= toJSON (forMachine dtal `map` peerAddrs)
             ]
  forMachine _dtal TrServerStopped                   =
    mconcat [ "kind" .= String "ServerStopped"
             ]
  forMachine _dtal (TrServerError exception)         =
    mconcat [ "kind" .= String "ServerError"
             , "reason" .= show exception
             ]
  forHuman = pack . show

docServer :: Documented (ServerTrace ntnAddr)
docServer = Documented
  [  DocMsg
      (TrAcceptConnection anyProto)
      []
      ""
  ,  DocMsg
      (TrAcceptError anyProto)
      []
      ""
  ,  DocMsg
      (TrAcceptPolicyTrace anyProto)
      []
      ""
  ,  DocMsg
      (TrServerStarted anyProto)
      []
      ""
  ,  DocMsg
      TrServerStopped
      []
      ""
  ,  DocMsg
      (TrServerError anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- InboundGovernor Tracer
--------------------------------------------------------------------------------

namesForInboundGovernor :: InboundGovernorTrace peerAddr -> [Text]
namesForInboundGovernor TrNewConnection {}         = ["NewConnection"]
namesForInboundGovernor TrResponderRestarted {}    = ["ResponderRestarted"]
namesForInboundGovernor TrResponderStartFailure {} = ["ResponderStartFailure"]
namesForInboundGovernor TrResponderErrored {}      = ["ResponderErrored"]
namesForInboundGovernor TrResponderStarted {}      = ["ResponderStarted"]
namesForInboundGovernor TrResponderTerminated {}   = ["ResponderTerminated"]
namesForInboundGovernor TrPromotedToWarmRemote {}  = ["PromotedToWarmRemote"]
namesForInboundGovernor TrPromotedToHotRemote {}   = ["PromotedToHotRemote"]
namesForInboundGovernor TrDemotedToColdRemote {}   = ["DemotedToColdRemote"]
namesForInboundGovernor TrDemotedToWarmRemote {}   = ["DemotedToWarmRemote"]
namesForInboundGovernor TrWaitIdleRemote {}        = ["WaitIdleRemote"]
namesForInboundGovernor TrMuxCleanExit {}          = ["MuxCleanExit"]
namesForInboundGovernor TrMuxErrored {}            = ["MuxErrored"]
namesForInboundGovernor TrInboundGovernorCounters {} = ["InboundGovernorCounters"]
namesForInboundGovernor TrRemoteState {}           = ["RemoteState"]
namesForInboundGovernor InboundGovernor.TrUnexpectedlyFalseAssertion {} =
                            ["UnexpectedlyFalseAssertion"]
namesForInboundGovernor InboundGovernor.TrInboundGovernorError {} =
                            ["InboundGovernorError"]

severityInboundGovernor :: InboundGovernorTrace peerAddr -> SeverityS
severityInboundGovernor TrNewConnection {}                              = Debug
severityInboundGovernor TrResponderRestarted {}                         = Debug
severityInboundGovernor TrResponderStartFailure {}                      = Error
severityInboundGovernor TrResponderErrored {}                           = Info
severityInboundGovernor TrResponderStarted {}                           = Debug
severityInboundGovernor TrResponderTerminated {}                        = Debug
severityInboundGovernor TrPromotedToWarmRemote {}                       = Info
severityInboundGovernor TrPromotedToHotRemote {}                        = Info
severityInboundGovernor TrDemotedToColdRemote {}                        = Info
severityInboundGovernor TrDemotedToWarmRemote {}                        = Info
severityInboundGovernor TrWaitIdleRemote {}                             = Debug
severityInboundGovernor TrMuxCleanExit {}                               = Debug
severityInboundGovernor TrMuxErrored {}                                 = Info
severityInboundGovernor TrInboundGovernorCounters {}                    = Info
severityInboundGovernor TrRemoteState {}                                = Debug
severityInboundGovernor InboundGovernor.TrUnexpectedlyFalseAssertion {} = Error
severityInboundGovernor InboundGovernor.TrInboundGovernorError {}       = Error

instance (ToJSON addr, Show addr)
      => LogFormatting (InboundGovernorTrace addr) where
  forMachine _dtal (TrNewConnection p connId)            =
    mconcat [ "kind" .= String "NewConnection"
             , "provenance" .= show p
             , "connectionId" .= toJSON connId
             ]
  forMachine _dtal (TrResponderRestarted connId m)       =
    mconcat [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  forMachine _dtal (TrResponderStartFailure connId m s)  =
    mconcat [ "kind" .= String "ResponderStartFailure"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  forMachine _dtal (TrResponderErrored connId m s)       =
    mconcat [ "kind" .= String "ResponderErrored"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  forMachine _dtal (TrResponderStarted connId m)         =
    mconcat [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  forMachine _dtal (TrResponderTerminated connId m)      =
    mconcat [ "kind" .= String "ResponderTerminated"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  forMachine _dtal (TrPromotedToWarmRemote connId opRes) =
    mconcat [ "kind" .= String "PromotedToWarmRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  forMachine _dtal (TrPromotedToHotRemote connId)        =
    mconcat [ "kind" .= String "PromotedToHotRemote"
             , "connectionId" .= toJSON connId
             ]
  forMachine _dtal (TrDemotedToColdRemote connId od)     =
    mconcat [ "kind" .= String "DemotedToColdRemote"
             , "connectionId" .= toJSON connId
             , "result" .= show od
             ]
  forMachine _dtal (TrDemotedToWarmRemote connId)     =
    mconcat [ "kind" .= String "DemotedToWarmRemote"
             , "connectionId" .= toJSON connId
             ]
  forMachine _dtal (TrWaitIdleRemote connId opRes) =
    mconcat [ "kind" .= String "WaitIdleRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  forMachine _dtal (TrMuxCleanExit connId)               =
    mconcat [ "kind" .= String "MuxCleanExit"
             , "connectionId" .= toJSON connId
             ]
  forMachine _dtal (TrMuxErrored connId s)               =
    mconcat [ "kind" .= String "MuxErrored"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  forMachine _dtal (TrInboundGovernorCounters counters) =
    mconcat [ "kind" .= String "InboundGovernorCounters"
             , "idlePeers" .= idlePeersRemote counters
             , "coldPeers" .= coldPeersRemote counters
             , "warmPeers" .= warmPeersRemote counters
             , "hotPeers" .= hotPeersRemote counters
             ]
  forMachine _dtal (TrRemoteState st) =
    mconcat [ "kind" .= String "RemoteState"
             , "remoteSt" .= toJSON st
             ]
  forMachine _dtal (InboundGovernor.TrUnexpectedlyFalseAssertion info) =
    mconcat [ "kind" .= String "UnexpectedlyFalseAssertion"
             , "remoteSt" .= String (pack . show $ info)
             ]
  forMachine _dtal (InboundGovernor.TrInboundGovernorError err) =
    mconcat [ "kind" .= String "InboundGovernorError"
             , "remoteSt" .= String (pack . show $ err)
             ]
  forHuman = pack . show
  asMetrics (TrInboundGovernorCounters InboundGovernorCounters {..}) =
            [ IntM
                "cardano.node.inbound-governor.warm"
                (fromIntegral warmPeersRemote)
            , IntM
                "cardano.node.inbound-governor.hot"
                (fromIntegral hotPeersRemote)
              ]
  asMetrics _ = []

protoProvenance :: ConnectionManager.Provenance
protoProvenance = ConnectionManager.Inbound

protoConnectionId :: peerAddr -> ConnectionId peerAddr
protoConnectionId pa = ConnectionId pa pa

protoMiniProtocolNum :: MiniProtocolNum
protoMiniProtocolNum = MiniProtocolNum 1

protoSomeException :: SomeException
protoSomeException = SomeException (AssertionFailed "just fooled")

protoAbstractState :: AbstractState
protoAbstractState = UnknownConnectionSt

protoOperationResult :: a -> OperationResult a
protoOperationResult = OperationSuccess

protoDemotedToColdRemoteTr :: DemotedToColdRemoteTr
protoDemotedToColdRemoteTr = CommitTr

protoInboundGovernorCounters :: InboundGovernorCounters
protoInboundGovernorCounters = InboundGovernorCounters 1 1 1 1

protoRemoteAddr :: SockAddr
protoRemoteAddr = SockAddrUnix "loopback"

protoLocalAddress :: LocalAddress
protoLocalAddress = LocalAddress "loopback"

-- protoIGAssertionLocation :: peerAddr -> IGAssertionLocation peerAddr
-- protoIGAssertionLocation pa = InboundGovernorLoop Nothing protoAbstractState


-- Not possible to prvide such prototype,
-- as type and constructor are not exported
-- protoIGAssertionLocation :: IGAssertionLocation peerAddr
-- protoIGAssertionLocation = InboundGovernorLoop Nothing protoAbstractState

docInboundGovernorLocal ::
   Documented (InboundGovernorTrace LocalAddress)
docInboundGovernorLocal = docInboundGovernor protoLocalAddress

docInboundGovernorRemote ::
   Documented (InboundGovernorTrace SockAddr)
docInboundGovernorRemote = docInboundGovernor protoRemoteAddr

docInboundGovernor :: peerAddr -> Documented (InboundGovernorTrace peerAddr)
docInboundGovernor peerAddr = Documented
  [  DocMsg
      (TrNewConnection protoProvenance (protoConnectionId peerAddr))
      []
      ""
  ,  DocMsg
      (TrResponderRestarted (protoConnectionId peerAddr) protoMiniProtocolNum)
      []
      ""
  ,  DocMsg
      (TrResponderStartFailure (protoConnectionId peerAddr) protoMiniProtocolNum
        protoSomeException)
      []
      ""
  ,  DocMsg
      (TrResponderErrored (protoConnectionId peerAddr) protoMiniProtocolNum
        protoSomeException)
      []
      ""
  ,  DocMsg
      (TrResponderStarted  (protoConnectionId peerAddr) protoMiniProtocolNum)
      []
      ""
  ,  DocMsg
      (TrResponderTerminated  (protoConnectionId peerAddr) protoMiniProtocolNum)
      []
      ""
  ,  DocMsg
      (TrPromotedToWarmRemote (protoConnectionId peerAddr)
        (protoOperationResult protoAbstractState))
      []
      ""
  ,  DocMsg
      (TrPromotedToHotRemote (protoConnectionId peerAddr))
      []
      ""
  ,  DocMsg
      (TrDemotedToColdRemote (protoConnectionId peerAddr)
        (protoOperationResult protoDemotedToColdRemoteTr))
      []
      "All mini-protocols terminated.  The boolean is true if this connection\
      \ was not used by p2p-governor, and thus the connection will be terminated."
  ,  DocMsg
      (TrDemotedToWarmRemote (protoConnectionId peerAddr))
      []
      "All mini-protocols terminated.  The boolean is true if this connection\
      \ was not used by p2p-governor, and thus the connection will be terminated."
  ,  DocMsg
      (TrWaitIdleRemote (protoConnectionId peerAddr)
        (protoOperationResult protoAbstractState))
      []
      ""
  ,  DocMsg
      (TrMuxCleanExit (protoConnectionId peerAddr))
      []
      ""
  ,  DocMsg
      (TrMuxErrored (protoConnectionId peerAddr) protoSomeException)
      []
      ""
  ,  DocMsg
      (TrInboundGovernorCounters protoInboundGovernorCounters)
      []
      ""
  ,  DocMsg
      (TrRemoteState Map.empty)
      []
      ""
  -- ,  DocMsg
  --     (InboundGovernor.TrUnexpectedlyFalseAssertion protoIGAssertionLocation)
  --     []
  --     ""
  -- ,  DocMsg
  --     (InboundGovernor.TrInboundGovernorError protoSomeException)
  --     []
  --     ""
  ]

--------------------------------------------------------------------------------
-- InboundGovernor Transition Tracer
--------------------------------------------------------------------------------

namesForInboundGovernorTransition
  :: InboundGovernor.RemoteTransitionTrace peerAddr -> [Text]
namesForInboundGovernorTransition _ = ["InboundGovernorTransition"]

severityInboundGovernorTransition
  :: InboundGovernor.RemoteTransitionTrace peerAddr -> SeverityS
severityInboundGovernorTransition _ = Debug

instance (Show peerAddr, ToJSON peerAddr)
      => LogFormatting (InboundGovernor.RemoteTransitionTrace peerAddr) where
    forMachine _dtal (InboundGovernor.TransitionTrace peerAddr tr) =
      mconcat $ reverse
        [ "kind"    .= String "ConnectionManagerTransition"
        , "address" .= toJSON peerAddr
        , "from"    .= toJSON (ConnectionManager.fromState tr)
        , "to"      .= toJSON (ConnectionManager.toState   tr)
        ]
    forHuman = pack . show
    asMetrics _ = []

docInboundGovernorTransition
  :: Documented (InboundGovernor.RemoteTransitionTrace peerAddr)
docInboundGovernorTransition = Documented
  [ DocMsg
      anyProto
      []
      ""
  ]
