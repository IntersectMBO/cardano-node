{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.P2P
  () where

import           Cardano.Logging
import           Cardano.Prelude hiding (group, show)
import           Data.Aeson (Object, ToJSON, ToJSONKey, Value (..), object, toJSON, toJSONList,
                   (.=))
import           Data.Aeson.Types (listValue)
import           Data.List (last)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.List (last)
import           Data.Text (pack)
import           Network.Socket (SockAddr (..))
import           Prelude (show)

import           Cardano.Node.Configuration.TopologyP2P ()
import           Cardano.Tracing.OrphanInstances.Network ()

import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Node.Tracing.Tracers.NonP2P ()

import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types (ConnectionManagerCounters (..),
                   ConnectionManagerTrace (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..))
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.InboundGovernor.State (InboundGovernorCounters (..))
import qualified Ouroboros.Network.NodeToNode as NtN
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

instance MetaTrace (TraceLocalRootPeers ntnAddr exception) where
  namespaceFor TraceLocalRootDomains {} = Namespace [] ["LocalRootDomains"]
  namespaceFor TraceLocalRootWaiting {} = Namespace [] ["LocalRootWaiting"]
  namespaceFor TraceLocalRootResult {} = Namespace [] ["LocalRootResult"]
  namespaceFor TraceLocalRootGroups {} = Namespace [] ["LocalRootGroups"]
  namespaceFor TraceLocalRootFailure {} = Namespace [] ["LocalRootFailure"]
  namespaceFor TraceLocalRootError {} = Namespace [] ["LocalRootError"]

  severityFor (Namespace [] ["LocalRootDomains"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootWaiting"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootResult"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootGroups"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootFailure"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootError"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace [] ["LocalRootDomains"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootWaiting"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootResult"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootGroups"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootFailure"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootError"]) = Just
    ""
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["LocalRootDomains"]
    , Namespace [] ["LocalRootWaiting"]
    , Namespace [] ["LocalRootResult"]
    , Namespace [] ["LocalRootGroups"]
    , Namespace [] ["LocalRootFailure"]
    , Namespace [] ["LocalRootError"]
    ]

--------------------------------------------------------------------------------
-- PublicRootPeers Tracer
--------------------------------------------------------------------------------

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

instance MetaTrace TracePublicRootPeers where
  namespaceFor TracePublicRootRelayAccessPoint {} = Namespace [] ["PublicRootRelayAccessPoint"]
  namespaceFor TracePublicRootDomains {} = Namespace [] ["PublicRootDomains"]
  namespaceFor TracePublicRootResult {} = Namespace [] ["PublicRootResult"]
  namespaceFor TracePublicRootFailure {} = Namespace [] ["PublicRootFailure"]

  severityFor (Namespace [] ["PublicRootRelayAccessPoint"]) _ = Just Info
  severityFor (Namespace [] ["PublicRootDomains"]) _ = Just Info
  severityFor (Namespace [] ["PublicRootResult"]) _ = Just Info
  severityFor (Namespace [] ["PublicRootFailure"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace [] ["PublicRootRelayAccessPoint"]) = Just
    ""
  documentFor (Namespace [] ["PublicRootDomains"]) = Just
    ""
  documentFor (Namespace [] ["PublicRootResult"]) = Just
    ""
  documentFor (Namespace [] ["PublicRootFailure"]) = Just
    ""
  documentFor _ = Nothing

  allNamespaces = [
      Namespace [] ["PublicRootRelayAccessPoint"]
    , Namespace [] ["PublicRootDomains"]
    , Namespace [] ["PublicRootResult"]
    , Namespace [] ["PublicRootFailure"]
    ]

--------------------------------------------------------------------------------
-- PeerSelection Tracer
--------------------------------------------------------------------------------

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
             , "result" .= toJSONList (map (first show <$>) res)
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
  forMachine _dtal (TraceDemoteLocalAsynchronous msp) =
    mconcat [ "kind" .= String "DemoteLocalAsynchronous"
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

instance MetaTrace (TracePeerSelection SockAddr) where
    namespaceFor TraceLocalRootPeersChanged {} =
      Namespace [] ["LocalRootPeersChanged"]
    namespaceFor TraceTargetsChanged {}        =
      Namespace [] ["TargetsChanged"]
    namespaceFor TracePublicRootsRequest {}    =
      Namespace [] ["PublicRootsRequest"]
    namespaceFor TracePublicRootsResults {}    =
      Namespace [] ["PublicRootsResults"]
    namespaceFor TracePublicRootsFailure {}    =
      Namespace [] ["PublicRootsFailure"]
    namespaceFor TraceGossipRequests {}        =
      Namespace [] ["GossipRequests"]
    namespaceFor TraceGossipResults {}         =
      Namespace [] ["GossipResults"]
    namespaceFor TraceForgetColdPeers {}       =
      Namespace [] ["ForgetColdPeers"]
    namespaceFor TracePromoteColdPeers {}      =
      Namespace [] ["PromoteColdPeers"]
    namespaceFor TracePromoteColdLocalPeers {} =
      Namespace [] ["PromoteColdLocalPeers"]
    namespaceFor TracePromoteColdFailed {}     =
      Namespace [] ["PromoteColdFailed"]
    namespaceFor TracePromoteColdDone {}       =
      Namespace [] ["PromoteColdDone"]
    namespaceFor TracePromoteWarmPeers {}      =
      Namespace [] ["PromoteWarmPeers"]
    namespaceFor TracePromoteWarmLocalPeers {} =
      Namespace [] ["PromoteWarmLocalPeers"]
    namespaceFor TracePromoteWarmFailed {}     =
      Namespace [] ["PromoteWarmFailed"]
    namespaceFor TracePromoteWarmDone {}       =
      Namespace [] ["PromoteWarmDone"]
    namespaceFor TracePromoteWarmAborted {}    =
      Namespace [] ["PromoteWarmAborted"]
    namespaceFor TraceDemoteWarmPeers {}       =
      Namespace [] ["DemoteWarmPeers"]
    namespaceFor TraceDemoteWarmFailed {}      =
      Namespace [] ["DemoteWarmFailed"]
    namespaceFor TraceDemoteWarmDone {}        =
      Namespace [] ["DemoteWarmDone"]
    namespaceFor TraceDemoteHotPeers {}        =
      Namespace [] ["DemoteHotPeers"]
    namespaceFor TraceDemoteLocalHotPeers {}   =
      Namespace [] ["DemoteLocalHotPeers"]
    namespaceFor TraceDemoteHotFailed {}       =
      Namespace [] ["DemoteHotFailed"]
    namespaceFor TraceDemoteHotDone {}         =
      Namespace [] ["DemoteHotDone"]
    namespaceFor TraceDemoteAsynchronous {}    =
      Namespace [] ["DemoteAsynchronous"]
    namespaceFor TraceDemoteLocalAsynchronous {} =
      Namespace [] ["DemoteLocalAsynchronous"]
    namespaceFor TraceGovernorWakeup {}        =
      Namespace [] ["GovernorWakeup"]
    namespaceFor TraceChurnWait {}             =
      Namespace [] ["ChurnWait"]
    namespaceFor TraceChurnMode {}             =
      Namespace [] ["ChurnMode"]

    severityFor (Namespace [] ["LocalRootPeersChanged"]) _ = Just Notice
    severityFor (Namespace [] ["TargetsChanged"]) _ = Just Notice
    severityFor (Namespace [] ["PublicRootsRequest"]) _ = Just Info
    severityFor (Namespace [] ["PublicRootsResults"]) _ = Just Info
    severityFor (Namespace [] ["PublicRootsFailure"]) _ = Just Error
    severityFor (Namespace [] ["GossipRequests"]) _ = Just Debug
    severityFor (Namespace [] ["GossipResults"]) _ = Just Debug
    severityFor (Namespace [] ["ForgetColdPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdLocalPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdFailed"]) _ = Just Info
    severityFor (Namespace [] ["PromoteColdDone"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmLocalPeers"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmFailed"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmDone"]) _ = Just Info
    severityFor (Namespace [] ["PromoteWarmAborted"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmFailed"]) _ = Just Info
    severityFor (Namespace [] ["DemoteWarmDone"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteLocalHotPeers"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotFailed"]) _ = Just Info
    severityFor (Namespace [] ["DemoteHotDone"]) _ = Just Info
    severityFor (Namespace [] ["DemoteAsynchronous"]) _ = Just Info
    severityFor (Namespace [] ["DemoteLocalAsynchronous"]) _ = Just Warning
    severityFor (Namespace [] ["GovernorWakeup"]) _ = Just Info
    severityFor (Namespace [] ["ChurnWait"]) _ = Just Info
    severityFor (Namespace [] ["ChurnMode"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace [] ["LocalRootPeersChanged"]) = Just  ""
    documentFor (Namespace [] ["TargetsChanged"]) = Just  ""
    documentFor (Namespace [] ["PublicRootsRequest"]) = Just  ""
    documentFor (Namespace [] ["PublicRootsResults"]) = Just  ""
    documentFor (Namespace [] ["PublicRootsFailure"]) = Just  ""
    documentFor (Namespace [] ["GossipRequests"]) = Just
      "target known peers, actual known peers, peers available for gossip,\
      \ peers selected for gossip"
    documentFor (Namespace [] ["GossipResults"]) = Just  ""
    documentFor (Namespace [] ["ForgetColdPeers"]) = Just
      "target known peers, actual known peers, selected peers"
    documentFor (Namespace [] ["PromoteColdPeers"]) = Just
      "target established, actual established, selected peers"
    documentFor (Namespace [] ["PromoteColdLocalPeers"]) = Just
      "target local established, actual local established, selected peers"
    documentFor (Namespace [] ["PromoteColdFailed"]) = Just
      "target established, actual established, peer, delay until next\
      \ promotion, reason"
    documentFor (Namespace [] ["PromoteColdDone"]) = Just
      "target active, actual active, selected peers"
    documentFor (Namespace [] ["PromoteWarmPeers"]) = Just
      "target active, actual active, selected peers"
    documentFor (Namespace [] ["PromoteWarmLocalPeers"]) = Just
      "local per-group (target active, actual active), selected peers"
    documentFor (Namespace [] ["PromoteWarmFailed"]) = Just
      "target active, actual active, peer, reason"
    documentFor (Namespace [] ["PromoteWarmDone"]) = Just
      "target active, actual active, peer"
    documentFor (Namespace [] ["PromoteWarmAborted"]) = Just ""
    documentFor (Namespace [] ["DemoteWarmPeers"]) = Just
      "target established, actual established, selected peers"
    documentFor (Namespace [] ["DemoteWarmFailed"]) = Just
      "target established, actual established, peer, reason"
    documentFor (Namespace [] ["DemoteWarmDone"]) = Just
      "target established, actual established, peer"
    documentFor (Namespace [] ["DemoteHotPeers"]) = Just
      "target active, actual active, selected peers"
    documentFor (Namespace [] ["DemoteLocalHotPeers"]) = Just
      "local per-group (target active, actual active), selected peers"
    documentFor (Namespace [] ["DemoteHotFailed"]) = Just
      "target active, actual active, peer, reason"
    documentFor (Namespace [] ["DemoteHotDone"]) = Just
      "target active, actual active, peer"
    documentFor (Namespace [] ["DemoteAsynchronous"]) = Just  ""
    documentFor (Namespace [] ["DemoteLocalAsynchronous"]) = Just  ""
    documentFor (Namespace [] ["GovernorWakeup"]) = Just  ""
    documentFor (Namespace [] ["ChurnWait"]) = Just  ""
    documentFor (Namespace [] ["ChurnMode"]) = Just  ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["LocalRootPeersChanged"]
      , Namespace [] ["TargetsChanged"]
      , Namespace [] ["PublicRootsRequest"]
      , Namespace [] ["PublicRootsResults"]
      , Namespace [] ["PublicRootsFailure"]
      , Namespace [] ["GossipRequests"]
      , Namespace [] ["GossipResults"]
      , Namespace [] ["ForgetColdPeers"]
      , Namespace [] ["PromoteColdPeers"]
      , Namespace [] ["PromoteColdLocalPeers"]
      , Namespace [] ["PromoteColdFailed"]
      , Namespace [] ["PromoteColdDone"]
      , Namespace [] ["PromoteWarmPeers"]
      , Namespace [] ["PromoteWarmLocalPeers"]
      , Namespace [] ["PromoteWarmFailed"]
      , Namespace [] ["PromoteWarmDone"]
      , Namespace [] ["PromoteWarmAborted"]
      , Namespace [] ["DemoteWarmPeers"]
      , Namespace [] ["DemoteWarmFailed"]
      , Namespace [] ["DemoteWarmDone"]
      , Namespace [] ["DemoteHotPeers"]
      , Namespace [] ["DemoteLocalHotPeers"]
      , Namespace [] ["DemoteHotFailed"]
      , Namespace [] ["DemoteHotDone"]
      , Namespace [] ["DemoteAsynchronous"]
      , Namespace [] ["DemoteLocalAsynchronous"]
      , Namespace [] ["GovernorWakeup"]
      , Namespace [] ["ChurnWait"]
      , Namespace [] ["ChurnMode"]
      ]

--------------------------------------------------------------------------------
-- DebugPeerSelection Tracer
--------------------------------------------------------------------------------

instance LogFormatting (DebugPeerSelection SockAddr) where
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

instance MetaTrace (DebugPeerSelection SockAddr) where
    namespaceFor TraceGovernorState {} = Namespace [] ["GovernorState"]

    severityFor (Namespace _ ["GovernorState"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["GovernorState"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [
      Namespace [] ["GovernorState"]
      ]


--------------------------------------------------------------------------------
-- PeerSelectionCounters
--------------------------------------------------------------------------------

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
        "Net.PeerSelection.Cold"
        (fromIntegral coldPeers)
    , IntM
        "Net.PeerSelection.Warm"
        (fromIntegral warmPeers)
    , IntM
        "Net.PeerSelection.Hot"
        (fromIntegral hotPeers)
      ]

instance MetaTrace PeerSelectionCounters where
    namespaceFor PeerSelectionCounters {} = Namespace [] ["Counters"]

    severityFor (Namespace _ ["Counters"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["Counters"]) = Just
      "Counters for cold, warm and hot peers"
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["Counters"]) =
     [ ("Net.PeerSelection.Cold", "Number of cold peers")
      , ("Net.PeerSelection.Warm", "Number of warm peers")
      , ("Net.PeerSelection.Hot", "Number of hot peers") ]
    metricsDocFor _ = []

    allNamespaces =[
      Namespace [] ["Counters"]
      ]


--------------------------------------------------------------------------------
-- PeerSelectionActions Tracer
--------------------------------------------------------------------------------

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance Show lAddr => LogFormatting (PeerSelectionActionsTrace SockAddr lAddr) where
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
    mconcat [ "kind" .= String "PeerMonitoringError"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  forMachine _dtal (PeerMonitoringResult connId wf) =
    mconcat [ "kind" .= String "PeerMonitoringResult"
             , "connectionId" .= toJSON connId
             , "withProtocolTemp" .= show wf
             ]
  forHuman = pack . show

instance MetaTrace (PeerSelectionActionsTrace SockAddr lAddr) where
    namespaceFor PeerStatusChanged {} = Namespace [] ["StatusChanged"]
    namespaceFor PeerStatusChangeFailure {} = Namespace [] ["StatusChangeFailure"]
    namespaceFor PeerMonitoringError {} = Namespace [] ["MonitoringError"]
    namespaceFor PeerMonitoringResult {} = Namespace [] ["MonitoringResult"]

    severityFor (Namespace _ ["StatusChanged"]) _ = Just Info
    severityFor (Namespace _ ["StatusChangeFailure"]) _ = Just Error
    severityFor (Namespace _ ["MonitoringError"]) _ = Just Error
    severityFor (Namespace _ ["MonitoringResult"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["StatusChanged"]) = Just
      ""
    documentFor (Namespace _ ["StatusChangeFailure"]) = Just
      ""
    documentFor (Namespace _ ["MonitoringError"]) = Just
      ""
    documentFor (Namespace _ ["MonitoringResult"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["StatusChanged"]
      , Namespace [] ["StatusChangeFailure"]
      , Namespace [] ["MonitoringError"]
      , Namespace [] ["MonitoringResult"]
      ]

--------------------------------------------------------------------------------
-- Connection Manager Tracer
--------------------------------------------------------------------------------

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
    forHuman = pack . show
    asMetrics (TrConnectionManagerCounters ConnectionManagerCounters {..}) =
          [ IntM
              "Net.ConnectionManager.FullDuplexConns"
              (fromIntegral fullDuplexConns)
          , IntM
              "Net.ConnectionManager.DuplexConns"
              (fromIntegral duplexConns)
          , IntM
              "Net.ConnectionManager.UnidirectionalConns"
              (fromIntegral unidirectionalConns)
          , IntM
              "Net.ConnectionManager.InboundConns"
              (fromIntegral inboundConns)
          , IntM
              "Net.ConnectionManager.OutboundConns"
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
    forMachine _dtal (TrConnectionHandlerError e err cerr) =
      mconcat
        [ "kind" .= String "Error"
        , "context" .= show e
        , "reason" .= show err
        , "command" .= show cerr
        ]

instance MetaTrace (ConnectionManagerTrace addr
                      (ConnectionHandlerTrace versionNumber agreedOptions)) where
    namespaceFor TrIncludeConnection {}  = Namespace [] ["IncludeConnection"]
    namespaceFor TrUnregisterConnection {}  = Namespace [] ["UnregisterConnection"]
    namespaceFor TrConnect {}  = Namespace [] ["Connect"]
    namespaceFor TrConnectError {}  = Namespace [] ["ConnectError"]
    namespaceFor TrTerminatingConnection {}  = Namespace [] ["TerminatingConnection"]
    namespaceFor TrTerminatedConnection {}  = Namespace [] ["TerminatedConnection"]
    namespaceFor TrConnectionHandler {}  = Namespace [] ["ConnectionHandler"]
    namespaceFor TrShutdown {}  = Namespace [] ["Shutdown"]
    namespaceFor TrConnectionExists {}  = Namespace [] ["ConnectionExists"]
    namespaceFor TrForbiddenConnection {}  = Namespace [] ["ForbiddenConnection"]
    namespaceFor TrImpossibleConnection {}  = Namespace [] ["ImpossibleConnection"]
    namespaceFor TrConnectionFailure {}  = Namespace [] ["ConnectionFailure"]
    namespaceFor TrConnectionNotFound {}  = Namespace [] ["ConnectionNotFound"]
    namespaceFor TrForbiddenOperation {}  = Namespace [] ["ForbiddenOperation"]
    namespaceFor TrPruneConnections {}  = Namespace [] ["PruneConnections"]
    namespaceFor TrConnectionCleanup {}  = Namespace [] ["ConnectionCleanup"]
    namespaceFor TrConnectionTimeWait {}  = Namespace [] ["ConnectionTimeWait"]
    namespaceFor TrConnectionTimeWaitDone {}  = Namespace [] ["ConnectionTimeWaitDone"]
    namespaceFor TrConnectionManagerCounters {}  = Namespace [] ["ConnectionManagerCounters"]
    namespaceFor TrState {}  = Namespace [] ["State"]
    namespaceFor ConnectionManager.TrUnexpectedlyFalseAssertion {}  =
      Namespace [] ["UnexpectedlyFalseAssertion"]

    severityFor (Namespace _  ["IncludeConnection"]) _ = Just Debug
    severityFor (Namespace _  ["UnregisterConnection"]) _ = Just Debug
    severityFor (Namespace _  ["Connect"]) _ = Just Debug
    severityFor (Namespace _  ["ConnectError"]) _ = Just Info
    severityFor (Namespace _  ["TerminatingConnection"]) _ = Just Debug
    severityFor (Namespace _  ["TerminatedConnection"]) _ = Just Debug
    severityFor (Namespace _  ["ConnectionHandler"])
      (Just (TrConnectionHandler _ ev')) = Just $
        case ev' of
          TrHandshakeSuccess {}     -> Info
          TrHandshakeClientError {} -> Notice
          TrHandshakeServerError {} -> Info
          TrConnectionHandlerError _ _ ShutdownNode  -> Critical
          TrConnectionHandlerError _ _ ShutdownPeer  -> Info
    severityFor (Namespace _  ["ConnectionHandler"]) _ = Just Info
    severityFor (Namespace _  ["Shutdown"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionExists"]) _ = Just Info
    severityFor (Namespace _  ["ForbiddenConnection"]) _ = Just Info
    severityFor (Namespace _  ["ImpossibleConnection"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionFailure"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionNotFound"]) _ = Just Debug
    severityFor (Namespace _  ["ForbiddenOperation"]) _ = Just Info
    severityFor (Namespace _  ["PruneConnections"]) _ = Just Notice
    severityFor (Namespace _  ["ConnectionCleanup"]) _ = Just Debug
    severityFor (Namespace _  ["ConnectionTimeWait"]) _ = Just Debug
    severityFor (Namespace _  ["ConnectionTimeWaitDone"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionManagerCounters"]) _ = Just Info
    severityFor (Namespace _  ["State"]) _ = Just Info
    severityFor (Namespace _  ["UnexpectedlyFalseAssertion"]) _ = Just Error
    severityFor _ _ = Nothing

    documentFor (Namespace _  ["IncludeConnection"]) = Just ""
    documentFor (Namespace _  ["UnregisterConnection"]) = Just ""
    documentFor (Namespace _  ["Connect"]) = Just ""
    documentFor (Namespace _  ["ConnectError"]) = Just ""
    documentFor (Namespace _  ["TerminatingConnection"]) = Just ""
    documentFor (Namespace _  ["TerminatedConnection"]) = Just ""
    documentFor (Namespace _  ["ConnectionHandler"]) = Just ""
    documentFor (Namespace _  ["Shutdown"]) = Just ""
    documentFor (Namespace _  ["ConnectionExists"]) = Just ""
    documentFor (Namespace _  ["ForbiddenConnection"]) = Just ""
    documentFor (Namespace _  ["ImpossibleConnection"]) = Just ""
    documentFor (Namespace _  ["ConnectionFailure"]) = Just ""
    documentFor (Namespace _  ["ConnectionNotFound"]) = Just ""
    documentFor (Namespace _  ["ForbiddenOperation"]) = Just ""
    documentFor (Namespace _  ["PruneConnections"]) = Just ""
    documentFor (Namespace _  ["ConnectionCleanup"]) = Just ""
    documentFor (Namespace _  ["ConnectionTimeWait"]) = Just ""
    documentFor (Namespace _  ["ConnectionTimeWaitDone"]) = Just ""
    documentFor (Namespace _  ["ConnectionManagerCounters"]) = Just ""
    documentFor (Namespace _  ["State"]) = Just ""
    documentFor (Namespace _  ["UnexpectedlyFalseAssertion"]) = Just ""
    documentFor _ = Nothing

    metricsDocFor (Namespace _  ["ConnectionManagerCounters"]) =
      [("Net.ConnectionManager.FullDuplexConns","")
      ,("Net.ConnectionManager.DuplexConns","")
      ,("Net.ConnectionManager.UnidirectionalConns","")
      ,("Net.ConnectionManager.InboundConns","")
      ,("Net.ConnectionManager.OutboundConns","")
      ]
    metricsDocFor _ = []

    allNamespaces = [
        Namespace [] ["IncludeConnection"]
      , Namespace [] ["UnregisterConnection"]
      , Namespace [] ["Connect"]
      , Namespace [] ["ConnectError"]
      , Namespace [] ["TerminatingConnection"]
      , Namespace [] ["TerminatedConnection"]
      , Namespace [] ["ConnectionHandler"]
      , Namespace [] ["Shutdown"]
      , Namespace [] ["ConnectionExists"]
      , Namespace [] ["ForbiddenConnection"]
      , Namespace [] ["ImpossibleConnection"]
      , Namespace [] ["ConnectionFailure"]
      , Namespace [] ["ConnectionNotFound"]
      , Namespace [] ["ForbiddenOperation"]
      , Namespace [] ["PruneConnections"]
      , Namespace [] ["ConnectionCleanup"]
      , Namespace [] ["ConnectionTimeWait"]
      , Namespace [] ["ConnectionTimeWaitDone"]
      , Namespace [] ["ConnectionManagerCounters"]
      , Namespace [] ["State"]
      , Namespace [] ["UnexpectedlyFalseAssertion"]
      ]

--------------------------------------------------------------------------------
-- Connection Manager Transition Tracer
--------------------------------------------------------------------------------

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

instance MetaTrace (ConnectionManager.AbstractTransitionTrace peerAddr) where
    namespaceFor ConnectionManager.TransitionTrace {} =
      Namespace [] ["Transition"]

    severityFor (Namespace _  ["Transition"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _  ["Transition"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [Namespace [] ["Transition"]]

--------------------------------------------------------------------------------
-- Server Tracer
--------------------------------------------------------------------------------

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

instance MetaTrace (ServerTrace addr) where
    namespaceFor TrAcceptConnection {} = Namespace [] ["AcceptConnection"]
    namespaceFor TrAcceptError {} = Namespace [] ["AcceptError"]
    namespaceFor TrAcceptPolicyTrace {} = Namespace [] ["AcceptPolicy"]
    namespaceFor TrServerStarted {} = Namespace [] ["Started"]
    namespaceFor TrServerStopped {} = Namespace [] ["Stopped"]
    namespaceFor TrServerError {} = Namespace [] ["Error"]

    severityFor (Namespace _ ["AcceptConnection"]) _ = Just Debug
    severityFor (Namespace _ ["AcceptError"]) _ = Just Error
    severityFor (Namespace _ ["AcceptPolicy"]) _ = Just Notice
    severityFor (Namespace _ ["Started"]) _ = Just Notice
    severityFor (Namespace _ ["Stopped"]) _ = Just Notice
    severityFor (Namespace _ ["Error"]) _ = Just Critical
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["AcceptConnection"]) = Just ""
    documentFor (Namespace _ ["AcceptError"]) = Just ""
    documentFor (Namespace _ ["AcceptPolicy"]) = Just ""
    documentFor (Namespace _ ["Started"]) = Just ""
    documentFor (Namespace _ ["Stopped"]) = Just ""
    documentFor (Namespace _ ["Error"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["AcceptConnection"]
      , Namespace [] ["AcceptError"]
      , Namespace [] ["AcceptPolicy"]
      , Namespace [] ["Started"]
      , Namespace [] ["Stopped"]
      , Namespace [] ["Error"]
      ]

--------------------------------------------------------------------------------
-- InboundGovernor Tracer
--------------------------------------------------------------------------------

instance LogFormatting (InboundGovernorTrace SockAddr) where
  forMachine = forMachineGov
  forHuman = pack . show
  asMetrics (TrInboundGovernorCounters InboundGovernorCounters {..}) =
            [ IntM
                "Net.InboundGovernor.Idle"
                (fromIntegral idlePeersRemote)
            , IntM
                "Net.InboundGovernor.Cold"
                (fromIntegral coldPeersRemote)
            , IntM
                "Net.InboundGovernor.Warm"
                (fromIntegral warmPeersRemote)
            , IntM
                "Net.InboundGovernor.Hot"
                (fromIntegral hotPeersRemote)
              ]
  asMetrics _ = []

instance LogFormatting (InboundGovernorTrace LocalAddress) where
  forMachine = forMachineGov
  forHuman = pack . show
  asMetrics (TrInboundGovernorCounters InboundGovernorCounters {..}) =
            [ IntM
                "Net.LocalInboundGovernor.Idle"
                (fromIntegral idlePeersRemote)
            , IntM
                "Net.LocalInboundGovernor.Cold"
                (fromIntegral coldPeersRemote)
            , IntM
                "Net.LocalInboundGovernor.Warm"
                (fromIntegral warmPeersRemote)
            , IntM
                "Net.LocalInboundGovernor.Hot"
                (fromIntegral hotPeersRemote)
              ]
  asMetrics _ = []


forMachineGov :: (ToJSON adr, Show adr) => DetailLevel -> InboundGovernorTrace adr -> Object
forMachineGov _dtal (TrNewConnection p connId)            =
  mconcat [ "kind" .= String "NewConnection"
            , "provenance" .= show p
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrResponderRestarted connId m)       =
  mconcat [ "kind" .= String "ResponderStarted"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            ]
forMachineGov _dtal (TrResponderStartFailure connId m s)  =
  mconcat [ "kind" .= String "ResponderStartFailure"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            , "reason" .= show s
            ]
forMachineGov _dtal (TrResponderErrored connId m s)       =
  mconcat [ "kind" .= String "ResponderErrored"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            , "reason" .= show s
            ]
forMachineGov _dtal (TrResponderStarted connId m)         =
  mconcat [ "kind" .= String "ResponderStarted"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            ]
forMachineGov _dtal (TrResponderTerminated connId m)      =
  mconcat [ "kind" .= String "ResponderTerminated"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            ]
forMachineGov _dtal (TrPromotedToWarmRemote connId opRes) =
  mconcat [ "kind" .= String "PromotedToWarmRemote"
            , "connectionId" .= toJSON connId
            , "result" .= toJSON opRes
            ]
forMachineGov _dtal (TrPromotedToHotRemote connId)        =
  mconcat [ "kind" .= String "PromotedToHotRemote"
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrDemotedToColdRemote connId od)     =
  mconcat [ "kind" .= String "DemotedToColdRemote"
            , "connectionId" .= toJSON connId
            , "result" .= show od
            ]
forMachineGov _dtal (TrDemotedToWarmRemote connId)     =
  mconcat [ "kind" .= String "DemotedToWarmRemote"
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrWaitIdleRemote connId opRes) =
  mconcat [ "kind" .= String "WaitIdleRemote"
            , "connectionId" .= toJSON connId
            , "result" .= toJSON opRes
            ]
forMachineGov _dtal (TrMuxCleanExit connId)               =
  mconcat [ "kind" .= String "MuxCleanExit"
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrMuxErrored connId s)               =
  mconcat [ "kind" .= String "MuxErrored"
            , "connectionId" .= toJSON connId
            , "reason" .= show s
            ]
forMachineGov _dtal (TrInboundGovernorCounters counters) =
  mconcat [ "kind" .= String "InboundGovernorCounters"
            , "idlePeers" .= idlePeersRemote counters
            , "coldPeers" .= coldPeersRemote counters
            , "warmPeers" .= warmPeersRemote counters
            , "hotPeers" .= hotPeersRemote counters
            ]
forMachineGov _dtal (TrRemoteState st) =
  mconcat [ "kind" .= String "RemoteState"
            , "remoteSt" .= toJSON st
            ]
forMachineGov _dtal (InboundGovernor.TrUnexpectedlyFalseAssertion info) =
  mconcat [ "kind" .= String "UnexpectedlyFalseAssertion"
            , "remoteSt" .= String (pack . show $ info)
            ]
forMachineGov _dtal (InboundGovernor.TrInboundGovernorError err) =
  mconcat [ "kind" .= String "InboundGovernorError"
            , "remoteSt" .= String (pack . show $ err)
            ]

instance MetaTrace (InboundGovernorTrace addr) where
    namespaceFor TrNewConnection {}         = Namespace [] ["NewConnection"]
    namespaceFor TrResponderRestarted {}    = Namespace [] ["ResponderRestarted"]
    namespaceFor TrResponderStartFailure {} = Namespace [] ["ResponderStartFailure"]
    namespaceFor TrResponderErrored {}      = Namespace [] ["ResponderErrored"]
    namespaceFor TrResponderStarted {}      = Namespace [] ["ResponderStarted"]
    namespaceFor TrResponderTerminated {}   = Namespace [] ["ResponderTerminated"]
    namespaceFor TrPromotedToWarmRemote {}  = Namespace [] ["PromotedToWarmRemote"]
    namespaceFor TrPromotedToHotRemote {}   = Namespace [] ["PromotedToHotRemote"]
    namespaceFor TrDemotedToColdRemote {}   = Namespace [] ["DemotedToColdRemote"]
    namespaceFor TrDemotedToWarmRemote {}   = Namespace [] ["DemotedToWarmRemote"]
    namespaceFor TrWaitIdleRemote {}        = Namespace [] ["WaitIdleRemote"]
    namespaceFor TrMuxCleanExit {}          = Namespace [] ["MuxCleanExit"]
    namespaceFor TrMuxErrored {}            = Namespace [] ["MuxErrored"]
    namespaceFor TrInboundGovernorCounters {} = Namespace [] ["InboundGovernorCounters"]
    namespaceFor TrRemoteState {}            = Namespace [] ["RemoteState"]
    namespaceFor InboundGovernor.TrUnexpectedlyFalseAssertion {} =
                                Namespace [] ["UnexpectedlyFalseAssertion"]
    namespaceFor InboundGovernor.TrInboundGovernorError {} =
                                Namespace [] ["InboundGovernorError"]

    severityFor (Namespace _ ["NewConnection"]) _ = Just Debug
    severityFor (Namespace _ ["ResponderRestarted"]) _ = Just Debug
    severityFor (Namespace _ ["ResponderStartFailure"]) _ = Just Error
    severityFor (Namespace _ ["ResponderErrored"]) _ = Just Info
    severityFor (Namespace _ ["ResponderStarted"]) _ = Just Debug
    severityFor (Namespace _ ["ResponderTerminated"]) _ = Just Debug
    severityFor (Namespace _ ["PromotedToWarmRemote"]) _ = Just Info
    severityFor (Namespace _ ["PromotedToHotRemote"]) _ = Just Info
    severityFor (Namespace _ ["DemotedToColdRemote"]) _ = Just Info
    severityFor (Namespace _ ["DemotedToWarmRemote"]) _ = Just Info
    severityFor (Namespace _ ["WaitIdleRemote"]) _ = Just Debug
    severityFor (Namespace _ ["MuxCleanExit"]) _ = Just Debug
    severityFor (Namespace _ ["MuxErrored"]) _ = Just Info
    severityFor (Namespace _ ["InboundGovernorCounters"]) _ = Just Info
    severityFor (Namespace _ ["RemoteState"]) _ = Just Debug
    severityFor (Namespace _ ["UnexpectedlyFalseAssertion"]) _ = Just Error
    severityFor (Namespace _ ["InboundGovernorError"]) _ = Just Error
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["NewConnection"]) = Just ""
    documentFor (Namespace _ ["ResponderRestarted"]) = Just ""
    documentFor (Namespace _ ["ResponderStartFailure"]) = Just ""
    documentFor (Namespace _ ["ResponderErrored"]) = Just ""
    documentFor (Namespace _ ["ResponderStarted"]) = Just ""
    documentFor (Namespace _ ["ResponderTerminated"]) = Just ""
    documentFor (Namespace _ ["PromotedToWarmRemote"]) = Just ""
    documentFor (Namespace _ ["PromotedToHotRemote"]) = Just ""
    documentFor (Namespace _ ["DemotedToColdRemote"]) = Just
      "All mini-protocols terminated.  The boolean is true if this connection\
      \ was not used by p2p-governor, and thus the connection will be terminated."
    documentFor (Namespace _ ["DemotedToWarmRemote"]) = Just
      "All mini-protocols terminated.  The boolean is true if this connection\
      \ was not used by p2p-governor, and thus the connection will be terminated."
    documentFor (Namespace _ ["WaitIdleRemote"]) = Just ""
    documentFor (Namespace _ ["MuxCleanExit"]) = Just ""
    documentFor (Namespace _ ["MuxErrored"]) = Just ""
    documentFor (Namespace _ ["InboundGovernorCounters"]) = Just ""
    documentFor (Namespace _ ["RemoteState"]) = Just ""
    documentFor (Namespace _ ["UnexpectedlyFalseAssertion"]) = Just ""
    documentFor (Namespace _ ["InboundGovernorError"]) = Just ""
    documentFor _ = Nothing

    metricsDocFor (Namespace [] ["InboundGovernorCounters"]) =
          [("Net.InboundGovernor.Idle","")
          ,("Net.InboundGovernor.Cold","")
          ,("Net.InboundGovernor.Warm","")
          ,("Net.InboundGovernor.Hot","")
          ]
    metricsDocFor (Namespace ons ["InboundGovernorCounters"]) =
      if last ons == "Local"
        then
          [("Net.LocalInboundGovernor.Idle","")
          ,("Net.LocalInboundGovernor.Cold","")
          ,("Net.LocalInboundGovernor.Warm","")
          ,("Net.LocalInboundGovernor.Hot","")
          ]
        else
          [("Net.InboundGovernor.Idle","")
          ,("Net.InboundGovernor.Cold","")
          ,("Net.InboundGovernor.Warm","")
          ,("Net.InboundGovernor.Hot","")
          ]
    metricsDocFor _ = []

    allNamespaces = [
        Namespace [] ["NewConnection"]
      , Namespace [] ["ResponderRestarted"]
      , Namespace [] ["ResponderStartFailure"]
      , Namespace [] ["ResponderErrored"]
      , Namespace [] ["ResponderStarted"]
      , Namespace [] ["ResponderTerminated"]
      , Namespace [] ["PromotedToWarmRemote"]
      , Namespace [] ["PromotedToHotRemote"]
      , Namespace [] ["DemotedToColdRemote"]
      , Namespace [] ["DemotedToWarmRemote"]
      , Namespace [] ["WaitIdleRemote"]
      , Namespace [] ["MuxCleanExit"]
      , Namespace [] ["MuxErrored"]
      , Namespace [] ["InboundGovernorCounters"]
      , Namespace [] ["RemoteState"]
      , Namespace [] ["UnexpectedlyFalseAssertion"]
      , Namespace [] ["InboundGovernorError"]
      ]

--------------------------------------------------------------------------------
-- InboundGovernor Transition Tracer
--------------------------------------------------------------------------------


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

instance MetaTrace (InboundGovernor.RemoteTransitionTrace peerAddr) where
    namespaceFor InboundGovernor.TransitionTrace {} = Namespace [] ["Transition"]

    severityFor  (Namespace [] ["Transition"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor  (Namespace [] ["Transition"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [Namespace [] ["Transition"]]


--------------------------------------------------------------------------------
-- AcceptPolicy Tracer
--------------------------------------------------------------------------------

instance LogFormatting NtN.AcceptConnectionsPolicyTrace where
    forMachine _dtal (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
               , "delay" .= show delay
               , "numberOfConnection" .= show numOfConnections
               ]
    forMachine _dtal (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
               , "softLimit" .= show softLimit
               ]
    forMachine _dtal (NtN.ServerTraceAcceptConnectionResume numOfConnections) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionResume"
               , "numberOfConnection" .= show numOfConnections
               ]
    forHuman   = showT

instance MetaTrace NtN.AcceptConnectionsPolicyTrace where
    namespaceFor NtN.ServerTraceAcceptConnectionRateLimiting {} =
      Namespace [] ["ConnectionRateLimiting"]
    namespaceFor NtN.ServerTraceAcceptConnectionHardLimit {} =
      Namespace [] ["ConnectionHardLimit"]
    namespaceFor NtN.ServerTraceAcceptConnectionResume {} =
      Namespace [] ["ConnectionLimitResume"]

    severityFor (Namespace _ ["ConnectionRateLimiting"]) _ = Just Info
    severityFor (Namespace _ ["ConnectionHardLimit"]) _ = Just Warning
    severityFor (Namespace _ ["ConnectionLimitResume"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["ConnectionRateLimiting"]) = Just
      "Rate limiting accepting connections,\
        \ delaying next accept for given time, currently serving n connections."
    documentFor (Namespace _ ["ConnectionHardLimit"]) = Just
      "Hard rate limit reached,\
        \ waiting until the number of connections drops below n."
    documentFor (Namespace _ ["ConnectionLimitResume"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["ConnectionRateLimiting"]
      , Namespace [] ["ConnectionHardLimit"]
      , Namespace [] ["ConnectionLimitResume"]
      ]
