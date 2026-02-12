{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.P2P
  () where

import           Cardano.Logging
import           Cardano.Network.Diffusion.Types
import qualified Cardano.Network.PeerSelection.ExtraRootPeers as Cardano.PublicRootPeers
import qualified Cardano.Network.PeerSelection.Governor.Types as Cardano
import           Cardano.Node.Configuration.TopologyP2P ()
import           Cardano.Node.Tracing.Tracers.NodeToNode ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.ConnMap (ConnMap (..))
import           Ouroboros.Network.ConnectionManager.Core as ConnectionManager (Trace (..))
import           Ouroboros.Network.ConnectionManager.Types (ConnectionManagerCounters (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import           Ouroboros.Network.InboundGovernor as InboundGovernor (Trace (..))
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.InboundGovernor.State as InboundGovernor (Counters (..))
import qualified Cardano.Network.NodeToNode as NtN
import           Ouroboros.Network.OrphanInstances ()
import           Ouroboros.Network.PeerSelection.Churn (ChurnCounters (..))
import           Ouroboros.Network.PeerSelection.Governor (DebugPeerSelection (..),
                   DebugPeerSelectionState (..), PeerSelectionCounters, PeerSelectionState (..),
                   PeerSelectionTargets (..), PeerSelectionView (..), TracePeerSelection (..),
                   peerSelectionStateToCounters)
import           Ouroboros.Network.PeerSelection.Governor.Types (DemotionTimeoutException)
import           Ouroboros.Network.PeerSelection.PeerStateActions (PeerSelectionActionsTrace (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSTrace (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
                   (TraceLocalRootPeers (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
                   (TracePublicRootPeers (..))
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.Types ()
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (..))
import           Ouroboros.Network.Server as Server
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Tracing ()

import           Control.Exception (displayException, fromException)
import           Data.Aeson (Object, ToJSON, ToJSONKey, Value (..), object, toJSON, toJSONList,
                   (.=))
import           Data.Aeson.Types (listValue)
import           Data.Bifunctor (Bifunctor (..))
import           Data.Foldable (Foldable (..))
import qualified Data.IP as IP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (pack)
import           Network.Socket (SockAddr (..))

--------------------------------------------------------------------------------
-- ChurnMode Tracer
--------------------------------------------------------------------------------

instance LogFormatting TraceChurnMode where
  forMachine _dtal (TraceChurnMode mode) =
    mconcat [ "kind" .= String "ChurnMode"
            , "churnMode" .= String (pack . show $ mode)
            ]

instance MetaTrace TraceChurnMode where
  namespaceFor TraceChurnMode {} =
    Namespace [] ["PeerSelection", "ChurnMode"]
  severityFor _ (Just TraceChurnMode {}) = Just Info
  severityFor _ Nothing = Nothing

  documentFor (Namespace _ ["PeerSelection", "ChurnMode"]) = Just $ mconcat
    ["Affects churning strategy. For a synced node or operating in GenesisMode "
    , " consensus mode, the default strategy is used. Otherwise for a syncing PraosMode"
    , " node, the legacy bulk sync churning intervals are used whose durations"
    , " depend on whether bootstrap peers are enabled."
    ]
  documentFor _ = Nothing

  allNamespaces = [
      Namespace [] ["PeerSelection", "ChurnMode"]
    ]
