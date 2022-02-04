{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tracing.Config
  ( TraceOptions (..)
  , TraceSelection (..)
  , OnOff (..)
  , PartialTraceOptions (..)
  , PartialTraceSelection (..)
  , partialTraceSelectionToEither
  , defaultPartialTraceConfiguration
  , lastToEither

  -- * Trace symbols
  , TraceConnectionManagerCounters
  , TracePeerSelectionCounters
  , TraceInboundGovernorCounters
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Data.Aeson
import qualified Data.Text as Text
import           Generic.Data (gmappend)

import           Cardano.BM.Tracing (TracingVerbosity (..))
import           Cardano.Node.Orphans ()


data TraceOptions
  = TracingOff
  | TracingOnLegacy TraceSelection
  | TraceDispatcher TraceSelection
  deriving (Eq, Show)

data PartialTraceOptions
  = PartialTracingOff
  | PartialTracingOnLegacy PartialTraceSelection
  | PartialTraceDispatcher PartialTraceSelection
  deriving (Eq, Show)

instance Monoid PartialTraceOptions where
  mempty = PartialTracingOff

-- Mimics Last's semantics
instance Semigroup PartialTraceOptions where

  tracingA <> tracingB =
    case (tracingA, tracingB) of
      (PartialTracingOff, PartialTracingOff) -> PartialTracingOff

      (PartialTracingOnLegacy ptsA, PartialTracingOnLegacy ptsB) ->
        PartialTracingOnLegacy (ptsA <> ptsB)

      (PartialTraceDispatcher ptsA, PartialTraceDispatcher ptsB) ->
        PartialTraceDispatcher (ptsA <> ptsB)

      (_ , PartialTracingOff) -> PartialTracingOff

      (PartialTracingOff, tracing) -> tracing

      (PartialTracingOnLegacy _, PartialTraceDispatcher pts) ->
        PartialTraceDispatcher pts

      (PartialTraceDispatcher _, PartialTracingOnLegacy pts) ->
        PartialTracingOnLegacy pts

type TraceAcceptPolicy = ("TraceAcceptPolicy" :: Symbol)
type TraceBlockchainTime = ("TraceBlockchainTime" :: Symbol)
type TraceBlockFetchClient = ("TraceBlockFetchClient" :: Symbol)
type TraceBlockFetchDecisions = ("TraceBlockFetchDecisions" :: Symbol)
type TraceBlockFetchProtocol = ("TraceBlockFetchProtocol" :: Symbol)
type TraceBlockFetchProtocolSerialised = ("TraceBlockFetchProtocolSerialised" :: Symbol)
type TraceBlockFetchServer = ("TraceBlockFetchServer" :: Symbol)
type TraceChainDB = ("TraceChainDB" :: Symbol)
type TraceChainSyncClient = ("TraceChainSyncClient" :: Symbol)
type TraceChainSyncBlockServer = ("TraceChainSyncBlockServer" :: Symbol)
type TraceChainSyncHeaderServer = ("TraceChainSyncHeaderServer" :: Symbol)
type TraceChainSyncProtocol = ("TraceChainSyncProtocol" :: Symbol)
type TraceConnectionManager = ("TraceConnectionManager" :: Symbol)
type TraceConnectionManagerCounters = ("TraceConnectionManagerCounters" :: Symbol)
type TraceConnectionManagerTransitions = ("TraceConnectionManagerTransitions" :: Symbol)
type DebugPeerSelectionInitiator = ("DebugPeerSelectionInitiator" :: Symbol)
type DebugPeerSelectionInitiatorResponder = ("DebugPeerSelectionInitiatorResponder" :: Symbol)
type TraceDiffusionInitialization = ("TraceDiffusionInitialization" :: Symbol)
type TraceDnsResolver = ("TraceDnsResolver" :: Symbol)
type TraceDnsSubscription = ("TraceDnsSubscription" :: Symbol)
type TraceErrorPolicy = ("TraceErrorPolicy" :: Symbol)
type TraceForge = ("TraceForge" :: Symbol)
type TraceForgeStateInfo = ("TraceForgeStateInfo" :: Symbol)
type TraceHandshake = ("TraceHandshake" :: Symbol)
type TraceIpSubscription = ("TraceIpSubscription" :: Symbol)
type TraceKeepAliveClient = ("TraceKeepAliveClient" :: Symbol)
type TraceLedgerPeers = ("TraceLedgerPeers" :: Symbol)
type TraceLocalChainSyncProtocol = ("TraceLocalChainSyncProtocol" :: Symbol)
type TraceLocalConnectionManager = ("TraceLocalConnectionManager" :: Symbol)
type TraceLocalErrorPolicy = ("TraceLocalErrorPolicy" :: Symbol)
type TraceLocalHandshake = ("TraceLocalHandshake" :: Symbol)
type TraceLocalInboundGovernor = ("TraceLocalInboundGovernor" :: Symbol)
type TraceLocalRootPeers = ("TraceLocalRootPeers" :: Symbol)
type TraceLocalServer = ("TraceLocalServer" :: Symbol)
type TraceLocalStateQueryProtocol = ("TraceLocalStateQueryProtocol" :: Symbol)
type TraceLocalTxMonitorProtocol = ("TraceLocalTxMonitorProtocol" :: Symbol)
type TraceLocalTxSubmissionProtocol = ("TraceLocalTxSubmissionProtocol" :: Symbol)
type TraceLocalTxSubmissionServer = ("TraceLocalTxSubmissionServer" :: Symbol)
type TraceMempool = ("TraceMempool" :: Symbol)
type TraceMux = ("TraceMux" :: Symbol)
type TraceLocalMux = ("TraceLocalMux" :: Symbol)
type TracePeerSelection = ("TracePeerSelection" :: Symbol)
type TracePeerSelectionCounters = ("TracePeerSelectionCounters" :: Symbol)
type TracePeerSelectionActions = ("TracePeerSelectionActions" :: Symbol)
type TracePublicRootPeers = ("TracePublicRootPeers" :: Symbol)
type TraceServer = ("TraceServer" :: Symbol)
type TraceInboundGovernor = ("TraceInboundGovernor" :: Symbol)
type TraceInboundGovernorCounters = ("TraceInboundGovernorCounters" :: Symbol)
type TraceInboundGovernorTransitions = ("TraceInboundGovernorTransitions" :: Symbol)
type TraceTxInbound = ("TraceTxInbound" :: Symbol)
type TraceTxOutbound = ("TraceTxOutbound" :: Symbol)
type TraceTxSubmissionProtocol = ("TraceTxSubmissionProtocol" :: Symbol)
type TraceTxSubmission2Protocol = ("TraceTxSubmission2Protocol" :: Symbol)

newtype OnOff (name :: Symbol) = OnOff { isOn :: Bool } deriving (Eq, Show)

instance FromJSON (OnOff a) where
    parseJSON (Data.Aeson.Bool b)= return $ OnOff b
    parseJSON _ = mzero

proxyName :: KnownSymbol name => Proxy name -> Text
proxyName p = Text.pack (symbolVal p)

data TraceSelection
  = TraceSelection
  { traceVerbosity :: !TracingVerbosity

  -- Per-trace toggles, alpha-sorted.
  , traceAcceptPolicy :: OnOff TraceAcceptPolicy
  , traceBlockFetchClient :: OnOff TraceBlockFetchClient
  , traceBlockFetchDecisions :: OnOff TraceBlockFetchDecisions
  , traceBlockFetchProtocol :: OnOff TraceBlockFetchProtocol
  , traceBlockFetchProtocolSerialised :: OnOff TraceBlockFetchProtocolSerialised
  , traceBlockFetchServer :: OnOff TraceBlockFetchServer
  , traceBlockchainTime :: OnOff TraceBlockchainTime
  , traceChainDB :: OnOff TraceChainDB
  , traceChainSyncBlockServer :: OnOff TraceChainSyncBlockServer
  , traceChainSyncClient :: OnOff TraceChainSyncClient
  , traceChainSyncHeaderServer :: OnOff TraceChainSyncHeaderServer
  , traceChainSyncProtocol :: OnOff TraceChainSyncProtocol
  , traceConnectionManager :: OnOff TraceConnectionManager
  , traceConnectionManagerCounters :: OnOff TraceConnectionManagerCounters
  , traceConnectionManagerTransitions :: OnOff TraceConnectionManagerTransitions
  , traceDebugPeerSelectionInitiatorTracer :: OnOff DebugPeerSelectionInitiator
  , traceDebugPeerSelectionInitiatorResponderTracer :: OnOff DebugPeerSelectionInitiatorResponder
  , traceDiffusionInitialization :: OnOff TraceDiffusionInitialization
  , traceDnsResolver :: OnOff TraceDnsResolver
  , traceDnsSubscription :: OnOff TraceDnsSubscription
  , traceErrorPolicy :: OnOff TraceErrorPolicy
  , traceForge :: OnOff TraceForge
  , traceForgeStateInfo :: OnOff TraceForgeStateInfo
  , traceHandshake :: OnOff TraceHandshake
  , traceInboundGovernor :: OnOff TraceInboundGovernor
  , traceInboundGovernorCounters :: OnOff TraceInboundGovernorCounters
  , traceInboundGovernorTransitions :: OnOff TraceInboundGovernorTransitions
  , traceIpSubscription :: OnOff TraceIpSubscription
  , traceKeepAliveClient :: OnOff TraceKeepAliveClient
  , traceLedgerPeers :: OnOff TraceLedgerPeers
  , traceLocalChainSyncProtocol :: OnOff TraceLocalChainSyncProtocol
  , traceLocalConnectionManager :: OnOff TraceLocalConnectionManager
  , traceLocalErrorPolicy :: OnOff TraceLocalErrorPolicy
  , traceLocalHandshake :: OnOff TraceLocalHandshake
  , traceLocalInboundGovernor :: OnOff TraceLocalInboundGovernor
  , traceLocalMux :: OnOff TraceLocalMux
  , traceLocalRootPeers :: OnOff TraceLocalRootPeers
  , traceLocalServer :: OnOff TraceLocalServer
  , traceLocalStateQueryProtocol :: OnOff TraceLocalStateQueryProtocol
  , traceLocalTxMonitorProtocol :: OnOff TraceLocalTxMonitorProtocol
  , traceLocalTxSubmissionProtocol :: OnOff TraceLocalTxSubmissionProtocol
  , traceLocalTxSubmissionServer :: OnOff TraceLocalTxSubmissionServer
  , traceMempool :: OnOff TraceMempool
  , traceMux :: OnOff TraceMux
  , tracePeerSelection :: OnOff TracePeerSelection
  , tracePeerSelectionCounters :: OnOff TracePeerSelectionCounters
  , tracePeerSelectionActions :: OnOff TracePeerSelectionActions
  , tracePublicRootPeers :: OnOff TracePublicRootPeers
  , traceServer :: OnOff TraceServer
  , traceTxInbound :: OnOff TraceTxInbound
  , traceTxOutbound :: OnOff TraceTxOutbound
  , traceTxSubmissionProtocol :: OnOff TraceTxSubmissionProtocol
  , traceTxSubmission2Protocol :: OnOff TraceTxSubmission2Protocol
  } deriving (Eq, Show)



data PartialTraceSelection
  = PartialTraceSelection
      { pTraceVerbosity :: !(Last TracingVerbosity)

      -- Per-trace toggles, alpha-sorted.
      , pTraceAcceptPolicy :: Last (OnOff TraceAcceptPolicy)
      , pTraceBlockchainTime :: Last (OnOff TraceBlockchainTime)
      , pTraceBlockFetchClient :: Last (OnOff TraceBlockFetchClient)
      , pTraceBlockFetchDecisions :: Last (OnOff TraceBlockFetchDecisions)
      , pTraceBlockFetchProtocol :: Last (OnOff TraceBlockFetchProtocol)
      , pTraceBlockFetchProtocolSerialised :: Last (OnOff TraceBlockFetchProtocolSerialised)
      , pTraceBlockFetchServer :: Last (OnOff TraceBlockFetchServer)
      , pTraceChainDB :: Last (OnOff TraceChainDB)
      , pTraceChainSyncBlockServer :: Last (OnOff TraceChainSyncBlockServer)
      , pTraceChainSyncClient :: Last (OnOff TraceChainSyncClient)
      , pTraceChainSyncHeaderServer :: Last (OnOff TraceChainSyncHeaderServer)
      , pTraceChainSyncProtocol :: Last (OnOff TraceChainSyncProtocol)
      , pTraceConnectionManager :: Last (OnOff TraceConnectionManager)
      , pTraceConnectionManagerCounters :: Last (OnOff TraceConnectionManagerCounters)
      , pTraceConnectionManagerTransitions :: Last (OnOff TraceConnectionManagerTransitions)
      , pTraceDebugPeerSelectionInitiatorTracer :: Last (OnOff DebugPeerSelectionInitiator)
      , pTraceDiffusionInitialization :: Last (OnOff TraceDiffusionInitialization)
      , pTraceDebugPeerSelectionInitiatorResponderTracer :: Last (OnOff DebugPeerSelectionInitiatorResponder)
      , pTraceDnsResolver :: Last (OnOff TraceDnsResolver)
      , pTraceDnsSubscription :: Last (OnOff TraceDnsSubscription)
      , pTraceErrorPolicy :: Last (OnOff TraceErrorPolicy)
      , pTraceForge :: Last (OnOff TraceForge)
      , pTraceForgeStateInfo :: Last (OnOff TraceForgeStateInfo)
      , pTraceHandshake :: Last (OnOff TraceHandshake)
      , pTraceInboundGovernor :: Last (OnOff TraceInboundGovernor)
      , pTraceInboundGovernorCounters :: Last (OnOff TraceInboundGovernorCounters)
      , pTraceInboundGovernorTransitions :: Last (OnOff TraceInboundGovernorTransitions)
      , pTraceIpSubscription :: Last (OnOff TraceIpSubscription)
      , pTraceKeepAliveClient :: Last (OnOff TraceKeepAliveClient)
      , pTraceLedgerPeers :: Last (OnOff TraceLedgerPeers)
      , pTraceLocalChainSyncProtocol :: Last (OnOff TraceLocalChainSyncProtocol)
      , pTraceLocalConnectionManager :: Last (OnOff TraceLocalConnectionManager)
      , pTraceLocalErrorPolicy :: Last (OnOff TraceLocalErrorPolicy)
      , pTraceLocalHandshake :: Last (OnOff TraceLocalHandshake)
      , pTraceLocalInboundGovernor :: Last (OnOff TraceLocalInboundGovernor)
      , pTraceLocalMux :: Last (OnOff TraceLocalMux)
      , pTraceLocalRootPeers :: Last (OnOff TraceLocalRootPeers)
      , pTraceLocalServer :: Last (OnOff TraceLocalServer)
      , pTraceLocalStateQueryProtocol :: Last (OnOff TraceLocalStateQueryProtocol)
      , pTraceLocalTxMonitorProtocol :: Last (OnOff TraceLocalTxMonitorProtocol)
      , pTraceLocalTxSubmissionProtocol :: Last (OnOff TraceLocalTxSubmissionProtocol)
      , pTraceLocalTxSubmissionServer :: Last (OnOff TraceLocalTxSubmissionServer)
      , pTraceMempool :: Last (OnOff TraceMempool)
      , pTraceMux :: Last (OnOff TraceMux)
      , pTracePeerSelection :: Last (OnOff TracePeerSelection)
      , pTracePeerSelectionCounters :: Last (OnOff TracePeerSelectionCounters)
      , pTracePeerSelectionActions :: Last (OnOff TracePeerSelectionActions)
      , pTracePublicRootPeers :: Last (OnOff TracePublicRootPeers)
      , pTraceServer :: Last (OnOff TraceServer)
      , pTraceTxInbound :: Last (OnOff TraceTxInbound)
      , pTraceTxOutbound :: Last (OnOff TraceTxOutbound)
      , pTraceTxSubmissionProtocol :: Last (OnOff TraceTxSubmissionProtocol)
      , pTraceTxSubmission2Protocol :: Last (OnOff TraceTxSubmission2Protocol)
      } deriving (Eq, Generic, Show)


instance Semigroup PartialTraceSelection where
  (<>) = gmappend

instance FromJSON PartialTraceSelection where
  parseJSON = withObject "PartialTraceSelection" $ \v -> do
    PartialTraceSelection
        <$> Last <$> v .:? "TracingVerbosity"
        -- Per-trace toggles, alpha-sorted.
        <*> (Last <$> v .:? proxyName (Proxy @TraceAcceptPolicy))
        <*> (Last <$> v .:? proxyName (Proxy @TraceBlockchainTime))
        <*> (Last <$> v .:? proxyName (Proxy @TraceBlockFetchClient))
        <*> (Last <$> v .:? proxyName (Proxy @TraceBlockFetchDecisions))
        <*> (Last <$> v .:? proxyName (Proxy @TraceBlockFetchProtocol))
        <*> (Last <$> v .:? proxyName (Proxy @TraceBlockFetchProtocolSerialised))
        <*> (Last <$> v .:? proxyName (Proxy @TraceBlockFetchServer))
        <*> (Last <$> v .:? proxyName (Proxy @TraceChainDB))
        <*> (Last <$> v .:? proxyName (Proxy @TraceChainSyncBlockServer))
        <*> (Last <$> v .:? proxyName (Proxy @TraceChainSyncClient))
        <*> (Last <$> v .:? proxyName (Proxy @TraceChainSyncHeaderServer))
        <*> (Last <$> v .:? proxyName (Proxy @TraceChainSyncProtocol))
        <*> (Last <$> v .:? proxyName (Proxy @TraceConnectionManager))
        <*> (Last <$> v .:? proxyName (Proxy @TraceConnectionManagerCounters))
        <*> (Last <$> v .:? proxyName (Proxy @TraceConnectionManagerTransitions))
        <*> (Last <$> v .:? proxyName (Proxy @DebugPeerSelectionInitiator))
        <*> (Last <$> v .:? proxyName (Proxy @DebugPeerSelectionInitiatorResponder))
        <*> (Last <$> v .:? proxyName (Proxy @TraceDiffusionInitialization))
        <*> (Last <$> v .:? proxyName (Proxy @TraceDnsResolver))
        <*> (Last <$> v .:? proxyName (Proxy @TraceDnsSubscription))
        <*> (Last <$> v .:? proxyName (Proxy @TraceErrorPolicy))
        <*> (Last <$> v .:? proxyName (Proxy @TraceForge))
        <*> (Last <$> v .:? proxyName (Proxy @TraceForgeStateInfo))
        <*> (Last <$> v .:? proxyName (Proxy @TraceHandshake))
        <*> (Last <$> v .:? proxyName (Proxy @TraceIpSubscription))
        <*> (Last <$> v .:? proxyName (Proxy @TraceKeepAliveClient))
        <*> (Last <$> v .:? proxyName (Proxy @TraceInboundGovernorTransitions))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLedgerPeers))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalChainSyncProtocol))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalConnectionManager))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalErrorPolicy))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalHandshake))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalInboundGovernor))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalRootPeers))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalServer))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalStateQueryProtocol))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalTxMonitorProtocol))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalTxSubmissionProtocol))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalTxSubmissionServer))
        <*> (Last <$> v .:? proxyName (Proxy @TraceMempool))
        <*> (Last <$> v .:? proxyName (Proxy @TraceMux))
        <*> (Last <$> v .:? proxyName (Proxy @TraceLocalMux))
        <*> (Last <$> v .:? proxyName (Proxy @TracePeerSelection))
        <*> (Last <$> v .:? proxyName (Proxy @TracePeerSelectionCounters))
        <*> (Last <$> v .:? proxyName (Proxy @TracePeerSelectionActions))
        <*> (Last <$> v .:? proxyName (Proxy @TracePublicRootPeers))
        <*> (Last <$> v .:? proxyName (Proxy @TraceServer))
        <*> (Last <$> v .:? proxyName (Proxy @TraceInboundGovernor))
        <*> (Last <$> v .:? proxyName (Proxy @TraceInboundGovernorCounters))
        <*> (Last <$> v .:? proxyName (Proxy @TraceTxInbound))
        <*> (Last <$> v .:? proxyName (Proxy @TraceTxOutbound))
        <*> (Last <$> v .:? proxyName (Proxy @TraceTxSubmissionProtocol))
        <*> (Last <$> v .:? proxyName (Proxy @TraceTxSubmission2Protocol))


defaultPartialTraceConfiguration :: PartialTraceSelection
defaultPartialTraceConfiguration =
  PartialTraceSelection
    { pTraceVerbosity = Last Nothing
    -- Per-trace toggles, alpha-sorted.
    , pTraceAcceptPolicy = pure $ OnOff False
    , pTraceBlockchainTime = pure $ OnOff False
    , pTraceBlockFetchClient = pure $ OnOff False
    , pTraceBlockFetchDecisions = pure $ OnOff True
    , pTraceBlockFetchProtocol = pure $ OnOff False
    , pTraceBlockFetchProtocolSerialised = pure $ OnOff False
    , pTraceBlockFetchServer = pure $ OnOff False
    , pTraceChainDB = pure $ OnOff True
    , pTraceChainSyncBlockServer = pure $ OnOff False
    , pTraceChainSyncClient = pure $ OnOff True
    , pTraceChainSyncHeaderServer = pure $ OnOff False
    , pTraceChainSyncProtocol = pure $ OnOff False
    , pTraceConnectionManager = pure $ OnOff False
    , pTraceConnectionManagerCounters = pure $ OnOff True
    , pTraceConnectionManagerTransitions = pure $ OnOff False
    , pTraceDebugPeerSelectionInitiatorTracer = pure $ OnOff False
    , pTraceDebugPeerSelectionInitiatorResponderTracer = pure $ OnOff False
    , pTraceDiffusionInitialization = pure $ OnOff False
    , pTraceDnsResolver = pure $ OnOff False
    , pTraceDnsSubscription = pure $ OnOff True
    , pTraceErrorPolicy = pure $ OnOff True
    , pTraceForge = pure $ OnOff True
    , pTraceForgeStateInfo = pure $ OnOff True
    , pTraceHandshake = pure $ OnOff False
    , pTraceInboundGovernor = pure $ OnOff False
    , pTraceInboundGovernorCounters = pure $ OnOff True
    , pTraceInboundGovernorTransitions = pure $ OnOff True
    , pTraceIpSubscription = pure $ OnOff True
    , pTraceKeepAliveClient = pure $ OnOff False
    , pTraceLedgerPeers = pure $ OnOff False
    , pTraceLocalChainSyncProtocol = pure $ OnOff False
    , pTraceLocalConnectionManager = pure $ OnOff False
    , pTraceLocalErrorPolicy = pure $ OnOff True
    , pTraceLocalHandshake = pure $ OnOff False
    , pTraceLocalInboundGovernor = pure $ OnOff False
    , pTraceLocalMux = pure $ OnOff False
    , pTraceLocalTxMonitorProtocol = pure $ OnOff False
    , pTraceLocalRootPeers = pure $ OnOff False
    , pTraceLocalServer = pure $ OnOff False
    , pTraceLocalStateQueryProtocol = pure $ OnOff False
    , pTraceLocalTxSubmissionProtocol = pure $ OnOff False
    , pTraceLocalTxSubmissionServer = pure $ OnOff False
    , pTraceMempool = pure $ OnOff True
    , pTraceMux = pure $ OnOff True
    , pTracePeerSelection = pure $ OnOff False
    , pTracePeerSelectionCounters = pure $ OnOff True
    , pTracePeerSelectionActions = pure $ OnOff False
    , pTracePublicRootPeers = pure $ OnOff False
    , pTraceServer = pure $ OnOff False
    , pTraceTxInbound = pure $ OnOff False
    , pTraceTxOutbound = pure $ OnOff False
    , pTraceTxSubmissionProtocol = pure $ OnOff False
    , pTraceTxSubmission2Protocol = pure $ OnOff False
    }


partialTraceSelectionToEither ::  PartialTraceOptions -> Either Text TraceOptions
partialTraceSelectionToEither  PartialTracingOff = Right TracingOff
partialTraceSelectionToEither (PartialTraceDispatcher pTraceSelection) = do
   let PartialTraceSelection {..} = defaultPartialTraceConfiguration <> pTraceSelection
   traceVerbosity <- first Text.pack $ lastToEither "Default value not specified for TracingVerbosity" pTraceVerbosity
   traceAcceptPolicy <- proxyLastToEither (Proxy @TraceAcceptPolicy) pTraceAcceptPolicy
   traceBlockFetchClient <- proxyLastToEither (Proxy @TraceBlockchainTime) pTraceBlockFetchClient
   traceBlockFetchDecisions <- proxyLastToEither (Proxy @TraceBlockFetchClient) pTraceBlockFetchDecisions
   traceBlockFetchProtocol <- proxyLastToEither (Proxy @TraceBlockFetchDecisions) pTraceBlockFetchProtocol
   traceBlockFetchProtocolSerialised <- proxyLastToEither (Proxy @TraceBlockFetchProtocol) pTraceBlockFetchProtocolSerialised
   traceBlockFetchServer <- proxyLastToEither (Proxy @TraceBlockFetchProtocolSerialised) pTraceBlockFetchServer
   traceBlockchainTime <- proxyLastToEither (Proxy @TraceBlockFetchServer) pTraceBlockchainTime
   traceChainDB <- proxyLastToEither (Proxy @TraceChainDB) pTraceChainDB
   traceChainSyncBlockServer <- proxyLastToEither (Proxy @TraceChainSyncClient) pTraceChainSyncBlockServer
   traceChainSyncClient <- proxyLastToEither (Proxy @TraceChainSyncBlockServer) pTraceChainSyncClient
   traceChainSyncHeaderServer <- proxyLastToEither (Proxy @TraceChainSyncHeaderServer) pTraceChainSyncHeaderServer
   traceChainSyncProtocol <- proxyLastToEither (Proxy @TraceChainSyncProtocol) pTraceChainSyncProtocol
   traceConnectionManager <- proxyLastToEither (Proxy @TraceConnectionManager) pTraceConnectionManager
   traceConnectionManagerCounters <- proxyLastToEither (Proxy @TraceConnectionManagerCounters) pTraceConnectionManagerCounters
   traceConnectionManagerTransitions <- proxyLastToEither (Proxy @TraceConnectionManagerTransitions) pTraceConnectionManagerTransitions
   traceDebugPeerSelectionInitiatorTracer <- proxyLastToEither (Proxy @DebugPeerSelectionInitiator) pTraceDebugPeerSelectionInitiatorTracer
   traceDebugPeerSelectionInitiatorResponderTracer <- proxyLastToEither (Proxy @DebugPeerSelectionInitiatorResponder) pTraceDebugPeerSelectionInitiatorResponderTracer
   traceDiffusionInitialization <- proxyLastToEither (Proxy @TraceDiffusionInitialization) pTraceDiffusionInitialization
   traceDnsResolver <- proxyLastToEither (Proxy @TraceDnsResolver) pTraceDnsResolver
   traceDnsSubscription <- proxyLastToEither (Proxy @TraceDnsSubscription) pTraceDnsSubscription
   traceErrorPolicy <- proxyLastToEither (Proxy @TraceErrorPolicy) pTraceErrorPolicy
   traceForge <- proxyLastToEither (Proxy @TraceForge) pTraceForge
   traceForgeStateInfo <- proxyLastToEither (Proxy @TraceForgeStateInfo) pTraceForgeStateInfo
   traceHandshake <- proxyLastToEither (Proxy @TraceHandshake) pTraceHandshake
   traceInboundGovernor <- proxyLastToEither (Proxy @TraceIpSubscription) pTraceInboundGovernor
   traceInboundGovernorCounters <- proxyLastToEither (Proxy @TraceKeepAliveClient) pTraceInboundGovernorCounters
   traceInboundGovernorTransitions <- proxyLastToEither (Proxy @TraceInboundGovernorTransitions) pTraceInboundGovernorTransitions
   traceIpSubscription <- proxyLastToEither (Proxy @TraceLedgerPeers) pTraceIpSubscription
   traceKeepAliveClient <- proxyLastToEither (Proxy @TraceLocalChainSyncProtocol) pTraceKeepAliveClient
   traceLedgerPeers <- proxyLastToEither (Proxy @TraceLocalConnectionManager) pTraceLedgerPeers
   traceLocalChainSyncProtocol <- proxyLastToEither (Proxy @TraceLocalErrorPolicy) pTraceLocalChainSyncProtocol
   traceLocalConnectionManager <- proxyLastToEither (Proxy @TraceLocalHandshake) pTraceLocalConnectionManager
   traceLocalErrorPolicy <- proxyLastToEither (Proxy @TraceLocalInboundGovernor) pTraceLocalErrorPolicy
   traceLocalHandshake <- proxyLastToEither (Proxy @TraceLocalRootPeers) pTraceLocalHandshake
   traceLocalInboundGovernor <- proxyLastToEither (Proxy @TraceLocalServer) pTraceLocalInboundGovernor
   traceLocalMux <- proxyLastToEither (Proxy @TraceLocalStateQueryProtocol) pTraceLocalMux
   traceLocalTxMonitorProtocol <- proxyLastToEither (Proxy @TraceLocalTxMonitorProtocol) pTraceLocalTxMonitorProtocol
   traceLocalRootPeers <- proxyLastToEither (Proxy @TraceLocalTxSubmissionProtocol) pTraceLocalRootPeers
   traceLocalServer <- proxyLastToEither (Proxy @TraceLocalTxSubmissionServer) pTraceLocalServer
   traceLocalStateQueryProtocol <- proxyLastToEither (Proxy @TraceMempool) pTraceLocalStateQueryProtocol
   traceLocalTxSubmissionProtocol <- proxyLastToEither (Proxy @TraceMux) pTraceLocalTxSubmissionProtocol
   traceLocalTxSubmissionServer <- proxyLastToEither (Proxy @TraceLocalMux) pTraceLocalTxSubmissionServer
   traceMempool <- proxyLastToEither (Proxy @TracePeerSelection) pTraceMempool
   traceMux <- proxyLastToEither (Proxy @TracePeerSelectionCounters) pTraceMux
   tracePeerSelection <- proxyLastToEither (Proxy @TracePeerSelectionActions) pTracePeerSelection
   tracePeerSelectionCounters <- proxyLastToEither (Proxy @TracePublicRootPeers) pTracePeerSelectionCounters
   tracePeerSelectionActions <- proxyLastToEither (Proxy @TraceServer) pTracePeerSelectionActions
   tracePublicRootPeers <- proxyLastToEither (Proxy @TraceInboundGovernor) pTracePublicRootPeers
   traceServer <- proxyLastToEither (Proxy @TraceInboundGovernorCounters) pTraceServer
   traceTxInbound <- proxyLastToEither (Proxy @TraceTxInbound) pTraceTxInbound
   traceTxOutbound <- proxyLastToEither (Proxy @TraceTxOutbound) pTraceTxOutbound
   traceTxSubmissionProtocol <- proxyLastToEither (Proxy @TraceTxSubmissionProtocol) pTraceTxSubmissionProtocol
   traceTxSubmission2Protocol <- proxyLastToEither (Proxy @TraceTxSubmission2Protocol) pTraceTxSubmission2Protocol
   Right $ TracingOnLegacy $ TraceSelection
             { traceVerbosity
             , traceAcceptPolicy
             , traceBlockFetchClient
             , traceBlockFetchDecisions
             , traceBlockFetchProtocol
             , traceBlockFetchProtocolSerialised
             , traceBlockFetchServer
             , traceBlockchainTime
             , traceChainDB
             , traceChainSyncBlockServer
             , traceChainSyncClient
             , traceChainSyncHeaderServer
             , traceChainSyncProtocol
             , traceConnectionManager
             , traceConnectionManagerCounters
             , traceConnectionManagerTransitions
             , traceDebugPeerSelectionInitiatorTracer
             , traceDebugPeerSelectionInitiatorResponderTracer
             , traceDiffusionInitialization
             , traceDnsResolver
             , traceDnsSubscription
             , traceErrorPolicy
             , traceForge
             , traceForgeStateInfo
             , traceHandshake
             , traceInboundGovernor
             , traceInboundGovernorCounters
             , traceInboundGovernorTransitions
             , traceIpSubscription
             , traceKeepAliveClient
             , traceLedgerPeers
             , traceLocalChainSyncProtocol
             , traceLocalConnectionManager
             , traceLocalErrorPolicy
             , traceLocalHandshake
             , traceLocalInboundGovernor
             , traceLocalMux
             , traceLocalTxMonitorProtocol
             , traceLocalRootPeers
             , traceLocalServer
             , traceLocalStateQueryProtocol
             , traceLocalTxSubmissionProtocol
             , traceLocalTxSubmissionServer
             , traceMempool
             , traceMux
             , tracePeerSelection
             , tracePeerSelectionCounters
             , tracePeerSelectionActions
             , tracePublicRootPeers
             , traceServer
             , traceTxInbound
             , traceTxOutbound
             , traceTxSubmissionProtocol
             , traceTxSubmission2Protocol
             }

partialTraceSelectionToEither (PartialTracingOnLegacy pTraceSelection) = do
  -- This will be removed once the old tracing system is deprecated.
  let PartialTraceSelection {..} = defaultPartialTraceConfiguration <> pTraceSelection
  traceVerbosity <- first Text.pack $ lastToEither "Default value not specified for TracingVerbosity" pTraceVerbosity
  traceAcceptPolicy <- proxyLastToEither (Proxy @TraceAcceptPolicy) pTraceAcceptPolicy
  traceBlockFetchClient <- proxyLastToEither (Proxy @TraceBlockchainTime) pTraceBlockFetchClient
  traceBlockFetchDecisions <- proxyLastToEither (Proxy @TraceBlockFetchClient) pTraceBlockFetchDecisions
  traceBlockFetchProtocol <- proxyLastToEither (Proxy @TraceBlockFetchDecisions) pTraceBlockFetchProtocol
  traceBlockFetchProtocolSerialised <- proxyLastToEither (Proxy @TraceBlockFetchProtocol) pTraceBlockFetchProtocolSerialised
  traceBlockFetchServer <- proxyLastToEither (Proxy @TraceBlockFetchProtocolSerialised) pTraceBlockFetchServer
  traceBlockchainTime <- proxyLastToEither (Proxy @TraceBlockFetchServer) pTraceBlockchainTime
  traceChainDB <- proxyLastToEither (Proxy @TraceChainDB) pTraceChainDB
  traceChainSyncBlockServer <- proxyLastToEither (Proxy @TraceChainSyncClient) pTraceChainSyncBlockServer
  traceChainSyncClient <- proxyLastToEither (Proxy @TraceChainSyncBlockServer) pTraceChainSyncClient
  traceChainSyncHeaderServer <- proxyLastToEither (Proxy @TraceChainSyncHeaderServer) pTraceChainSyncHeaderServer
  traceChainSyncProtocol <- proxyLastToEither (Proxy @TraceChainSyncProtocol) pTraceChainSyncProtocol
  traceConnectionManager <- proxyLastToEither (Proxy @TraceConnectionManager) pTraceConnectionManager
  traceConnectionManagerCounters <- proxyLastToEither (Proxy @TraceConnectionManagerCounters) pTraceConnectionManagerCounters
  traceConnectionManagerTransitions <- proxyLastToEither (Proxy @TraceConnectionManagerTransitions) pTraceConnectionManagerTransitions
  traceDebugPeerSelectionInitiatorTracer <- proxyLastToEither (Proxy @DebugPeerSelectionInitiator) pTraceDebugPeerSelectionInitiatorTracer
  traceDebugPeerSelectionInitiatorResponderTracer <- proxyLastToEither (Proxy @DebugPeerSelectionInitiatorResponder) pTraceDebugPeerSelectionInitiatorResponderTracer
  traceDiffusionInitialization <- proxyLastToEither (Proxy @TraceDiffusionInitialization) pTraceDiffusionInitialization
  traceDnsResolver <- proxyLastToEither (Proxy @TraceDnsResolver) pTraceDnsResolver
  traceDnsSubscription <- proxyLastToEither (Proxy @TraceDnsSubscription) pTraceDnsSubscription
  traceErrorPolicy <- proxyLastToEither (Proxy @TraceErrorPolicy) pTraceErrorPolicy
  traceForge <- proxyLastToEither (Proxy @TraceForge) pTraceForge
  traceForgeStateInfo <- proxyLastToEither (Proxy @TraceForgeStateInfo) pTraceForgeStateInfo
  traceHandshake <- proxyLastToEither (Proxy @TraceHandshake) pTraceHandshake
  traceInboundGovernor <- proxyLastToEither (Proxy @TraceIpSubscription) pTraceInboundGovernor
  traceInboundGovernorCounters <- proxyLastToEither (Proxy @TraceKeepAliveClient) pTraceInboundGovernorCounters
  traceInboundGovernorTransitions <- proxyLastToEither (Proxy @TraceInboundGovernorTransitions) pTraceInboundGovernorTransitions
  traceIpSubscription <- proxyLastToEither (Proxy @TraceLedgerPeers) pTraceIpSubscription
  traceKeepAliveClient <- proxyLastToEither (Proxy @TraceLocalChainSyncProtocol) pTraceKeepAliveClient
  traceLedgerPeers <- proxyLastToEither (Proxy @TraceLocalConnectionManager) pTraceLedgerPeers
  traceLocalChainSyncProtocol <- proxyLastToEither (Proxy @TraceLocalErrorPolicy) pTraceLocalChainSyncProtocol
  traceLocalConnectionManager <- proxyLastToEither (Proxy @TraceLocalHandshake) pTraceLocalConnectionManager
  traceLocalErrorPolicy <- proxyLastToEither (Proxy @TraceLocalInboundGovernor) pTraceLocalErrorPolicy
  traceLocalHandshake <- proxyLastToEither (Proxy @TraceLocalRootPeers) pTraceLocalHandshake
  traceLocalInboundGovernor <- proxyLastToEither (Proxy @TraceLocalServer) pTraceLocalInboundGovernor
  traceLocalMux <- proxyLastToEither (Proxy @TraceLocalStateQueryProtocol) pTraceLocalMux
  traceLocalRootPeers <- proxyLastToEither (Proxy @TraceLocalTxSubmissionProtocol) pTraceLocalRootPeers
  traceLocalServer <- proxyLastToEither (Proxy @TraceLocalTxSubmissionServer) pTraceLocalServer
  traceLocalTxMonitorProtocol <- proxyLastToEither (Proxy @TraceLocalTxMonitorProtocol) pTraceLocalTxMonitorProtocol
  traceLocalStateQueryProtocol <- proxyLastToEither (Proxy @TraceMempool) pTraceLocalStateQueryProtocol
  traceLocalTxSubmissionProtocol <- proxyLastToEither (Proxy @TraceMux) pTraceLocalTxSubmissionProtocol
  traceLocalTxSubmissionServer <- proxyLastToEither (Proxy @TraceLocalMux) pTraceLocalTxSubmissionServer
  traceMempool <- proxyLastToEither (Proxy @TracePeerSelection) pTraceMempool
  traceMux <- proxyLastToEither (Proxy @TracePeerSelectionCounters) pTraceMux
  tracePeerSelection <- proxyLastToEither (Proxy @TracePeerSelectionActions) pTracePeerSelection
  tracePeerSelectionCounters <- proxyLastToEither (Proxy @TracePublicRootPeers) pTracePeerSelectionCounters
  tracePeerSelectionActions <- proxyLastToEither (Proxy @TraceServer) pTracePeerSelectionActions
  tracePublicRootPeers <- proxyLastToEither (Proxy @TraceInboundGovernor) pTracePublicRootPeers
  traceServer <- proxyLastToEither (Proxy @TraceInboundGovernorCounters) pTraceServer
  traceTxInbound <- proxyLastToEither (Proxy @TraceTxInbound) pTraceTxInbound
  traceTxOutbound <- proxyLastToEither (Proxy @TraceTxOutbound) pTraceTxOutbound
  traceTxSubmissionProtocol <- proxyLastToEither (Proxy @TraceTxSubmissionProtocol) pTraceTxSubmissionProtocol
  traceTxSubmission2Protocol <- proxyLastToEither (Proxy @TraceTxSubmission2Protocol) pTraceTxSubmission2Protocol
  Right $ TracingOnLegacy $ TraceSelection
            { traceVerbosity
            , traceAcceptPolicy
            , traceBlockFetchClient
            , traceBlockFetchDecisions
            , traceBlockFetchProtocol
            , traceBlockFetchProtocolSerialised
            , traceBlockFetchServer
            , traceBlockchainTime
            , traceChainDB
            , traceChainSyncBlockServer
            , traceChainSyncClient
            , traceChainSyncHeaderServer
            , traceChainSyncProtocol
            , traceConnectionManager
            , traceConnectionManagerCounters
            , traceConnectionManagerTransitions
            , traceDebugPeerSelectionInitiatorTracer
            , traceDebugPeerSelectionInitiatorResponderTracer
            , traceDiffusionInitialization
            , traceDnsResolver
            , traceDnsSubscription
            , traceErrorPolicy
            , traceForge
            , traceForgeStateInfo
            , traceHandshake
            , traceInboundGovernor
            , traceInboundGovernorCounters
            , traceInboundGovernorTransitions
            , traceIpSubscription
            , traceKeepAliveClient
            , traceLedgerPeers
            , traceLocalChainSyncProtocol
            , traceLocalConnectionManager
            , traceLocalErrorPolicy
            , traceLocalHandshake
            , traceLocalInboundGovernor
            , traceLocalMux
            , traceLocalRootPeers
            , traceLocalServer
            , traceLocalStateQueryProtocol
            , traceLocalTxMonitorProtocol
            , traceLocalTxSubmissionProtocol
            , traceLocalTxSubmissionServer
            , traceMempool
            , traceMux
            , tracePeerSelection
            , tracePeerSelectionCounters
            , tracePeerSelectionActions
            , tracePublicRootPeers
            , traceServer
            , traceTxInbound
            , traceTxOutbound
            , traceTxSubmissionProtocol
            , traceTxSubmission2Protocol
            }

proxyLastToEither :: KnownSymbol name => Proxy name -> Last a -> Either Text a
proxyLastToEither name (Last x) =
  maybe (Left $ "Default value not specified for " <> proxyName name) Right x

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

