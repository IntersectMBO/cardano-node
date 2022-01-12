{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Tracing.Config
  ( TraceOptions (..)
  , TraceSelection (..)
  , traceConfigParser
  , OnOff (..)

  -- * Trace symbols
  , TraceConnectionManagerCounters
  , TracePeerSelectionCounters
  , TraceInboundGovernorCounters
  ) where

import           Cardano.Prelude

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Text (pack)

import           Cardano.BM.Tracing (TracingVerbosity (..))
import           Cardano.Node.Orphans ()


data TraceOptions
  = TracingOff
  | TracingOnLegacy TraceSelection
  | TraceDispatcher TraceSelection
  deriving (Eq, Show)

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
type TraceTxInbound = ("TraceTxInbound" :: Symbol)
type TraceTxOutbound = ("TraceTxOutbound" :: Symbol)
type TraceTxSubmissionProtocol = ("TraceTxSubmissionProtocol" :: Symbol)
type TraceTxSubmission2Protocol = ("TraceTxSubmission2Protocol" :: Symbol)

newtype OnOff (name :: Symbol) = OnOff { isOn :: Bool } deriving (Eq, Show)

instance FromJSON (OnOff a) where
    parseJSON (Data.Aeson.Bool b)= return $ OnOff b
    parseJSON _ = mzero

getName :: forall name. KnownSymbol name => OnOff name -> Text
getName _ = pack (symbolVal (Proxy @name))

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


traceConfigParser :: Object -> (TraceSelection -> TraceOptions) -> Parser TraceOptions
traceConfigParser v ctor =
  let acceptPolicy :: OnOff TraceAcceptPolicy
      acceptPolicy = OnOff False
      blockFetchClient :: OnOff TraceBlockFetchClient
      blockFetchClient = OnOff False
      blockFetchDecisions :: OnOff TraceBlockFetchDecisions
      blockFetchDecisions = OnOff True
      blockFetchProtocol :: OnOff TraceBlockFetchProtocol
      blockFetchProtocol = OnOff False
      blockFetchProtocolSerialised :: OnOff TraceBlockFetchProtocolSerialised
      blockFetchProtocolSerialised = OnOff False
      blockFetchServer :: OnOff TraceBlockFetchServer
      blockFetchServer = OnOff False
      blockchainTime :: OnOff TraceBlockchainTime
      blockchainTime = OnOff False
      chainDB :: OnOff TraceChainDB
      chainDB = OnOff True
      chainSyncBlockServer :: OnOff TraceChainSyncBlockServer
      chainSyncBlockServer = OnOff False
      chainSyncClient :: OnOff TraceChainSyncClient
      chainSyncClient = OnOff True
      chainSyncHeaderServer :: OnOff TraceChainSyncHeaderServer
      chainSyncHeaderServer = OnOff False
      chainSyncProtocol :: OnOff TraceChainSyncProtocol
      chainSyncProtocol = OnOff False
      connectionManager :: OnOff TraceConnectionManager
      connectionManager = OnOff False
      connectionManagerCounters :: OnOff TraceConnectionManagerCounters
      connectionManagerCounters = OnOff True
      debugPeerSelectionInitiator :: OnOff DebugPeerSelectionInitiator
      debugPeerSelectionInitiator = OnOff False
      debugPeerSelectionInitiatorResponder :: OnOff DebugPeerSelectionInitiatorResponder
      debugPeerSelectionInitiatorResponder = OnOff False
      diffusionInitialization :: OnOff TraceDiffusionInitialization
      diffusionInitialization = OnOff False
      dnsResolver :: OnOff TraceDnsResolver
      dnsResolver = OnOff False
      dnsSubscription :: OnOff TraceDnsSubscription
      dnsSubscription = OnOff True
      errorPolicy :: OnOff TraceErrorPolicy
      errorPolicy = OnOff True
      forge :: OnOff TraceForge
      forge = OnOff True
      forgeStateInfo :: OnOff TraceForgeStateInfo
      forgeStateInfo = OnOff True
      handshake :: OnOff TraceHandshake
      handshake = OnOff False
      inboundGovernor :: OnOff TraceInboundGovernor
      inboundGovernor = OnOff False
      inboundGovernorCounters :: OnOff TraceInboundGovernorCounters
      inboundGovernorCounters = OnOff True
      ipSubscription :: OnOff TraceIpSubscription
      ipSubscription = OnOff True
      keepAliveClient :: OnOff TraceKeepAliveClient
      keepAliveClient = OnOff False
      ledgerPeers :: OnOff TraceLedgerPeers
      ledgerPeers = OnOff False
      localChainSyncProtocol :: OnOff TraceLocalChainSyncProtocol
      localChainSyncProtocol = OnOff False
      localConnectionManager :: OnOff TraceLocalConnectionManager
      localConnectionManager = OnOff False
      localErrorPolicy :: OnOff TraceLocalErrorPolicy
      localErrorPolicy = OnOff True
      localHandshake :: OnOff TraceLocalHandshake
      localHandshake = OnOff False
      localInboundGovernor :: OnOff TraceLocalInboundGovernor
      localInboundGovernor = OnOff False
      localMux :: OnOff TraceLocalMux
      localMux = OnOff False
      localRootPeers :: OnOff TraceLocalRootPeers
      localRootPeers = OnOff False
      localServer :: OnOff TraceLocalServer
      localServer = OnOff False
      localStateQueryProtocol :: OnOff TraceLocalStateQueryProtocol
      localStateQueryProtocol = OnOff False
      localTxSubmissionProtocol :: OnOff TraceLocalTxSubmissionProtocol
      localTxSubmissionProtocol = OnOff False
      localTxSubmissionServer :: OnOff TraceLocalTxSubmissionServer
      localTxSubmissionServer = OnOff False
      mempool :: OnOff TraceMempool
      mempool = OnOff True
      mux :: OnOff TraceMux
      mux = OnOff True
      peerSelection :: OnOff TracePeerSelection
      peerSelection = OnOff False
      peerSelectionCounters :: OnOff TracePeerSelectionCounters
      peerSelectionCounters = OnOff True
      peerSelectionActions :: OnOff TracePeerSelectionActions
      peerSelectionActions = OnOff False
      publicRootPeers :: OnOff TracePublicRootPeers
      publicRootPeers = OnOff False
      server :: OnOff TraceServer
      server = OnOff False
      txInbound :: OnOff TraceTxInbound
      txInbound = OnOff False
      txOutbound :: OnOff TraceTxOutbound
      txOutbound = OnOff False
      txSubmissionProtocol :: OnOff TraceTxSubmissionProtocol
      txSubmissionProtocol = OnOff False
      txSubmission2Protocol :: OnOff TraceTxSubmission2Protocol
      txSubmission2Protocol = OnOff False in

  ctor <$> (TraceSelection
    <$> v .:? "TracingVerbosity" .!= NormalVerbosity
    -- Per-trace toggles, alpha-sorted.
    <*> v .:? getName acceptPolicy .!= acceptPolicy
    <*> v .:? getName blockFetchClient .!=  blockFetchClient
    <*> v .:? getName blockFetchDecisions .!= blockFetchDecisions
    <*> v .:? getName blockFetchProtocol .!= blockFetchProtocol
    <*> v .:? getName blockFetchProtocolSerialised .!= blockFetchProtocolSerialised
    <*> v .:? getName blockFetchServer .!= blockFetchServer
    <*> v .:? getName blockchainTime .!= blockchainTime
    <*> v .:? getName chainDB .!=  chainDB
    <*> v .:? getName chainSyncBlockServer .!= chainSyncBlockServer
    <*> v .:? getName chainSyncClient .!= chainSyncClient
    <*> v .:? getName chainSyncHeaderServer .!= chainSyncHeaderServer
    <*> v .:? getName chainSyncProtocol .!= chainSyncProtocol
    <*> v .:? getName connectionManager .!= connectionManager
    <*> v .:? getName connectionManagerCounters .!= connectionManagerCounters
    <*> v .:? getName debugPeerSelectionInitiator
                       .!= debugPeerSelectionInitiator
    <*> v .:? getName debugPeerSelectionInitiatorResponder
                       .!= debugPeerSelectionInitiatorResponder
    <*> v .:? getName diffusionInitialization .!= diffusionInitialization
    <*> v .:? getName dnsResolver .!= dnsResolver
    <*> v .:? getName dnsSubscription .!= dnsSubscription
    <*> v .:? getName errorPolicy .!=  errorPolicy
    <*> v .:? getName forge .!= forge
    <*> v .:? getName forgeStateInfo .!= forgeStateInfo
    <*> v .:? getName handshake .!= handshake
    <*> v .:? getName inboundGovernor .!= inboundGovernor
    <*> v .:? getName inboundGovernorCounters .!= inboundGovernorCounters
    <*> v .:? getName ipSubscription .!= ipSubscription
    <*> v .:? getName keepAliveClient .!= keepAliveClient
    <*> v .:? getName ledgerPeers .!= ledgerPeers
    <*> v .:? getName localChainSyncProtocol .!= localChainSyncProtocol
    <*> v .:? getName localConnectionManager .!= localConnectionManager
    <*> v .:? getName localErrorPolicy .!= localErrorPolicy
    <*> v .:? getName localHandshake .!= localHandshake
    <*> v .:? getName localInboundGovernor .!= localInboundGovernor
    <*> v .:? getName localMux .!= localMux
    <*> v .:? getName localRootPeers .!= localRootPeers
    <*> v .:? getName localServer .!= localServer
    <*> v .:? getName localStateQueryProtocol .!= localStateQueryProtocol
    <*> v .:? getName localTxSubmissionProtocol .!= localTxSubmissionProtocol
    <*> v .:? getName localTxSubmissionServer .!= localTxSubmissionServer
    <*> v .:? getName mempool .!= mempool
    <*> v .:? getName mux .!= mux
    <*> v .:? getName peerSelection .!= peerSelection
    <*> v .:? getName peerSelectionCounters .!= peerSelectionCounters
    <*> v .:? getName peerSelectionActions .!= peerSelectionActions
    <*> v .:? getName publicRootPeers .!= publicRootPeers
    <*> v .:? getName server .!= server
    <*> v .:? getName txInbound .!= txInbound
    <*> v .:? getName txOutbound .!= txOutbound
    <*> v .:? getName txSubmissionProtocol .!= txSubmissionProtocol
    <*> v .:? getName txSubmission2Protocol .!= txSubmission2Protocol)
