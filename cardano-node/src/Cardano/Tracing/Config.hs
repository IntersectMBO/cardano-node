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
  ) where

import           Cardano.Prelude

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Text (pack)

import           Cardano.BM.Tracing (TracingVerbosity (..))

import           Cardano.Node.Orphans ()


data TraceOptions
  = TracingOff
  | TracingOn TraceSelection
  deriving (Eq, Show)

type TraceAcceptPolicy = ("TraceAcceptPolicy" :: Symbol)
type TraceBlockFetchClient = ("TraceBlockFetchClient" :: Symbol)
type TraceBlockFetchDecisions = ("TraceBlockFetchDecisions" :: Symbol)
type TraceBlockFetchProtocol = ("TraceBlockFetchProtocol" :: Symbol)
type TraceBlockFetchProtocolSerialised = ("TraceBlockFetchProtocolSerialised" :: Symbol)
type TraceBlockFetchServer = ("TraceBlockFetchServer" :: Symbol)
type TraceBlockchainTime = ("TraceBlockchainTime" :: Symbol)
type TraceChainDB = ("TraceChainDB" :: Symbol)
type TraceChainSyncBlockServer = ("TraceChainSyncBlockServer" :: Symbol)
type TraceChainSyncClient = ("TraceChainSyncClient" :: Symbol)
type TraceChainSyncHeaderServer = ("TraceChainSyncHeaderServer" :: Symbol)
type TraceChainSyncProtocol = ("TraceChainSyncProtocol" :: Symbol)
type TraceConnectionManager = ("TraceConnectionManager" :: Symbol)
type DebugPeerSelectionInitiator = ("DebugPeerSelectionInitiator" :: Symbol)
type DebugPeerSelectionInitiatorResponder = ("DebugPeerSelectionInitiatorResponder" :: Symbol)
type TraceDiffusionInitialization = ("TraceDiffusionInitialization" :: Symbol)
type TraceDnsResolver = ("TraceDnsResolver" :: Symbol)
type TraceForge = ("TraceForge" :: Symbol)
type TraceForgeStateInfo = ("TraceForgeStateInfo" :: Symbol)
type TraceHandshake = ("TraceHandshake" :: Symbol)
type TraceKeepAliveClient = ("TraceKeepAliveClient" :: Symbol)
type TraceLocalChainSyncProtocol = ("TraceLocalChainSyncProtocol" :: Symbol)
type TraceLocalConnectionManager = ("TraceLocalConnectionManager" :: Symbol)
type TraceLocalHandshake = ("TraceLocalHandshake" :: Symbol)
type TraceLocalRootPeers = ("TraceLocalRootPeers" :: Symbol)
type TraceLocalServer = ("TraceLocalServer" :: Symbol)
type TraceLocalStateQueryProtocol = ("TraceLocalStateQueryProtocol" :: Symbol)
type TraceLocalTxSubmissionProtocol = ("TraceLocalTxSubmissionProtocol" :: Symbol)
type TraceLocalTxSubmissionServer = ("TraceLocalTxSubmissionServer" :: Symbol)
type TraceMempool = ("TraceMempool" :: Symbol)
type TraceMux = ("TraceMux" :: Symbol)
type TracePeerSelection = ("TracePeerSelection" :: Symbol)
type TracePeerSelectionActions = ("TracePeerSelectionActions" :: Symbol)
type TracePublicRootPeers = ("TracePublicRootPeers" :: Symbol)
type TraceServer = ("TraceServer" :: Symbol)
type TraceTxInbound = ("TraceTxInbound" :: Symbol)
type TraceTxOutbound = ("TraceTxOutbound" :: Symbol)
type TraceTxSubmissionProtocol = ("TraceTxSubmissionProtocol" :: Symbol)

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
  , traceDebugPeerSelectionInitiatorTracer :: OnOff DebugPeerSelectionInitiator
  , traceDebugPeerSelectionInitiatorResponderTracer :: OnOff DebugPeerSelectionInitiatorResponder
  , traceDiffusionInitialization :: OnOff TraceDiffusionInitialization
  , traceDnsResolver :: OnOff TraceDnsResolver
  , traceForge :: OnOff TraceForge
  , traceForgeStateInfo :: OnOff TraceForgeStateInfo
  , traceHandshake :: OnOff TraceHandshake
  , traceKeepAliveClient :: OnOff TraceKeepAliveClient
  , traceLocalChainSyncProtocol :: OnOff TraceLocalChainSyncProtocol
  , traceLocalConnectionManager :: OnOff TraceLocalConnectionManager
  , traceLocalHandshake :: OnOff TraceLocalHandshake
  , traceLocalRootPeers :: OnOff TraceLocalRootPeers
  , traceLocalServer :: OnOff TraceLocalServer
  , traceLocalStateQueryProtocol :: OnOff TraceLocalStateQueryProtocol
  , traceLocalTxSubmissionProtocol :: OnOff TraceLocalTxSubmissionProtocol
  , traceLocalTxSubmissionServer :: OnOff TraceLocalTxSubmissionServer
  , traceMempool :: OnOff TraceMempool
  , traceMux :: OnOff TraceMux
  , tracePeerSelection :: OnOff TracePeerSelection
  , tracePeerSelectionActions :: OnOff TracePeerSelectionActions
  , tracePublicRootPeers :: OnOff TracePublicRootPeers
  , traceServer :: OnOff TraceServer
  , traceTxInbound :: OnOff TraceTxInbound
  , traceTxOutbound :: OnOff TraceTxOutbound
  , traceTxSubmissionProtocol :: OnOff TraceTxSubmissionProtocol
  } deriving (Eq, Show)


traceConfigParser :: Object -> Parser TraceOptions
traceConfigParser v =
  -- TODO: By using 'TypeApplication' we can cut half of the lines below!
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
      debugPeerSelectionInitiator :: OnOff DebugPeerSelectionInitiator
      debugPeerSelectionInitiator = OnOff False
      debugPeerSelectionInitiatorResponder :: OnOff DebugPeerSelectionInitiatorResponder
      debugPeerSelectionInitiatorResponder = OnOff False
      diffusionInitialization :: OnOff TraceDiffusionInitialization
      diffusionInitialization = OnOff False
      dnsResolver :: OnOff TraceDnsResolver
      dnsResolver = OnOff False
      forge :: OnOff TraceForge
      forge = OnOff True
      forgeStateInfo :: OnOff TraceForgeStateInfo
      forgeStateInfo = OnOff True
      handshake :: OnOff TraceHandshake
      handshake = OnOff False
      keepAliveClient :: OnOff TraceKeepAliveClient
      keepAliveClient = OnOff False
      localChainSyncProtocol :: OnOff TraceLocalChainSyncProtocol
      localChainSyncProtocol = OnOff False
      localConnectionManager :: OnOff TraceLocalConnectionManager
      localConnectionManager = OnOff False
      localHandshake :: OnOff TraceLocalHandshake
      localHandshake = OnOff False
      localRootPeers :: OnOff TraceLocalRootPeers
      localRootPeers = OnOff False
      localServer = OnOff @TraceLocalServer False
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
      txSubmissionProtocol = OnOff False in

  TracingOn <$> (TraceSelection
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
    <*> v .:? getName debugPeerSelectionInitiator
                       .!= debugPeerSelectionInitiator
    <*> v .:? getName debugPeerSelectionInitiatorResponder
                       .!= debugPeerSelectionInitiatorResponder
    <*> v .:? getName diffusionInitialization .!= diffusionInitialization
    <*> v .:? getName dnsResolver .!= dnsResolver
    <*> v .:? getName forge .!= forge
    <*> v .:? getName forgeStateInfo .!= forgeStateInfo
    <*> v .:? getName handshake .!= handshake
    <*> v .:? getName keepAliveClient .!= keepAliveClient
    <*> v .:? getName localChainSyncProtocol .!= localChainSyncProtocol
    <*> v .:? getName localConnectionManager .!= localConnectionManager
    <*> v .:? getName localHandshake .!= localHandshake
    <*> v .:? getName localRootPeers .!= localRootPeers
    <*> v .:? getName localServer .!= localServer
    <*> v .:? getName localStateQueryProtocol .!= localStateQueryProtocol
    <*> v .:? getName localTxSubmissionProtocol .!= localTxSubmissionProtocol
    <*> v .:? getName localTxSubmissionServer .!= localTxSubmissionServer
    <*> v .:? getName mempool .!= mempool
    <*> v .:? getName mux .!= mux
    <*> v .:? getName peerSelection .!= peerSelection
    <*> v .:? getName peerSelectionActions .!= peerSelectionActions
    <*> v .:? getName publicRootPeers .!= publicRootPeers
    <*> v .:? getName server .!= server
    <*> v .:? getName txInbound .!= txInbound
    <*> v .:? getName txOutbound .!= txOutbound
    <*> v .:? getName txSubmissionProtocol .!= txSubmissionProtocol)
