{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracing.Config
  ( TraceOptions (..)
  , TraceSelection (..)
  , traceConfigParser
  ) where

import           Cardano.Prelude hiding (show)

import           Data.Aeson
import           Data.Aeson.Types (Parser)

import           Cardano.BM.Tracing (TracingVerbosity (..))

import           Cardano.Node.Orphans ()


data TraceOptions
  = TracingOff
  | TracingOn TraceSelection
  deriving (Eq, Show)

data TraceSelection
  = TraceSelection
  { traceVerbosity :: !TracingVerbosity

  -- Per-trace toggles, alpha-sorted.
  , traceAcceptPolicy :: !Bool
  , traceBlockchainTime :: !Bool
  , traceBlockFetchClient :: !Bool
  , traceBlockFetchDecisions :: !Bool
  , traceBlockFetchProtocol :: !Bool
  , traceBlockFetchProtocolSerialised :: !Bool
  , traceBlockFetchServer :: !Bool
  , traceChainDB :: !Bool
  , traceChainSyncClient :: !Bool
  , traceChainSyncBlockServer :: !Bool
  , traceChainSyncHeaderServer :: !Bool
  , traceChainSyncProtocol :: !Bool
  , traceDnsResolver :: !Bool
  , traceDnsSubscription :: !Bool
  , traceErrorPolicy :: !Bool
  , traceForge :: !Bool
  , traceForgeStateInfo :: !Bool
  , traceHandshake :: !Bool
  , traceIpSubscription :: !Bool
  , traceKeepAliveClient :: !Bool
  , traceLocalChainSyncProtocol :: !Bool
  , traceLocalErrorPolicy :: !Bool
  , traceLocalHandshake :: !Bool
  , traceLocalTxSubmissionProtocol :: !Bool
  , traceLocalTxSubmissionServer :: !Bool
  , traceLocalStateQueryProtocol :: !Bool
  , traceMempool :: !Bool
  , traceMux :: !Bool
  , traceTxInbound :: !Bool
  , traceTxOutbound :: !Bool
  , traceTxSubmissionProtocol :: !Bool
  } deriving (Eq, Show)


traceConfigParser :: Object -> Parser TraceOptions
traceConfigParser v =
  TracingOn <$> (TraceSelection
    <$> v .:? "TracingVerbosity" .!= NormalVerbosity
    -- Per-trace toggles, alpha-sorted.
    <*> v .:? "TraceAcceptPolicy" .!= False
    <*> v .:? "TraceBlockchainTime" .!= False
    <*> v .:? "TraceBlockFetchClient" .!= False
    <*> v .:? "TraceBlockFetchDecisions" .!= True
    <*> v .:? "TraceBlockFetchProtocol" .!= False
    <*> v .:? "TraceBlockFetchProtocolSerialised" .!= False
    <*> v .:? "TraceBlockFetchServer" .!= False
    <*> v .:? "TraceChainDb" .!= True
    <*> v .:? "TraceChainSyncClient" .!= True
    <*> v .:? "TraceChainSyncBlockServer" .!= False
    <*> v .:? "TraceChainSyncHeaderServer" .!= False
    <*> v .:? "TraceChainSyncProtocol" .!= False
    <*> v .:? "TraceDNSResolver" .!= False
    <*> v .:? "TraceDNSSubscription" .!= True
    <*> v .:? "TraceErrorPolicy" .!= True
    <*> v .:? "TraceForge" .!= True
    <*> v .:? "TraceForgeStateInfo" .!= True
    <*> v .:? "TraceHandshake" .!= False
    <*> v .:? "TraceIpSubscription" .!= True
    <*> v .:? "TraceKeepAliveClient" .!= False
    <*> v .:? "TraceLocalChainSyncProtocol" .!= False
    <*> v .:? "TraceLocalErrorPolicy" .!= True
    <*> v .:? "TraceLocalHandshake" .!= False
    <*> v .:? "TraceLocalTxSubmissionProtocol" .!= False
    <*> v .:? "TraceLocalTxSubmissionServer" .!= False
    <*> v .:? "TraceLocalStateQueryProtocol" .!= False
    <*> v .:? "TraceMempool" .!= True
    <*> v .:? "TraceMux" .!= True
    <*> v .:? "TraceTxInbound" .!= False
    <*> v .:? "TraceTxOutbound" .!= False
    <*> v .:? "TraceTxSubmissionProtocol" .!= False)
