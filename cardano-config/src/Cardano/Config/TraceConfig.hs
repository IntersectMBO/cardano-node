{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Cardano.Config.TraceConfig
  (
    TraceConfig
  , traceConfigEnabled
  , traceConfigVerbosity
  , traceConfigMute
  , traceConfigParser

  , TraceId
  , traceEnabled

  -- Per-trace toggles, alpha-sorted.
  -- Note, the TraceConfig' type itself MUST not be exported.
  -- The explanation is provided at its declaration site.
  , traceAcceptPolicy
  , traceBlockFetchClient
  , traceBlockFetchDecisions
  , traceBlockFetchProtocol
  , traceBlockFetchProtocolSerialised
  , traceBlockFetchServer
  , traceChainDB
  , traceChainSyncClient
  , traceChainSyncBlockServer
  , traceChainSyncHeaderServer
  , traceChainSyncProtocol
  , traceDnsResolver
  , traceDnsSubscription
  , traceErrorPolicy
  , traceForge
  , traceForgeState
  , traceHandshake
  , traceIpSubscription
  , traceLocalChainSyncProtocol
  , traceLocalErrorPolicy
  , traceLocalHandshake
  , traceLocalTxSubmissionProtocol
  , traceLocalTxSubmissionServer
  , traceLocalStateQueryProtocol
  , traceMempool
  , traceMux
  , traceTxInbound
  , traceTxOutbound
  , traceTxSubmissionProtocol
  )
where

import           Prelude (Show(..))
import           Cardano.Prelude hiding (show)

import           Data.Aeson
import           Data.Aeson.Types (Parser)

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))

import           Cardano.Config.Orphanage ()


newtype TraceConfig = TraceConfig TraceConfig'

instance Show TraceConfig where
  show (TraceConfig tr) = show tr

type TraceId = TraceConfig' -> Bool

-- | Tracing configuration -- either disabled completely,
--   or carries verbosity + fine-grained toggles.
--
--   Note, how this type has exported partial accessors,
--   but also, how these cannot be used outside the module,
--   because this type itself is not exported.
--   This, therefore confines the partiality to the module.
--
--   The tradeoff, is that instead of exporting accessors as tags,
--   we'd need to declare an ADT to enumerate the tracers,
--   and that enumeration would also have had to be maintained
--   -- thereby severely reducing benefits of totalisation.
--
--   Finally -- this type must not be exported.
--   If it is not clear why, please re-read the above.
data TraceConfig'

  -- | Intended to mirror Configuration with TurnOnLogging: False.
  = TracingDisabled

  -- | Detailed tracing options.  Except for verbosity, eaach option controls
  --   a particular tracer.
  | TraceOptions
  -- Common part.
  { traceVerbosity :: !TracingVerbosity

  -- Per-trace toggles, alpha-sorted.
  , traceAcceptPolicy :: !Bool
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
  , traceForgeState :: !Bool
  , traceHandshake :: !Bool
  , traceIpSubscription :: !Bool
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

traceConfigMute :: TraceConfig
traceConfigMute = TraceConfig TracingDisabled

traceConfigParser :: Object -> Parser TraceConfig
traceConfigParser v =
  fmap TraceConfig $
   TraceOptions
     <$> v .:? "TracingVerbosity" .!= NormalVerbosity
     -- Per-trace toggles, alpha-sorted.
     <*> v .:? "TraceAcceptPolicy" .!= False
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
     <*> v .:? "TraceForgeState" .!= True
     <*> v .:? "TraceHandshake" .!= False
     <*> v .:? "TraceIpSubscription" .!= True
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
     <*> v .:? "TraceTxSubmissionProtocol" .!= False

-- | Is tracing enabled at all?
traceConfigEnabled :: TraceConfig -> Bool
traceConfigEnabled = \case
  TraceConfig TracingDisabled -> False
  TraceConfig TraceOptions{} -> True

-- | Is a particular trace enabled in the config?
traceEnabled :: TraceConfig -> TraceId -> Bool
traceEnabled (TraceConfig TracingDisabled) _ = False
traceEnabled (TraceConfig topts@TraceOptions{}) proj = proj topts
{-# INLINE traceEnabled #-}

traceConfigVerbosity :: TraceConfig -> TracingVerbosity
traceConfigVerbosity = \case
  TraceConfig TracingDisabled -> MinimalVerbosity
  TraceConfig TraceOptions{traceVerbosity} -> traceVerbosity
