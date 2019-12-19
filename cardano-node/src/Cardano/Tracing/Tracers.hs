{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Tracing.Tracers
  ( Tracers (..)
  , TraceConstraints
  , TraceOptions(..)
  , mkTracers
  , nullTracers
  , withTip
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import           Control.Monad.Class.MonadSTM
import           Control.Tracer

import           Codec.CBOR.Read (DeserialiseFailure)
import           Data.Functor.Const (Const (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)
import qualified Network.Socket as Socket (SockAddr)
import           Network.Mux.Types (WithMuxBearer, MuxTrace)

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.Tracing
import           Cardano.BM.Trace (traceNamedObject)
import           Cardano.BM.Data.Tracer (WithSeverity (..), addName,
                     annotateSeverity)
import           Cardano.BM.Data.Transformers

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
                    (GenTx, TraceEventMempool (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                     ProtocolTracers' (..), nullProtocolTracers)
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Point,
                     blockNo, unBlockNo, unSlotNo)
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import           Ouroboros.Network.NodeToNode (NodeToNodeProtocols,
                     WithAddr, ErrorPolicyTrace)
import           Ouroboros.Network.Point (fromWithOrigin)
import           Ouroboros.Network.Subscription

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Cardano.Config.Protocol (TraceConstraints)
import           Cardano.Config.Types
import           Cardano.Tracing.ToObjectOrphans
import           Cardano.Tracing.MicroBenchmarking

import           Control.Tracer.Transformers

data Tracers peer blk = Tracers {
      -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
      chainDBTracer         :: Tracer IO (WithTip blk (ChainDB.TraceEvent blk))

      -- | Consensus-specific tracers.
    , consensusTracers      :: Consensus.Tracers IO peer blk

      -- | Tracers for the protocol messages.
    , protocolTracers       :: ProtocolTracers IO peer blk DeserialiseFailure

      -- | Trace the IP subscription manager (flag '--trace-ip-subscription' will turn on textual output)
    , ipSubscriptionTracer  :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))

      -- | Trace the DNS subscription manager (flag '--trace-dns-subscription' will turn on textual output)
    , dnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))

      -- | Trace the DNS resolver (flag '--trace-dns-resolver' will turn on textual output)
    , dnsResolverTracer     :: Tracer IO (WithDomainName DnsTrace)

      -- | Trace error policy resolution (flag '--trace-error-policy' will turn on textual output)
    , errorPolicyTracer     :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)

      -- | Trace the Mux (flag --trace-mux' will turn on textual output)
    , muxTracer             :: Tracer IO (WithMuxBearer peer (MuxTrace NodeToNodeProtocols))
    }

data ForgeTracers = ForgeTracers
  { ftForged :: Trace IO Text
  , ftCouldNotForge :: Trace IO Text
  , ftAdopted :: Trace IO Text
  , ftDidntAdoptBlock :: Trace IO Text
  , ftForgedInvalid :: Trace IO Text
  }

nullTracers :: Tracers peer blk
nullTracers = Tracers {
      chainDBTracer = nullTracer,
      consensusTracers = Consensus.nullTracers,
      protocolTracers = nullProtocolTracers,
      ipSubscriptionTracer = nullTracer,
      dnsSubscriptionTracer = nullTracer,
      dnsResolverTracer = nullTracer,
      errorPolicyTracer = nullTracer,
      muxTracer = nullTracer
    }

-- | Smart constructor of 'NodeTraces'.
--
mkTracers :: forall peer blk.
              ( ProtocolLedgerView blk
              , TraceConstraints blk
              , Show peer
              )
           => TraceOptions
           -> Trace IO Text
           -> IO (Tracers peer blk)
mkTracers traceOptions tracer = do
  -- We probably don't want to pay the extra IO cost per-counter-increment. -- sk
  staticMetaCC <- mkLOMeta Critical Confidential
  let name :: [Text] = ["metrics", "Forge"]
  forgeTracers <-
    ForgeTracers
      <$> (counting $ liftCounting staticMetaCC name "forged" tracer)
      <*> (counting $ liftCounting staticMetaCC name "could-not-forge" tracer)
      <*> (counting $ liftCounting staticMetaCC name "adopted" tracer)
      <*> (counting $ liftCounting staticMetaCC name "didnt-adopt" tracer)
      <*> (counting $ liftCounting staticMetaCC name "forged-invalid" tracer)

  -- The outcomes we want to measure, the outcome extractor
  -- for measuring the time it takes a transaction to get into
  -- a block.
  -- txsOutcomeExtractor <- mkOutcomeExtractor

  pure Tracers
    { chainDBTracer
        = tracerOnOff (traceChainDB traceOptions)
          $ annotateSeverity
          $ teeTraceChainTip StructuredLogging tracingVerbosity
          $ addName "ChainDB" tracer
    , consensusTracers
        = mkConsensusTracers forgeTracers traceOptions
    , protocolTracers
        = mkProtocolsTracers traceOptions
    , ipSubscriptionTracer
        = tracerOnOff (traceIpSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = tracerOnOff (traceDnsSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "DnsSubscription" tracer
    , dnsResolverTracer
        = tracerOnOff (traceDnsResolver traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "DnsResolver" tracer
    , errorPolicyTracer
        = tracerOnOff (traceErrorPolicy traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ErrorPolicy" tracer
    , muxTracer
        =  tracerOnOff (traceMux traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "Mux" tracer
    }
  where
    -- Turn on/off a tracer depending on what was parsed from the command line.
    tracerOnOff :: Bool -> Tracer IO a -> Tracer IO a
    tracerOnOff False _ = nullTracer
    tracerOnOff True tracer' = tracer'
    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceVerbosity traceOptions

    teeTraceChainTip :: TracingFormatting
                     -> TracingVerbosity
                     -> Tracer IO (LogObject Text)
                     -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip tform tverb tr = Tracer $ \ev -> do
        traceWith (teeTraceChainTip' tr) ev
        traceWith (toLogObject' tform tverb tr) ev
    teeTraceChainTip' :: Tracer IO (LogObject Text)
                      -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip' tr =
        Tracer $ \(WithSeverity _ (WithTip _tip ev')) ->
          case ev' of
              (ChainDB.TraceAddBlockEvent ev) -> case ev of
                  ChainDB.SwitchedToChain _ c -> do
                      meta <- mkLOMeta Critical Confidential
                      let tr' = appendName "metrics" tr
                          ChainInformation { slots, blocks, density } = chainInformation c
                          epochSlots :: Word64 = 21600  -- TODO
                      traceNamedObject tr' (meta, LogValue "density" . PureD $ fromRational density)
                      traceNamedObject tr' (meta, LogValue "slotNum" . PureI $ fromIntegral slots)
                      traceNamedObject tr' (meta, LogValue "blockNum" . PureI $ fromIntegral blocks)
                      traceNamedObject tr' (meta, LogValue "slotInEpoch" . PureI $ fromIntegral (slots `rem` epochSlots))
                      traceNamedObject tr' (meta, LogValue "epoch" . PureI $ fromIntegral (slots `div` epochSlots))
                  _ -> pure ()
              _ -> pure ()
    teeTraceBlockFetchDecision :: TracingFormatting
        -> TracingVerbosity
        -> Tracer IO (LogObject Text)
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision tform tverb tr = Tracer $ \ev -> do
      traceWith (teeTraceBlockFetchDecision' tr) ev
      traceWith (toLogObject' tform tverb tr) ev
    teeTraceBlockFetchDecision' :: Tracer IO (LogObject Text)
                -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision' tr =
        Tracer $ \(WithSeverity _ peers) -> do
          meta <- mkLOMeta Notice Confidential
          let tr' = appendName "peers" tr
          traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)

    mempoolTraceTransformer :: Tracer IO (LogObject a)
                            -> Tracer IO (TraceEventMempool blk)
    mempoolTraceTransformer tr = Tracer $ \mempoolEvent -> do
        let tr' = appendName "metrics" tr
            (n, tot) = case mempoolEvent of
                  TraceMempoolAddTxs      txs0 tot0 _ -> (length txs0, tot0)
                  TraceMempoolRejectedTxs txs0 tot0 _ -> (length txs0, tot0)
                  TraceMempoolRemoveTxs   txs0 tot0  _-> (length txs0, tot0)
                  TraceMempoolManuallyRemovedTxs txs0 txs1 tot0
                                                    -> ( length txs0 + length txs1
                                                       , tot0
                                                       )
            logValue1 :: LOContent a
            logValue1 = LogValue "txsInMempool" $ PureI $ fromIntegral tot

            logValue2 :: LOContent a
            logValue2 = LogValue "txsProcessed" $ PureI $ fromIntegral n

        meta <- mkLOMeta Critical Confidential

        traceNamedObject tr (meta, logValue1)
        traceNamedObject tr' (meta, logValue1)

        traceNamedObject tr (meta, logValue2)
        traceNamedObject tr' (meta, logValue2)


    mempoolTracer :: Tracer IO (TraceEventMempool blk)
    mempoolTracer = Tracer $ \ev -> do
      traceWith (mempoolTraceTransformer tracer) ev
      traceWith (measureTxsStart tracer) ev
      traceWith (showTracing $ withName "Mempool" tracer) ev

    forgeTracer :: ForgeTracers -> TraceOptions -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
    forgeTracer forgeTracers traceOpts = Tracer $ \ev -> do
        traceWith (measureTxsEnd tracer) ev
        traceWith (consensusForgeTracer) ev
      where
        -- The consensus tracer.
        consensusForgeTracer = tracerOnOff (traceForge traceOpts)
          $ annotateSeverity
          $ teeForge forgeTracers StructuredLogging tracingVerbosity
          $ addName "Forge" tracer

    teeForge
      :: ForgeTracers
      -> TracingFormatting
      -> TracingVerbosity
      -> Trace IO Text
      -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk (GenTx blk)))
    teeForge ft tform tverb tr = Tracer $ \ev -> do
      traceWith (teeForge' tr) ev
      flip traceWith ev $ fanning $ \(WithSeverity _ e) ->
        case e of
          Consensus.TraceForgeEvent{} -> teeForge' (ftForged ft)
          Consensus.TraceCouldNotForge{} -> teeForge' (ftCouldNotForge ft)
          Consensus.TraceAdoptedBlock{} -> teeForge' (ftAdopted ft)
          Consensus.TraceDidntAdoptBlock{} -> teeForge' (ftDidntAdoptBlock ft)
          Consensus.TraceForgedInvalidBlock{} -> teeForge' (ftForgedInvalid ft)
      traceWith (toLogObject' tform tverb tr) ev

    teeForge'
      :: Trace IO Text
      -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk (GenTx blk)))
    teeForge' tr =
      Tracer $ \(WithSeverity _ ev) -> do
        meta <- mkLOMeta Critical Confidential
        traceNamedObject (appendName "metrics" tr) . (meta,) $
          case ev of
            Consensus.TraceForgeEvent    slot _ ->
              LogValue "forgedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceCouldNotForge slot _ ->
              LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceAdoptedBlock slot _ _ _ ->
              LogValue "adoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceDidntAdoptBlock slot _ ->
              LogValue "notAdoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceForgedInvalidBlock slot _ _ ->
              LogValue "forgedInvalidSlotLast" $ PureI $ fromIntegral $ unSlotNo slot

    mkConsensusTracers :: ForgeTracers -> TraceOptions -> Consensus.Tracers' peer blk (Tracer IO)
    mkConsensusTracers forgeTracers traceOpts = Consensus.Tracers
      { Consensus.chainSyncClientTracer
        = tracerOnOff (traceChainSyncClient traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ChainSyncClient" tracer
      , Consensus.chainSyncServerHeaderTracer
        =  tracerOnOff (traceChainSyncHeaderServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ChainSyncHeaderServer" tracer
      , Consensus.chainSyncServerBlockTracer
        = tracerOnOff (traceChainSyncBlockServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ChainSyncBlockServer" tracer
      , Consensus.blockFetchDecisionTracer
        = tracerOnOff (traceBlockFetchDecisions traceOpts)
          $ annotateSeverity
          $ teeTraceBlockFetchDecision StructuredLogging tracingVerbosity
          $ addName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer
        = tracerOnOff (traceBlockFetchClient traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer
        = tracerOnOff (traceBlockFetchServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "BlockFetchServer" tracer
      , Consensus.txInboundTracer
        = tracerOnOff (traceTxInbound traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "TxInbound" tracer
      , Consensus.txOutboundTracer
        = tracerOnOff (traceTxOutbound traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer
        = tracerOnOff (traceLocalTxSubmissionServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer
        = tracerOnOff (traceMempool traceOpts) $ mempoolTracer
      , Consensus.forgeTracer
        = forgeTracer forgeTracers traceOpts
      }


    mkProtocolsTracers :: TraceOptions -> ProtocolTracers' peer blk DeserialiseFailure (Tracer IO)
    mkProtocolsTracers traceOpts = ProtocolTracers
      { ptChainSyncTracer
        = tracerOnOff (traceChainSyncProtocol traceOpts)
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , ptBlockFetchTracer
        = tracerOnOff (traceBlockFetchProtocol traceOpts)
        $ showTracing $ withName "BlockFetchProtocol" tracer
      , ptBlockFetchSerialisedTracer
        = tracerOnOff (traceBlockFetchProtocolSerialised traceOpts)
        $ showTracing $ withName "BlockFetchProtocol" tracer
      , ptTxSubmissionTracer
        = tracerOnOff (traceTxSubmissionProtocol traceOpts)
        $ showTracing $ withName "TxSubmissionProtocol" tracer
      , ptLocalChainSyncTracer
        = tracerOnOff (traceLocalChainSyncProtocol traceOpts)
        $ showTracing $ withName "LocalChainSyncProtocol" tracer
      , ptLocalTxSubmissionTracer
        = tracerOnOff (traceLocalTxSubmissionProtocol traceOpts)
        $ showTracing $ withName "LocalTxSubmissionProtocol" tracer
      }

-- | get information about a chain fragment

data ChainInformation = ChainInformation {
    slots :: Word64,
    blocks :: Word64,
    density :: Rational
    -- ^ the actual number of blocks created over the maximum
    -- expected number of blocks that could be created
  }

chainInformation :: forall block . AF.HasHeader block
                 => AF.AnchoredFragment block -> ChainInformation
chainInformation frag = ChainInformation
    { slots     = slotN
    , blocks    = blockN
    , density   = calcDensity blockD slotD
    }
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromMaybe 1 (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _ -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b


--
-- Tracing utils
--

withName :: String
         -> Tracer IO (LogObject Text)
         -> Tracer IO String
withName name tr = contramap pack $ toLogObject $ appendName (pack name) tr


-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
--
withTip :: TVar IO (Point blk)
        -> Tracer IO (WithTip blk a)
        -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
