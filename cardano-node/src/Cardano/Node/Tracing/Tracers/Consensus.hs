{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.Consensus
  ( severityChainSyncClientEvent
  , namesForChainSyncClientEvent
  , docChainSyncClientEvent

  , severityChainSyncServerEvent
  , namesForChainSyncServerEvent
  , docChainSyncServerEventHeader
  , docChainSyncServerEventBlock

  , severityBlockFetchDecision
  , namesForBlockFetchDecision
  , docBlockFetchDecision

  , severityBlockFetchClient
  , namesForBlockFetchClient
  , docBlockFetchClient

  , ClientMetrics(..)
  , initialClientMetrics
  , calculateBlockFetchClientMetrics

  , severityBlockFetchServer
  , namesForBlockFetchServer
  , docBlockFetchServer

  , severityTxInbound
  , namesForTxInbound
  , docTxInbound

  , severityTxOutbound
  , namesForTxOutbound
  , docTxOutbound

  , severityLocalTxSubmissionServer
  , namesForLocalTxSubmissionServer
  , docLocalTxSubmissionServer

  , severityMempool
  , namesForMempool
  , docMempool

  , TraceStartLeadershipCheckPlus (..)
  , ForgeTracerType
  , forgeTracerTransform
  , severityForge
  , namesForForge
  , docForge

  , namesForBlockchainTime
  , severityBlockchainTime
  , docBlockchainTime

  , namesForKeepAliveClient
  , severityKeepAliveClient
  , docKeepAliveClient

  ) where


import           Control.Monad.Class.MonadTime (Time (..))
import           Data.Aeson (ToJSON, Value (Number, String), toJSON, (.=))
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as Pq
import           Data.SOP.Strict
import qualified Data.Text as Text
import           Data.Time (DiffTime, NominalDiffTime)
import           Text.Show


import           Cardano.Slotting.Slot (WithOrigin (..))

import           Cardano.Logging
import           Cardano.Node.Queries (HasKESInfo (..))
import           Cardano.Node.Tracing.Era.Byron ()
import           Cardano.Node.Tracing.Era.Shelley ()
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Render
import           Cardano.Node.Tracing.Tracers.StartLeadershipCheck
import           Cardano.Prelude hiding (All, Show, show)

import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))

import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block hiding (blockPrevHash)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.TxSubmission.Inbound hiding (txId)
import           Ouroboros.Network.TxSubmission.Outbound

import qualified Data.Aeson as Aeson
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent (..), LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId, HasTxId,
                   LedgerSupportsMempool, txForgetValidated, txId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints, estimateBlockSize)
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Util.Enclose



instance LogFormatting a => LogFormatting (TraceLabelCreds a) where
  forMachine dtal (TraceLabelCreds creds a)  =
    mconcat [ "credentials" .= toJSON creds
             , "val"         .= forMachine dtal a
            ]
-- TODO Trace label creds as well
  forHuman (TraceLabelCreds _t a)         = forHuman a
  asMetrics (TraceLabelCreds _t a)        = asMetrics a


instance (LogFormatting (LedgerUpdate blk), LogFormatting (LedgerWarning blk))
      =>  LogFormatting (LedgerEvent blk) where
  forMachine dtal = \case
    LedgerUpdate  update  -> forMachine dtal update
    LedgerWarning warning -> forMachine dtal warning

tipToObject :: forall blk. ConvertRawHash blk => Tip blk -> Aeson.Object
tipToObject = \case
  TipGenesis -> mconcat
    [ "slot"    .= toJSON (0 :: Int)
    , "block"   .= String "genesis"
    , "blockNo" .= toJSON ((-1) :: Int)
    ]
  Tip slot hash blockno -> mconcat
    [ "slot"    .= slot
    , "block"   .= String (renderHeaderHash (Proxy @blk) hash)
    , "blockNo" .= blockno
    ]

--------------------------------------------------------------------------------
-- ChainSyncClient Tracer
--------------------------------------------------------------------------------

severityChainSyncClientEvent ::
  BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk) -> SeverityS
severityChainSyncClientEvent (BlockFetch.TraceLabelPeer _ e) =
    severityChainSyncClientEvent' e

namesForChainSyncClientEvent ::
  BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk) -> [Text]
namesForChainSyncClientEvent (BlockFetch.TraceLabelPeer _ e) = namesForChainSyncClientEvent' e

severityChainSyncClientEvent' :: TraceChainSyncClientEvent blk -> SeverityS
severityChainSyncClientEvent' TraceDownloadedHeader {}  = Info
severityChainSyncClientEvent' TraceFoundIntersection {} = Info
severityChainSyncClientEvent' TraceRolledBack {}        = Notice
severityChainSyncClientEvent' TraceException {}         = Warning
severityChainSyncClientEvent' TraceTermination {}       = Notice

namesForChainSyncClientEvent' :: TraceChainSyncClientEvent blk -> [Text]
namesForChainSyncClientEvent' TraceDownloadedHeader {} =
      ["DownloadedHeader"]
namesForChainSyncClientEvent' TraceFoundIntersection {} =
      ["FoundIntersection"]
namesForChainSyncClientEvent' TraceRolledBack {} =
      ["RolledBack"]
namesForChainSyncClientEvent' TraceException {} =
      ["Exception"]
namesForChainSyncClientEvent' TraceTermination {} =
      ["Termination"]

instance (ConvertRawHash blk, LedgerSupportsProtocol blk)
      => LogFormatting (TraceChainSyncClientEvent blk) where
  forHuman (TraceDownloadedHeader pt) =
    "While following a candidate chain, we rolled forward by downloading a\
    \ header. " <> showT (headerPoint pt)
  forHuman (TraceRolledBack tip) =
    "While following a candidate chain, we rolled back to the given point: "
      <> showT tip
  forHuman (TraceException exc) =
    "An exception was thrown by the Chain Sync Client. "
      <> showT exc
  forHuman TraceFoundIntersection {} =
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  forHuman (TraceTermination res) =
      "The client has terminated. " <> showT res

  forMachine _dtal (TraceDownloadedHeader h) =
      mconcat [ "kind" .= String "DownloadedHeader"
              , tipToObject (tipFromHeader h)
              ]
  forMachine dtal (TraceRolledBack tip) =
      mconcat [ "kind" .= String "RolledBack"
               , "tip" .= forMachine dtal tip ]
  forMachine _dtal (TraceException exc) =
      mconcat [ "kind" .= String "Exception"
               , "exception" .= String (Text.pack $ show exc) ]
  forMachine _dtal TraceFoundIntersection {} =
      mconcat [ "kind" .= String "FoundIntersection" ]
  forMachine _dtal (TraceTermination reason) =
      mconcat [ "kind" .= String "Termination"
               , "reason" .= String (Text.pack $ show reason) ]


docChainSyncClientEvent ::
  Documented (BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk))
docChainSyncClientEvent = Documented [
    DocMsg
      ["DownloadedHeader"]
      []
      "While following a candidate chain, we rolled forward by downloading a\
      \ header."
  , DocMsg
      ["RolledBack"]
      []
      "While following a candidate chain, we rolled back to the given point."
  , DocMsg
      ["FoundIntersection"]
      []
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  , DocMsg
      ["Exception"]
      []
      "An exception was thrown by the Chain Sync Client."
  , DocMsg
      ["Termination"]
      []
      "The client has terminated."
  ]

--------------------------------------------------------------------------------
-- ChainSyncServer Tracer
--------------------------------------------------------------------------------

severityChainSyncServerEvent :: TraceChainSyncServerEvent blk -> SeverityS
severityChainSyncServerEvent (TraceChainSyncServerUpdate _tip _upd _blocking enclosing) =
    case enclosing of
      RisingEdge  -> Info
      FallingEdge -> Debug

namesForChainSyncServerEvent :: TraceChainSyncServerEvent blk -> [Text]
namesForChainSyncServerEvent (TraceChainSyncServerUpdate _tip _update _blocking _enclosing) =
      ["Update"]

instance ConvertRawHash blk
      => LogFormatting (TraceChainSyncServerEvent blk) where
  forMachine dtal (TraceChainSyncServerUpdate tip update blocking enclosing) =
      mconcat $
               [ "kind" .= String "ChainSyncServer.Update"
               , "tip" .= tipToObject tip
               , case update of
                   AddBlock pt -> "addBlock" .= renderPointForDetails dtal pt
                   RollBack pt -> "rollBackTo" .= renderPointForDetails dtal pt
               , "blockingRead" .= case blocking of Blocking -> True; NonBlocking -> False
               ]
               <> [ "risingEdge" .= True | RisingEdge <- [enclosing] ]

  asMetrics (TraceChainSyncServerUpdate _tip (AddBlock _pt) _blocking FallingEdge) =
      [CounterM "cardano.node.chainSync.rollForward" Nothing]
  asMetrics _ = []



-- | Metrics documented here, but implemented specially
docChainSyncServerEventHeader :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEventHeader = Documented [
    DocMsg
      ["Update"]
      [("ChainSync.HeadersServed", "A counter triggered only on header event")]
      "A server read has occurred, either for an add block or a rollback"
  ]

docChainSyncServerEventBlock :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEventBlock =
    addDocumentedNamespace
      []
      docChainSyncServerEventBlock'

docChainSyncServerEventBlock' :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEventBlock' = Documented [
    DocMsg
      ["Update"]
      []
      "A server read has occurred, either for an add block or a rollback"
  ]

--------------------------------------------------------------------------------
-- BlockFetchDecision Tracer
--------------------------------------------------------------------------------

severityBlockFetchDecision ::
     [BlockFetch.TraceLabelPeer peer (FetchDecision [Point header])]
  -> SeverityS
severityBlockFetchDecision []  = Info
severityBlockFetchDecision l   = maximum $
  map (\(BlockFetch.TraceLabelPeer _ a) -> fetchDecisionSeverity a) l
    where
      fetchDecisionSeverity :: FetchDecision a -> SeverityS
      fetchDecisionSeverity fd =
        case fd of
          Left FetchDeclineChainNotPlausible     -> Debug
          Left FetchDeclineChainNoIntersection   -> Notice
          Left FetchDeclineAlreadyFetched        -> Debug
          Left FetchDeclineInFlightThisPeer      -> Debug
          Left FetchDeclineInFlightOtherPeer     -> Debug
          Left FetchDeclinePeerShutdown          -> Info
          Left FetchDeclinePeerSlow              -> Info
          Left FetchDeclineReqsInFlightLimit {}  -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {}           -> Info
          Left FetchDeclineConcurrencyLimit {}   -> Info
          Right _                                -> Info

namesForBlockFetchDecision ::
     [BlockFetch.TraceLabelPeer peer (FetchDecision [Point header])]
  -> [Text]
namesForBlockFetchDecision _ = []

instance (LogFormatting peer, Show peer) =>
    LogFormatting [TraceLabelPeer peer (FetchDecision [Point header])] where
  forMachine DMinimal _ = mempty
  forMachine _ []       = mconcat
    [ "kind"  .= String "EmptyPeersFetch"]
  forMachine _ xs       = mconcat
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> forMachine DDetailed x : acc) [] xs) ]

  asMetrics peers = [IntM "BlockFetch.ConnectedPeers" (fromIntegral (length peers))]

instance (LogFormatting peer, Show peer, LogFormatting a)
  => LogFormatting (TraceLabelPeer peer a) where
  forMachine dtal (TraceLabelPeer peerid a) =
    mconcat [ "peer" .= forMachine dtal peerid ] <> forMachine dtal a
  forHuman (TraceLabelPeer peerid a) = "Peer is " <> showT peerid
                                        <> ". " <> forHuman a
  asMetrics (TraceLabelPeer _peerid a) = asMetrics a

instance LogFormatting (FetchDecision [Point header]) where
  forMachine _dtal (Left decline) =
    mconcat [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (showT decline)
             ]
  forMachine _dtal (Right results) =
    mconcat [ "kind" .= String "FetchDecision results"
             , "length" .= String (showT $ length results)
             ]

docBlockFetchDecision ::
  Documented [BlockFetch.TraceLabelPeer remotePeer (FetchDecision [Point (Header blk)])]
docBlockFetchDecision = Documented [
    DocMsg
      []
      [("BlockFetch.ConnectedPeers", "Number of connected peers")]
      "Throughout the decision making process we accumulate reasons to decline\
      \ to fetch any blocks. This message carries the intermediate and final\
      \ results."
  ]

--------------------------------------------------------------------------------
-- BlockFetchClient Tracer
--------------------------------------------------------------------------------

data CdfCounter = CdfCounter {
    limit :: Int64
  , counter :: Int64
}

decCdf :: Ord a => Num a => a -> CdfCounter -> CdfCounter
decCdf v cdf =
  if v < fromIntegral (limit cdf)
    then cdf {counter = counter cdf - 1}
    else cdf

incCdf ::Ord a => Num a => a -> CdfCounter -> CdfCounter
incCdf v cdf =
  if v < fromIntegral (limit cdf)
    then cdf {counter = counter cdf + 1}
    else cdf

data ClientMetrics = ClientMetrics {
    cmSlotMap  :: IntPSQ Word64 NominalDiffTime
  , cmCdf1sVar :: CdfCounter
  , cmCdf3sVar :: CdfCounter
  , cmCdf5sVar :: CdfCounter
  , cmDelay    :: Double
  , cmBlockSize :: Word32
  , cmTraceIt  :: Bool
}

instance LogFormatting ClientMetrics where
  forMachine _dtal _ = mempty
  asMetrics ClientMetrics {..} =
    if cmTraceIt
      then
        let  size = Pq.size cmSlotMap
             msgs =
               [ DoubleM
                    "cardano.node.metrics.blockfetchclient.blockdelay.s"
                    cmDelay
               , IntM
                    "cardano.node.metrics.blockfetchclient.blocksize"
                    (fromIntegral cmBlockSize)
               , DoubleM "cardano.node.metrics.blockfetchclient.blockdelay.cdfOne"
                    (fromIntegral (counter cmCdf1sVar) / fromIntegral size)
               , DoubleM "cardano.node.metrics.blockfetchclient.blockdelay.cdfThree"
                    (fromIntegral (counter cmCdf3sVar) / fromIntegral size)
               , DoubleM "cardano.node.metrics.blockfetchclient.blockdelay.cdfFive"
                    (fromIntegral (counter cmCdf5sVar) / fromIntegral size)
               ]
        in if cmDelay > 5
             then
               CounterM "cardano.node.metrics.blockfetchclient.lateblocks" Nothing
                 : msgs
             else msgs
      else []

initialClientMetrics :: ClientMetrics
initialClientMetrics =
    ClientMetrics
      Pq.empty
      (CdfCounter 1 0)
      (CdfCounter 3 0)
      (CdfCounter 5 0)
      0
      0
      False

calculateBlockFetchClientMetrics ::
     ClientMetrics
  -> LoggingContext
  -> BlockFetch.TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> IO ClientMetrics
calculateBlockFetchClientMetrics cm@ClientMetrics {..} _lc
            (TraceLabelPeer _ (BlockFetch.CompletedBlockFetch p _ _ _ forgeDelay blockSize)) =
    case pointSlot p of
            Origin -> pure cm {cmTraceIt = False}  -- Nothing to do.
            At (SlotNo slotNo) -> do
               if Pq.null cmSlotMap && forgeDelay > 20
                  then pure cm {cmTraceIt = False} -- During startup wait until we are in sync
                  else case Pq.lookup (fromIntegral slotNo) cmSlotMap of
                        Just _ -> pure cm {cmTraceIt = False}  -- dupe, we only track the first
                        Nothing -> do
                          let slotMap' = Pq.insert (fromIntegral slotNo) slotNo forgeDelay cmSlotMap
                          if Pq.size slotMap' > 1080 -- TODO k/2, should come from config file
                            then case Pq.minView slotMap' of
                                 Nothing -> pure cm {cmTraceIt = False} -- Err. We just inserted an element!
                                 Just (_, minSlotNo, minDelay, slotMap'') ->
                                   if minSlotNo == slotNo
                                      then pure cm {cmTraceIt = False, cmSlotMap = slotMap'}
                                      else let
                                         cdf1sVar = decCdf minDelay cmCdf1sVar
                                         cdf3sVar = decCdf minDelay cmCdf3sVar
                                         cdf5sVar = decCdf minDelay cmCdf5sVar
                                         cdf1sVar' = incCdf forgeDelay cdf1sVar
                                         cdf3sVar' = incCdf forgeDelay cdf3sVar
                                         cdf5sVar' = incCdf forgeDelay cdf5sVar
                                         in pure cm {
                                              cmCdf1sVar  = cdf1sVar'
                                            , cmCdf3sVar  = cdf3sVar'
                                            , cmCdf5sVar  = cdf5sVar'
                                            , cmDelay     = realToFrac  forgeDelay
                                            , cmBlockSize = blockSize
                                            , cmTraceIt   = True
                                            , cmSlotMap   = slotMap''}
                            else let
                               cdf1sVar' = incCdf forgeDelay cmCdf1sVar
                               cdf3sVar' = incCdf forgeDelay cmCdf3sVar
                               cdf5sVar' = incCdf forgeDelay cmCdf5sVar
                                -- -- Wait until we have at least 45 samples before we start providing
                                -- -- cdf estimates.
                               in if Pq.size slotMap' >= 45
                                    then pure cm {
                                         cmCdf1sVar  = cdf1sVar'
                                       , cmCdf3sVar  = cdf3sVar'
                                       , cmCdf5sVar  = cdf5sVar'
                                       , cmDelay     = realToFrac forgeDelay
                                       , cmBlockSize = blockSize
                                       , cmTraceIt   = True
                                       , cmSlotMap   = slotMap'}
                                   else pure cm {
                                        cmCdf1sVar  = cdf1sVar'
                                      , cmCdf3sVar  = cdf3sVar'
                                      , cmCdf5sVar  = cdf5sVar'
                                      , cmTraceIt   = False
                                      , cmSlotMap   = slotMap'}

calculateBlockFetchClientMetrics cm _lc _ = pure cm

severityBlockFetchClient ::
     BlockFetch.TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> SeverityS
severityBlockFetchClient (BlockFetch.TraceLabelPeer _p bf) = severityBlockFetchClient' bf

severityBlockFetchClient' ::
     BlockFetch.TraceFetchClientState header
  -> SeverityS
severityBlockFetchClient' BlockFetch.AddedFetchRequest {}        = Info
severityBlockFetchClient' BlockFetch.AcknowledgedFetchRequest {} = Info
severityBlockFetchClient' BlockFetch.SendFetchRequest {}         = Info
severityBlockFetchClient' BlockFetch.StartedFetchBatch {}        = Info
severityBlockFetchClient' BlockFetch.CompletedBlockFetch {}      = Info
severityBlockFetchClient' BlockFetch.CompletedFetchBatch {}      = Info
severityBlockFetchClient' BlockFetch.RejectedFetchBatch {}       = Info
severityBlockFetchClient' BlockFetch.ClientTerminating {}        = Notice

namesForBlockFetchClient ::
    BlockFetch.TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> [Text]
namesForBlockFetchClient (BlockFetch.TraceLabelPeer _p bf) = namesForBlockFetchClient' bf

namesForBlockFetchClient' ::
    BlockFetch.TraceFetchClientState header
  -> [Text]
namesForBlockFetchClient' BlockFetch.AddedFetchRequest {} =
  ["AddedFetchRequest"]
namesForBlockFetchClient' BlockFetch.AcknowledgedFetchRequest {}  =
  ["AcknowledgedFetchRequest"]
namesForBlockFetchClient' BlockFetch.SendFetchRequest {} =
  ["SendFetchRequest"]
namesForBlockFetchClient' BlockFetch.StartedFetchBatch {} =
  ["StartedFetchBatch"]
namesForBlockFetchClient' BlockFetch.CompletedFetchBatch {} =
  ["CompletedFetchBatch"]
namesForBlockFetchClient' BlockFetch.CompletedBlockFetch  {} =
  ["CompletedBlockFetch"]
namesForBlockFetchClient' BlockFetch.RejectedFetchBatch  {} =
  ["RejectedFetchBatch"]
namesForBlockFetchClient' BlockFetch.ClientTerminating {} =
  ["ClientTerminating"]


instance (HasHeader header, ConvertRawHash header) =>
  LogFormatting (BlockFetch.TraceFetchClientState header) where
  forMachine _dtal BlockFetch.AddedFetchRequest {} =
    mconcat [ "kind" .= String "AddedFetchRequest" ]
  forMachine _dtal BlockFetch.AcknowledgedFetchRequest {} =
    mconcat [ "kind" .= String "AcknowledgedFetchRequest" ]
  forMachine _dtal (BlockFetch.SendFetchRequest af) =
    mconcat [ "kind" .= String "SendFetchRequest"
            , "head" .= String (renderChainHash
                                 (renderHeaderHash (Proxy @header))
                                 (AF.headHash af))
            , "length" .= toJSON (fragmentLength af)]
   where
     -- NOTE: this ignores the Byron era with its EBB complication:
     -- the length would be underestimated by 1, if the AF is anchored
     -- at the epoch boundary.
     fragmentLength :: AF.AnchoredFragment header -> Int
     fragmentLength f = fromIntegral . unBlockNo $
        case (f, f) of
          (AS.Empty{}, AS.Empty{}) -> 0
          (firstHdr AS.:< _, _ AS.:> lastHdr) ->
            blockNo lastHdr - blockNo firstHdr + 1
  forMachine _dtal (BlockFetch.CompletedBlockFetch pt _ _ _ delay blockSize) =
    mconcat [ "kind"  .= String "CompletedBlockFetch"
            , "delay" .= (realToFrac delay :: Double)
            , "size"  .= blockSize
            , "block" .= String
              (case pt of
                 GenesisPoint -> "Genesis"
                 BlockPoint _ h -> renderHeaderHash (Proxy @header) h)
            ]
  forMachine _dtal BlockFetch.CompletedFetchBatch {} =
    mconcat [ "kind" .= String "CompletedFetchBatch" ]
  forMachine _dtal BlockFetch.StartedFetchBatch {} =
    mconcat [ "kind" .= String "StartedFetchBatch" ]
  forMachine _dtal BlockFetch.RejectedFetchBatch {} =
    mconcat [ "kind" .= String "RejectedFetchBatch" ]
  forMachine _dtal (BlockFetch.ClientTerminating outstanding) =
    mconcat [ "kind" .= String "ClientTerminating"
            , "outstanding" .= outstanding
            ]


docBlockFetchClient ::
  Documented (BlockFetch.TraceLabelPeer remotePeer (BlockFetch.TraceFetchClientState (Header blk)))
docBlockFetchClient = addDocumentedNamespace [] docBlockFetchClient'

docBlockFetchClient' ::
  Documented (BlockFetch.TraceLabelPeer remotePeer (BlockFetch.TraceFetchClientState (Header blk)))
docBlockFetchClient' = Documented [
    DocMsg
      ["AddedFetchRequest"]
      []
      "The block fetch decision thread has added a new fetch instruction\
      \ consisting of one or more individual request ranges."
  ,
    DocMsg
      ["AcknowledgedFetchRequest"]
      []
      "Mark the point when the fetch client picks up the request added\
      \ by the block fetch decision thread. Note that this event can happen\
      \ fewer times than the 'AddedFetchRequest' due to fetch request merging."
  ,
    DocMsg
      ["SendFetchRequest"]
      []
      "Mark the point when fetch request for a fragment is actually sent\
       \ over the wire."
  ,
    DocMsg
      ["StartedFetchBatch"]
      []
      "Mark the start of receiving a streaming batch of blocks. This will\
      \ be followed by one or more 'CompletedBlockFetch' and a final\
      \ 'CompletedFetchBatch'"
  ,
    DocMsg
      ["CompletedFetchBatch"]
      []
      "Mark the successful end of receiving a streaming batch of blocks"
  ,
    DocMsg
      ["CompletedBlockFetch"]
      []
      "Mark the successful end of receiving a streaming batch of blocks."
  ,
    DocMsg
      ["RejectedFetchBatch"]
      []
      "If the other peer rejects our request then we have this event\
      \ instead of 'StartedFetchBatch' and 'CompletedFetchBatch'."
  ,
    DocMsg
      ["ClientTerminating"]
      []
      "The client is terminating.  Log the number of outstanding\
      \ requests."
  ]

--------------------------------------------------------------------------------
-- BlockFetchServer Tracer
--------------------------------------------------------------------------------

severityBlockFetchServer ::
     TraceBlockFetchServerEvent blk
  -> SeverityS
severityBlockFetchServer _ = Info

namesForBlockFetchServer ::
     TraceBlockFetchServerEvent blk
  -> [Text]
namesForBlockFetchServer TraceBlockFetchServerSendBlock {} = ["SendBlock"]

instance ConvertRawHash blk => LogFormatting (TraceBlockFetchServerEvent blk) where
  forMachine _dtal (TraceBlockFetchServerSendBlock blk) =
    mconcat [ "kind" .= String "BlockFetchServer"
             , "block" .= String (renderChainHash
                                    @blk
                                    (renderHeaderHash (Proxy @blk))
                                    $ pointHash blk)]
-- TODO JNF
  asMetrics (TraceBlockFetchServerSendBlock _p) =
    [CounterM "BlockFetch.BlocksServed" Nothing]


docBlockFetchServer ::
  Documented (TraceBlockFetchServerEvent blk)
docBlockFetchServer = addDocumentedNamespace [] docBlockFetchServer'


docBlockFetchServer' ::
  Documented (TraceBlockFetchServerEvent blk)
docBlockFetchServer' = Documented [
    DocMsg
      ["SendBlock"]
      [("BlockFetch.BlocksServed", "")]
      "The server sent a block to the peer."
  ]


--------------------------------------------------------------------------------
-- TxInbound Tracer
--------------------------------------------------------------------------------

severityTxInbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))
  -> SeverityS
severityTxInbound (BlockFetch.TraceLabelPeer _p ti) = severityTxInbound' ti

severityTxInbound' ::
    TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)
  -> SeverityS
severityTxInbound' TraceTxSubmissionCollected {}         = Debug
severityTxInbound' TraceTxSubmissionProcessed {}         = Debug
severityTxInbound' TraceTxInboundTerminated              = Notice
severityTxInbound' TraceTxInboundCannotRequestMoreTxs {} = Debug
severityTxInbound' TraceTxInboundCanRequestMoreTxs {}    = Debug

namesForTxInbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))
  -> [Text]
namesForTxInbound (BlockFetch.TraceLabelPeer _p ti) = namesForTxInbound' ti

namesForTxInbound' ::
    TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)
  -> [Text]
namesForTxInbound' (TraceTxSubmissionCollected _) =
    ["Collected"]
namesForTxInbound' (TraceTxSubmissionProcessed _) =
    ["Processed"]
namesForTxInbound' TraceTxInboundTerminated   =
    ["Terminated"]
namesForTxInbound' TraceTxInboundCanRequestMoreTxs {} =
    ["CanRequestMoreTxs"]
namesForTxInbound' TraceTxInboundCannotRequestMoreTxs {} =
    ["CannotRequestMoreTxs"]

instance LogFormatting (TraceTxSubmissionInbound txid tx) where
  forMachine _dtal (TraceTxSubmissionCollected count) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionCollected"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxSubmissionProcessed processed) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  forMachine _dtal TraceTxInboundTerminated =
    mconcat
      [ "kind" .= String "TraceTxInboundTerminated"
      ]
  forMachine _dtal (TraceTxInboundCanRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TraceTxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxInboundCannotRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TraceTxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]

  asMetrics (TraceTxSubmissionCollected count)=
    [CounterM "TxSubmission.Submitted" (Just count)]
  asMetrics (TraceTxSubmissionProcessed processed) =
    [ CounterM "TxSubmission.Accepted"
        (Just (ptxcAccepted processed))
    , CounterM "TxSubmission.Rejected"
        (Just (ptxcRejected processed))
    ]
  asMetrics _ = []

docTxInbound ::
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionInbound txid tx))
docTxInbound = addDocumentedNamespace [] docTxInbound'

docTxInbound' ::
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionInbound txid tx))
docTxInbound' = Documented [
    DocMsg
    ["Collected"]
    [ ("TxSubmission.Submitted", "")]
    "Number of transactions just about to be inserted."
  ,
    DocMsg
    ["Processed"]
    [ ("TxSubmission.Accepted", "")
    , ("TxSubmission.Rejected", "")
    ]
    "Just processed transaction pass/fail breakdown."
  ,
    DocMsg
    ["Terminated"]
    []
    "Server received 'MsgDone'."
  ,
    DocMsg
    ["CanRequestMoreTxs"]
    []
    "There are no replies in flight, but we do know some more txs we\
    \ can ask for, so lets ask for them and more txids."
  ,
    DocMsg
    ["CannotRequestMoreTxs"]
    []
    "There's no replies in flight, and we have no more txs we can\
    \ ask for so the only remaining thing to do is to ask for more\
    \ txids. Since this is the only thing to do now, we make this a\
    \ blocking call."
  ]


--------------------------------------------------------------------------------
-- TxOutbound Tracer
--------------------------------------------------------------------------------

severityTxOutbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  -> SeverityS
severityTxOutbound (BlockFetch.TraceLabelPeer _p _ti) = Info

namesForTxOutbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  -> [Text]
namesForTxOutbound (BlockFetch.TraceLabelPeer _p ti) = namesForTxOutbound' ti

namesForTxOutbound' ::
    TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)
  -> [Text]
namesForTxOutbound' TraceTxSubmissionOutboundRecvMsgRequestTxs {} =
    ["TxSubmissionOutboundRecvMsgRequest"]
namesForTxOutbound' TraceTxSubmissionOutboundSendMsgReplyTxs {} =
    ["TxSubmissionOutboundSendMsgReply"]
namesForTxOutbound' TraceControlMessage {} =
    ["ControlMessage"]

instance (Show txid, Show tx)
      => LogFormatting (TraceTxSubmissionOutbound txid tx) where
  forMachine DDetailed (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (Text.pack $ show txids)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  forMachine DDetailed (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (Text.pack $ show txs)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]
  forMachine _dtal (TraceControlMessage _msg) =
    mconcat
      [ "kind" .= String "TraceControlMessage"
      ]

docTxOutbound :: forall remotePeer txid tx.
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionOutbound txid tx))
docTxOutbound =  addDocumentedNamespace [] docTxOutbound'

docTxOutbound' :: forall remotePeer txid tx.
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionOutbound txid tx))
docTxOutbound' = Documented [
    DocMsg
    ["RecvMsgRequest"]
    []
    "The IDs of the transactions requested."
  ,
    DocMsg
    ["SendMsgReply"]
    []
    "The transactions to be sent in the response."
  ,
    DocMsg
    ["ControlMessage"]
    []
    ""
  ]

--------------------------------------------------------------------------------
-- TxSubmissionServer Tracer
--------------------------------------------------------------------------------

severityLocalTxSubmissionServer ::
     TraceLocalTxSubmissionServerEvent blk
  -> SeverityS
severityLocalTxSubmissionServer _ = Info

namesForLocalTxSubmissionServer ::
  TraceLocalTxSubmissionServerEvent blk
  -> [Text]
namesForLocalTxSubmissionServer TraceReceivedTx {} = ["ReceivedTx"]

instance LogFormatting (TraceLocalTxSubmissionServerEvent blk) where
  forMachine _dtal (TraceReceivedTx _gtx) =
    mconcat [ "kind" .= String "ReceivedTx" ]

docLocalTxSubmissionServer :: Documented (TraceLocalTxSubmissionServerEvent blk)
docLocalTxSubmissionServer =
    addDocumentedNamespace [] docLocalTxSubmissionServer'

docLocalTxSubmissionServer' :: Documented (TraceLocalTxSubmissionServerEvent blk)
docLocalTxSubmissionServer' = Documented [
    DocMsg
    ["ReceivedTx"]
    []
    "A transaction was received."
  ]

--------------------------------------------------------------------------------
-- Mempool Tracer
--------------------------------------------------------------------------------

severityMempool ::
     TraceEventMempool blk
  -> SeverityS
severityMempool _ = Info

namesForMempool :: TraceEventMempool blk -> [Text]
namesForMempool TraceMempoolAddedTx {}            = ["AddedTx"]
namesForMempool TraceMempoolRejectedTx {}         = ["RejectedTx"]
namesForMempool TraceMempoolRemoveTxs {}          = ["RemoveTxs"]
namesForMempool TraceMempoolManuallyRemovedTxs {} = ["ManuallyRemovedTxs"]

instance
  ( Show (ApplyTxErr blk)
  , LogFormatting (ApplyTxErr blk)
  , LogFormatting (GenTx blk)
  , ToJSON (GenTxId blk)
  , LedgerSupportsMempool blk
  ) => LogFormatting (TraceEventMempool blk) where
  forMachine dtal (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mconcat
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= forMachine dtal (txForgetValidated tx)
      , "mempoolSize" .= forMachine dtal mpSzAfter
      ]
  forMachine dtal (TraceMempoolRejectedTx tx txApplyErr mpSz) =
    mconcat
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "err" .= forMachine dtal txApplyErr
      , "tx" .= forMachine dtal tx
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolRemoveTxs txs mpSz) =
    mconcat
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs" .= map (forMachine dtal . txForgetValidated) txs
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mconcat
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsRemoved" .= txs0
      , "txsInvalidated" .= map (forMachine dtal . txForgetValidated) txs1
      , "mempoolSize" .= forMachine dtal mpSz
      ]

  asMetrics (TraceMempoolAddedTx _tx _mpSzBefore mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRejectedTx _tx _txApplyErr mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRemoveTxs _txs mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs [] _txs1 mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs txs _txs1 mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    , CounterM "Mempool.TxsProcessedNum" (Just (fromIntegral $ length txs))
    ]

instance LogFormatting MempoolSize where
  forMachine _dtal MempoolSize{msNumTxs, msNumBytes} =
    mconcat
      [ "numTxs" .= msNumTxs
      , "bytes" .= msNumBytes
      ]

docMempool :: forall blk. Documented (TraceEventMempool blk)
docMempool = addDocumentedNamespace [] docMempool'

docMempool' :: forall blk. Documented (TraceEventMempool blk)
docMempool' = Documented [
    DocMsg
      ["AddedTx"]
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      ]
      "New, valid transaction that was added to the Mempool."
  , DocMsg
      ["RejectedTx"]
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      ]
      "New, invalid transaction thas was rejected and thus not added to\
      \ the Mempool."
  , DocMsg
      ["RemoveTxs"]
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      ]
      "Previously valid transactions that are no longer valid because of\
      \ changes in the ledger state. These transactions have been removed\
      \ from the Mempool."
  , DocMsg
      ["ManuallyRemovedTxs"]
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      , ("Mempool.TxsProcessedNum", "")
      ]
      "Transactions that have been manually removed from the Mempool."
  ]


--------------------------------------------------------------------------------
-- ForgeEvent Tracer
--------------------------------------------------------------------------------

severityForge :: ForgeTracerType blk -> SeverityS
severityForge (Left t)  = severityForge'' t
severityForge (Right t) = severityForge'''' t

severityForge'' :: TraceForgeEvent blk -> SeverityS
severityForge'' TraceStartLeadershipCheck {}    = Info
severityForge'' TraceSlotIsImmutable {}         = Error
severityForge'' TraceBlockFromFuture {}         = Error
severityForge'' TraceBlockContext {}            = Debug
severityForge'' TraceNoLedgerState {}           = Error
severityForge'' TraceLedgerState {}             = Debug
severityForge'' TraceNoLedgerView {}            = Error
severityForge'' TraceLedgerView {}              = Debug
severityForge'' TraceForgeStateUpdateError {}   = Error
severityForge'' TraceNodeCannotForge {}         = Error
severityForge'' TraceNodeNotLeader {}           = Info
severityForge'' TraceNodeIsLeader {}            = Info
severityForge'' TraceForgeTickedLedgerState {}  = Debug
severityForge'' TraceForgingMempoolSnapshot {}  = Debug
severityForge'' TraceForgedBlock {}             = Info
severityForge'' TraceDidntAdoptBlock {}         = Error
severityForge'' TraceForgedInvalidBlock {}      = Error
severityForge'' TraceAdoptedBlock {}            = Info

severityForge'''' :: TraceStartLeadershipCheckPlus -> SeverityS
severityForge'''' _ = Info

namesForForge :: ForgeTracerType blk -> [Text]
namesForForge (Left t)  = namesForForge'' t
namesForForge (Right t) = namesForForge'''' t

namesForForge'' :: TraceForgeEvent blk -> [Text]
namesForForge'' TraceStartLeadershipCheck {}   = ["StartLeadershipCheck"]
namesForForge'' TraceSlotIsImmutable {}        = ["SlotIsImmutable"]
namesForForge'' TraceBlockFromFuture {}        = ["BlockFromFuture"]
namesForForge'' TraceBlockContext {}           = ["BlockContext"]
namesForForge'' TraceNoLedgerState {}          = ["NoLedgerState"]
namesForForge'' TraceLedgerState {}            = ["LedgerState"]
namesForForge'' TraceNoLedgerView {}           = ["NoLedgerView"]
namesForForge'' TraceLedgerView {}             = ["LedgerView"]
namesForForge'' TraceForgeStateUpdateError {}  = ["ForgeStateUpdateError"]
namesForForge'' TraceNodeCannotForge {}        = ["NodeCannotForge"]
namesForForge'' TraceNodeNotLeader {}          = ["NodeNotLeader"]
namesForForge'' TraceNodeIsLeader {}           = ["NodeIsLeader"]
namesForForge'' TraceForgeTickedLedgerState {} = ["ForgeTickedLedgerState"]
namesForForge'' TraceForgingMempoolSnapshot {} = ["ForgingMempoolSnapshot"]
namesForForge'' TraceForgedBlock {}            = ["ForgedBlock"]
namesForForge'' TraceDidntAdoptBlock {}        = ["DidntAdoptBlock"]
namesForForge'' TraceForgedInvalidBlock {}     = ["ForgedInvalidBlock"]
namesForForge'' TraceAdoptedBlock {}           = ["AdoptedBlock"]

namesForForge'''' :: TraceStartLeadershipCheckPlus -> [Text]
namesForForge'''' TraceStartLeadershipCheckPlus{} = ["StartLeadershipCheckPlus"]

instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , GetHeader blk
         , HasHeader blk
         , HasKESInfo blk
         , HasTxId (GenTx blk)
         , LedgerSupportsProtocol blk
         , LedgerSupportsMempool blk
         , SerialiseNodeToNodeConstraints blk
         , Show (ForgeStateUpdateError blk)
         , Show (CannotForge blk)
         , Show (TxId (GenTx blk))
         , LogFormatting (InvalidBlockReason blk)
         , LogFormatting (CannotForge blk)
         , LogFormatting (ForgeStateUpdateError blk))
      => LogFormatting (TraceForgeEvent blk) where
  forMachine _dtal (TraceStartLeadershipCheck slotNo) =
    mconcat
      [ "kind" .= String "TraceStartLeadershipCheck"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    mconcat
      [ "kind" .= String "TraceSlotIsImmutable"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceBlockFromFuture currentSlot tip) =
    mconcat
      [ "kind" .= String "TraceBlockFromFuture"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= toJSON (unSlotNo tip)
      ]
  forMachine dtal (TraceBlockContext currentSlot tipBlkNo tipPoint) =
    mconcat
      [ "kind" .= String "TraceBlockContext"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceNoLedgerState slotNo _pt) =
    mconcat
      [ "kind" .= String "TraceNoLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerState slotNo _pt) =
    mconcat
      [ "kind" .= String "TraceLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNoLedgerView slotNo _) =
    mconcat
      [ "kind" .= String "TraceNoLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerView slotNo) =
    mconcat
      [ "kind" .= String "TraceLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgeStateUpdateError slotNo reason) =
    mconcat
      [ "kind" .= String "TraceForgeStateUpdateError"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine dtal (TraceNodeCannotForge slotNo reason) =
    mconcat
      [ "kind" .= String "TraceNodeCannotForge"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine _dtal (TraceNodeNotLeader slotNo) =
    mconcat
      [ "kind" .= String "TraceNodeNotLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNodeIsLeader slotNo) =
    mconcat
      [ "kind" .= String "TraceNodeIsLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgeTickedLedgerState slotNo prevPt) =
    mconcat
      [ "kind" .= String "TraceForgeTickedLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "prev" .= renderPointForDetails dtal prevPt
      ]
  forMachine dtal (TraceForgingMempoolSnapshot slotNo prevPt mpHash mpSlot) =
    mconcat
      [ "kind"        .= String "TraceForgingMempoolSnapshot"
      , "slot"        .= toJSON (unSlotNo slotNo)
      , "prev"        .= renderPointForDetails dtal prevPt
      , "mempoolHash" .= String (renderChainHash @blk (renderHeaderHash (Proxy @blk)) mpHash)
      , "mempoolSlot" .= toJSON (unSlotNo mpSlot)
      ]
  forMachine _dtal (TraceForgedBlock slotNo _ blk _) =
    mconcat
      [ "kind" .= String "TraceForgedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "block"     .= String (renderHeaderHash (Proxy @blk) $ blockHash blk)
      , "blockNo"   .= toJSON (unBlockNo $ blockNo blk)
      , "blockPrev" .= String (renderChainHash
                                @blk
                                (renderHeaderHash (Proxy @blk))
                                $ blockPrevHash blk)
      ]
  forMachine _dtal (TraceDidntAdoptBlock slotNo _) =
    mconcat
      [ "kind" .= String "TraceDidntAdoptBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgedInvalidBlock slotNo _ reason) =
    mconcat
      [ "kind" .= String "TraceForgedInvalidBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine DDetailed (TraceAdoptedBlock slotNo blk txs) =
    mconcat
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          DDetailed
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
      , "txIds" .= toJSON (map (show . txId . txForgetValidated) txs)
      ]
  forMachine dtal (TraceAdoptedBlock slotNo blk _txs) =
    mconcat
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          dtal
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
      ]



  forHuman (TraceStartLeadershipCheck slotNo) =
      "Checking for leadership in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceSlotIsImmutable slotNo immutableTipPoint immutableTipBlkNo) =
      "Couldn't forge block because current slot is immutable: "
        <> "immutable tip: " <> renderPointAsPhrase immutableTipPoint
        <> ", immutable tip block no: " <> showT (unBlockNo immutableTipBlkNo)
        <> ", current slot: " <> showT (unSlotNo slotNo)
  forHuman (TraceBlockFromFuture currentSlot tipSlot) =
      "Couldn't forge block because current tip is in the future: "
        <> "current tip slot: " <> showT (unSlotNo tipSlot)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
  forHuman (TraceBlockContext currentSlot tipBlockNo tipPoint) =
      "New block will fit onto: "
        <> "tip: " <> renderPointAsPhrase tipPoint
        <> ", tip block no: " <> showT (unBlockNo tipBlockNo)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
  forHuman (TraceNoLedgerState slotNo pt) =
      "Could not obtain ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
  forHuman (TraceLedgerState slotNo pt) =
      "Obtained a ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
  forHuman (TraceNoLedgerView slotNo _) =
      "Could not obtain ledger view for slot " <> showT (unSlotNo slotNo)
  forHuman (TraceLedgerView slotNo) =
      "Obtained a ledger view for slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgeStateUpdateError slotNo reason) =
      "Updating the forge state in slot "
        <> showT (unSlotNo slotNo)
        <> " failed because: "
        <> showT reason
  forHuman (TraceNodeCannotForge slotNo reason) =
      "We are the leader in slot "
        <> showT (unSlotNo slotNo)
        <> ", but we cannot forge because: "
        <> showT reason
  forHuman (TraceNodeNotLeader slotNo) =
      "Not leading slot " <> showT (unSlotNo slotNo)
  forHuman (TraceNodeIsLeader slotNo) =
      "Leading slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgeTickedLedgerState slotNo prevPt) =
      "While forging in slot "
        <> showT (unSlotNo slotNo)
        <> " we ticked the ledger state ahead from "
        <> renderPointAsPhrase prevPt
  forHuman (TraceForgingMempoolSnapshot slotNo prevPt mpHash mpSlot) =
      "While forging in slot "
        <> showT (unSlotNo slotNo)
        <> " we acquired a mempool snapshot valid against "
        <> renderPointAsPhrase prevPt
        <> " from a mempool that was prepared for "
        <> renderChainHash @blk (renderHeaderHash (Proxy @blk)) mpHash
        <> " ticked to slot "
        <> showT (unSlotNo mpSlot)
  forHuman (TraceForgedBlock slotNo _ _ _) =
      "Forged block in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceDidntAdoptBlock slotNo _) =
      "Didn't adopt forged block in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgedInvalidBlock slotNo _ reason) =
      "Forged invalid block in slot "
        <> showT (unSlotNo slotNo)
        <> ", reason: " <> showT reason
  forHuman (TraceAdoptedBlock slotNo blk _txs) =
      "Adopted block forged in slot "
        <> showT (unSlotNo slotNo)
        <> ": " <> renderHeaderHash (Proxy @blk) (blockHash blk)

  asMetrics (TraceForgeStateUpdateError slot reason) =
    IntM "cardano.node.forgeStateUpdateError" (fromIntegral $ unSlotNo slot) :
      (case getKESInfo (Proxy @blk) reason of
        Nothing -> []
        Just kesInfo ->
          [ IntM
              "Forge.OperationalCertificateStartKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesStartPeriod $ kesInfo)
          , IntM
              "Forge.OperationalCertificateExpiryKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesEndPeriod $ kesInfo)
          , IntM
              "Forge.CurrentKESPeriod"
              0
          , IntM
              "Forge.RemainingKESPeriods"
              0
          ])

  asMetrics (TraceStartLeadershipCheck slot) =
    [IntM "Forge.AboutToLeadSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceSlotIsImmutable slot _tipPoint _tipBlkNo) =
    [IntM "Forge.SlotIsImmutable" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockFromFuture slot _slotNo) =
    [IntM "Forge.BlockFromFuture" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockContext slot _tipBlkNo _tipPoint) =
    [IntM "Forge.BlockContext" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerState slot _) =
    [IntM "Forge.CouldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerState slot _) =
    [IntM "Forge.LedgerState" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerView slot _) =
    [IntM "Forge.CouldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerView slot) =
    [IntM "Forge.LedgerView" (fromIntegral $ unSlotNo slot)]
  -- see above
  asMetrics (TraceNodeCannotForge slot _reason) =
    [IntM "Forge.NodeCannotForge" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeNotLeader slot) =
    [IntM "Forge.NodeNotLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeIsLeader slot) =
    [IntM "Forge.NodeIsLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics TraceForgeTickedLedgerState {} = []
  asMetrics TraceForgingMempoolSnapshot {} = []
  asMetrics (TraceForgedBlock slot _ _ _) =
    [IntM "Forge.ForgedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceDidntAdoptBlock slot _) =
    [IntM "Forge.NotAdoptedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceForgedInvalidBlock slot _ _) =
    [IntM "Forge.ForgedInvalidSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceAdoptedBlock slot _ _) =
    [IntM "Forge.AdoptedOwnBlockSlotLast" (fromIntegral $ unSlotNo slot)]

instance LogFormatting TraceStartLeadershipCheckPlus where
  forMachine _dtal TraceStartLeadershipCheckPlus {..} =
        mconcat [ "kind" .= String "TraceStartLeadershipCheck"
                , "slot" .= toJSON (unSlotNo tsSlotNo)
                , "utxoSize" .= Number (fromIntegral tsUtxoSize)
                , "delegMapSize" .= Number (fromIntegral tsUtxoSize)
                , "chainDensity" .= Number (fromRational (toRational tsChainDensity))
                ]
  forHuman TraceStartLeadershipCheckPlus {..} =
      "Checking for leadership in slot " <> showT (unSlotNo tsSlotNo)
      <> " utxoSize " <> showT tsUtxoSize
      <> " delegMapSize " <> showT tsDelegMapSize
      <> " chainDensity " <> showT tsChainDensity
  asMetrics TraceStartLeadershipCheckPlus {..} =
    [IntM "Forge.UtxoSize" (fromIntegral tsUtxoSize),
     IntM "Forge.DelegMapSize" (fromIntegral tsDelegMapSize)]

docForge :: Documented (Either (TraceForgeEvent blk)
                               TraceStartLeadershipCheckPlus)
docForge = addDocumentedNamespace [] docForge'

docForge' :: Documented (Either (TraceForgeEvent blk)
                               TraceStartLeadershipCheckPlus)
docForge' = Documented [
    DocMsg
      ["StartLeadershipCheck"]
      [("Forge.AboutToLeadSlotLast", "")]
      "Start of the leadership check."
  , DocMsg
      ["SlotIsImmutable"]
      [("Forge.SlotIsImmutable", "")]
      "Leadership check failed: the tip of the ImmutableDB inhabits the\
      \  current slot\
      \ \
      \  This might happen in two cases.\
      \ \
      \   1. the clock moved backwards, on restart we ignored everything from the\
      \      VolatileDB since it's all in the future, and now the tip of the\
      \      ImmutableDB points to a block produced in the same slot we're trying\
      \      to produce a block in\
      \ \
      \   2. k = 0 and we already adopted a block from another leader of the same\
      \      slot.\
      \ \
      \  We record both the current slot number as well as the tip of the\
      \  ImmutableDB.\
      \ \
      \ See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      ["BlockFromFuture"]
      [("Forge.BlockFromFuture", "")]
      "Leadership check failed: the current chain contains a block from a slot\
      \  /after/ the current slot\
      \ \
      \  This can only happen if the system is under heavy load.\
      \ \
      \  We record both the current slot number as well as the slot number of the\
      \  block at the tip of the chain.\
      \ \
      \  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      ["BlockContext"]
      [("Forge.BlockContext", "")]
      "We found out to which block we are going to connect the block we are about\
      \  to forge.\
      \ \
      \  We record the current slot number, the block number of the block to\
      \  connect to and its point.\
      \ \
      \  Note that block number of the block we will try to forge is one more than\
      \  the recorded block number."
  , DocMsg
      ["NoLedgerState"]
      [("Forge.CouldNotForgeSlotLast", "")]
      "Leadership check failed: we were unable to get the ledger state for the\
      \  point of the block we want to connect to\
      \ \
      \  This can happen if after choosing which block to connect to the node\
      \  switched to a different fork. We expect this to happen only rather\
      \  rarely, so this certainly merits a warning; if it happens a lot, that\
      \  merits an investigation.\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      ["LedgerState"]
      [("Forge.LedgerState", "")]
      "We obtained a ledger state for the point of the block we want to\
      \  connect to\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      ["NoLedgerView"]
      [("Forge.CouldNotForgeSlotLast", "")]
      "Leadership check failed: we were unable to get the ledger view for the\
      \  current slot number\
      \ \
      \  This will only happen if there are many missing blocks between the tip of\
      \  our chain and the current slot.\
      \ \
      \  We record also the failure returned by 'forecastFor'."
  , DocMsg
      ["LedgerView"]
      [("Forge.LedgerView", "")]
      "We obtained a ledger view for the current slot number\
      \ \
      \  We record the current slot number."
  , DocMsg
      ["ForgeStateUpdateError"]
      [ ("Forge.OperationalCertificateStartKESPeriod", "")
      , ("Forge.OperationalCertificateExpiryKESPeriod", "")
      , ("Forge.CurrentKESPeriod", "")
      , ("Forge.RemainingKESPeriods", "")
      ]
      "Updating the forge state failed.\
      \ \
      \  For example, the KES key could not be evolved anymore.\
      \ \
      \  We record the error returned by 'updateForgeState'."
  , DocMsg
      ["NodeCannotForge"]
      [("Forge.NodeCannotForge", "")]
      "We did the leadership check and concluded that we should lead and forge\
      \  a block, but cannot.\
      \ \
      \  This should only happen rarely and should be logged with warning severity.\
      \ \
      \  Records why we cannot forge a block."
  , DocMsg
      ["NodeNotLeader"]
      [("Forge.NodeNotLeader", "")]
      "We did the leadership check and concluded we are not the leader\
      \ \
      \  We record the current slot number"
  , DocMsg
      ["NodeIsLeader"]
      [("Forge.NodeIsLeader", "")]
      "We did the leadership check and concluded we /are/ the leader\
      \\n\
      \  The node will soon forge; it is about to read its transactions from the\
      \  Mempool. This will be followed by ForgedBlock."
  , DocMsg
      ["ForgedBlock"]
      [("Forge.ForgedSlotLast", "")]
      "We forged a block.\
      \\n\
      \  We record the current slot number, the point of the predecessor, the block\
      \  itself, and the total size of the mempool snapshot at the time we produced\
      \  the block (which may be significantly larger than the block, due to\
      \  maximum block size)\
      \\n\
      \  This will be followed by one of three messages:\
      \\n\
      \  * AdoptedBlock (normally)\
      \\n\
      \  * DidntAdoptBlock (rarely)\
      \\n\
      \  * ForgedInvalidBlock (hopefully never -- this would indicate a bug)"
  , DocMsg
      ["DidntAdoptBlock"]
      [("Forge.NotAdoptedSlotLast", "")]
      "We did not adopt the block we produced, but the block was valid. We\
      \  must have adopted a block that another leader of the same slot produced\
      \  before we got the chance of adopting our own block. This is very rare,\
      \  this warrants a warning."
  , DocMsg
      ["ForgedInvalidBlock"]
      [("Forge.ForgedInvalidSlotLast", "")]
      "We forged a block that is invalid according to the ledger in the\
      \  ChainDB. This means there is an inconsistency between the mempool\
      \  validation and the ledger validation. This is a serious error!"
  , DocMsg
      ["AdoptedBlock"]
      [("Forge.AdoptedOwnBlockSlotLast", "")]
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."
  , DocMsg
      ["StartLeadershipCheckPlus"]
      [ ("Forge.AboutToLeadSlotLast", "")
      , ("Forge.UtxoSize", "")
      , ("Forge.DelegMapSize", "")
      ]
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."

  ]

instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , GetHeader blk
         , HasHeader blk
         , HasKESInfo blk
         , LedgerSupportsProtocol blk
         , LedgerSupportsMempool blk
         , SerialiseNodeToNodeConstraints blk
         , HasTxId (GenTx blk)
         , Show (ForgeStateUpdateError blk)
         , Show (CannotForge blk)
         , LogFormatting (InvalidBlockReason blk)
         , LogFormatting (CannotForge blk)
         , LogFormatting (ForgeStateUpdateError blk))
         => LogFormatting (ForgeTracerType blk) where
  forMachine dtal (Left i)  = forMachine dtal i
  forMachine dtal (Right i) = forMachine dtal i
  forHuman (Left i)  = forHuman i
  forHuman (Right i) = forHuman i
  asMetrics (Left i)  = asMetrics i
  asMetrics (Right i) = asMetrics i

--------------------------------------------------------------------------------
-- BlockchainTimeEvent Tracer
--------------------------------------------------------------------------------

namesForBlockchainTime :: TraceBlockchainTimeEvent t -> [Text]
namesForBlockchainTime TraceStartTimeInTheFuture {} = ["StartTimeInTheFuture"]
namesForBlockchainTime TraceCurrentSlotUnknown {}   = ["CurrentSlotUnknown"]
namesForBlockchainTime TraceSystemClockMovedBack {} = ["SystemClockMovedBack"]

severityBlockchainTime :: TraceBlockchainTimeEvent t -> SeverityS
severityBlockchainTime TraceStartTimeInTheFuture {} = Warning
severityBlockchainTime TraceCurrentSlotUnknown {}   = Warning
severityBlockchainTime TraceSystemClockMovedBack {} = Warning

instance Show t => LogFormatting (TraceBlockchainTimeEvent t) where
    forMachine _dtal (TraceStartTimeInTheFuture (SystemStart start) toWait) =
        mconcat [ "kind" .= String "TStartTimeInTheFuture"
                 , "systemStart" .= String (showT start)
                 , "toWait" .= String (showT toWait)
                 ]
    forMachine _dtal (TraceCurrentSlotUnknown time _) =
        mconcat [ "kind" .= String "CurrentSlotUnknown"
                 , "time" .= String (showT time)
                 ]
    forMachine _dtal (TraceSystemClockMovedBack prevTime newTime) =
        mconcat [ "kind" .= String "SystemClockMovedBack"
                 , "prevTime" .= String (showT prevTime)
                 , "newTime" .= String (showT newTime)
                 ]
    forHuman (TraceStartTimeInTheFuture (SystemStart start) toWait) =
      "Waiting "
      <> (Text.pack . show) toWait
      <> " until genesis start time at "
      <> (Text.pack . show) start
    forHuman (TraceCurrentSlotUnknown time _) =
      "Too far from the chain tip to determine the current slot number for the time "
       <> (Text.pack . show) time
    forHuman (TraceSystemClockMovedBack prevTime newTime) =
      "The system wall clock time moved backwards, but within our tolerance "
      <> "threshold. Previous 'current' time: "
      <> (Text.pack . show) prevTime
      <> ". New 'current' time: "
      <> (Text.pack . show) newTime

docBlockchainTime :: Documented (TraceBlockchainTimeEvent t)
docBlockchainTime =
    addDocumentedNamespace [] docBlockchainTime'

docBlockchainTime' :: Documented (TraceBlockchainTimeEvent t)
docBlockchainTime' = Documented [
    DocMsg
      ["StartTimeInTheFuture"]
      []
      "The start time of the blockchain time is in the future\
      \\n\
      \ We have to block (for 'NominalDiffTime') until that time comes."
  , DocMsg
      ["CurrentSlotUnknown"]
      []
      "Current slot is not yet known\
      \\n\
      \ This happens when the tip of our current chain is so far in the past that\
      \ we cannot translate the current wallclock to a slot number, typically\
      \ during syncing. Until the current slot number is known, we cannot\
      \ produce blocks. Seeing this message during syncing therefore is\
      \ normal and to be expected.\
      \\n\
      \ We record the current time (the time we tried to translate to a 'SlotNo')\
      \ as well as the 'PastHorizonException', which provides detail on the\
      \ bounds between which we /can/ do conversions. The distance between the\
      \ current time and the upper bound should rapidly decrease with consecutive\
      \ 'CurrentSlotUnknown' messages during syncing."
  , DocMsg
      ["SystemClockMovedBack"]
      []
      "The system clock moved back an acceptable time span, e.g., because of\
      \ an NTP sync.\
      \\n\
      \ The system clock moved back such that the new current slot would be\
      \ smaller than the previous one. If this is within the configured limit, we\
      \ trace this warning but *do not change the current slot*. The current slot\
      \ never decreases, but the current slot may stay the same longer than\
      \ expected.\
      \\n\
      \ When the system clock moved back more than the configured limit, we shut\
      \ down with a fatal exception."
  ]

--------------------------------------------------------------------------------
-- KeepAliveClient Tracer
--------------------------------------------------------------------------------

namesForKeepAliveClient :: TraceKeepAliveClient peer -> [Text]
namesForKeepAliveClient _ = []

severityKeepAliveClient :: TraceKeepAliveClient peer -> SeverityS
severityKeepAliveClient _ = Info

instance Show remotePeer => LogFormatting (TraceKeepAliveClient remotePeer) where
    forMachine _dtal (AddSample peer rtt pgsv) =
        mconcat
          [ "kind" .= String "AddSample"
          , "address" .= show peer
          , "rtt" .= rtt
          , "sampleTime" .= show (dTime $ sampleTime pgsv)
          , "outboundG" .= (realToFrac $ gGSV (outboundGSV pgsv) :: Double)
          , "inboundG" .= (realToFrac $ gGSV (inboundGSV pgsv) :: Double)
          ]
        where
          gGSV :: GSV -> DiffTime
          gGSV (GSV g _ _) = g

          dTime :: Time -> Double
          dTime (Time d) = realToFrac d

    forHuman = showT

docKeepAliveClient :: Documented (TraceKeepAliveClient peer)
docKeepAliveClient = Documented [
    DocMsg
      []
      []
      ""
  ]
