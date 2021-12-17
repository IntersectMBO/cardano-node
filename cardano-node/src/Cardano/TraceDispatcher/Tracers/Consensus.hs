{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Tracers.Consensus
  (
    severityChainSyncClientEvent
  , namesForChainSyncClientEvent
  , docChainSyncClientEvent

  , severityChainSyncServerEvent
  , namesForChainSyncServerEvent
  , docChainSyncServerEvent

  , severityBlockFetchDecision
  , namesForBlockFetchDecision
  , docBlockFetchDecision

  , severityBlockFetchClient
  , namesForBlockFetchClient
  , docBlockFetchClient

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
import           Data.SOP.Strict
import qualified Data.Text as Text
import           Data.Time (DiffTime)
import           Text.Show

import           Cardano.Logging
import           Cardano.Node.Queries (HasKESInfo (..))
import           Cardano.Prelude hiding (All, Show, show)
import           Cardano.TraceDispatcher.Era.Byron ()
import           Cardano.TraceDispatcher.Era.Shelley ()
import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Render
import           Cardano.TraceDispatcher.Tracers.StartLeadershipCheck

import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))

import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent (..),
                     LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId, LedgerSupportsMempool, txForgetValidated)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..),
                     TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints,
                     estimateBlockSize)
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey

import           Ouroboros.Network.Block hiding (blockPrevHash)
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))


instance LogFormatting a => LogFormatting (TraceLabelCreds a) where
  forMachine dtal (TraceLabelCreds creds a)  =
    mkObject [ "credentials" .= toJSON creds
             , "val"         .= forMachine dtal a
            ]
-- TODO Trace lable creds as well
  forHuman (TraceLabelCreds _t a)         = forHuman a
  asMetrics (TraceLabelCreds _t a)        = asMetrics a


instance (LogFormatting (LedgerUpdate blk), LogFormatting (LedgerWarning blk))
      =>  LogFormatting (LedgerEvent blk) where
  forMachine dtal = \case
    LedgerUpdate  update  -> forMachine dtal update
    LedgerWarning warning -> forMachine dtal warning

tipToObject :: forall blk. ConvertRawHash blk => Tip blk -> [(Text, Value)]
tipToObject = \case
  TipGenesis ->
    [ "slot"    .= toJSON (0 :: Int)
    , "block"   .= String "genesis"
    , "blockNo" .= toJSON ((-1) :: Int)
    ]
  Tip slot hash blockno ->
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
namesForChainSyncClientEvent (BlockFetch.TraceLabelPeer _ e) =
    namesForChainSyncClientEvent' e

severityChainSyncClientEvent' :: TraceChainSyncClientEvent blk -> SeverityS
severityChainSyncClientEvent' TraceDownloadedHeader {}  = Info
severityChainSyncClientEvent' TraceFoundIntersection {} = Info
severityChainSyncClientEvent' TraceRolledBack {}        = Notice
severityChainSyncClientEvent' TraceException {}         = Warning
severityChainSyncClientEvent' TraceTermination {}       = Notice

namesForChainSyncClientEvent' :: TraceChainSyncClientEvent blk -> [Text]
namesForChainSyncClientEvent' TraceDownloadedHeader {} =
      ["ChainSyncClientEvent.DownloadedHeader"]
namesForChainSyncClientEvent' TraceFoundIntersection {} =
      ["ChainSyncClientEvent.FoundIntersection"]
namesForChainSyncClientEvent' TraceRolledBack {} =
      ["ChainSyncClientEvent.RolledBack"]
namesForChainSyncClientEvent' TraceException {} =
      ["ChainSyncClientEvent.Exception"]
namesForChainSyncClientEvent' TraceTermination {} =
      ["ChainSyncClientEvent.Termination"]

instance (Show (Header blk), ConvertRawHash blk, LedgerSupportsProtocol blk)
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
      mkObject $
               [ "kind" .= String "DownloadedHeader"
               ] <> tipToObject (tipFromHeader h)
  forMachine dtal (TraceRolledBack tip) =
      mkObject [ "kind" .= String "RolledBack"
               , "tip" .= forMachine dtal tip ]
  forMachine _dtal (TraceException exc) =
      mkObject [ "kind" .= String "Exception"
               , "exception" .= String (Text.pack $ show exc) ]
  forMachine _dtal TraceFoundIntersection {} =
      mkObject [ "kind" .= String "FoundIntersection" ]
  forMachine _dtal (TraceTermination reason) =
      mkObject [ "kind" .= String "Termination"
               , "reason" .= String (Text.pack $ show reason) ]

docChainSyncClientEvent ::
  Documented (BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk))
docChainSyncClientEvent = Documented [
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceDownloadedHeader anyProto))
      []
      "While following a candidate chain, we rolled forward by downloading a\
      \ header."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceRolledBack anyProto))
      []
      "While following a candidate chain, we rolled back to the given point."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceFoundIntersection anyProto anyProto anyProto))
      []
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceException anyProto))
      []
      "An exception was thrown by the Chain Sync Client."
  , DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (TraceTermination anyProto))
      []
      "The client has terminated."
  ]

--------------------------------------------------------------------------------
-- ChainSyncServer Tracer
--------------------------------------------------------------------------------

severityChainSyncServerEvent :: TraceChainSyncServerEvent blk -> SeverityS
severityChainSyncServerEvent TraceChainSyncServerRead        {} = Info
severityChainSyncServerEvent TraceChainSyncServerReadBlocked {} = Info
severityChainSyncServerEvent TraceChainSyncRollForward       {} = Info
severityChainSyncServerEvent TraceChainSyncRollBackward      {} = Info

namesForChainSyncServerEvent :: TraceChainSyncServerEvent blk -> [Text]
namesForChainSyncServerEvent TraceChainSyncServerRead        {} =
      ["ChainSyncServerEvent.ServerRead"]
namesForChainSyncServerEvent TraceChainSyncServerReadBlocked {} =
      ["ChainSyncServerEvent.ServerReadBlocked"]
namesForChainSyncServerEvent TraceChainSyncRollForward       {} =
      ["ChainSyncServerEvent.RollForward"]
namesForChainSyncServerEvent TraceChainSyncRollBackward      {} =
      ["ChainSyncServerEvent.RollBackward"]

instance ConvertRawHash blk
      => LogFormatting (TraceChainSyncServerEvent blk) where
  forMachine _dtal (TraceChainSyncServerRead tip (AddBlock _hdr)) =
      mkObject $
               [ "kind" .= String "ChainSyncServerRead.AddBlock"
               ] <> tipToObject tip
  forMachine _dtal (TraceChainSyncServerRead tip (RollBack _pt)) =
      mkObject $
               [ "kind" .= String "ChainSyncServerRead.RollBack"
               ] <> tipToObject tip
  forMachine _dtal (TraceChainSyncServerReadBlocked tip (AddBlock _hdr)) =
      mkObject $
               [ "kind" .= String "ChainSyncServerReadBlocked.AddBlock"
               ] <> tipToObject tip
  forMachine _dtal (TraceChainSyncServerReadBlocked tip (RollBack _pt)) =
      mkObject $
               [ "kind" .= String "ChainSyncServerReadBlocked.RollBack"
               ] <> tipToObject tip
  forMachine dtal (TraceChainSyncRollForward point) =
      mkObject [ "kind" .= String "ChainSyncServerRead.RollForward"
               , "point" .= forMachine dtal point
               ]
  forMachine dtal (TraceChainSyncRollBackward point) =
      mkObject [ "kind" .= String "ChainSyncServerRead.ChainSyncRollBackward"
               , "point" .= forMachine dtal point
               ]

  asMetrics (TraceChainSyncRollForward _point) =
      [CounterM "cardano.node.chainSync.rollForward" Nothing]
  asMetrics _ = []


docChainSyncServerEvent :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEvent = Documented [
    DocMsg
      (TraceChainSyncServerRead anyProto anyProto)
      []
      "A server read has occured, either for an add block or a rollback"
    , DocMsg
      (TraceChainSyncServerReadBlocked anyProto anyProto)
      []
      "A server read has blocked, either for an add block or a rollback"
    , DocMsg
      (TraceChainSyncRollForward anyProto)
      [("cardano.node.chainSync.rollForward", "TODO TracerDoc")]
      "Roll forward to the given point."
    , DocMsg
      (TraceChainSyncRollBackward anyProto)
      []
      "TODO TracerDoc"
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
  forMachine DMinimal _ = emptyObject
  forMachine _ []       = mkObject
    [ "kind"  .= String "EmptyPeersFetch"]
  forMachine _ xs       = mkObject
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> forMachine DDetailed x : acc) [] xs) ]

  asMetrics peers = [IntM "cardano.node.connectedPeers" (fromIntegral (length peers))]

instance (LogFormatting peer, Show peer, LogFormatting a)
  => LogFormatting (TraceLabelPeer peer a) where
  forMachine dtal (TraceLabelPeer peerid a) =
    mkObject [ "peer" .= forMachine dtal peerid ] <> forMachine dtal a
  forHuman (TraceLabelPeer peerid a) = "Peer is " <> showT peerid
                                        <> ". " <> forHuman a
  asMetrics (TraceLabelPeer _peerid a) = asMetrics a

instance LogFormatting (FetchDecision [Point header]) where
  forMachine _dtal (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (showT decline)
             ]
  forMachine _dtal (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (showT $ length results)
             ]

docBlockFetchDecision ::
  Documented [BlockFetch.TraceLabelPeer remotePeer (FetchDecision [Point (Header blk)])]
docBlockFetchDecision = Documented [
    DocMsg
      [BlockFetch.TraceLabelPeer anyProto (Right anyProto)]
      [("cardano.node.connectedPeers", "Number of connected peers")]
      "Throughout the decision making process we accumulate reasons to decline\
      \ to fetch any blocks. This message carries the intermediate and final\
      \ results."
  ]

--------------------------------------------------------------------------------
-- BlockFetchClient Tracer
--------------------------------------------------------------------------------

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
namesForBlockFetchClient' BlockFetch.CompletedBlockFetch  {} =
      ["CompletedBlockFetch"]
namesForBlockFetchClient' BlockFetch.CompletedFetchBatch {} =
      ["CompletedFetchBatch"]
namesForBlockFetchClient' BlockFetch.RejectedFetchBatch  {} =
      ["RejectedFetchBatch"]
namesForBlockFetchClient' BlockFetch.ClientTerminating {} =
      ["ClientTerminating"]

instance LogFormatting (BlockFetch.TraceFetchClientState header) where
  forMachine _dtal BlockFetch.AddedFetchRequest {} =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  forMachine _dtal BlockFetch.AcknowledgedFetchRequest {} =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  forMachine _dtal BlockFetch.SendFetchRequest {} =
    mkObject [ "kind" .= String "SendFetchRequest" ]
  forMachine _dtal BlockFetch.CompletedBlockFetch {} =
    mkObject [ "kind" .= String "CompletedBlockFetch" ]
  forMachine _dtal BlockFetch.CompletedFetchBatch {} =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  forMachine _dtal BlockFetch.StartedFetchBatch {} =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  forMachine _dtal BlockFetch.RejectedFetchBatch {} =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]
  forMachine _dtal BlockFetch.ClientTerminating {} =
    mkObject [ "kind" .= String "ClientTerminating" ]

docBlockFetchClient ::
  Documented (BlockFetch.TraceLabelPeer remotePeer (BlockFetch.TraceFetchClientState (Header blk)))
docBlockFetchClient = Documented [
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.AddedFetchRequest
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "The block fetch decision thread has added a new fetch instruction\
      \ consisting of one or more individual request ranges."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.AcknowledgedFetchRequest
          anyProto))
      []
      "Mark the point when the fetch client picks up the request added\
      \ by the block fetch decision thread. Note that this event can happen\
      \ fewer times than the 'AddedFetchRequest' due to fetch request merging."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.StartedFetchBatch
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "Mark the start of receiving a streaming batch of blocks. This will\
      \ be followed by one or more 'CompletedBlockFetch' and a final\
      \ 'CompletedFetchBatch'"
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.CompletedFetchBatch
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "Mark the successful end of receiving a streaming batch of blocks."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.RejectedFetchBatch
          anyProto
          anyProto
          anyProto
          anyProto))
      []
      "If the other peer rejects our request then we have this event\
      \ instead of 'StartedFetchBatch' and 'CompletedFetchBatch'."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer anyProto
        (BlockFetch.ClientTerminating 1))
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
    mkObject [ "kind" .= String "BlockFetchServer"
             , "block" .= String (renderChainHash
                                    @blk
                                    (renderHeaderHash (Proxy @blk))
                                    $ pointHash blk)]
-- TODO JNF
  asMetrics (TraceBlockFetchServerSendBlock _p) =
    [CounterM "cardano.node.served.block" Nothing]

docBlockFetchServer ::
  Documented (TraceBlockFetchServerEvent blk)
docBlockFetchServer = Documented [
    DocMsg
      (TraceBlockFetchServerSendBlock GenesisPoint)
      [("cardano.node.served.block", "TODO TracerDoc")]
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
    ["TxSubmissionCollected"]
namesForTxInbound' (TraceTxSubmissionProcessed _) =
    ["TxSubmissionProcessed"]
namesForTxInbound' TraceTxInboundTerminated   =
    ["TxInboundTerminated"]
namesForTxInbound' TraceTxInboundCanRequestMoreTxs {} =
    ["TxInboundCanRequestMoreTxs"]
namesForTxInbound' TraceTxInboundCannotRequestMoreTxs {} =
    ["TxInboundCannotRequestMoreTxs"]

instance LogFormatting (TraceTxSubmissionInbound txid tx) where
  forMachine _dtal (TraceTxSubmissionCollected count) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionCollected"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxSubmissionProcessed processed) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  forMachine _dtal TraceTxInboundTerminated =
    mkObject
      [ "kind" .= String "TraceTxInboundTerminated"
      ]
  forMachine _dtal (TraceTxInboundCanRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxInboundCannotRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]

  asMetrics (TraceTxSubmissionCollected count)=
    [CounterM "cardano.node.submissions.submitted" (Just count)]
  asMetrics (TraceTxSubmissionProcessed processed) =
    [ CounterM "cardano.node.submissions.accepted"
        (Just (ptxcAccepted processed))
    , CounterM "cardano.node.submissions.rejected"
        (Just (ptxcRejected processed))
    ]
  asMetrics _ = []

docTxInbound ::
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionInbound txid tx))
docTxInbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionCollected 1))
    [ ("cardano.node.submissions.submitted", "TODO TracerDoc")]
    "Number of transactions just about to be inserted."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionProcessed (ProcessedTxCount 1 2)))
    [ ("cardano.node.submissions.accepted", "TODO TracerDoc")
    , ("cardano.node.submissions.rejected", "TODO TracerDoc")
    ]
    "Just processed transaction pass/fail breakdown."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      TraceTxInboundTerminated)
    []
    "Server received 'MsgDone'."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxInboundCanRequestMoreTxs 1))
    []
    "There are no replies in flight, but we do know some more txs we\
    \ can ask for, so lets ask for them and more txids."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxInboundCannotRequestMoreTxs 1))
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
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (Text.pack $ show txids)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  forMachine DDetailed (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (Text.pack $ show txs)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]
  forMachine _dtal (TraceControlMessage _msg) =
    mkObject
      [ "kind" .= String "TraceControlMessage"
      ]

docTxOutbound :: forall remotePeer txid tx.
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionOutbound txid tx))
docTxOutbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionOutboundRecvMsgRequestTxs anyProto))
    []
    "The IDs of the transactions requested."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceTxSubmissionOutboundSendMsgReplyTxs anyProto))
    []
    "The transactions to be sent in the response."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer anyProto
      (TraceControlMessage anyProto))
    []
    "TODO TracerDoc"
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
    mkObject [ "kind" .= String "ReceivedTx" ]

docLocalTxSubmissionServer :: Documented (TraceLocalTxSubmissionServerEvent blk)
docLocalTxSubmissionServer = Documented [
    DocMsg
    (TraceReceivedTx anyProto)
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
    mkObject
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= forMachine dtal (txForgetValidated tx)
      , "mempoolSize" .= forMachine dtal mpSzAfter
      ]
  forMachine dtal (TraceMempoolRejectedTx tx txApplyErr mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "err" .= forMachine dtal txApplyErr
      , "tx" .= forMachine dtal tx
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolRemoveTxs txs mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs" .= map (forMachine dtal . txForgetValidated) txs
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsRemoved" .= txs0
      , "txsInvalidated" .= map (forMachine dtal . txForgetValidated) txs1
      , "mempoolSize" .= forMachine dtal mpSz
      ]

  asMetrics (TraceMempoolAddedTx _tx _mpSzBefore mpSz) =
    [ IntM "cardano.node.txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "cardano.node.mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRejectedTx _tx _txApplyErr mpSz) =
    [ IntM "cardano.node.txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "cardano.node.mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRemoveTxs _txs mpSz) =
    [ IntM "cardano.node.txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "cardano.node.mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs [] _txs1 mpSz) =
    [ IntM "cardano.node.txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "cardano.node.mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs txs _txs1 mpSz) =
    [ IntM "cardano.node.txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "cardano.node.mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    , CounterM "cardano.node.txsProcessedNum" (Just (fromIntegral $ length txs))
    ]

instance LogFormatting MempoolSize where
  forMachine _dtal MempoolSize{msNumTxs, msNumBytes} =
    mkObject
      [ "numTxs" .= msNumTxs
      , "bytes" .= msNumBytes
      ]

docMempool :: forall blk. Documented (TraceEventMempool blk)
docMempool = Documented [
    DocMsg
      (TraceMempoolAddedTx anyProto anyProto anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      ]
      "New, valid transaction that was added to the Mempool."
  , DocMsg
      (TraceMempoolRejectedTx anyProto anyProto anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      ]
      "New, invalid transaction thas was rejected and thus not added to\
      \ the Mempool."
  , DocMsg
      (TraceMempoolRemoveTxs [anyProto] anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      ]
      "Previously valid transactions that are no longer valid because of\
      \ changes in the ledger state. These transactions have been removed\
      \ from the Mempool."
  , DocMsg
      (TraceMempoolManuallyRemovedTxs [anyProto] [anyProto] anyProto)
      [ ("cardano.node.txsInMempool","Transactions in mempool")
      , ("cardano.node.mempoolBytes", "Byte size of the mempool")
      , ("cardano.node.txsProcessedNum", "TODO TracerDoc")
      ]
      "Transactions that have been manually removed from the Mempool."
  ]


--------------------------------------------------------------------------------
-- ForgeEvent Tracer
--------------------------------------------------------------------------------

severityForge :: ForgeTracerType blk -> SeverityS
severityForge (Left t)  = severityForge' t
severityForge (Right t) = severityForge''' t

severityForge' :: TraceLabelCreds (TraceForgeEvent blk) -> SeverityS
severityForge' (TraceLabelCreds _t e) = severityForge'' e

severityForge'' :: TraceForgeEvent blk -> SeverityS
severityForge'' TraceStartLeadershipCheck {}  = Info
severityForge'' TraceSlotIsImmutable {}       = Error
severityForge'' TraceBlockFromFuture {}       = Error
severityForge'' TraceBlockContext {}          = Debug
severityForge'' TraceNoLedgerState {}         = Error
severityForge'' TraceLedgerState {}           = Debug
severityForge'' TraceNoLedgerView {}          = Error
severityForge'' TraceLedgerView {}            = Debug
severityForge'' TraceForgeStateUpdateError {} = Error
severityForge'' TraceNodeCannotForge {}       = Error
severityForge'' TraceNodeNotLeader {}         = Info
severityForge'' TraceNodeIsLeader {}          = Info
severityForge'' TraceForgedBlock {}           = Info
severityForge'' TraceDidntAdoptBlock {}       = Error
severityForge'' TraceForgedInvalidBlock {}    = Error
severityForge'' TraceAdoptedBlock {}          = Info

severityForge''' :: TraceLabelCreds TraceStartLeadershipCheckPlus -> SeverityS
severityForge''' _ = Info

namesForForge :: ForgeTracerType blk -> [Text]
namesForForge (Left t)  = namesForForge' t
namesForForge (Right t) = namesForForge''' t

namesForForge' :: TraceLabelCreds (TraceForgeEvent blk) -> [Text]
namesForForge' (TraceLabelCreds _t e) = namesForForge'' e

namesForForge'' :: TraceForgeEvent blk -> [Text]
namesForForge'' TraceStartLeadershipCheck {}  = ["StartLeadershipCheck"]
namesForForge'' TraceSlotIsImmutable {}       = ["SlotIsImmutable"]
namesForForge'' TraceBlockFromFuture {}       = ["BlockFromFuture"]
namesForForge'' TraceBlockContext {}          = ["BlockContext"]
namesForForge'' TraceNoLedgerState {}         = ["NoLedgerState"]
namesForForge'' TraceLedgerState {}           = ["LedgerState"]
namesForForge'' TraceNoLedgerView {}          = ["NoLedgerView"]
namesForForge'' TraceLedgerView {}            = ["LedgerView"]
namesForForge'' TraceForgeStateUpdateError {} = ["ForgeStateUpdateError"]
namesForForge'' TraceNodeCannotForge {}       = ["NodeCannotForge"]
namesForForge'' TraceNodeNotLeader {}         = ["NodeNotLeader"]
namesForForge'' TraceNodeIsLeader {}          = ["NodeIsLeader"]
namesForForge'' TraceForgedBlock {}           = ["ForgedBlock"]
namesForForge'' TraceDidntAdoptBlock {}       = ["DidntAdoptBlock"]
namesForForge'' TraceForgedInvalidBlock {}    = ["ForgedInvalidBlock"]
namesForForge'' TraceAdoptedBlock {}          = ["AdoptedBlock"]

namesForForge''' :: TraceLabelCreds TraceStartLeadershipCheckPlus -> [Text]
namesForForge''' (TraceLabelCreds _ TraceStartLeadershipCheckPlus {})  =
  ["StartLeadershipCheckPlus"]


instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , GetHeader blk
         , HasHeader blk
         , HasKESInfo blk
         , LedgerSupportsProtocol blk
         , SerialiseNodeToNodeConstraints blk
         , Show (ForgeStateUpdateError blk)
         , Show (CannotForge blk)
         , LogFormatting (InvalidBlockReason blk)
         , LogFormatting (CannotForge blk)
         , LogFormatting (ForgeStateUpdateError blk))
      => LogFormatting (TraceForgeEvent blk) where
  forMachine _dtal (TraceStartLeadershipCheck slotNo) =
    mkObject
      [ "kind" .= String "TraceStartLeadershipCheck"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    mkObject
      [ "kind" .= String "TraceSlotIsImmutable"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceBlockFromFuture currentSlot tip) =
    mkObject
      [ "kind" .= String "TraceBlockFromFuture"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= toJSON (unSlotNo tip)
      ]
  forMachine dtal (TraceBlockContext currentSlot tipBlkNo tipPoint) =
    mkObject
      [ "kind" .= String "TraceBlockContext"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceNoLedgerState slotNo _pt) =
    mkObject
      [ "kind" .= String "TraceNoLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerState slotNo _pt) =
    mkObject
      [ "kind" .= String "TraceLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNoLedgerView slotNo _) =
    mkObject
      [ "kind" .= String "TraceNoLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerView slotNo) =
    mkObject
      [ "kind" .= String "TraceLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgeStateUpdateError slotNo reason) =
    mkObject
      [ "kind" .= String "TraceForgeStateUpdateError"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine dtal (TraceNodeCannotForge slotNo reason) =
    mkObject
      [ "kind" .= String "TraceNodeCannotForge"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine _dtal (TraceNodeNotLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeNotLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNodeIsLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeIsLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceForgedBlock slotNo _ blk _) =
    mkObject
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
    mkObject
      [ "kind" .= String "TraceDidntAdoptBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgedInvalidBlock slotNo _ reason) =
    mkObject
      [ "kind" .= String "TraceForgedInvalidBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine DDetailed (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          DDetailed
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
--      , "txIds" .= toJSON (map (show . txId) txs) TODO JNF
      ]
  forMachine dtal (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
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
      --  <> ", TxIds: " <> showT (map txId txs) TODO Fix

  asMetrics (TraceForgeStateUpdateError slot reason) =
    IntM "cardano.node.forgeStateUpdateError" (fromIntegral $ unSlotNo slot) :
      (case getKESInfo (Proxy @blk) reason of
        Nothing -> []
        Just kesInfo ->
          [ IntM
              "cardano.node.operationalCertificateStartKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesStartPeriod $ kesInfo)
          , IntM
              "cardano.node.operationalCertificateExpiryKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesEndPeriod $ kesInfo)
          , IntM
              "cardano.node.currentKESPeriod"
              0
          , IntM
              "cardano.node.remainingKESPeriods"
              0
          ])

  asMetrics (TraceStartLeadershipCheck slot) =
    [IntM "cardano.node.aboutToLeadSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceSlotIsImmutable slot _tipPoint _tipBlkNo) =
    [IntM "cardano.node.slotIsImmutable" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockFromFuture slot _slotNo) =
    [IntM "cardano.node.blockFromFuture" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockContext slot _tipBlkNo _tipPoint) =
    [IntM "cardano.node.blockContext" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerState slot _) =
    [IntM "cardano.node.couldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerState slot _) =
    [IntM "cardano.node.ledgerState" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerView slot _) =
    [IntM "cardano.node.couldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerView slot) =
    [IntM "cardano.node.ledgerView" (fromIntegral $ unSlotNo slot)]
  -- see above
  asMetrics (TraceNodeCannotForge slot _reason) =
    [IntM "cardano.node.nodeCannotForge" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeNotLeader slot) =
    [IntM "cardano.node.nodeNotLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeIsLeader slot) =
    [IntM "cardano.node.nodeIsLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceForgedBlock slot _ _ _) =
    [IntM "cardano.node.forgedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceDidntAdoptBlock slot _) =
    [IntM "cardano.node.notAdoptedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceForgedInvalidBlock slot _ _) =
    [IntM "cardano.node.forgedInvalidSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceAdoptedBlock slot _ _) =
    [IntM "cardano.node.adoptedSlotLast" (fromIntegral $ unSlotNo slot)]

instance LogFormatting TraceStartLeadershipCheckPlus where
  forMachine _dtal TraceStartLeadershipCheckPlus {..} =
        mkObject [ "kind" .= String "TraceStartLeadershipCheckPlus"
                 , "slotNo" .= toJSON (unSlotNo tsSlotNo)
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
    [IntM "cardano.node.utxoSize" (fromIntegral tsUtxoSize),
     IntM "cardano.node.delegMapSize" (fromIntegral tsDelegMapSize)]
     -- TODO JNF: Why not deleg map size?


docForge :: Documented (Either (TraceLabelCreds (TraceForgeEvent blk))
                               (TraceLabelCreds TraceStartLeadershipCheckPlus))
docForge = Documented [
    DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceStartLeadershipCheck anyProto)))
      [("cardano.node.aboutToLeadSlotLast", "TODO TracerDoc")]
      "Start of the leadership check."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceSlotIsImmutable anyProto anyProto anyProto)))
      [("cardano.node.slotIsImmutable", "TODO TracerDoc")]
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
      (Left (TraceLabelCreds anyProto
        (TraceBlockFromFuture anyProto anyProto)))
      [("cardano.node.blockFromFuture", "TODO TracerDoc")]
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
      (Left (TraceLabelCreds anyProto
        (TraceBlockContext anyProto anyProto anyProto)))
      [("cardano.node.blockContext", "TODO TracerDoc")]
      "We found out to which block we are going to connect the block we are about\
      \  to forge.\
      \ \
      \  We record the current slot number, the block number of the block to\
      \  connect to and its point.\
      \ \
      \  Note that block number of the block we will try to forge is one more than\
      \  the recorded block number."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNoLedgerState anyProto anyProto)))
      [("cardano.node.couldNotForgeSlotLast", "TODO TracerDoc")]
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
      (Left (TraceLabelCreds anyProto
        (TraceLedgerState anyProto anyProto)))
      [("cardano.node.ledgerState", "TODO TracerDoc")]
      "We obtained a ledger state for the point of the block we want to\
      \  connect to\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNoLedgerView anyProto anyProto)))
      [("cardano.node.couldNotForgeSlotLast", "TODO TracerDoc")]
      "Leadership check failed: we were unable to get the ledger view for the\
      \  current slot number\
      \ \
      \  This will only happen if there are many missing blocks between the tip of\
      \  our chain and the current slot.\
      \ \
      \  We record also the failure returned by 'forecastFor'."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceLedgerView anyProto)))
      [("cardano.node.ledgerView", "TODO TracerDoc")]
      "We obtained a ledger view for the current slot number\
      \ \
      \  We record the current slot number."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceForgeStateUpdateError anyProto anyProto)))
      [ ("cardano.node.operationalCertificateStartKESPeriod", "TODO TracerDoc")
      , ("cardano.node.operationalCertificateExpiryKESPeriod", "TODO TracerDoc")
      , ("cardano.node.currentKESPeriod", "TODO TracerDoc")
      , ("cardano.node.remainingKESPeriods", "TODO TracerDoc")
      ]
      "Updating the forge state failed.\
      \ \
      \  For example, the KES key could not be evolved anymore.\
      \ \
      \  We record the error returned by 'updateForgeState'."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNodeCannotForge anyProto anyProto)))
      [("cardano.node.nodeCannotForge", "TODO TracerDoc")]
      "We did the leadership check and concluded that we should lead and forge\
      \  a block, but cannot.\
      \ \
      \  This should only happen rarely and should be logged with warning severity.\
      \ \
      \  Records why we cannot forge a block."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNodeNotLeader anyProto)))
      [("cardano.node.nodeNotLeader", "TODO TracerDoc")]
      "We did the leadership check and concluded we are not the leader\
      \ \
      \  We record the current slot number"
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceNodeIsLeader anyProto)))
      [("cardano.node.nodeIsLeader", "TODO TracerDoc")]
      "We did the leadership check and concluded we /are/ the leader\
      \\n\
      \  The node will soon forge; it is about to read its transactions from the\
      \  Mempool. This will be followed by ForgedBlock."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceForgedBlock anyProto anyProto anyProto anyProto)))
      [("cardano.node.forgedSlotLast", "TODO TracerDoc")]
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
      (Left (TraceLabelCreds anyProto
        (TraceDidntAdoptBlock anyProto anyProto)))
      [("cardano.node.notAdoptedSlotLast", "TODO TracerDoc")]
      "We did not adopt the block we produced, but the block was valid. We\
      \  must have adopted a block that another leader of the same slot produced\
      \  before we got the chance of adopting our own block. This is very rare,\
      \  this warrants a warning."
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceForgedInvalidBlock anyProto anyProto anyProto)))
      [("cardano.node.forgedInvalidSlotLast", "TODO TracerDoc")]
      "We forged a block that is invalid according to the ledger in the\
      \  ChainDB. This means there is an inconsistency between the mempool\
      \  validation and the ledger validation. This is a serious error!"
  , DocMsg
      (Left (TraceLabelCreds anyProto
        (TraceAdoptedBlock anyProto anyProto [anyProto])))
      [("cardano.node.adoptedSlotLast", "TODO TracerDoc")]
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."
  , DocMsg
      (Right (TraceLabelCreds anyProto
        (TraceStartLeadershipCheckPlus anyProto 0 0 0.0)))
      [ ("cardano.node.aboutToLeadSlotLast", "TODO TracerDoc")
      , ("cardano.node.utxoSize", "TODO TracerDoc")
      , ("cardano.node.delegMapSize", "TODO TracerDoc")
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
         , SerialiseNodeToNodeConstraints blk
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
        mkObject [ "kind" .= String "TStartTimeInTheFuture"
                 , "systemStart" .= String (showT start)
                 , "toWait" .= String (showT toWait)
                 ]
    forMachine _dtal (TraceCurrentSlotUnknown time _) =
        mkObject [ "kind" .= String "CurrentSlotUnknown"
                 , "time" .= String (showT time)
                 ]
    forMachine _dtal (TraceSystemClockMovedBack prevTime newTime) =
        mkObject [ "kind" .= String "SystemClockMovedBack"
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
docBlockchainTime = Documented [
    DocMsg
      (TraceStartTimeInTheFuture anyProto anyProto)
      []
      "The start time of the blockchain time is in the future\
      \\n\
      \ We have to block (for 'NominalDiffTime') until that time comes."
  , DocMsg
      (TraceCurrentSlotUnknown anyProto anyProto)
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
      (TraceSystemClockMovedBack anyProto anyProto)
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
namesForKeepAliveClient _ = ["KeepAliveClient"]

severityKeepAliveClient :: TraceKeepAliveClient peer -> SeverityS
severityKeepAliveClient _ = Info

instance Show remotePeer => LogFormatting (TraceKeepAliveClient remotePeer) where
    forMachine _dtal (AddSample peer rtt pgsv) =
        mkObject
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
      (AddSample anyProto anyProto anyProto)
      []
      "TODO TracerDoc"
  ]
