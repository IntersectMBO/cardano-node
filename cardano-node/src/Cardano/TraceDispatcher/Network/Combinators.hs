{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.TraceDispatcher.Network.Combinators
  (
    severityTChainSync
  , namesForTChainSync

  , severityTTxSubmission
  , namesForTTxSubmission

  , severityTStateQuery
  , namesForTStateQuery

  , severityTChainSyncNode
  , namesForTChainSyncNode

  , severityTChainSyncSerialised
  , namesForTChainSyncSerialised

  , severityTBlockFetch
  , namesForTBlockFetch

  , severityTBlockFetchSerialised
  , namesForTBlockFetchSerialised

  , severityTxSubmissionNode
  , namesForTxSubmissionNode

  , severityTxSubmission2Node
  , namesForTxSubmission2Node

  , severityIPSubscription
  , namesForIPSubscription

  , severityDNSSubscription
  , namesForDNSSubscription

  ) where


import           Cardano.Logging
import           Cardano.Prelude

import qualified Network.Socket as Socket

import           Ouroboros.Network.Block (Point, Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..),
                     Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..),
                     Message (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Hello,
                     Message (..))
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TXS
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TXS
import           Ouroboros.Network.Subscription.Dns (WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (ConnectResult (..),
                     SubscriberError, SubscriptionTrace (..))

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)


severityTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSync (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessageAndAgency _agency msg) = severityTChainSync''' msg

    severityTChainSync''' :: Message
                                     (ChainSync header point tip) from to
                                   -> SeverityS
    severityTChainSync''' MsgRequestNext {}       = Info
    severityTChainSync''' MsgAwaitReply {}        = Info
    severityTChainSync''' MsgRollForward {}       = Info
    severityTChainSync''' MsgRollBackward {}      = Info
    severityTChainSync''' MsgFindIntersect {}     = Info
    severityTChainSync''' MsgIntersectFound {}    = Info
    severityTChainSync''' MsgIntersectNotFound {} = Info
    severityTChainSync''' MsgDone {}              = Info

namesForTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> [Text]
namesForTChainSync (BlockFetch.TraceLabelPeer _ v) = "NodeToClient" : namesTChainSync v
  where

    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Recieve" : namesTChainSync' msg

    namesTChainSync' (AnyMessageAndAgency _agency msg) = namesTChainSync'' msg

    namesTChainSync'' :: Message (ChainSync header point tip) from to
                               -> [Text]
    namesTChainSync'' MsgRequestNext {}       = ["RequestNext"]
    namesTChainSync'' MsgAwaitReply {}        = ["AwaitReply"]
    namesTChainSync'' MsgRollForward {}       = ["RollForward"]
    namesTChainSync'' MsgRollBackward {}      = ["RollBackward"]
    namesTChainSync'' MsgFindIntersect {}     = ["FindIntersect"]
    namesTChainSync'' MsgIntersectFound {}    = ["IntersectFound"]
    namesTChainSync'' MsgIntersectNotFound {} = ["IntersectNotFound"]
    namesTChainSync'' MsgDone {}              = ["Done"]

severityTTxSubmission :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LTS.LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -> SeverityS
severityTTxSubmission (BlockFetch.TraceLabelPeer _ v) = severityTTxSubmission' v
  where
    severityTTxSubmission' (TraceSendMsg msg) = severityTTxSubmission'' msg
    severityTTxSubmission' (TraceRecvMsg msg) = severityTTxSubmission'' msg

    severityTTxSubmission'' (AnyMessageAndAgency _agency msg) = severityTTxSubmission''' msg

    severityTTxSubmission''' :: Message
                                        (LTS.LocalTxSubmission tx reject) from to
                                      -> SeverityS
    severityTTxSubmission''' LTS.MsgSubmitTx {} = Info
    severityTTxSubmission''' LTS.MsgAcceptTx {} = Info
    severityTTxSubmission''' LTS.MsgRejectTx {} = Info
    severityTTxSubmission''' LTS.MsgDone {}     = Info


namesForTTxSubmission :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LTS.LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -> [Text]
namesForTTxSubmission (BlockFetch.TraceLabelPeer _ v) = namesTTxSubmission v
  where
    namesTTxSubmission (TraceSendMsg msg) = "Send" : namesTTxSubmission' msg
    namesTTxSubmission (TraceRecvMsg msg) = "Recieve" : namesTTxSubmission' msg

    namesTTxSubmission' (AnyMessageAndAgency _agency msg) = namesTTxSubmission'' msg

    namesTTxSubmission'' :: Message
                                    (LTS.LocalTxSubmission tx reject) from to
                                  -> [Text]
    namesTTxSubmission'' LTS.MsgSubmitTx {} = ["SubmitTx"]
    namesTTxSubmission'' LTS.MsgAcceptTx {} = ["AcceptTx"]
    namesTTxSubmission'' LTS.MsgRejectTx {} = ["RejectTx"]
    namesTTxSubmission'' LTS.MsgDone {}     = ["Done"]

severityTStateQuery :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LSQ.LocalStateQuery blk (Point blk) query))
  -> SeverityS
severityTStateQuery (BlockFetch.TraceLabelPeer _ v) = severityTStateQuery' v
  where
    severityTStateQuery' (TraceSendMsg msg) = severityTStateQuery'' msg
    severityTStateQuery' (TraceRecvMsg msg) = severityTStateQuery'' msg

    severityTStateQuery'' (AnyMessageAndAgency _agency msg) = severityTStateQuery''' msg

    severityTStateQuery''' :: Message
                                    (LSQ.LocalStateQuery block point query1) from to
                                  -> SeverityS
    severityTStateQuery''' LSQ.MsgAcquire {}   = Info
    severityTStateQuery''' LSQ.MsgAcquired {}  = Info
    severityTStateQuery''' LSQ.MsgFailure {}   = Warning
    severityTStateQuery''' LSQ.MsgQuery {}     = Info
    severityTStateQuery''' LSQ.MsgResult {}    = Info
    severityTStateQuery''' LSQ.MsgRelease {}   = Info
    severityTStateQuery''' LSQ.MsgReAcquire {} = Info
    severityTStateQuery''' LSQ.MsgDone {}      = Info

namesForTStateQuery :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LSQ.LocalStateQuery blk (Point blk) query))
  -> [Text]
namesForTStateQuery (BlockFetch.TraceLabelPeer _ v) = namesForTStateQuery' v
  where
    namesForTStateQuery' (TraceSendMsg msg) = namesForTStateQuery'' msg
    namesForTStateQuery' (TraceRecvMsg msg) = namesForTStateQuery'' msg

    namesForTStateQuery'' (AnyMessageAndAgency _agency msg) = namesForTStateQuery''' msg

    namesForTStateQuery''' :: Message
                                    (LSQ.LocalStateQuery block point query1) from to
                                  -> [Text]

    namesForTStateQuery''' LSQ.MsgAcquire {}   = ["Acquire"]
    namesForTStateQuery''' LSQ.MsgAcquired {}  = ["Acquired"]
    namesForTStateQuery''' LSQ.MsgFailure {}   = ["Acquired"]
    namesForTStateQuery''' LSQ.MsgQuery {}     = ["Query"]
    namesForTStateQuery''' LSQ.MsgResult {}    = ["Result"]
    namesForTStateQuery''' LSQ.MsgRelease {}   = ["Release"]
    namesForTStateQuery''' LSQ.MsgReAcquire {} = ["ReAcquire"]
    namesForTStateQuery''' LSQ.MsgDone {}      = ["Done"]

severityTChainSyncNode :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Header blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSyncNode (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessageAndAgency _agency msg) = severityTChainSync''' msg

    severityTChainSync''' :: Message
                                     (ChainSync header point tip) from to
                                   -> SeverityS
    severityTChainSync''' MsgRequestNext {}       = Info
    severityTChainSync''' MsgAwaitReply {}        = Info
    severityTChainSync''' MsgRollForward {}       = Info
    severityTChainSync''' MsgRollBackward {}      = Info
    severityTChainSync''' MsgFindIntersect {}     = Info
    severityTChainSync''' MsgIntersectFound {}    = Info
    severityTChainSync''' MsgIntersectNotFound {} = Info
    severityTChainSync''' MsgDone {}              = Info

namesForTChainSyncNode :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Header blk) (Point blk) (Tip blk))) -> [Text]
namesForTChainSyncNode (BlockFetch.TraceLabelPeer _ v) = "NodeToNode" : namesTChainSync v
  where

    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Recieve" : namesTChainSync' msg

    namesTChainSync' (AnyMessageAndAgency _agency msg) = namesTChainSync'' msg

    namesTChainSync'' :: Message (ChainSync header point tip) from to
                               -> [Text]
    namesTChainSync'' MsgRequestNext {}       = ["RequestNext"]
    namesTChainSync'' MsgAwaitReply {}        = ["AwaitReply"]
    namesTChainSync'' MsgRollForward {}       = ["RollForward"]
    namesTChainSync'' MsgRollBackward {}      = ["RollBackward"]
    namesTChainSync'' MsgFindIntersect {}     = ["FindIntersect"]
    namesTChainSync'' MsgIntersectFound {}    = ["IntersectFound"]
    namesTChainSync'' MsgIntersectNotFound {} = ["IntersectNotFound"]
    namesTChainSync'' MsgDone {}              = ["Done"]

severityTChainSyncSerialised :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSyncSerialised (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessageAndAgency _agency msg) = severityTChainSync''' msg

    severityTChainSync''' :: Message
                                     (ChainSync header point tip) from to
                                   -> SeverityS
    severityTChainSync''' MsgRequestNext {}       = Info
    severityTChainSync''' MsgAwaitReply {}        = Info
    severityTChainSync''' MsgRollForward {}       = Info
    severityTChainSync''' MsgRollBackward {}      = Info
    severityTChainSync''' MsgFindIntersect {}     = Info
    severityTChainSync''' MsgIntersectFound {}    = Info
    severityTChainSync''' MsgIntersectNotFound {} = Info
    severityTChainSync''' MsgDone {}              = Info

namesForTChainSyncSerialised :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))) -> [Text]
namesForTChainSyncSerialised (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : "Serialised" : namesTChainSync v
  where
    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Recieve" : namesTChainSync' msg

    namesTChainSync' (AnyMessageAndAgency _agency msg) = namesTChainSync'' msg

    namesTChainSync'' :: Message (ChainSync header point tip) from to
                               -> [Text]
    namesTChainSync'' MsgRequestNext {}       = ["RequestNext"]
    namesTChainSync'' MsgAwaitReply {}        = ["AwaitReply"]
    namesTChainSync'' MsgRollForward {}       = ["RollForward"]
    namesTChainSync'' MsgRollBackward {}      = ["RollBackward"]
    namesTChainSync'' MsgFindIntersect {}     = ["FindIntersect"]
    namesTChainSync'' MsgIntersectFound {}    = ["IntersectFound"]
    namesTChainSync'' MsgIntersectNotFound {} = ["IntersectNotFound"]
    namesTChainSync'' MsgDone {}              = ["Done"]

severityTBlockFetch :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch blk (Point blk))) -> SeverityS
severityTBlockFetch (BlockFetch.TraceLabelPeer _ v) = severityTBlockFetch' v
  where
    severityTBlockFetch' (TraceSendMsg msg) = severityTBlockFetch'' msg
    severityTBlockFetch' (TraceRecvMsg msg) = severityTBlockFetch'' msg

    severityTBlockFetch'' (AnyMessageAndAgency _agency msg) = severityTBlockFetch''' msg

    severityTBlockFetch''' :: Message (BlockFetch x (Point blk)) from to
                                   -> SeverityS
    severityTBlockFetch''' MsgRequestRange {} = Info
    severityTBlockFetch''' MsgStartBatch {}   = Info
    severityTBlockFetch''' MsgNoBlocks {}     = Info
    severityTBlockFetch''' MsgBlock {}        = Info
    severityTBlockFetch''' MsgBatchDone {}    = Info
    severityTBlockFetch''' MsgClientDone {}   = Info

namesForTBlockFetch :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch blk (Point blk))) -> [Text]
namesForTBlockFetch (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : namesTBlockFetch v
  where
    namesTBlockFetch (TraceSendMsg msg) = "Send" : namesTBlockFetch' msg
    namesTBlockFetch (TraceRecvMsg msg) = "Recieve" : namesTBlockFetch' msg

    namesTBlockFetch' (AnyMessageAndAgency _agency msg) = namesTBlockFetch'' msg

    namesTBlockFetch'' :: Message (BlockFetch x (Point blk)) from to
                               -> [Text]
    namesTBlockFetch'' MsgRequestRange {} = ["RequestRange"]
    namesTBlockFetch'' MsgStartBatch {}   = ["StartBatch"]
    namesTBlockFetch'' MsgNoBlocks {}     = ["NoBlocks"]
    namesTBlockFetch'' MsgBlock {}        = ["Block"]
    namesTBlockFetch'' MsgBatchDone {}    = ["BatchDone"]
    namesTBlockFetch'' MsgClientDone {}   = ["ClientDone"]

severityTBlockFetchSerialised :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))) -> SeverityS
severityTBlockFetchSerialised (BlockFetch.TraceLabelPeer _ v) = severityTBlockFetch' v
  where
    severityTBlockFetch' (TraceSendMsg msg) = severityTBlockFetch'' msg
    severityTBlockFetch' (TraceRecvMsg msg) = severityTBlockFetch'' msg

    severityTBlockFetch'' (AnyMessageAndAgency _agency msg) = severityTBlockFetch''' msg

    severityTBlockFetch''' :: Message (BlockFetch x (Point blk)) from to
                                   -> SeverityS
    severityTBlockFetch''' MsgRequestRange {} = Info
    severityTBlockFetch''' MsgStartBatch {}   = Info
    severityTBlockFetch''' MsgNoBlocks {}     = Info
    severityTBlockFetch''' MsgBlock {}        = Info
    severityTBlockFetch''' MsgBatchDone {}    = Info
    severityTBlockFetch''' MsgClientDone {}   = Info

namesForTBlockFetchSerialised :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))) -> [Text]
namesForTBlockFetchSerialised (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : "Serialised" : namesTBlockFetch v
  where
    namesTBlockFetch (TraceSendMsg msg) = "Send" : namesTBlockFetch' msg
    namesTBlockFetch (TraceRecvMsg msg) = "Recieve" : namesTBlockFetch' msg

    namesTBlockFetch' (AnyMessageAndAgency _agency msg) = namesTBlockFetch'' msg

    namesTBlockFetch'' :: Message (BlockFetch x (Point blk)) from to
                               -> [Text]
    namesTBlockFetch'' MsgRequestRange {} = ["RequestRange"]
    namesTBlockFetch'' MsgStartBatch {}   = ["StartBatch"]
    namesTBlockFetch'' MsgNoBlocks {}     = ["NoBlocks"]
    namesTBlockFetch'' MsgBlock {}        = ["Block"]
    namesTBlockFetch'' MsgBatchDone {}    = ["BatchDone"]
    namesTBlockFetch'' MsgClientDone {}   = ["ClientDone"]

severityTxSubmissionNode :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission (GenTxId blk) (GenTx blk))) -> SeverityS
severityTxSubmissionNode (BlockFetch.TraceLabelPeer _ v) = severityTxSubNode v
  where
    severityTxSubNode (TraceSendMsg msg) = severityTxSubNode' msg
    severityTxSubNode (TraceRecvMsg msg) = severityTxSubNode' msg

    severityTxSubNode' (AnyMessageAndAgency _agency msg) = severityTxSubNode'' msg

    severityTxSubNode'' ::
        Message
          (TXS.TxSubmission (GenTxId blk) (GenTx blk))
          from
          to
     -> SeverityS
    severityTxSubNode'' TXS.MsgRequestTxIds {} = Info
    severityTxSubNode'' TXS.MsgReplyTxIds {}   = Info
    severityTxSubNode'' TXS.MsgRequestTxs {}   = Info
    severityTxSubNode'' TXS.MsgReplyTxs {}     = Info
    severityTxSubNode'' TXS.MsgDone {}         = Info
    severityTxSubNode'' _                      = Info
    -- TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.


namesForTxSubmissionNode :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission (GenTxId blk) (GenTx blk))) -> [Text]
namesForTxSubmissionNode (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : "TxSubmission" : namesTxSubNode v
  where
    namesTxSubNode (TraceSendMsg msg) = "Send" : namesTxSubNode' msg
    namesTxSubNode (TraceRecvMsg msg) = "Recieve" : namesTxSubNode' msg

    namesTxSubNode' (AnyMessageAndAgency _agency msg) = namesTxSubNode'' msg

    namesTxSubNode'' ::
         Message
          (TXS.TxSubmission (GenTxId blk) (GenTx blk))
          from
          to
      -> [Text]
    namesTxSubNode'' TXS.MsgRequestTxIds {} = ["RequestTxIds"]
    namesTxSubNode'' TXS.MsgReplyTxIds {}   = ["ReplyTxIds"]
    namesTxSubNode'' TXS.MsgRequestTxs {}   = ["RequestTxs"]
    namesTxSubNode'' TXS.MsgReplyTxs {}     = ["ReplyTxs"]
    namesTxSubNode'' TXS.MsgDone {}         = ["Done"]
    namesTxSubNode'' _                      = ["KThxBye"]
    -- TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.

severityTxSubmission2Node :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> SeverityS
severityTxSubmission2Node (BlockFetch.TraceLabelPeer _ v) = severityTxSubNode v
  where
    severityTxSubNode (TraceSendMsg msg) = severityTxSubNode' msg
    severityTxSubNode (TraceRecvMsg msg) = severityTxSubNode' msg

    severityTxSubNode' (AnyMessageAndAgency _agency msg) = severityTxSubNode'' msg

    severityTxSubNode'' ::
        Message
          (Hello (TXS.TxSubmission (GenTxId blk) (GenTx blk)) stIdle)
          from
          to
     -> SeverityS
    severityTxSubNode'' MsgHello {}                      = Debug
    severityTxSubNode'' (MsgTalk TXS.MsgRequestTxIds {}) = Info
    severityTxSubNode'' (MsgTalk TXS.MsgReplyTxIds {})   = Info
    severityTxSubNode'' (MsgTalk TXS.MsgRequestTxs {})   = Info
    severityTxSubNode'' (MsgTalk TXS.MsgReplyTxs {})     = Info
    severityTxSubNode'' (MsgTalk TXS.MsgDone {})         = Info
    severityTxSubNode'' (MsgTalk _)                      = Info
    -- TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.

namesForTxSubmission2Node :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> [Text]
namesForTxSubmission2Node (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : "TxSubmission2" : namesTxSubNode v
  where
    namesTxSubNode (TraceSendMsg msg) = "Send" : namesTxSubNode' msg
    namesTxSubNode (TraceRecvMsg msg) = "Recieve" : namesTxSubNode' msg

    namesTxSubNode' (AnyMessageAndAgency _agency msg) = namesTxSubNode'' msg

    namesTxSubNode'' ::
         Message
          (Hello (TXS.TxSubmission (GenTxId blk) (GenTx blk)) stIdle)
          from
          to
      -> [Text]
    namesTxSubNode'' MsgHello {}                      = ["MsgHello"]
    namesTxSubNode'' (MsgTalk TXS.MsgRequestTxIds {}) = ["RequestTxIds"]
    namesTxSubNode'' (MsgTalk TXS.MsgReplyTxIds {})   = ["ReplyTxIds"]
    namesTxSubNode'' (MsgTalk TXS.MsgRequestTxs {})   = ["RequestTxs"]
    namesTxSubNode'' (MsgTalk TXS.MsgReplyTxs {})     = ["ReplyTxs"]
    namesTxSubNode'' (MsgTalk TXS.MsgDone {})         = ["Done"]
    namesTxSubNode'' (MsgTalk _)                      = ["KThxBye"]
    -- TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.

severityIPSubscription ::
     WithIPList (SubscriptionTrace Socket.SockAddr)
  -> SeverityS
severityIPSubscription WithIPList {..} = case wilEvent of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess         -> Info
      ConnectSuccessLast     -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info

namesForSubscription ::
     SubscriptionTrace Socket.SockAddr
  -> [Text]
namesForSubscription SubscriptionTraceConnectStart {} = ["ConnectStart"]
namesForSubscription SubscriptionTraceConnectEnd {} = ["ConnectEnd"]
namesForSubscription SubscriptionTraceConnectException {} = ["ConnectException"]
namesForSubscription SubscriptionTraceSocketAllocationException {} = ["SocketAllocationException"]
namesForSubscription SubscriptionTraceTryConnectToPeer {}  = ["TryConnectToPeer"]
namesForSubscription SubscriptionTraceSkippingPeer {} = ["SkippingPeer"]
namesForSubscription SubscriptionTraceSubscriptionRunning = ["SubscriptionRunning"]
namesForSubscription SubscriptionTraceSubscriptionWaiting {} = ["SubscriptionWaiting"]
namesForSubscription SubscriptionTraceSubscriptionFailed = ["SubscriptionFailed"]
namesForSubscription SubscriptionTraceSubscriptionWaitingNewConnection {} = ["SubscriptionWaitingNewConnection"]
namesForSubscription SubscriptionTraceStart {} = ["Start"]
namesForSubscription SubscriptionTraceRestart {} = ["Restart"]
namesForSubscription SubscriptionTraceConnectionExist {} = ["ConnectionExist"]
namesForSubscription SubscriptionTraceUnsupportedRemoteAddr {} = ["UnsupportedRemoteAddr"]
namesForSubscription SubscriptionTraceMissingLocalAddress = ["MissingLocalAddress"]
namesForSubscription SubscriptionTraceApplicationException {} = ["ApplicationException"]
namesForSubscription SubscriptionTraceAllocateSocket {} = ["AllocateSocket"]
namesForSubscription SubscriptionTraceCloseSocket {} = ["CloseSocket"]

namesForIPSubscription ::
     WithIPList (SubscriptionTrace Socket.SockAddr)
  -> [Text]
namesForIPSubscription(WithIPList _ _ e) = "IP" : namesForSubscription e

namesForDNSSubscription ::
     WithDomainName (SubscriptionTrace Socket.SockAddr)
  -> [Text]
namesForDNSSubscription(WithDomainName _ e) = "DNS" : namesForSubscription e

severityDNSSubscription ::
     WithDomainName (SubscriptionTrace Socket.SockAddr)
  -> SeverityS
severityDNSSubscription WithDomainName {..} = case wdnEvent of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug
