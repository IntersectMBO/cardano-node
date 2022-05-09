{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.NodeToNode
  (
    severityTChainSyncNode
  , namesForTChainSyncNode

  , severityTChainSyncSerialised
  , namesForTChainSyncSerialised

  , severityTBlockFetch
  , namesForTBlockFetch
  , docTBlockFetch

  , severityTBlockFetchSerialised
  , namesForTBlockFetchSerialised

  , severityTxSubmissionNode
  , namesForTxSubmissionNode
  , docTTxSubmissionNode

  , severityTxSubmission2Node
  , namesForTxSubmission2Node
  , docTTxSubmission2Node
  -- * Utils
  , formatMessageWithAgency
  ) where

import           Prelude (String)
import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Data.Aeson (Value (String), toJSON, (.=), Object)
import           Data.Singletons
import           Data.Text (pack)
import           Network.TypedProtocol.Codec (AnyMessage (..))
import           Text.Show

import           Cardano.Node.Queries (ConvertTxId)
import           Cardano.Node.Tracing.Render (renderHeaderHash, renderTxIdForDetails)

import           Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, HasHeader, Header,
                   StandardHash, getHeader)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId, HasTxId, HasTxs,
                   LedgerSupportsMempool, extractTxs, txId)
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints, estimateBlockSize)
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)

import           Ouroboros.Network.Block (Point, Serialised, Tip, blockHash)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..), Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as STX
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TXS

--------------------------------------------------------------------------------
-- ChainSync Tracer
--------------------------------------------------------------------------------

severityTChainSyncNode :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Header blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSyncNode (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessage msg) = severityTChainSync''' msg

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
namesForTChainSyncNode (BlockFetch.TraceLabelPeer _ v) = namesTChainSync v
  where

    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Receive" : namesTChainSync' msg

    namesTChainSync' (AnyMessage msg) = namesTChainSync'' msg

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

--------------------------------------------------------------------------------
-- ChainSyncSerialised Tracer
--------------------------------------------------------------------------------

severityTChainSyncSerialised :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSyncSerialised (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessage msg) = severityTChainSync''' msg

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
namesForTChainSyncSerialised (BlockFetch.TraceLabelPeer _ v) = namesTChainSync v
  where
    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Receive" : namesTChainSync' msg

    namesTChainSync' (AnyMessage msg) = namesTChainSync'' msg

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

--------------------------------------------------------------------------------
-- BlockFetch Tracer
--------------------------------------------------------------------------------

severityTBlockFetch :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch blk (Point blk))) -> SeverityS
severityTBlockFetch (BlockFetch.TraceLabelPeer _ v) = severityTBlockFetch' v
  where
    severityTBlockFetch' (TraceSendMsg msg) = severityTBlockFetch'' msg
    severityTBlockFetch' (TraceRecvMsg msg) = severityTBlockFetch'' msg

    severityTBlockFetch'' (AnyMessage msg) = severityTBlockFetch''' msg

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
namesForTBlockFetch (BlockFetch.TraceLabelPeer _ v) = namesTBlockFetch v
  where
    namesTBlockFetch (TraceSendMsg msg) = "Send" : namesTBlockFetch' msg
    namesTBlockFetch (TraceRecvMsg msg) = "Receive" : namesTBlockFetch' msg

    namesTBlockFetch' (AnyMessage msg) = namesTBlockFetch'' msg

    namesTBlockFetch'' :: Message (BlockFetch x (Point blk)) from to
                               -> [Text]
    namesTBlockFetch'' MsgRequestRange {} = ["RequestRange"]
    namesTBlockFetch'' MsgStartBatch {}   = ["StartBatch"]
    namesTBlockFetch'' MsgNoBlocks {}     = ["NoBlocks"]
    namesTBlockFetch'' MsgBlock {}        = ["Block"]
    namesTBlockFetch'' MsgBatchDone {}    = ["BatchDone"]
    namesTBlockFetch'' MsgClientDone {}   = ["ClientDone"]


formatMessageWithAgency :: forall ps (st :: ps) (st' :: ps).
                  SingI st
               => Show (Sing st)
               => Show (Message ps st st')
               => DetailLevel
               -> Message ps st st'
               -> String
               -> Object
formatMessageWithAgency dtal msg _condensed | dtal >= DMaximum =
  mconcat [ "kind" .= String (pack $ show msg)
          , "agency" .= String (pack $ show (sing :: Sing st))
          ]
formatMessageWithAgency _ _msg condensed =
  mconcat [ "kind" .= String (pack condensed)
          , "agency" .= String (pack $ show (sing :: Sing st))
          ]


instance ( ConvertTxId blk
         , ConvertRawHash blk
         , HasHeader blk
         , GetHeader blk
         , HasTxId (GenTx blk)
         , SerialiseNodeToNodeConstraints blk
         , HasTxs blk
         , LedgerSupportsMempool blk
         , Show blk)
      => LogFormatting (AnyMessage (BlockFetch blk (Point blk))) where
  forMachine dtal@DMinimal (AnyMessage msg@(MsgBlock blk)) =
       formatMessageWithAgency dtal msg "MsgBlock"
    <> mconcat [ "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
               , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
               ]

  forMachine dtal (AnyMessage msg@(MsgBlock blk)) =
       formatMessageWithAgency dtal msg "MsgBlock"
    <> mconcat [ "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
               , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
               , "txIds" .= toJSON (presentTx <$> extractTxs blk)
               ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForDetails dtal . txId

  forMachine dtal (AnyMessage msg@MsgRequestRange{}) =
    formatMessageWithAgency dtal msg "MsgRequestRange"
  forMachine dtal (AnyMessage msg@MsgStartBatch{}) =
    formatMessageWithAgency dtal msg "MsgStartBatch"
  forMachine dtal (AnyMessage msg@MsgNoBlocks{}) =
    formatMessageWithAgency dtal msg "MsgNoBlocks"
  forMachine dtal (AnyMessage msg@MsgBatchDone{}) =
    formatMessageWithAgency dtal msg "MsgBatchDone"
  forMachine dtal (AnyMessage msg@MsgClientDone{}) =
    formatMessageWithAgency dtal msg "MsgClientDone"

docTBlockFetch :: Documented
     (BlockFetch.TraceLabelPeer peer
      (TraceSendRecv
        (BlockFetch x (Point blk))))
docTBlockFetch =
  addDocumentedNamespace  ["Send"] docTBlockFetch'
  `addDocs` addDocumentedNamespace  ["Receive"] docTBlockFetch'

docTBlockFetch' :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (BlockFetch x (Point blk))))
docTBlockFetch' = Documented [
      DocMsg
        ["RequestRange"]
        []
        "Request range of blocks."
    , DocMsg
        ["StartBatch"]
        []
        "Start block streaming."
    , DocMsg
        ["NoBlocks"]
        []
        "Respond that there are no blocks."
    , DocMsg
        ["Block"]
        []
        "Stream a single block."
    , DocMsg
        ["BatchDone"]
        []
        "End of block streaming."
    , DocMsg
        ["ClientDone"]
        []
        "Client termination message."
  ]

--------------------------------------------------------------------------------
-- BlockFetchSerialised Tracer
--------------------------------------------------------------------------------

severityTBlockFetchSerialised :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))) -> SeverityS
severityTBlockFetchSerialised (BlockFetch.TraceLabelPeer _ v) = severityTBlockFetch' v
  where
    severityTBlockFetch' (TraceSendMsg msg) = severityTBlockFetch'' msg
    severityTBlockFetch' (TraceRecvMsg msg) = severityTBlockFetch'' msg

    severityTBlockFetch'' (AnyMessage msg) = severityTBlockFetch''' msg

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
namesForTBlockFetchSerialised (BlockFetch.TraceLabelPeer _ v) = namesTBlockFetch v
  where
    namesTBlockFetch (TraceSendMsg msg) = "Send" : namesTBlockFetch' msg
    namesTBlockFetch (TraceRecvMsg msg) = "Receive" : namesTBlockFetch' msg

    namesTBlockFetch' (AnyMessage msg) = namesTBlockFetch'' msg

    namesTBlockFetch'' :: Message (BlockFetch x (Point blk)) from to
                               -> [Text]
    namesTBlockFetch'' MsgRequestRange {} = ["RequestRange"]
    namesTBlockFetch'' MsgStartBatch {}   = ["StartBatch"]
    namesTBlockFetch'' MsgNoBlocks {}     = ["NoBlocks"]
    namesTBlockFetch'' MsgBlock {}        = ["Block"]
    namesTBlockFetch'' MsgBatchDone {}    = ["BatchDone"]
    namesTBlockFetch'' MsgClientDone {}   = ["ClientDone"]

-- TODO Tracers
-- Provide complete implementation of forMachine
instance ( ConvertTxId blk
         , HasTxId (GenTx blk)
         , ConvertRawHash blk
         , StandardHash blk
         , HasTxs blk
         )
      => LogFormatting (AnyMessage (BlockFetch (Serialised blk) (Point blk))) where
  forMachine dtal@DMinimal (AnyMessage msg@(MsgBlock _blk)) =
    formatMessageWithAgency dtal msg "MsgBlock"

  forMachine dtal (AnyMessage msg@(MsgBlock _blk)) =
    formatMessageWithAgency dtal msg "MsgBlock"

  forMachine dtal (AnyMessage msg@MsgRequestRange{}) =
    formatMessageWithAgency dtal msg "MsgRequestRange"
  forMachine dtal (AnyMessage msg@MsgStartBatch{}) =
    formatMessageWithAgency dtal msg "MsgStartBatch"
  forMachine dtal (AnyMessage msg@MsgNoBlocks{}) =
    formatMessageWithAgency dtal msg "MsgNoBlocks"
  forMachine dtal (AnyMessage msg@MsgBatchDone{}) =
    formatMessageWithAgency dtal msg "MsgBatchDone"
  forMachine dtal (AnyMessage msg@MsgClientDone{}) =
    formatMessageWithAgency dtal msg "MsgClientDone"

  forHuman = pack . show


--------------------------------------------------------------------------------
-- TxSubmissionNode Tracer
--------------------------------------------------------------------------------

severityTxSubmissionNode :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> SeverityS
severityTxSubmissionNode (BlockFetch.TraceLabelPeer _ v) = severityTxSubNode v
  where
    severityTxSubNode (TraceSendMsg msg) = severityTxSubNode' msg
    severityTxSubNode (TraceRecvMsg msg) = severityTxSubNode' msg

    severityTxSubNode' (AnyMessage msg) = severityTxSubNode'' msg

    severityTxSubNode'' ::
        Message
          (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))
          from
          to
     -> SeverityS
    severityTxSubNode'' TXS.MsgInit {} = Info
    severityTxSubNode'' TXS.MsgRequestTxIds {} = Info
    severityTxSubNode'' TXS.MsgReplyTxIds {}   = Info
    severityTxSubNode'' TXS.MsgRequestTxs {}   = Info
    severityTxSubNode'' TXS.MsgReplyTxs {}     = Info
    severityTxSubNode'' TXS.MsgDone {}         = Info


namesForTxSubmissionNode :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> [Text]
namesForTxSubmissionNode (BlockFetch.TraceLabelPeer _ v) =
  namesTxSubNode v
  where
    namesTxSubNode (TraceSendMsg msg) = "Send" : namesTxSubNode' msg
    namesTxSubNode (TraceRecvMsg msg) = "Receive" : namesTxSubNode' msg

    namesTxSubNode' (AnyMessage msg) = namesTxSubNode'' msg

    namesTxSubNode'' ::
         Message
          (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))
          from
          to
      -> [Text]
    namesTxSubNode'' TXS.MsgInit {} = ["MsgInit"]
    namesTxSubNode'' TXS.MsgRequestTxIds {} = ["RequestTxIds"]
    namesTxSubNode'' TXS.MsgReplyTxIds {}   = ["ReplyTxIds"]
    namesTxSubNode'' TXS.MsgRequestTxs {}   = ["RequestTxs"]
    namesTxSubNode'' TXS.MsgReplyTxs {}     = ["ReplyTxs"]
    namesTxSubNode'' TXS.MsgDone {}         = ["Done"]


instance (Show txid, Show tx)
      => LogFormatting (AnyMessage (STX.TxSubmission2 txid tx)) where
  forMachine dtal (AnyMessage msg@STX.MsgInit) =
    formatMessageWithAgency dtal msg "MsgInit"
  forMachine dtal (AnyMessage msg@STX.MsgRequestTxs {}) =
    formatMessageWithAgency dtal msg "MsgRequestTxs"
  forMachine dtal (AnyMessage msg@STX.MsgReplyTxs {}) =
    formatMessageWithAgency dtal msg "MsgReplyTxs"
  forMachine dtal (AnyMessage msg@STX.MsgRequestTxIds {}) =
    formatMessageWithAgency dtal msg "MsgRequestTxIds"
  forMachine dtal (AnyMessage msg@STX.MsgReplyTxIds {}) =
    formatMessageWithAgency dtal msg "MsgReplyTxIds"
  forMachine dtal (AnyMessage msg@STX.MsgDone) =
    formatMessageWithAgency dtal msg "MsgDone"

docTTxSubmissionNode :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))))
docTTxSubmissionNode =
  addDocumentedNamespace  ["Send"] docTTxSubmissionNode'
  `addDocs` addDocumentedNamespace  ["Receive"] docTTxSubmissionNode'

docTTxSubmissionNode' :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))))
docTTxSubmissionNode' = Documented [
      DocMsg
        ["RequestTxIds"]
        []
        "Request a non-empty list of transaction identifiers from the client, \
        \and confirm a number of outstanding transaction identifiers. \
        \\n \
        \With 'TokBlocking' this is a a blocking operation: the response will \
        \always have at least one transaction identifier, and it does not expect \
        \a prompt response: there is no timeout. This covers the case when there \
        \is nothing else to do but wait. For example this covers leaf nodes that \
        \rarely, if ever, create and submit a transaction. \
        \\n \
        \With 'TokNonBlocking' this is a non-blocking operation: the response \
        \may be an empty list and this does expect a prompt response. This \
        \covers high throughput use cases where we wish to pipeline, by \
        \interleaving requests for additional transaction identifiers with \
        \requests for transactions, which requires these requests not block. \
        \\n \
        \The request gives the maximum number of transaction identifiers that \
        \can be accepted in the response. This must be greater than zero in the \
        \'TokBlocking' case. In the 'TokNonBlocking' case either the numbers \
        \acknowledged or the number requested must be non-zero. In either case, \
        \the number requested must not put the total outstanding over the fixed \
        \protocol limit. \
        \\n\
        \The request also gives the number of outstanding transaction \
        \identifiers that can now be acknowledged. The actual transactions \
        \to acknowledge are known to the peer based on the FIFO order in which \
        \they were provided. \
        \\n \
        \There is no choice about when to use the blocking case versus the \
        \non-blocking case, it depends on whether there are any remaining \
        \unacknowledged transactions (after taking into account the ones \
        \acknowledged in this message): \
        \\n \
        \* The blocking case must be used when there are zero remaining \
        \  unacknowledged transactions. \
        \\n \
        \* The non-blocking case must be used when there are non-zero remaining \
        \  unacknowledged transactions."
    , DocMsg
        ["ReplyTxIds"]
        []
        "Reply with a list of transaction identifiers for available \
        \transactions, along with the size of each transaction. \
        \\n \
        \The list must not be longer than the maximum number requested. \
        \\n \
        \In the 'StTxIds' 'StBlocking' state the list must be non-empty while \
        \in the 'StTxIds' 'StNonBlocking' state the list may be empty. \
        \\n \
        \These transactions are added to the notional FIFO of outstanding \
        \transaction identifiers for the protocol. \
        \\n \
        \The order in which these transaction identifiers are returned must be \
        \the order in which they are submitted to the mempool, to preserve \
        \dependent transactions."
    , DocMsg
        ["RequestTxs"]
        []
        "Request one or more transactions corresponding to the given  \
        \transaction identifiers.  \
        \\n \
        \While it is the responsibility of the replying peer to keep within \
        \pipelining in-flight limits, the sender must also cooperate by keeping \
        \the total requested across all in-flight requests within the limits. \
        \\n\
        \It is an error to ask for transaction identifiers that were not \
        \previously announced (via 'MsgReplyTxIds'). \
        \\n\
        \It is an error to ask for transaction identifiers that are not \
        \outstanding or that were already asked for."
    , DocMsg
        ["ReplyTxs"]
        []
        "Reply with the requested transactions, or implicitly discard.\
        \\n\
        \Transactions can become invalid between the time the transaction \
        \identifier was sent and the transaction being requested. Invalid \
        \(including committed) transactions do not need to be sent.\
        \\n\
        \Any transaction identifiers requested but not provided in this reply \
        \should be considered as if this peer had never announced them. (Note \
        \that this is no guarantee that the transaction is invalid, it may still \
        \be valid and available from another peer)."
    , DocMsg
        ["Done"]
        []
        "Termination message, initiated by the client when the server is \
        \making a blocking call for more transaction identifiers."
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  ]

--------------------------------------------------------------------------------
-- TxSubmissionNode2 Tracer
--------------------------------------------------------------------------------

severityTxSubmission2Node :: forall blk peer. BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> SeverityS
severityTxSubmission2Node (BlockFetch.TraceLabelPeer _ v) = severityTxSubNode v
  where
    severityTxSubNode (TraceSendMsg msg) = severityTxSubNode' msg
    severityTxSubNode (TraceRecvMsg msg) = severityTxSubNode' msg

    severityTxSubNode' (AnyMessage msg) = severityTxSubNode'' msg

    severityTxSubNode'' ::
        Message
          (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))
          from
          to
     -> SeverityS
    severityTxSubNode'' STX.MsgInit {}         = Debug
    severityTxSubNode'' TXS.MsgRequestTxIds {} = Info
    severityTxSubNode'' TXS.MsgReplyTxIds {}   = Info
    severityTxSubNode'' TXS.MsgRequestTxs {}   = Info
    severityTxSubNode'' TXS.MsgReplyTxs {}     = Info
    severityTxSubNode'' TXS.MsgDone {}         = Info

namesForTxSubmission2Node :: forall blk peer. BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> [Text]
namesForTxSubmission2Node (BlockFetch.TraceLabelPeer _ v) =
  namesTxSubNode v
  where
    namesTxSubNode (TraceSendMsg msg) = "Send" : namesTxSubNode' msg
    namesTxSubNode (TraceRecvMsg msg) = "Receive" : namesTxSubNode' msg

    namesTxSubNode' (AnyMessage msg) = namesTxSubNode'' msg

    namesTxSubNode'' ::
         Message
          (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))
          from
          to
      -> [Text]
    namesTxSubNode'' TXS.MsgInit {}         = ["MsgInit"]
    namesTxSubNode'' TXS.MsgRequestTxIds {} = ["RequestTxIds"]
    namesTxSubNode'' TXS.MsgReplyTxIds {}   = ["ReplyTxIds"]
    namesTxSubNode'' TXS.MsgRequestTxs {}   = ["RequestTxs"]
    namesTxSubNode'' TXS.MsgReplyTxs {}     = ["ReplyTxs"]
    namesTxSubNode'' TXS.MsgDone {}         = ["Done"]


docTTxSubmission2Node :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))))
docTTxSubmission2Node =
  addDocumentedNamespace  ["Send"] docTTxSubmission2Node'
  `addDocs` addDocumentedNamespace  ["Receive"] docTTxSubmission2Node'

docTTxSubmission2Node' :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))))
docTTxSubmission2Node' = Documented [
      DocMsg
        ["MsgHello"]
        []
        "Client side hello message."
    , DocMsg
        ["RequestTxIds"]
        []
        "Request a non-empty list of transaction identifiers from the client, \
        \and confirm a number of outstanding transaction identifiers. \
        \\n\
        \With 'TokBlocking' this is a a blocking operation: the response will \
        \always have at least one transaction identifier, and it does not expect \
        \a prompt response: there is no timeout. This covers the case when there \
        \is nothing else to do but wait. For example this covers leaf nodes that \
        \rarely, if ever, create and submit a transaction. \
        \\n\
        \With 'TokNonBlocking' this is a non-blocking operation: the response \
        \may be an empty list and this does expect a prompt response. This \
        \covers high throughput use cases where we wish to pipeline, by \
        \interleaving requests for additional transaction identifiers with \
        \requests for transactions, which requires these requests not block. \
        \\n\
        \The request gives the maximum number of transaction identifiers that \
        \can be accepted in the response. This must be greater than zero in the \
        \'TokBlocking' case. In the 'TokNonBlocking' case either the numbers \
        \acknowledged or the number requested must be non-zero. In either case, \
        \the number requested must not put the total outstanding over the fixed \
        \protocol limit. \
        \\n\
        \The request also gives the number of outstanding transaction \
        \identifiers that can now be acknowledged. The actual transactions \
        \to acknowledge are known to the peer based on the FIFO order in which \
        \they were provided. \
        \\n\
        \There is no choice about when to use the blocking case versus the \
        \non-blocking case, it depends on whether there are any remaining \
        \unacknowledged transactions (after taking into account the ones \
        \acknowledged in this message): \
        \\n\
        \* The blocking case must be used when there are zero remaining \
        \  unacknowledged transactions. \
        \\n\
        \* The non-blocking case must be used when there are non-zero remaining \
        \  unacknowledged transactions."
    , DocMsg
        ["ReplyTxIds"]
        []
        "Reply with a list of transaction identifiers for available \
        \transactions, along with the size of each transaction. \
        \\n \
        \The list must not be longer than the maximum number requested. \
        \\n \
        \In the 'StTxIds' 'StBlocking' state the list must be non-empty while \
        \in the 'StTxIds' 'StNonBlocking' state the list may be empty. \
        \\n \
        \These transactions are added to the notional FIFO of outstanding \
        \transaction identifiers for the protocol. \
        \\n \
        \The order in which these transaction identifiers are returned must be \
        \the order in which they are submitted to the mempool, to preserve \
        \dependent transactions."
    , DocMsg
        ["RequestTxs"]
        []
        "Request one or more transactions corresponding to the given  \
        \transaction identifiers.  \
        \\n \
        \While it is the responsibility of the replying peer to keep within \
        \pipelining in-flight limits, the sender must also cooperate by keeping \
        \the total requested across all in-flight requests within the limits. \
        \\n \
        \It is an error to ask for transaction identifiers that were not \
        \previously announced (via 'MsgReplyTxIds'). \
        \\n \
        \It is an error to ask for transaction identifiers that are not \
        \outstanding or that were already asked for."
    , DocMsg
        ["ReplyTxs"]
        []
        "Reply with the requested transactions, or implicitly discard. \
        \\n \
        \Transactions can become invalid between the time the transaction \
        \identifier was sent and the transaction being requested. Invalid \
        \(including committed) transactions do not need to be sent. \
        \\n \
        \Any transaction identifiers requested but not provided in this reply \
        \should be considered as if this peer had never announced them. (Note \
        \that this is no guarantee that the transaction is invalid, it may still \
        \be valid and available from another peer)."
    , DocMsg
        ["Done"]
        []
        "Termination message, initiated by the client when the server is \
        \making a blocking call for more transaction identifiers."
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  ]
