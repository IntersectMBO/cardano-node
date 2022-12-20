-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Tracing.Tracers.NodeToNode
   (
   ) where

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Data.Aeson (Value (String), toJSON, (.=))
import           Data.Text (pack)
import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import           Text.Show

import           Cardano.Node.Queries (ConvertTxId)
import           Cardano.Node.Tracing.Render (renderHeaderHash, renderTxIdForDetails)

import           Ouroboros.Consensus.Block (ConvertRawHash, GetHeader, StandardHash, getHeader)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, HasTxId, HasTxs,
                   LedgerSupportsMempool, extractTxs, txId)
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints, estimateBlockSize)

import           Ouroboros.Network.Block (Point, Serialised, blockHash)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..), Message (..))
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as STX

--------------------------------------------------------------------------------
-- BlockFetch Tracer
--------------------------------------------------------------------------------

instance ( ConvertTxId blk
         , GetHeader blk
         , HasTxId (GenTx blk)
         , SerialiseNodeToNodeConstraints blk
         , HasTxs blk
         , LedgerSupportsMempool blk
         )
      => LogFormatting (AnyMessageAndAgency (BlockFetch blk (Point blk))) where
  forMachine DMinimal (AnyMessageAndAgency stok (MsgBlock blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  forMachine dtal (AnyMessageAndAgency stok (MsgBlock blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForDetails dtal . txId

  forMachine _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mconcat [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mconcat [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mconcat [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mconcat [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mconcat [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance MetaTrace (AnyMessageAndAgency (BlockFetch blk1 (Point blk2))) where
    namespaceFor (AnyMessageAndAgency _stok MsgRequestRange{}) =
      Namespace [] ["RequestRange"]
    namespaceFor (AnyMessageAndAgency _stok MsgStartBatch{}) =
      Namespace [] ["StartBatch"]
    namespaceFor (AnyMessageAndAgency _stok MsgNoBlocks{}) =
      Namespace [] ["NoBlocks"]
    namespaceFor (AnyMessageAndAgency _stok MsgBlock{}) =
      Namespace [] ["Block"]
    namespaceFor (AnyMessageAndAgency _stok MsgBatchDone{}) =
      Namespace [] ["BatchDone"]
    namespaceFor (AnyMessageAndAgency _stok MsgClientDone{}) =
      Namespace [] ["ClientDone"]

    severityFor (Namespace _ ["RequestRange"]) _ = Just Info
    severityFor (Namespace _ ["StartBatch"]) _ = Just Info
    severityFor (Namespace _ ["NoBlocks"]) _ = Just Info
    severityFor (Namespace _ ["Block"]) _ = Just Info
    severityFor (Namespace _ ["BatchDone"]) _ = Just Info
    severityFor (Namespace _ ["ClientDone"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RequestRange"]) = Just
        "Request range of blocks."
    documentFor (Namespace _ ["StartBatch"]) = Just
        "Start block streaming."
    documentFor (Namespace _ ["NoBlocks"]) = Just
        "Respond that there are no blocks."
    documentFor (Namespace _ ["Block"]) = Just
        "Stream a single block."
    documentFor (Namespace _ ["BatchDone"]) = Just
        "End of block streaming."
    documentFor (Namespace _ ["ClientDone"]) = Just
        "Client termination message."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["RequestRange"]
      , Namespace [] ["StartBatch"]
      , Namespace [] ["NoBlocks"]
      , Namespace [] ["Block"]
      , Namespace [] ["BatchDone"]
      , Namespace [] ["ClientDone"]
      ]


--------------------------------------------------------------------------------
-- BlockFetchSerialised Tracer
--------------------------------------------------------------------------------

-- TODO Tracers
-- Provide complete implementation of forMachine
instance ( ConvertTxId blk
         , HasTxId (GenTx blk)
         , ConvertRawHash blk
         , StandardHash blk
         , HasTxs blk
         )
      => LogFormatting (AnyMessageAndAgency (BlockFetch (Serialised blk) (Point blk))) where
  forMachine DMinimal (AnyMessageAndAgency stok (MsgBlock _blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
            -- , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
            -- , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  forMachine _dtal (AnyMessageAndAgency stok (MsgBlock _blk)) =
    mconcat [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
          -- , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
          --  , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
          --   , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      -- where
      --   presentTx :: GenTx blk -> Value
      --   presentTx =  String . renderTxIdForDetails dtal . txId

  forMachine _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mconcat [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mconcat [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mconcat [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mconcat [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mconcat [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

  forHuman = pack . show

--------------------------------------------------------------------------------
-- TxSubmissionNode2 Tracer
--------------------------------------------------------------------------------

instance (Show txid, Show tx)
      => LogFormatting (AnyMessageAndAgency (STX.TxSubmission2 txid tx)) where
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgInit) =
    mconcat
      [ "kind" .= String "MsgInit"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxs txids)) =
    mconcat
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxs txs)) =
    mconcat
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgRequestTxIds {}) =
    mconcat
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxIds _)) =
    mconcat
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgDone) =
    mconcat
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance MetaTrace (AnyMessageAndAgency (STX.TxSubmission2 txid tx)) where
    namespaceFor (AnyMessageAndAgency _stok STX.MsgInit {}) =
      Namespace [] ["MsgInit"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxs {}) =
      Namespace [] ["RequestTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxs {}) =
      Namespace [] ["ReplyTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxIds {}) =
      Namespace [] ["RequestTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxIds {}) =
      Namespace [] ["ReplyTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgDone {}) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["MsgInit"]) _ = Just Info
    severityFor (Namespace _ ["RequestTxIds"]) _ = Just Info
    severityFor (Namespace _ ["ReplyTxIds"]) _ = Just Info
    severityFor (Namespace _ ["RequestTxs"]) _ = Just Info
    severityFor (Namespace _ ["ReplyTxs"]) _ = Just Info
    severityFor (Namespace _ ["Done"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["MsgInit"]) = Just
        "Client side hello message."
    documentFor (Namespace _ ["RequestTxIds"]) = Just
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
    documentFor (Namespace _ ["ReplyTxIds"]) = Just
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
    documentFor (Namespace _ ["RequestTxs"]) = Just
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
    documentFor (Namespace _ ["ReplyTxs"]) = Just
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
    documentFor (Namespace _ ["Done"]) = Just
        "Termination message, initiated by the client when the server is \
         \making a blocking call for more transaction identifiers."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["MsgInit"]
      , Namespace [] ["RequestTxIds"]
      , Namespace [] ["ReplyTxIds"]
      , Namespace [] ["RequestTxs"]
      , Namespace [] ["ReplyTxs"]
      , Namespace [] ["Done"]
      ]
