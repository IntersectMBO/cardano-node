{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Network.Formatting
  (
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import           Data.Text (pack)
import qualified Network.Socket as Socket
import           Text.Show

import           Cardano.TraceDispatcher.Common.Formatting ()
import           Cardano.TraceDispatcher.Render

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Cardano.TraceDispatcher.Common.ConvertTxId

import           Ouroboros.Consensus.Block (ConvertRawHash, GetHeader,
                     getHeader)
import           Ouroboros.Consensus.Byron.Ledger (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, HasTxId,
                     HasTxs, extractTxs, txId)
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints,
                     estimateBlockSize)

import           Ouroboros.Network.Block (HasHeader, Point, Serialised,
                     blockHash)
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import           Ouroboros.Network.Codec (PeerHasAgency (..))
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.Trans.Hello.Type
                     (ClientHasAgency (..), Message (..), ServerHasAgency (..))
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as STX
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TXS
import           Ouroboros.Network.Subscription.Dns (WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (SubscriptionTrace,
                     WithIPList (..))

instance LogFormatting (AnyMessageAndAgency ps)
      => LogFormatting (TraceSendRecv ps) where
  forMachine dtal (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (TraceRecvMsg m) = mkObject
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (TraceSendMsg m) = "Send: " <> forHuman m
  forHuman (TraceRecvMsg m) = "Receive: " <> forHuman m

  asMetrics (TraceSendMsg m) = asMetrics m
  asMetrics (TraceRecvMsg m) = asMetrics m

instance LogFormatting (AnyMessageAndAgency (ChainSync blk pt tip)) where
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mkObject [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mkObject [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mkObject [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mkObject [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mkObject [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mkObject [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mkObject [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mkObject [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]

instance LogFormatting (AnyMessageAndAgency (LTS.LocalTxSubmission tx err)) where
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgSubmitTx{}) =
    mkObject [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgAcceptTx{}) =
    mkObject [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgRejectTx{}) =
    mkObject [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]


instance (forall result. Show (Query blk result))
      => LogFormatting (AnyMessageAndAgency (LSQ.LocalStateQuery blk pt (Query blk))) where
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquire{}) =
    mkObject [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquired{}) =
    mkObject [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgFailure{}) =
    mkObject [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgQuery{}) =
    mkObject [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgResult{}) =
    mkObject [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgRelease{}) =
    mkObject [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgReAcquire{}) =
    mkObject [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

--
-- | instances of @forMachine@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( ConvertTxId' blk
         , ConvertRawHash blk
         , HasHeader blk
         , GetHeader blk
         , HasTxId (GenTx blk)
         , SerialiseNodeToNodeConstraints blk
         , HasTxs blk
         )
      => LogFormatting (AnyMessageAndAgency (BlockFetch blk (Point blk))) where
  forMachine DBrief (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  forMachine dtal (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForDetails dtal . txId

  forMachine _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ( ConvertTxId' blk
         , HasTxId (GenTx blk)
         , ConvertRawHash blk
         , HasTxs blk
         )
      => LogFormatting (AnyMessageAndAgency (BlockFetch (Serialised blk) (Point blk))) where
  forMachine DBrief (AnyMessageAndAgency stok (MsgBlock _blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
            -- , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
            -- , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  forMachine _dtal (AnyMessageAndAgency stok (MsgBlock _blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
          -- TODO
          -- , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
          --  , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
          --   , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      -- where
      --   presentTx :: GenTx blk -> Value
      --   presentTx =  String . renderTxIdForDetails dtal . txId

  forMachine _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance (Show txid, Show tx)
      => LogFormatting (AnyMessageAndAgency (STX.TxSubmission txid tx)) where
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxs txids)) =
    mkObject
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxs txs)) =
    mkObject
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxIds _ _ _)) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxIds _)) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgDone) =
    mkObject
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  forMachine _dtal (AnyMessageAndAgency stok _) =
    mkObject
      [ "kind" .= String "MsgKThxBye"
      , "agency" .= String (pack $ show stok)
      ]

instance (Show txid, Show tx)
      => LogFormatting (AnyMessageAndAgency (TXS.TxSubmission2 txid tx)) where
  forMachine _dtal (AnyMessageAndAgency
                   -- we need this pattern match for GHC to recognise this
                   -- function as total.
                   stok@(ClientAgency TokHello)
                   MsgHello) =
    mkObject
      [ "kind" .= String "MsgHello"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine dtal (AnyMessageAndAgency
                  (ClientAgency (TokClientTalk stok))
                  (MsgTalk msg)) =
    forMachine dtal (AnyMessageAndAgency (ClientAgency stok) msg)
  forMachine dtal (AnyMessageAndAgency
                  (ServerAgency (TokServerTalk stok))
                  (MsgTalk msg)) =
    forMachine dtal (AnyMessageAndAgency (ServerAgency stok) msg)

instance LogFormatting (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  forMachine _dtal (WithIPList localAddresses dests ev) =
    mkObject [ "kind" .= String "IP SubscriptionTrace"
             , "localAddresses" .= String (pack $ show localAddresses)
             , "dests" .= String (pack $ show dests)
             , "event" .= String (pack $ show ev)]
  forHuman (WithIPList localAddresses dests ev) =
                     pack (show ev)
                  <> ". Local addresses are "
                  <> (pack $ show localAddresses)
                  <> ". Destinations are "
                  <> (pack $ show dests)
                  <> "."

instance LogFormatting (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  forMachine _dtal (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DNS SubscriptionTrace"
             , "domain" .= String (pack $ show dom)
             , "event" .= String (pack $ show ev)]
  forHuman (WithDomainName dom ev) =
                     pack (show ev)
                  <> ". Domain is "
                  <> pack (show dom)
                  <> "."
