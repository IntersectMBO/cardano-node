{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-unticked-promoted-constructors #-}

module Cardano.Benchmarking.GeneratorTx.NodeToNode
  ( BenchmarkTxSubmitTracers (..)
  , SendRecvConnect
  , SendRecvTxSubmission
  , benchmarkConnectTxSubmit
  ) where

import           Prelude
import           Cardano.Prelude (Text, Void, forever)

import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Data.Aeson ((.=), ToJSON (..), Value (..), toJSON)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Network.Mux (AppType(InitiatorApp), WithMuxBearer (..))
import           Network.Socket (AddrInfo (..), SockAddr)

import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), mkLOMeta)
import           Cardano.BM.Data.Tracer (DefinePrivacyAnnotation (..),
                     DefineSeverity (..), ToObject (..), TracingFormatting (..),
                     TracingVerbosity (..), Transformable (..),
                     emptyObject, mkObject, trStructured)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Mempool.API (GenTxId, GenTx)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (RunNode, nodeNetworkMagic)
import           Ouroboros.Consensus.NodeNetwork (ProtocolCodecs(..), protocolCodecs)
import           Ouroboros.Consensus.Config (TopLevelConfig)

import           Ouroboros.Network.Codec (AnyMessage (..))
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import           Ouroboros.Network.Mux
                   (OuroborosApplication(..), MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
-- TODO: #1685 (ouroboros-network) IO manager terms and types should be exported
-- from NodeToNode module as well.
import           Ouroboros.Network.NodeToClient (AssociateWithIOCP)
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient(..), blockFetchClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull, chainSyncClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (Versions, simpleSingletonVersions)
import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSubmissionClient, txSubmissionClientPeer)
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TS
import           Ouroboros.Network.Snocket (socketSnocket)

type SendRecvConnect = WithMuxBearer
                         NtN.RemoteConnectionId
                         (TraceSendRecv (Handshake
                                           NtN.NodeToNodeVersion
                                           CBOR.Term))

instance ToObject SendRecvConnect where
  toObject MinimalVerbosity _ = emptyObject -- do not log
  toObject NormalVerbosity (WithMuxBearer _ _) =
    mkObject ["kind" .= String "SendRecvConnect"]
  toObject MaximalVerbosity (WithMuxBearer connId _) =
    mkObject [ "kind"   .= String "SendRecvConnect"
             , "connId" .= String (T.pack . show $ connId)
             ]

instance DefineSeverity SendRecvConnect

instance DefinePrivacyAnnotation SendRecvConnect

instance forall m . (m ~ IO) => Transformable Text m SendRecvConnect where
  -- transform to JSON Object
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer _ _verb _tr = nullTracer

--------------------------------------------------------------------------------------

type SendRecvTxSubmission blk = TraceSendRecv (TS.TxSubmission (GenTxId blk) (GenTx blk))

instance ToJSON (GenTxId ByronBlock) where
  toJSON txId = String (T.pack $ show txId)

instance ToObject (SendRecvTxSubmission ByronBlock) where
  toObject MinimalVerbosity _ = emptyObject -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceSendMsg (AnyMessage msg) ->
        case msg of
          TS.MsgRequestTxIds _ _ _                 -> mkObject ["kind" .= String "TxSubmissionSendRequestTxIds"]
          TS.MsgReplyTxIds (TS.BlockingReply _)    -> mkObject ["kind" .= String "TxSubmissionSendBReplyTxIds"]
          TS.MsgReplyTxIds (TS.NonBlockingReply _) -> mkObject ["kind" .= String "TxSubmissionSendNBReplyTxIds"]
          TS.MsgRequestTxs _                       -> mkObject ["kind" .= String "TxSubmissionSendRequestTxs"]
          TS.MsgReplyTxs _                         -> mkObject ["kind" .= String "TxSubmissionSendReplyTxs"]
          TS.MsgDone                               -> emptyObject -- No useful information.
      TraceRecvMsg (AnyMessage msg) ->
        case msg of
          TS.MsgRequestTxIds _ _ _                 -> mkObject ["kind" .= String "TxSubmissionRecvRequestTxIds"]
          TS.MsgReplyTxIds (TS.BlockingReply _)    -> mkObject ["kind" .= String "TxSubmissionRecvBReplyTxIds"]
          TS.MsgReplyTxIds (TS.NonBlockingReply _) -> mkObject ["kind" .= String "TxSubmissionRecvNBReplyTxIds"]
          TS.MsgRequestTxs _                       -> mkObject ["kind" .= String "TxSubmissionRecvRequestTxs"]
          TS.MsgReplyTxs _                         -> mkObject ["kind" .= String "TxSubmissionRecvReplyTxs"]
          TS.MsgDone                               -> emptyObject -- No useful information.

  toObject MaximalVerbosity t =
    case t of
      TraceSendMsg (AnyMessage msg) ->
        case msg of
          TS.MsgRequestTxIds _ ackNumber reqNumber ->
            mkObject [ "kind"   .= String "TxSubmissionSendRequestTxIds"
                     , "ackNum" .= Number (fromIntegral ackNumber)
                     , "reqNum" .= Number (fromIntegral reqNumber)
                     ]
          TS.MsgReplyTxIds (TS.BlockingReply txIds) ->
            mkObject [ "kind"  .= String "TxSubmissionSendBReplyTxIds"
                     , "txIds" .= toJSON txIds
                     ]
          TS.MsgReplyTxIds (TS.NonBlockingReply txIds) ->
            mkObject [ "kind"  .= String "TxSubmissionSendNBReplyTxIds"
                     , "txIds" .= toJSON txIds
                     ]
          TS.MsgRequestTxs txIds ->
            mkObject [ "kind"  .= String "TxSubmissionSendRequestTxs"
                     , "txIds" .= toJSON txIds
                     ]
          TS.MsgReplyTxs _ -> -- We shouldn't log a list of whole transactions here.
            mkObject [ "kind" .= String "TxSubmissionSendReplyTxs"
                     ]
          TS.MsgDone -> emptyObject -- No useful information.

      TraceRecvMsg (AnyMessage msg) ->
        case msg of
          TS.MsgRequestTxIds _ ackNumber reqNumber ->
            mkObject [ "kind"   .= String "TxSubmissionRecvRequestTxIds"
                     , "ackNum" .= Number (fromIntegral ackNumber)
                     , "reqNum" .= Number (fromIntegral reqNumber)
                     ]
          TS.MsgReplyTxIds (TS.BlockingReply txIds) ->
            mkObject [ "kind"  .= String "TxSubmissionRecvBReplyTxIds"
                     , "txIds" .= toJSON txIds
                     ]
          TS.MsgReplyTxIds (TS.NonBlockingReply txIds) ->
            mkObject [ "kind"  .= String "TxSubmissionRecvNBReplyTxIds"
                     , "txIds" .= toJSON txIds
                     ]
          TS.MsgRequestTxs txIds ->
            mkObject [ "kind"  .= String "TxSubmissionRecvRequestTxs"
                     , "txIds" .= toJSON txIds
                     ]
          TS.MsgReplyTxs _ -> -- We shouldn't log a list of whole transactions here.
            mkObject [ "kind" .= String "TxSubmissionRecvReplyTxs"
                     ]
          TS.MsgDone -> emptyObject -- No useful information.

instance DefineSeverity (SendRecvTxSubmission ByronBlock)

instance DefinePrivacyAnnotation (SendRecvTxSubmission ByronBlock)

instance Transformable Text IO (SendRecvTxSubmission ByronBlock) where
  -- transform to JSON Object
  trTransformer StructuredLogging verb tr = Tracer $ \arg -> do
    currentTime <- getCurrentTime
    let
      obj = toObject verb arg
      updatedObj =
        if obj == emptyObject
          then obj
          else
            -- Add a timestamp in 'ToObject'-representation.
            HM.insert "time" (String (T.pack . show $ currentTime)) obj
      tracer = if obj == emptyObject then nullTracer else tr
    meta <- mkLOMeta (defineSeverity arg) (definePrivacyAnnotation arg)
    traceWith tracer (mempty, LogObject mempty meta (LogStructured updatedObj))
  trTransformer _ _verb _tr = nullTracer

--------------------------------------------------------------------------------------

data BenchmarkTxSubmitTracers m blk = BenchmarkTracers
  { trSendRecvConnect      :: Tracer m SendRecvConnect
  , trSendRecvTxSubmission :: Tracer m (SendRecvTxSubmission blk)
  }

benchmarkConnectTxSubmit
  :: forall m blk . (RunNode blk, m ~ IO)
  => AssociateWithIOCP
  -> BenchmarkTxSubmitTracers m blk
  -- ^ For tracing the send/receive actions
  -> TopLevelConfig blk
  -- ^ The particular block protocol
  -> Maybe AddrInfo
  -- ^ local address information (typically local interface/port to use)
  -> AddrInfo
  -- ^ remote address information
  -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()
  -- ^ the particular txSubmission peer
  -> m ()
benchmarkConnectTxSubmit iocp trs cfg localAddr remoteAddr myTxSubClient = do
  NtN.connectTo
    (socketSnocket iocp)
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = trSendRecvConnect trs
      }
    peerMultiplex
    (addrAddress <$> localAddr)
    (addrAddress remoteAddr)
 where
  myCodecs :: ProtocolCodecs blk DeserialiseFailure m
                ByteString ByteString ByteString ByteString ByteString
                ByteString ByteString ByteString
  myCodecs  = protocolCodecs cfg (mostRecentNetworkProtocolVersion (Proxy @blk))

  peerMultiplex :: Versions NtN.NodeToNodeVersion NtN.DictVersion
                          (NtN.ConnectionId SockAddr ->
                              OuroborosApplication InitiatorApp ByteString m () Void)
  peerMultiplex =
    simpleSingletonVersions
      NtN.NodeToNodeV_1
      (NtN.NodeToNodeVersionData { NtN.networkMagic = nodeNetworkMagic (Proxy @blk) cfg})
      (NtN.DictVersion NtN.nodeToNodeCodecCBORTerm) $ \_ ->
      NtN.nodeToNodeProtocols $
        NtN.NodeToNodeProtocols
          { NtN.chainSyncProtocol = InitiatorProtocolOnly $
                                      MuxPeer
                                        nullTracer
                                        (pcChainSyncCodec myCodecs)
                                        (chainSyncClientPeer chainSyncClientNull)
          , NtN.blockFetchProtocol = InitiatorProtocolOnly $
                                       MuxPeer
                                         nullTracer
                                         (pcBlockFetchCodec myCodecs)
                                         (blockFetchClientPeer blockFetchClientNull)
          , NtN.txSubmissionProtocol = InitiatorProtocolOnly $
                                         MuxPeer
                                           (trSendRecvTxSubmission trs)
                                           (pcTxSubmissionCodec myCodecs)
                                           (txSubmissionClientPeer myTxSubClient)

          }
-- the null block fetch client
blockFetchClientNull
  :: forall block m a.  MonadTimer m
  => BlockFetchClient block m a
blockFetchClientNull
  = BlockFetchClient $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}
