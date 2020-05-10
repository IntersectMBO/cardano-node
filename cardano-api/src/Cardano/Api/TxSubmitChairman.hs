{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.TxSubmitChairman
  ( submitTx
  ) where

import           Cardano.Prelude

import           Control.Tracer

import           Cardano.Api.TxSubmit (prepareTxByron, prepareTxShelley)
import           Cardano.Api.Types

import           Ouroboros.Consensus.Cardano (protocolClientInfo)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo(..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Mempool (GenTx)

import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient hiding (NodeToClientVersion (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as LocalTxSub

import           Cardano.Chain.Slotting (EpochSlots (..))

import           Cardano.Config.Byron.Protocol (mkNodeClientProtocolRealPBFT)
import           Cardano.Config.Shelley.Protocol (mkNodeClientProtocolTPraos)
import           Cardano.Config.Types (SocketPath(..))


submitTx
  :: Network
  -> SocketPath
  -> TxSigned
  -> IO ()
submitTx network socketPath tx =
    NtC.withIOManager $ \iocp ->
      case tx of
        TxSignedByron _ _ _ _ ->
          submitGenTx
            nullTracer
            iocp
            (protocolClientInfo $ mkNodeClientProtocolRealPBFT $ EpochSlots 21600)
            network
            socketPath
            (prepareTxByron tx)

        TxSignedShelley _ -> do
          submitGenTx
            nullTracer
            iocp
            (protocolClientInfo mkNodeClientProtocolTPraos)
            network
            socketPath
            (prepareTxShelley tx)

submitGenTx
  :: forall blk.
     RunNode blk
  => Tracer IO Text
  -> IOManager
  -> ProtocolClientInfo blk
  -> Network
  -> SocketPath
  -> GenTx blk
  -> IO ()
submitGenTx tracer iomgr cfg nm (SocketPath path) genTx =
      connectTo
        (localSnocket iomgr path)
        NetworkConnectTracers {
            nctMuxTracer       = nullTracer,
            nctHandshakeTracer = nullTracer
            }
        (localInitiatorNetworkApplication tracer cfg nm genTx)
        path
        --`catch` handleMuxError tracer chainsVar socketPath

localInitiatorNetworkApplication
  :: forall blk.
     RunNode blk
  => Tracer IO Text
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> ProtocolClientInfo blk
  -> Network
  -> GenTx blk
  -> Versions NtC.NodeToClientVersion DictVersion
              (LocalConnectionId
               -> OuroborosApplication InitiatorApp LByteString IO () Void)
localInitiatorNetworkApplication tracer cfg nm genTx =
    foldMapVersions
      (\v ->
        NtC.versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (protocols v genTx))
      (supportedNodeToClientVersions proxy)
  where
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NodeToClientVersionData { networkMagic = toNetworkMagic nm }

    protocols clientVersion tx =
        NodeToClientProtocols {
          localChainSyncProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cChainSyncCodec
                chainSyncPeerNull

        , localTxSubmissionProtocol =
            InitiatorProtocolOnly $
              MuxPeerRaw $ \channel -> do
                traceWith tracer "Submitting transaction"
                result <- runPeer
                            nullTracer -- (contramap show tracer)
                            cTxSubmissionCodec
                            channel
                            (LocalTxSub.localTxSubmissionClientPeer
                               (txSubmissionClientSingle tx))
                case result of
                  Nothing  -> traceWith tracer "Transaction accepted"
                  Just _TODO -> traceWith tracer "Transaction rejected"
                  --TODO: return the result

        , localStateQueryProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cStateQueryCodec
                localStateQueryPeerNull
        }
      where
        Codecs
          { cChainSyncCodec
          , cTxSubmissionCodec
          , cStateQueryCodec
          } = defaultCodecs (pClientInfoCodecConfig cfg) clientVersion

txSubmissionClientSingle
  :: forall tx reject m.
     Applicative m
  => tx
  -> LocalTxSub.LocalTxSubmissionClient tx reject m (Maybe reject)
txSubmissionClientSingle tx = LocalTxSub.LocalTxSubmissionClient $ do
    pure $ LocalTxSub.SendMsgSubmitTx tx $ \mreject ->
      pure (LocalTxSub.SendMsgDone mreject)
