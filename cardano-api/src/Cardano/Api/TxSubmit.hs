{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.TxSubmit
  ( submitTx
  , submitGenTx
  , TxSubmitResult(..)
  ) where

import           Cardano.Prelude

import           Control.Tracer
import           Control.Concurrent.STM

import           Cardano.Api.Types

import           Ouroboros.Consensus.Cardano (protocolClientInfo)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo(..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Mempool (GenTx, ApplyTxErr)

import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient hiding (NodeToClientVersion (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

import           Cardano.Chain.Slotting (EpochSlots (..))

import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Cardano.Chain.UTxO as Byron

import           Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Cardano.Config.Byron.Protocol (mkNodeClientProtocolRealPBFT)
import           Cardano.Config.Shelley.Protocol (mkNodeClientProtocolTPraos)
import           Cardano.Config.Types (SocketPath(..))


data TxSubmitResult
   = TxSubmitSuccess
   | TxSubmitFailureByron   (ApplyTxErr ByronBlock)
   | TxSubmitFailureShelley (ApplyTxErr (ShelleyBlock TPraosStandardCrypto))

submitTx
  :: Network
  -> SocketPath
  -> TxSigned
  -> IO TxSubmitResult
submitTx network socketPath tx =
    NtC.withIOManager $ \iocp ->
      case tx of
        TxSignedByron txbody _txCbor _txHash vwit -> do
          let aTxAux = Byron.annotateTxAux (Byron.mkTxAux txbody vwit)
              genTx  = Byron.ByronTx (Byron.byronIdTx aTxAux) aTxAux
          result <- submitGenTx
                      nullTracer
                      iocp
                      (protocolClientInfo $ mkNodeClientProtocolRealPBFT $ EpochSlots 21600)
                      network
                      socketPath
                      genTx
          case result of
            SubmitSuccess  -> return TxSubmitSuccess
            SubmitFail err -> return (TxSubmitFailureByron err)

        TxSignedShelley stx -> do
          let genTx = mkShelleyTx stx
          result <- submitGenTx
                      nullTracer
                      iocp
                      (protocolClientInfo mkNodeClientProtocolTPraos)
                      network
                      socketPath
                      genTx
          case result of
            SubmitSuccess  -> return TxSubmitSuccess
            SubmitFail err -> return (TxSubmitFailureShelley err)


submitGenTx
  :: forall blk.
     RunNode blk
  => Tracer IO Text
  -> IOManager
  -> ProtocolClientInfo blk
  -> Network
  -> SocketPath
  -> GenTx blk
  -> IO (SubmitResult (ApplyTxErr blk))
submitGenTx tracer iomgr cfg nm (SocketPath path) genTx = do
    resultVar <- newEmptyTMVarIO
    connectTo
      (localSnocket iomgr path)
      NetworkConnectTracers {
          nctMuxTracer       = nullTracer,
          nctHandshakeTracer = nullTracer
          }
      (localInitiatorNetworkApplication tracer cfg nm resultVar genTx)
      path
      --`catch` handleMuxError tracer chainsVar socketPath
    atomically (readTMVar resultVar)

localInitiatorNetworkApplication
  :: forall blk.
     RunNode blk
  => Tracer IO Text
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> ProtocolClientInfo blk
  -> Network
  -> TMVar (SubmitResult (ApplyTxErr blk)) -- ^ Result will be placed here
  -> GenTx blk
  -> Versions NtC.NodeToClientVersion DictVersion
               (OuroborosApplication InitiatorApp LocalAddress LByteString IO () Void)
localInitiatorNetworkApplication tracer cfg nm resultVar genTx =
    foldMapVersions
      (\v ->
        NtC.versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (const $ protocols v genTx))
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
                            (localTxSubmissionClientPeer
                               (txSubmissionClientSingle tx))
                case result of
                  SubmitSuccess -> traceWith tracer "Transaction accepted"
                  SubmitFail _  -> traceWith tracer "Transaction rejected"
                atomically $ putTMVar resultVar result

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
  -> LocalTxSubmissionClient tx reject m (SubmitResult reject)
txSubmissionClientSingle tx =
    LocalTxSubmissionClient $
    pure $ SendMsgSubmitTx tx $ \result ->
    pure (SendMsgDone result)
