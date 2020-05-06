{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.TxSubmit
  ( TxSubmitStatus (..)
  , renderTxSubmitStatus
  , submitTransaction
  ) where

import           Cardano.Prelude hiding (Nat, atomically, option, (%))

import           Cardano.Api.TxSubmit.TxSubmitVar
import           Cardano.Api.TxSubmit.Types
import           Cardano.Api.Types

import           Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import           Cardano.BM.Trace (Trace, appendName, logInfo)

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.TracingOrphanInstances.Network ()

import qualified Codec.CBOR.Term as CBOR

import           Control.Tracer (Tracer)

import           Data.Functor.Contravariant (contramap)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)

import           Network.Mux (MuxTrace, WithMuxBearer)

import           Ouroboros.Consensus.Block (BlockConfig)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..), GenTx)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Cardano (Protocol (..), protocolInfo)
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (NodeToClientVersion, nodeToClientProtocolVersion,
                   supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)

import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import           Ouroboros.Consensus.Node.Run (nodeNetworkMagic)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (AppType (..) , MuxPeer (..),
                    RunMiniProtocol (..))

import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..),
                    NetworkSubscriptionTracers (..), NodeToClientProtocols (..),
                    TraceSendRecv, WithAddr (..))
import qualified Ouroboros.Network.NodeToClient as NtC

import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (LocalTxClientStIdle (..), LocalTxSubmissionClient (..),
                   localTxSubmissionClientPeer)
import           Ouroboros.Network.Subscription (SubscriptionTrace)


-- Submit Transaction - status indicates whether transaction has been submitted, not processed
-- could be Byron or Shelley
-- can mark as done once split into separate library
-- the status is just whether the transaction has been submitted, not whether it has
-- succeeded/appeared on chain
submitTransaction :: NodeApiEnv -> TxSigned -> IO TxSubmitStatus
submitTransaction nodeEnv txs = do
  tvar <- newTxSubmitVar
  res <- race
            (runTxSubmitNode tvar nullTracer (naeConfig nodeEnv) (naeSocket nodeEnv))
            (submitTx tvar $ prepareTx txs)
  case res of
    Left _ -> panic "Cardano.Api.TxSubmit.submitTransaction: runTxSubmitNode terminated unexpectedly."
    Right st -> pure st

prepareTx :: TxSigned -> GenTx ByronBlock
prepareTx txs =
  case txs of
    TxSignedByron tx _txCbor _txHash vwit ->
      let aTxAux = Byron.annotateTxAux (Byron.mkTxAux tx vwit)
      in Byron.ByronTx (Byron.byronIdTx aTxAux) aTxAux
    TxSignedShelley _tx -> panic "Cardano.Api.TxSubmit.submitTransaction: TxSignedShelley"


runTxSubmitNode :: TxSubmitVar -> Trace IO Text -> Genesis.Config -> SocketPath -> IO ()
runTxSubmitNode tsv trce gc socket = do
  logInfo trce "Running tx-submit node"
  void $ runTxSubmitNodeClient tsv (mkNodeConfig gc) trce socket


mkNodeConfig :: Genesis.Config -> TopLevelConfig ByronBlock
mkNodeConfig gc =
  pInfoConfig . protocolInfo $ ProtocolRealPBFT gc Nothing (Update.ProtocolVersion 0 2 0)
      (Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1) Nothing

runTxSubmitNodeClient
  :: forall blk. (blk ~ ByronBlock)
  => TxSubmitVar -> TopLevelConfig blk
  -> Trace IO Text -> SocketPath
  -> IO Void
runTxSubmitNodeClient tsv nodeConfig trce (SocketPath socketPath) = do
  logInfo trce $ "localInitiatorNetworkApplication: connecting to node via " <> textShow socketPath
  networkState <- NtC.newNetworkMutableState
  NtC.withIOManager $ \iocp -> do
    NtC.ncSubscriptionWorker
      (NtC.localSnocket iocp socketPath)
      NtC.NetworkSubscriptionTracers {
        nsMuxTracer = muxTracer,
        nsHandshakeTracer = handshakeTracer,
        nsErrorPolicyTracer = errorPolicyTracer,
        nsSubscriptionTracer = subscriptionTracer
        }
      networkState
      ClientSubscriptionParams
        { cspAddress = NtC.LocalAddress socketPath
        , cspConnectionAttemptDelay = Nothing
        , cspErrorPolicies = NtC.networkErrorPolicies <> consensusErrorPolicy proxy
        }
      (NtC.foldMapVersions
        (\v ->
          NtC.versionedNodeToClientProtocols
            (nodeToClientProtocolVersion proxy v)
            versionData
            (localInitiatorNetworkApplication
              trce (configBlock nodeConfig) v tsv))
        (supportedNodeToClientVersions proxy))
  where
    -- TODO: it should be passed as an argument
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NtC.NodeToClientVersionData (nodeNetworkMagic proxy nodeConfig)

    errorPolicyTracer :: Tracer IO (WithAddr NtC.LocalAddress NtC.ErrorPolicyTrace)
    errorPolicyTracer = contramap (Text.pack . show). toLogObject $ appendName "ErrorPolicy" trce

    muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    subscriptionTracer :: Tracer IO (Identity (SubscriptionTrace NtC.LocalAddress))
    subscriptionTracer = toLogObject $ appendName "Subscription" trce

    handshakeTracer :: Tracer IO
                        (WithMuxBearer (NtC.ConnectionId NtC.LocalAddress)
                        (TraceSendRecv (NtC.Handshake NtC.NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce


localInitiatorNetworkApplication
  :: Trace IO Text
  -> BlockConfig ByronBlock
  -> NodeToClientVersion ByronBlock
  -> TxSubmitVar
  -> NodeToClientProtocols 'InitiatorApp LBS.ByteString IO Void Void
localInitiatorNetworkApplication trce blockConfig byronClientVersion tsv =
    NodeToClientProtocols
      { localChainSyncProtocol =
          InitiatorProtocolOnly $ MuxPeer
            nullTracer
            cChainSyncCodec
            NtC.chainSyncPeerNull

      , localTxSubmissionProtocol =
          InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                runPeer
                  (contramap (Text.pack . show) . toLogObject $ appendName "cardano-tx-submit" trce)
                  cTxSubmissionCodec channel
                  (localTxSubmissionClientPeer (txSubmissionClient tsv))
      , localStateQueryProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              cStateQueryCodec
              NtC.localStateQueryPeerNull
      }
  where
    Codecs { cChainSyncCodec
           , cTxSubmissionCodec
           , cStateQueryCodec
           } = defaultCodecs blockConfig byronClientVersion 


-- | A 'LocalTxSubmissionClient' that submits transactions reading them from
-- a 'StrictTMVar'.  A real implementation should use a better synchronisation
-- primitive.  This demo creates and empty 'TMVar' in
-- 'muxLocalInitiatorNetworkApplication' above and never fills it with a tx.
--
txSubmissionClient
  :: TxSubmitVar
  -> LocalTxSubmissionClient (GenTx ByronBlock) (ApplyTxErr ByronBlock) IO Void
txSubmissionClient tsv =
    LocalTxSubmissionClient $
      readTxSubmit tsv >>= pure . loop
  where
    loop :: GenTx ByronBlock -> LocalTxClientStIdle (GenTx ByronBlock) (ApplyTxErr ByronBlock) IO Void
    loop tx =
      SendMsgSubmitTx tx $ \mbreject -> do
        writeTxSubmitResponse tsv mbreject
        nextTx <- readTxSubmit tsv
        pure $ loop nextTx
