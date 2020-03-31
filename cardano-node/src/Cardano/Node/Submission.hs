{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Node.Submission (
      TraceLowLevelSubmit (..)
    , submitGeneralTx
    ) where
import           Cardano.Prelude hiding (ByteString, option, threadDelay)
import           Prelude (String)

import           Data.Aeson ((.=), Value (..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import           Data.Void (Void)

import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadThrow)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Tracer (Tracer, nullTracer, traceWith)

import           Ouroboros.Consensus.Mempool (ApplyTxErr, GenTx)
import           Ouroboros.Consensus.NodeNetwork (ProtocolCodecs (..), protocolCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (mostRecentNetworkProtocolVersion)
import           Ouroboros.Consensus.Node.Run (RunNode)
import qualified Ouroboros.Consensus.Node.Run as Node
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Network.Codec (Codec, DeserialiseFailure)
import           Ouroboros.Network.Mux
                   ( AppType(..), OuroborosApplication(..),
                     MuxPeer(..), RunMiniProtocol(..) )
import           Ouroboros.Network.Block (Tip)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Driver (runPeer)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as LocalTxSub
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Codec as LocalTxSub
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import           Ouroboros.Network.Protocol.Handshake.Version ( Versions
                                                              , simpleSingletonVersions)
import           Ouroboros.Network.NodeToClient ( IOManager
                                                , NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToClient as NtC

import           Cardano.BM.Tracing
import           Cardano.BM.Data.Tracer (emptyObject, mkObject, trStructured)
import           Cardano.Config.Types (SocketPath(..))

-- | Low-tevel tracer
data TraceLowLevelSubmit
  = TraceLowLevelSubmitting
  -- ^ Submitting transaction.
  | TraceLowLevelAccepted
  -- ^ The transaction has been accepted.
  | TraceLowLevelRejected String
  -- ^ The transaction has been rejected, with corresponding error message.
  deriving (Show)

instance ToObject TraceLowLevelSubmit where
  toObject MinimalVerbosity _ = emptyObject -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceLowLevelSubmitting -> mkObject ["kind" .= String "TraceLowLevelSubmitting"]
      TraceLowLevelAccepted   -> mkObject ["kind" .= String "TraceLowLevelAccepted"]
      TraceLowLevelRejected _ -> mkObject ["kind" .= String "TraceLowLevelRejected"]
  toObject MaximalVerbosity t =
    case t of
      TraceLowLevelSubmitting ->
        mkObject [ "kind" .= String "TraceLowLevelSubmitting"
                 ]
      TraceLowLevelAccepted ->
        mkObject [ "kind" .= String "TraceLowLevelAccepted"
                 ]
      TraceLowLevelRejected errMsg ->
        mkObject [ "kind"   .= String "TraceLowLevelRejected"
                 , "errMsg" .= String (T.pack errMsg)
                 ]

instance HasSeverityAnnotation TraceLowLevelSubmit

instance HasPrivacyAnnotation TraceLowLevelSubmit

instance (MonadIO m) => Transformable Text m TraceLowLevelSubmit where
  -- transform to JSON Object
  trTransformer StructuredLogging verb tr = trStructured verb tr
  trTransformer _ _verb _tr = nullTracer

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}


-- | Submit a general transaction. This can be a UTxO transaction,
-- an update proposal, a vote or a delegation certificate.
submitGeneralTx
  :: ( RunNode blk, Show (ApplyTxErr blk))
  => IOManager
  -> SocketPath
  -> TopLevelConfig blk
  -> GenTx blk
  -> Tracer IO TraceLowLevelSubmit
  -> IO ()
submitGeneralTx iocp (SocketFile path) cfg tx tracer =
  NtC.connectTo
    (NtC.localSnocket iocp path)
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = nullTracer
      }
    (localInitiatorNetworkApplication tracer cfg tx)
    path

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , MonadST m
     , MonadThrow m
     , MonadTimer m
     , IOLike m
     , Show (ApplyTxErr blk)
     )
  => Tracer m TraceLowLevelSubmit
  -> TopLevelConfig blk
  -> GenTx blk
  -> Versions NtC.NodeToClientVersion NtC.DictVersion
              (peer -> OuroborosApplication InitiatorApp ByteString m () Void)
localInitiatorNetworkApplication tracer cfg tx =
    simpleSingletonVersions
      NtC.NodeToClientV_1
      (NtC.NodeToClientVersionData
        { NtC.networkMagic = Node.nodeNetworkMagic (Proxy @blk) cfg })
      (NtC.DictVersion NtC.nodeToClientCodecCBORTerm) $ \_peerid ->

    NtC.nodeToClientProtocols
      NtC.NodeToClientProtocols {
        NtC.localChainSyncProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              (pcLocalChainSyncCodec codecs)
              (chainSyncClientPeer NtC.chainSyncClientNull)

      , NtC.localTxSubmissionProtocol =
          InitiatorProtocolOnly $
            MuxPeerRaw $ \channel -> do
              traceWith tracer TraceLowLevelSubmitting
              result <- runPeer
                          nullTracer -- (contramap show tracer)
                          (pcLocalTxSubmissionCodec codecs)
                          channel
                          (LocalTxSub.localTxSubmissionClientPeer
                             (txSubmissionClientSingle tx))
              case result of
                Nothing  -> traceWith tracer TraceLowLevelAccepted
                Just msg -> traceWith tracer (TraceLowLevelRejected $ show msg)
      }
    where
      codecs = protocolCodecs cfg (mostRecentNetworkProtocolVersion (Proxy @ blk))

-- | A 'LocalTxSubmissionClient' that submits exactly one transaction, and then
-- disconnects, returning the confirmation or rejection.
--
txSubmissionClientSingle
  :: forall tx reject m.
     Applicative m
  => tx
  -> LocalTxSub.LocalTxSubmissionClient tx reject m (Maybe reject)
txSubmissionClientSingle tx = LocalTxSub.LocalTxSubmissionClient $ do
    pure $ LocalTxSub.SendMsgSubmitTx tx $ \mreject ->
      pure (LocalTxSub.SendMsgDone mreject)
