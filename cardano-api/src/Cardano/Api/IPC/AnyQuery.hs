{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.IPC.AnyQuery
  ( InvalidEraInMode(..)
  , invalidEraInModeToSbqeEraInMode
  , determineEraInModeAnyQuery
  , determineEraInModeAnyQuery_
  , determineEraExprAnyQuery
  , determineEraExprAnyQuery_
  , executeLocalStateQueryExprAnyQuery
  , executeLocalStateQueryExprAnyQuery_
  , queryExprAnyQuery
  , queryExprAnyQueryE
  , queryExprAnyQueryE_
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Oops (CouldBe, Variant)
import qualified Control.Monad.Oops as OO
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except.Extra (left)
import           Control.Tracer
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Void

import           Cardano.Ledger.Shelley.Scripts ()
import qualified Ouroboros.Consensus.Block as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Network.NodeToClient as Consensus
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import qualified Ouroboros.Network.Mux as Net
import           Ouroboros.Network.NodeToClient (NodeToClientProtocols (..),
                   NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as Net
import           Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined as Net.SyncP
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Network.Protocol.LocalTxMonitor.Client (localTxMonitorClientPeer)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx


import           Cardano.Api.Block
import           Cardano.Api.Convenience.Error
import           Cardano.Api.Eras
import           Cardano.Api.InMode
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.IPC.Types
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Protocol
import           Cardano.Api.Query


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}



-- | Execute a local state query expression.
executeLocalStateQueryExprAnyQuery
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError AllQueryErrors (BlockInMode mode) ChainPoint (AnyQuery mode) () IO a
  -> IO (Either AllQueryErrors a)
executeLocalStateQueryExprAnyQuery connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersionAnyQuery
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExprAnyQuery waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult

-- | Execute a local state query expression.
executeLocalStateQueryExprAnyQuery_
  :: forall e mode a. ()
  => e `CouldBe` AcquiringFailure
  => LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> ExceptT (Variant e) (LocalStateQueryExpr (BlockInMode mode) ChainPoint (AnyQuery mode) () IO) a
  -> ExceptT (Variant e) IO a
executeLocalStateQueryExprAnyQuery_ connectInfo mpoint f = do
  tmvResultLocalState <- liftIO (newEmptyTMVarIO @(Either (Variant e) a))
  let waitResult = readTMVar tmvResultLocalState

  liftIO $ connectToLocalNodeWithVersionAnyQuery
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr_ waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  ExceptT . return =<< liftIO (atomically waitResult)

-- Local state query client related
--------------------------------------------------

-- | Use 'queryExprAnyQuery' in a do block to construct monadic local state queries.
setupLocalStateQueryExprAnyQuery ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either AllQueryErrors a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError AllQueryErrors (BlockInMode mode) ChainPoint (AnyQuery mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (AnyQuery mode) IO ()
setupLocalStateQueryExprAnyQuery waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left  $ AllQueryErrorSimple (SimpleQueryErrorAcquiringFail $ toAcquiringFailure failure))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }


-- Connection related
--------------------------------------------------

type LocalNodeClientProtocolsInModeAnyQuery mode =
  LocalNodeClientProtocols
    (BlockInMode mode)
    ChainPoint
    ChainTip
    SlotNo
    (TxInMode mode)
    (TxIdInMode mode)
    (TxValidationErrorInMode mode)
    (AnyQuery mode)
    IO

-- | Establish a connection to a local node and execute the given set of
-- protocol handlers parameterized on the negotiated node-to-client protocol
-- version.
--
connectToLocalNodeWithVersionAnyQuery
  :: LocalNodeConnectInfo mode
  -> (NodeToClientVersion -> LocalNodeClientProtocolsInModeAnyQuery mode)
  -> IO ()
connectToLocalNodeWithVersionAnyQuery LocalNodeConnectInfo {
                     localNodeSocketPath,
                     localNodeNetworkId,
                     localConsensusModeParams
                   } clients =
    Net.withIOManager $ \iomgr ->
      Net.connectTo
        (Net.localSnocket iomgr)
        Net.NetworkConnectTracers {
          Net.nctMuxTracer       = nullTracer,
          Net.nctHandshakeTracer = nullTracer
        }
        versionedProtocls
        localNodeSocketPath
  where
    versionedProtocls =
      -- First convert from the mode-parametrised view of things to the
      -- block-parametrised view and then do the final setup for the versioned
      -- bundles of mini-protocols.
      case mkLocalNodeClientParamsAnyQuery localConsensusModeParams clients of
        LocalNodeClientParamsSingleBlock ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'
        LocalNodeClientParamsCardano ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'

mkVersionedProtocols :: forall block.
                        ( Consensus.ShowQuery (Consensus.Query block)
                        , ProtocolClient block
                        )
                     => NetworkId
                     -> ProtocolClientInfoArgs block
                     -> (NodeToClientVersion -> LocalNodeClientProtocolsForBlock block)
                     -> Net.Versions
                          Net.NodeToClientVersion
                          Net.NodeToClientVersionData
                          (Net.OuroborosApplication
                             Net.InitiatorMode
                             Net.LocalAddress
                             LBS.ByteString IO () Void)
mkVersionedProtocols networkid ptcl unversionedClients =
     --TODO: really we should construct specific combinations of
     -- protocols for the versions we know about, with different protocol
     -- versions taking different sets of typed client protocols.
    Net.foldMapVersions
      (\(ptclVersion, ptclBlockVersion) ->
          Net.versionedNodeToClientProtocols
            ptclVersion
            NodeToClientVersionData {
              networkMagic = toNetworkMagic networkid
            }
            (\_connid _ctl -> protocols (unversionedClients ptclVersion) ptclBlockVersion ptclVersion))
      (Map.toList (Consensus.supportedNodeToClientVersions proxy))
  where
    proxy :: Proxy block
    proxy = Proxy

    protocols :: LocalNodeClientProtocolsForBlock block
              -> Consensus.BlockNodeToClientVersion block
              -> NodeToClientVersion
              -> NodeToClientProtocols Net.InitiatorMode LBS.ByteString IO () Void
    protocols
      LocalNodeClientProtocolsForBlock {
        localChainSyncClientForBlock,
        localTxSubmissionClientForBlock,
        localStateQueryClientForBlock,
        localTxMonitoringClientForBlock
      }
      ptclBlockVersion
      ptclVersion =
        NodeToClientProtocols {
          localChainSyncProtocol =
            Net.InitiatorProtocolOnly $ case localChainSyncClientForBlock of
              NoLocalChainSyncClient
                -> Net.MuxPeer nullTracer cChainSyncCodec Net.chainSyncPeerNull
              LocalChainSyncClient client
                -> Net.MuxPeer
                      nullTracer
                      cChainSyncCodec
                      (Net.Sync.chainSyncClientPeer client)
              LocalChainSyncClientPipelined clientPipelined
                -> Net.MuxPeerPipelined
                      nullTracer
                      cChainSyncCodec
                      (Net.SyncP.chainSyncClientPeerPipelined clientPipelined)

        , localTxSubmissionProtocol =
            Net.InitiatorProtocolOnly $
              Net.MuxPeer
                nullTracer
                cTxSubmissionCodec
                (maybe Net.localTxSubmissionPeerNull
                       Net.Tx.localTxSubmissionClientPeer
                       localTxSubmissionClientForBlock)

        , localStateQueryProtocol =
            Net.InitiatorProtocolOnly $
              Net.MuxPeer
                nullTracer
                cStateQueryCodec
                (maybe Net.localStateQueryPeerNull
                       Net.Query.localStateQueryClientPeer
                       localStateQueryClientForBlock)
        , localTxMonitorProtocol =
            Net.InitiatorProtocolOnly $
              Net.MuxPeer
                nullTracer
                cTxMonitorCodec
                (maybe Net.localTxMonitorPeerNull
                       localTxMonitorClientPeer
                       localTxMonitoringClientForBlock)
        }
      where
        Consensus.Codecs {
          Consensus.cChainSyncCodec,
          Consensus.cTxMonitorCodec,
          Consensus.cTxSubmissionCodec,
          Consensus.cStateQueryCodec
        } = Consensus.clientCodecs codecConfig ptclBlockVersion ptclVersion

    codecConfig :: Consensus.CodecConfig block
    codecConfig = Consensus.pClientInfoCodecConfig
                    (protocolClientInfo ptcl)


-- | Convert from the mode-parametrised style to the block-parametrised style.
--
mkLocalNodeClientParamsAnyQuery
  :: forall mode block. ConsensusBlockForMode mode ~ block
  => ConsensusModeParams mode
  -> (NodeToClientVersion -> LocalNodeClientProtocolsInModeAnyQuery mode)
  -> LocalNodeClientParams
mkLocalNodeClientParamsAnyQuery modeparams clients =
    -- For each of the possible consensus modes we pick the concrete block type
    -- (by picking the appropriate 'ProtocolClient' value).
    --
    -- Though it is not immediately visible, this point where we use
    -- 'LocalNodeClientParams' is also where we pick up the necessary class
    -- instances. This works because in each case we have a monomorphic block
    -- type and the instances are all in scope. This is why the use of
    -- LocalNodeClientParams is repeated within each branch of the case:
    -- because it is only within each branch that the GADT match makes the
    -- block type monomorphic.
    --
    case modeparams of
      ByronModeParams epochSlots ->
        LocalNodeClientParamsSingleBlock
          (ProtocolClientInfoArgsByron epochSlots)
          (convLocalNodeClientProtocolsAnyQuery ByronMode . clients)

      ShelleyModeParams ->
        LocalNodeClientParamsSingleBlock
          ProtocolClientInfoArgsShelley
          (convLocalNodeClientProtocolsAnyQuery ShelleyMode . clients)

      CardanoModeParams epochSlots ->
       LocalNodeClientParamsCardano
         (ProtocolClientInfoArgsCardano epochSlots)
         (convLocalNodeClientProtocolsAnyQuery CardanoMode . clients)



convLocalNodeClientProtocolsAnyQuery
  :: forall mode block. ConsensusBlockForMode mode ~ block
  => ConsensusMode mode
  -> LocalNodeClientProtocolsInModeAnyQuery mode
  -> LocalNodeClientProtocolsForBlock block
convLocalNodeClientProtocolsAnyQuery
    mode
    LocalNodeClientProtocols {
      localChainSyncClient,
      localTxSubmissionClient,
      localStateQueryClient,
      localTxMonitoringClient
    } =
    LocalNodeClientProtocolsForBlock {
      localChainSyncClientForBlock    = case localChainSyncClient of
        NoLocalChainSyncClient -> NoLocalChainSyncClient
        LocalChainSyncClientPipelined clientPipelined -> LocalChainSyncClientPipelined $ convLocalChainSyncClientPipelined mode clientPipelined
        LocalChainSyncClient client -> LocalChainSyncClient $ convLocalChainSyncClient mode client,

      localTxSubmissionClientForBlock = convLocalTxSubmissionClient mode <$>
                                          localTxSubmissionClient,

      localStateQueryClientForBlock   = convLocalStateQueryClientAnyQuery mode <$>
                                          localStateQueryClient,

      localTxMonitoringClientForBlock = convLocalTxMonitoringClient mode <$>
                                          localTxMonitoringClient

    }

convLocalStateQueryClientAnyQuery
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> LocalStateQueryClient (BlockInMode mode) ChainPoint (AnyQuery mode) m a
  -> LocalStateQueryClient block (Consensus.Point block)
                           (Consensus.Query block) m a
convLocalStateQueryClientAnyQuery mode =
    Net.Query.mapLocalStateQueryClient
      (toConsensusPointInMode mode)
      toConsensusAnyQuery
      fromConsensusQueryAnyResult


-- Query related
--------------------------------------------------

getNtcVersion :: LocalStateQueryExprWithError AllQueryErrors block point (AnyQuery mode) r IO NodeToClientVersion
getNtcVersion = lift $ LocalStateQueryExpr ask

-- | Use 'queryExprAnyQueryE' in a do block to construct monadic local state queries.
queryExprAnyQueryE
  :: AnyQuery mode (Either EraMismatch a)
  -> LocalStateQueryExprWithError AllQueryErrors block point (AnyQuery mode) r IO a
queryExprAnyQueryE (AnyQueryAnyEra q) = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- getNtcVersion
  if ntcVersion >= minNtcVersion
    then do
      res <- lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
               Net.Query.SendMsgQuery (AnyQueryAnyEra q) $
                 Net.Query.ClientStQuerying
                 { Net.Query.recvMsgResult = f
                 }
      case res of
        -- It is not possible to have a QueryEraMismatch in the AnyQueryAnyEra case.
        -- However in order to have a single function that can handle both AnyQueryAnyEra
        -- and AnyQueryShelleyBasedEra queries we are also forced to pattern match here.
        Left eraMismatch -> left $ AllQueryErrorSbe $ SbqeQueryEraMismatch eraMismatch
        Right v -> return v
    else left . AllQueryErrorSimple . SimpleQueryErrorUnsupportedVer
              $ UnsupportedNtcVersionError minNtcVersion ntcVersion
queryExprAnyQueryE (AnyQueryShelleyBasedEra q) = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- getNtcVersion
  if ntcVersion >= minNtcVersion
    then do
      res <- lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
               Net.Query.SendMsgQuery (AnyQueryShelleyBasedEra q) $
                 Net.Query.ClientStQuerying
                 { Net.Query.recvMsgResult = f
                 }
      case res of
        Left eraMismatch -> left $ AllQueryErrorSbe $ SbqeQueryEraMismatch eraMismatch
        Right v -> return v
    else left . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorUnsupportedVer
              $ UnsupportedNtcVersionError minNtcVersion ntcVersion

queryExprAnyQueryE_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => AnyQuery mode a
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery mode) r IO) a
queryExprAnyQueryE_ aq = case aq of
  AnyQueryAnyEra q -> do
    let minNtcVersion = nodeToClientVersionOf q
    ntcVersion <- getNtcVersion_
    if ntcVersion >= minNtcVersion
      then do
        lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
                Net.Query.SendMsgQuery (AnyQueryAnyEra q) $
                  Net.Query.ClientStQuerying
                  { Net.Query.recvMsgResult = f
                  }
      else OO.throw $ UnsupportedNtcVersionError minNtcVersion ntcVersion
  AnyQueryShelleyBasedEra q -> do
    let minNtcVersion = nodeToClientVersionOf q
    ntcVersion <- getNtcVersion_
    if ntcVersion >= minNtcVersion
      then
        lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
                Net.Query.SendMsgQuery (AnyQueryShelleyBasedEra q) $
                  Net.Query.ClientStQuerying
                  { Net.Query.recvMsgResult = f
                  }

      else OO.throw $ UnsupportedNtcVersionError minNtcVersion ntcVersion

-- | Use 'queryExprAnyQuery' in a do block to construct monadic local state queries.
queryExprAnyQuery
  :: AnyQuery mode a
  -> LocalStateQueryExprWithError AllQueryErrors block point (AnyQuery mode) r IO a
queryExprAnyQuery (AnyQueryAnyEra q) = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- getNtcVersion
  if ntcVersion >= minNtcVersion
    then do
      lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
               Net.Query.SendMsgQuery (AnyQueryAnyEra q) $
                 Net.Query.ClientStQuerying
                 { Net.Query.recvMsgResult = f
                 }
    else left $ AllQueryErrorSimple $ SimpleQueryErrorUnsupportedVer $ UnsupportedNtcVersionError minNtcVersion ntcVersion
queryExprAnyQuery (AnyQueryShelleyBasedEra q) = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- getNtcVersion
  if ntcVersion >= minNtcVersion
    then
      lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
               Net.Query.SendMsgQuery (AnyQueryShelleyBasedEra q) $
                 Net.Query.ClientStQuerying
                 { Net.Query.recvMsgResult = f
                 }

    else left $ AllQueryErrorSbe $ SbqeSimpleQueryError $ SimpleQueryErrorUnsupportedVer $ UnsupportedNtcVersionError minNtcVersion ntcVersion

-- | A monad expression that determines what era the node is in.
determineEraExprAnyQuery ::
     ConsensusModeParams mode
  -> LocalStateQueryExprWithError AllQueryErrors block point (AnyQuery mode) r IO AnyCardanoEra
determineEraExprAnyQuery cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExprAnyQuery . AnyQueryAnyEra $ QueryCurrentEra CardanoModeIsMultiEra

determineEraExprAnyQuery_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ConsensusModeParams mode
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery mode) r IO) AnyCardanoEra
determineEraExprAnyQuery_ cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExprAnyQueryE_ $ AnyQueryAnyEra $ QueryCurrentEra CardanoModeIsMultiEra

determineEraInModeAnyQuery
  :: CardanoEra era
  -> ConsensusModeParams mode
  -> LocalStateQueryExprWithError AllQueryErrors block point (AnyQuery mode) r IO (EraInMode era mode)
determineEraInModeAnyQuery era cModeParams = do
  let cMode = consensusModeOnly cModeParams
  case toEraInMode era cMode of
    Nothing -> left . AllQueryErrorSbe $ SbqeEraInMode (anyCardanoEra era) (AnyConsensusMode cMode)
    Just eInMode -> return eInMode

data InvalidEraInMode = InvalidEraInMode AnyCardanoEra AnyConsensusMode

invalidEraInModeToSbqeEraInMode :: InvalidEraInMode -> ShelleyBasedQueryError
invalidEraInModeToSbqeEraInMode (InvalidEraInMode era cMode) = SbqeEraInMode era cMode

determineEraInModeAnyQuery_ :: ()
  => e `CouldBe` InvalidEraInMode
  => CardanoEra era
  -> ConsensusModeParams mode
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery mode) r IO) (EraInMode era mode)
determineEraInModeAnyQuery_ era cModeParams = do
  let cMode = consensusModeOnly cModeParams
  case toEraInMode era cMode of
    Nothing -> OO.throw $ InvalidEraInMode (anyCardanoEra era) (AnyConsensusMode cMode)
    Just eInMode -> return eInMode
