{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Node IPC protocols
--
module Cardano.Api.IPC (
    -- * Node interaction
    -- | Operations that involve talking to a local Cardano node.
    connectToLocalNode,
    connectToLocalNodeWithVersion,
    LocalNodeConnectInfo(..),
    localConsensusMode,
    LocalNodeClientParams(..),
    mkLocalNodeClientParams,
    LocalNodeClientProtocols(..),
    LocalChainSyncClient(..),
    LocalNodeClientProtocolsInMode,

    -- ** Modes
    -- | TODO move to Cardano.Api
    ByronMode,
    ShelleyMode,
    CardanoMode,
    ConsensusModeParams(..),
    EpochSlots(..),

--  connectToRemoteNode,

    -- *** Chain sync protocol
    ChainSyncClient(..),
    ChainSyncClientPipelined(..),
    BlockInMode(..),

    -- *** Local tx submission
    LocalTxSubmissionClient(..),
    TxInMode(..),
    TxValidationErrorInMode(..),
    TxValidationError,
    submitTxToNodeLocal,
    SubmitResult(..),

    -- *** Local state query
    LocalStateQueryClient(..),
    AcquireFailure(..),
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),
    queryNodeLocalState,

    -- *** Local tx monitoring
    LocalTxMonitorClient(..),
    LocalTxMonitoringQuery(..),
    LocalTxMonitoringResult(..),
    Consensus.MempoolSizeAndCapacity(..),
    queryTxMonitoringLocal,

    EraHistory(..),
    getProgress,

    -- *** Common queries
    getLocalChainTip,

    -- *** Helpers
    --TODO: These should be exported via Cardano.Api.Mode
    ConsensusMode(..),
    consensusModeOnly,

    NodeToClientVersion(..)
  ) where

import           Prelude

import           Data.Void (Void)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

import           Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, putTMVar, takeTMVar,
                   tryPutTMVar)
import           Control.Monad (void)
import           Control.Tracer (nullTracer)

import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import qualified Ouroboros.Network.Block as Net
import qualified Ouroboros.Network.Mux as Net
import           Ouroboros.Network.NodeToClient (NodeToClientProtocols (..),
                   NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as Net
import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))
import           Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined as Net.SyncP
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (LocalStateQueryClient (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query
import           Ouroboros.Network.Protocol.LocalTxMonitor.Client (LocalTxMonitorClient (..),
                   localTxMonitorClientPeer)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Client as CTxMon
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as Consensus
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (LocalTxSubmissionClient (..),
                   SubmitResult (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import qualified Ouroboros.Consensus.Block as Consensus
import           Ouroboros.Consensus.Cardano.CanHardFork
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus
import qualified Ouroboros.Consensus.Ledger.SupportsProtocol as Consensus
import qualified Ouroboros.Consensus.Network.NodeToClient as Consensus
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus

import           Cardano.Api.Block
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.InMode
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Protocol.Types
import           Cardano.Api.Query
import           Cardano.Api.TxBody

-- ----------------------------------------------------------------------------
-- The types for the client side of the node-to-client IPC protocols
--

-- | The protocols we can use with a local node. Use in conjunction with
-- 'connectToLocalNode'.
--
-- These protocols use the types from the rest of this API. The conversion
-- to\/from the types used by the underlying wire formats is handled by
-- 'connectToLocalNode'.
--
data LocalNodeClientProtocols block point tip slot tx txid txerr query m =
     LocalNodeClientProtocols {
       localChainSyncClient
         :: LocalChainSyncClient block point tip m

     , localTxSubmissionClient
         :: Maybe (LocalTxSubmissionClient tx txerr          m ())

     , localStateQueryClient
         :: Maybe (LocalStateQueryClient   block point query m ())

     , localTxMonitoringClient
         :: Maybe (LocalTxMonitorClient txid tx slot m ())
     }

data LocalChainSyncClient block point tip m
  = NoLocalChainSyncClient
  | LocalChainSyncClientPipelined (ChainSyncClientPipelined block point tip   m ())
  | LocalChainSyncClient          (ChainSyncClient          block point tip   m ())

-- public, exported
type LocalNodeClientProtocolsInMode mode =
       LocalNodeClientProtocols
         (BlockInMode mode)
         ChainPoint
         ChainTip
         SlotNo
         (TxInMode mode)
         (TxIdInMode mode)
         (TxValidationErrorInMode mode)
         (QueryInMode mode)
         IO

data LocalNodeConnectInfo mode =
     LocalNodeConnectInfo {
       localConsensusModeParams :: ConsensusModeParams mode,
       localNodeNetworkId       :: NetworkId,
       localNodeSocketPath      :: FilePath
     }

localConsensusMode :: LocalNodeConnectInfo mode -> ConsensusMode mode
localConsensusMode LocalNodeConnectInfo {localConsensusModeParams} =
    consensusModeOnly localConsensusModeParams

consensusModeOnly :: ConsensusModeParams mode
                  -> ConsensusMode       mode
consensusModeOnly ByronModeParams{}   = ByronMode
consensusModeOnly ShelleyModeParams{} = ShelleyMode
consensusModeOnly CardanoModeParams{} = CardanoMode


-- ----------------------------------------------------------------------------
-- Actually connect to the node
--

-- | Establish a connection to a local node and execute the given set of
-- protocol handlers.
--
connectToLocalNode :: LocalNodeConnectInfo mode
                   -> LocalNodeClientProtocolsInMode mode
                   -> IO ()
connectToLocalNode localNodeConnectInfo handlers
  = connectToLocalNodeWithVersion localNodeConnectInfo (const handlers)

-- | Establish a connection to a local node and execute the given set of
-- protocol handlers parameterized on the negotiated node-to-client protocol
-- version.
--
connectToLocalNodeWithVersion :: LocalNodeConnectInfo mode
                              -> (NodeToClientVersion -> LocalNodeClientProtocolsInMode mode)
                              -> IO ()
connectToLocalNodeWithVersion LocalNodeConnectInfo {
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
      case mkLocalNodeClientParams localConsensusModeParams clients of
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


-- | This type defines the boundary between the mode-parametrised style used in
-- this API and the block-parametrised style used by the underlying network
-- and consensus libraries.
--
-- This interface itself is in the block-parametrised style, with the block
-- type itself being an hidden\/existential type.
--
-- It bundles together all the necessary class instances, the consensus
-- protocol client identifier, and the set of client side mini-protocol
-- handlers for the node-to-client protocol.
--
data LocalNodeClientParams where
     LocalNodeClientParamsSingleBlock
       :: (ProtocolClient block,
           Consensus.LedgerSupportsProtocol
           (Consensus.ShelleyBlock
              (Consensus.TPraos Consensus.StandardCrypto)
              (Consensus.ShelleyEra Consensus.StandardCrypto))

           )
       => ProtocolClientInfoArgs block
       -> (NodeToClientVersion -> LocalNodeClientProtocolsForBlock block)
       -> LocalNodeClientParams

     LocalNodeClientParamsCardano
      :: (ProtocolClient block, CardanoHardForkConstraints (ConsensusCryptoForBlock block))
      => ProtocolClientInfoArgs block
      -> (NodeToClientVersion -> LocalNodeClientProtocolsForBlock block)
      -> LocalNodeClientParams

data LocalNodeClientProtocolsForBlock block =
     LocalNodeClientProtocolsForBlock {
       localChainSyncClientForBlock
         :: LocalChainSyncClient  block
                                  (Consensus.Point block)
                                  (Net.Tip         block)
                                   IO

     , localStateQueryClientForBlock
         :: Maybe (LocalStateQueryClient  block
                                         (Consensus.Point block)
                                         (Consensus.Query block)
                                          IO ())

     , localTxSubmissionClientForBlock
         :: Maybe (LocalTxSubmissionClient (Consensus.GenTx      block)
                                           (Consensus.ApplyTxErr block)
                                            IO ())
     , localTxMonitoringClientForBlock
         :: Maybe (LocalTxMonitorClient (Consensus.TxId  (Consensus.GenTx block))
                                        (Consensus.GenTx block)
                                        SlotNo IO ())
     }


-- | Convert from the mode-parametrised style to the block-parametrised style.
--
mkLocalNodeClientParams :: forall mode block.
                           ConsensusBlockForMode mode ~ block
                        => ConsensusModeParams mode
                        -> (NodeToClientVersion -> LocalNodeClientProtocolsInMode mode)
                        -> LocalNodeClientParams
mkLocalNodeClientParams modeparams clients =
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
          (convLocalNodeClientProtocols ByronMode . clients)

      ShelleyModeParams ->
        LocalNodeClientParamsSingleBlock
          ProtocolClientInfoArgsShelley
          (convLocalNodeClientProtocols ShelleyMode . clients)

      CardanoModeParams epochSlots ->
       LocalNodeClientParamsCardano
         (ProtocolClientInfoArgsCardano epochSlots)
         (convLocalNodeClientProtocols CardanoMode . clients)


convLocalNodeClientProtocols :: forall mode block.
                                ConsensusBlockForMode mode ~ block
                             => ConsensusMode mode
                             -> LocalNodeClientProtocolsInMode mode
                             -> LocalNodeClientProtocolsForBlock block
convLocalNodeClientProtocols
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

      localStateQueryClientForBlock   = convLocalStateQueryClient mode <$>
                                          localStateQueryClient,

      localTxMonitoringClientForBlock = convLocalTxMonitoringClient mode <$>
                                          localTxMonitoringClient

    }

convLocalTxMonitoringClient
  :: forall mode block m a. ConsensusBlockForMode mode ~ block
  => Functor m
  => ConsensusMode mode
  -> LocalTxMonitorClient (TxIdInMode mode)  (TxInMode mode) SlotNo m a
  -> LocalTxMonitorClient (Consensus.TxId (Consensus.GenTx block)) (Consensus.GenTx block) SlotNo m a
convLocalTxMonitoringClient mode =
  mapLocalTxMonitoringClient
    toConsensusTxId
    (fromConsensusGenTx mode)

convLocalChainSyncClient
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> ChainSyncClient (BlockInMode mode) ChainPoint ChainTip m a
  -> ChainSyncClient block (Net.Point block) (Net.Tip block) m a
convLocalChainSyncClient mode =
    Net.Sync.mapChainSyncClient
      (toConsensusPointInMode mode)
      (fromConsensusPointInMode mode)
      (fromConsensusBlock mode)
      (fromConsensusTip mode)

convLocalChainSyncClientPipelined
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> ChainSyncClientPipelined (BlockInMode mode) ChainPoint ChainTip m a
  -> ChainSyncClientPipelined block (Net.Point block) (Net.Tip block) m a
convLocalChainSyncClientPipelined mode =
  mapChainSyncClientPipelined
    (toConsensusPointInMode mode)
    (fromConsensusPointInMode mode)
    (fromConsensusBlock mode)
    (fromConsensusTip mode)

convLocalTxSubmissionClient
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> LocalTxSubmissionClient (TxInMode mode) (TxValidationErrorInMode mode) m a
  -> LocalTxSubmissionClient (Consensus.GenTx block)
                             (Consensus.ApplyTxErr block) m a
convLocalTxSubmissionClient mode =
    Net.Tx.mapLocalTxSubmissionClient
      toConsensusGenTx
      (fromConsensusApplyTxErr mode)


convLocalStateQueryClient
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) m a
  -> LocalStateQueryClient block (Consensus.Point block)
                           (Consensus.Query block) m a
convLocalStateQueryClient mode =
    Net.Query.mapLocalStateQueryClient
      (toConsensusPointInMode mode)
      toConsensusQuery
      fromConsensusQueryResult


--TODO: Move to consensus
mapLocalTxMonitoringClient
  :: forall txid txid' tx tx' m a. Functor m
  => (txid -> txid')
  -> (tx'-> tx)
  -> LocalTxMonitorClient txid tx SlotNo m a
  -> LocalTxMonitorClient txid' tx' SlotNo m a
mapLocalTxMonitoringClient convTxid convTx ltxmc =
  let LocalTxMonitorClient idleEff = ltxmc
  in LocalTxMonitorClient (fmap convClientStateIdle idleEff)
 where
   convClientStateIdle
     :: CTxMon.ClientStIdle txid  tx  SlotNo m a
     -> CTxMon.ClientStIdle txid' tx' SlotNo m a
   convClientStateIdle (CTxMon.SendMsgAcquire f) =
     CTxMon.SendMsgAcquire $ (fmap . fmap) convClientStateAcquired f
   convClientStateIdle (CTxMon.SendMsgDone a) = CTxMon.SendMsgDone a

   convClientStateAcquired
     :: CTxMon.ClientStAcquired txid  tx  SlotNo m a
     -> CTxMon.ClientStAcquired txid' tx' SlotNo m a
   convClientStateAcquired (CTxMon.SendMsgNextTx f) =
     CTxMon.SendMsgNextTx (\mTx -> convClientStateAcquired <$> f (convTx <$> mTx))
   convClientStateAcquired (CTxMon.SendMsgHasTx txid f)=
     CTxMon.SendMsgHasTx (convTxid txid) ((fmap . fmap) convClientStateAcquired f)
   convClientStateAcquired (CTxMon.SendMsgGetSizes f) =
     CTxMon.SendMsgGetSizes $ (fmap . fmap) convClientStateAcquired f
   convClientStateAcquired (CTxMon.SendMsgAwaitAcquire f) =
     CTxMon.SendMsgAwaitAcquire $ (fmap . fmap ) convClientStateAcquired f
   convClientStateAcquired (CTxMon.SendMsgRelease eff) =
     CTxMon.SendMsgRelease (convClientStateIdle <$> eff)

-- ----------------------------------------------------------------------------
-- Wrappers for specific protocol use-cases
--

--TODO: change this query to be just a protocol client handler to be used with
-- connectToLocalNode. This would involve changing connectToLocalNode to be
-- able to return protocol handler results properly.

-- | Establish a connection to a node and execute a single query using the
-- local state query protocol.
--
queryNodeLocalState :: forall mode fp result.
                       LocalNodeConnectInfo mode
                    -> Maybe ChainPoint
                    -> QueryInMode mode fp result
                    -> IO (Either Net.Query.AcquireFailure result)
queryNodeLocalState connctInfo mpoint query = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      LocalNodeClientProtocols {
        localChainSyncClient    = NoLocalChainSyncClient,
        localStateQueryClient   = Just (singleQuery mpoint resultVar),
        localTxSubmissionClient = Nothing,
        localTxMonitoringClient = Nothing
      }
    atomically (takeTMVar resultVar)
  where
    singleQuery
      :: Maybe ChainPoint
      -> TMVar (Either Net.Query.AcquireFailure result)
      -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint
                                         (QueryInMode mode) IO ()
    singleQuery mPointVar' resultVar' =
      LocalStateQueryClient $ do
      pure $
        Net.Query.SendMsgAcquire mPointVar' $
        Net.Query.ClientStAcquiring
          { Net.Query.recvMsgAcquired =
              pure $ Net.Query.SendMsgQuery query $
                Net.Query.ClientStQuerying
                  { Net.Query.recvMsgResult = \result -> do
                    atomically $ putTMVar resultVar' (Right result)

                    pure $ Net.Query.SendMsgRelease $
                      pure $ Net.Query.SendMsgDone ()
                  }
          , Net.Query.recvMsgFailure = \failure -> do
              atomically $ putTMVar resultVar' (Left failure)
              pure $ Net.Query.SendMsgDone ()
          }

submitTxToNodeLocal :: forall mode.
                       LocalNodeConnectInfo mode
                    -> TxInMode mode
                    -> IO (Net.Tx.SubmitResult (TxValidationErrorInMode mode))
submitTxToNodeLocal connctInfo tx = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      LocalNodeClientProtocols {
        localChainSyncClient    = NoLocalChainSyncClient,
        localTxSubmissionClient = Just (localTxSubmissionClientSingle resultVar),
        localStateQueryClient   = Nothing,
        localTxMonitoringClient = Nothing
      }
    atomically (takeTMVar resultVar)
  where
    localTxSubmissionClientSingle
      :: TMVar (Net.Tx.SubmitResult (TxValidationErrorInMode mode))
      -> Net.Tx.LocalTxSubmissionClient (TxInMode mode)
                                        (TxValidationErrorInMode mode)
                                        IO ()
    localTxSubmissionClientSingle resultVar =
      LocalTxSubmissionClient $
        pure $ Net.Tx.SendMsgSubmitTx tx $ \result -> do
        atomically $ putTMVar resultVar result
        pure (Net.Tx.SendMsgDone ())


data LocalTxMonitoringResult mode
  = LocalTxMonitoringTxExists
      TxId
      SlotNo -- ^ Slot number at which the mempool snapshot was taken
  | LocalTxMonitoringTxDoesNotExist
      TxId
      SlotNo -- ^ Slot number at which the mempool snapshot was taken
  | LocalTxMonitoringNextTx
      (Maybe (TxInMode mode))
      SlotNo -- ^ Slot number at which the mempool snapshot was taken
  | LocalTxMonitoringMempoolSizeAndCapacity
      Consensus.MempoolSizeAndCapacity
      SlotNo -- ^ Slot number at which the mempool snapshot was taken

data LocalTxMonitoringQuery mode
  -- | Query if a particular tx exists in the mempool. Note that, the absence
  -- of a transaction does not imply anything about how the transaction was
  -- processed: it may have been dropped, or inserted in a block.
  = LocalTxMonitoringQueryTx (TxIdInMode mode)
  -- | The mempool is modeled as an ordered list of transactions and thus, can
  -- be traversed linearly. 'LocalTxMonitoringSendNextTx' requests the next transaction from the
  -- current list. This must be a transaction that was not previously sent to
  -- the client for this particular snapshot.
  | LocalTxMonitoringSendNextTx
  -- | Ask the server about the current mempool's capacity and sizes. This is
  -- fixed in a given snapshot.
  | LocalTxMonitoringMempoolInformation


queryTxMonitoringLocal
  :: forall mode. LocalNodeConnectInfo mode
  -> LocalTxMonitoringQuery mode
  -> IO (LocalTxMonitoringResult mode)
queryTxMonitoringLocal connectInfo localTxMonitoringQuery = do
  resultVar <- newEmptyTMVarIO

  let client = case localTxMonitoringQuery of
                 LocalTxMonitoringQueryTx txidInMode ->
                   localTxMonitorClientTxExists txidInMode resultVar
                 LocalTxMonitoringSendNextTx ->
                   localTxMonitorNextTx resultVar
                 LocalTxMonitoringMempoolInformation ->
                   localTxMonitorMempoolInfo resultVar

  connectToLocalNode
    connectInfo
    LocalNodeClientProtocols {
      localChainSyncClient    = NoLocalChainSyncClient,
      localTxSubmissionClient = Nothing,
      localStateQueryClient   = Nothing,
      localTxMonitoringClient = Just client
    }
  atomically (takeTMVar resultVar)
 where
  localTxMonitorClientTxExists
    :: TxIdInMode mode
    -> TMVar (LocalTxMonitoringResult mode)
    -> LocalTxMonitorClient (TxIdInMode mode) (TxInMode mode) SlotNo IO ()
  localTxMonitorClientTxExists tIdInMode@(TxIdInMode txid _) resultVar = do
    LocalTxMonitorClient $ return $
      CTxMon.SendMsgAcquire $ \slt -> do
         return $ CTxMon.SendMsgHasTx tIdInMode $ \txPresentBool -> do
           if txPresentBool
           then atomically . putTMVar resultVar $ LocalTxMonitoringTxExists txid slt
           else atomically . putTMVar resultVar $ LocalTxMonitoringTxDoesNotExist txid slt
           return $ CTxMon.SendMsgRelease $ return $ CTxMon.SendMsgDone ()

  localTxMonitorNextTx
    :: TMVar (LocalTxMonitoringResult mode)
    -> LocalTxMonitorClient (TxIdInMode mode) (TxInMode mode) SlotNo IO ()
  localTxMonitorNextTx resultVar =
    LocalTxMonitorClient $ return $ do
      CTxMon.SendMsgAcquire $ \slt -> do
        return $ CTxMon.SendMsgNextTx $ \mTx -> do
          atomically $ putTMVar resultVar $ LocalTxMonitoringNextTx mTx slt
          return $ CTxMon.SendMsgRelease $ return $ CTxMon.SendMsgDone ()

  localTxMonitorMempoolInfo
    :: TMVar (LocalTxMonitoringResult mode)
    -> LocalTxMonitorClient (TxIdInMode mode) (TxInMode mode) SlotNo IO ()
  localTxMonitorMempoolInfo resultVar =
     LocalTxMonitorClient $ return $ do
      CTxMon.SendMsgAcquire $ \slt -> do
        return$ CTxMon.SendMsgGetSizes $ \mempoolCapacity -> do
          atomically $ putTMVar resultVar $ LocalTxMonitoringMempoolSizeAndCapacity mempoolCapacity slt
          return $ CTxMon.SendMsgRelease $ return $ CTxMon.SendMsgDone ()

-- ----------------------------------------------------------------------------
-- Get tip as 'ChainPoint'
--

getLocalChainTip :: LocalNodeConnectInfo mode -> IO ChainTip
getLocalChainTip localNodeConInfo = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      localNodeConInfo
      LocalNodeClientProtocols
        { localChainSyncClient = LocalChainSyncClient $ chainSyncGetCurrentTip resultVar
        , localTxSubmissionClient = Nothing
        , localStateQueryClient = Nothing
        , localTxMonitoringClient = Nothing
        }
    atomically $ takeTMVar resultVar

chainSyncGetCurrentTip
  :: forall mode. TMVar ChainTip
  -> ChainSyncClient (BlockInMode mode) ChainPoint ChainTip IO ()
chainSyncGetCurrentTip tipVar =
  ChainSyncClient $ pure clientStIdle
 where
  clientStIdle :: Net.Sync.ClientStIdle (BlockInMode mode) ChainPoint ChainTip IO ()
  clientStIdle =
    Net.Sync.SendMsgRequestNext clientStNext (pure clientStNext)

  clientStNext :: Net.Sync.ClientStNext (BlockInMode mode) ChainPoint ChainTip IO ()
  clientStNext = Net.Sync.ClientStNext
    { Net.Sync.recvMsgRollForward = \_block tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ Net.Sync.SendMsgDone ()
    , Net.Sync.recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ Net.Sync.SendMsgDone ()
    }
