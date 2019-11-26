{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Chairman (runChairman) where

import           Cardano.Prelude hiding (ByteString, STM, atomically, catch, option, show)
import           Prelude (String, error, foldl1, show)

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad (void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)
import           Data.Typeable (Typeable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Network.Mux.Types (MuxError)

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Condense

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.Cbor
import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Block (BlockNo, HasHeader, HeaderHash, Point, Tip)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client hiding (SendMsgDone)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.NodeToClient

import           Cardano.Common.LocalSocket
import           Cardano.Tracing.Tracers (TraceConstraints)

-- | Run chairman: connect with all the core nodes.  Chairman will store the
-- forks from a common prefix.  If any of them is longer than the security
-- parameter it will throw an exception.
--
-- It is also possible to specify how many blocks should be validated.
--
runChairman :: forall blk.
               ( RunNode blk
               , TraceConstraints blk
               )
            => Protocol blk
            -> [CoreNodeId]
            -> SecurityParam
            -- ^ security parameter, if a fork is deeper than it 'runChairman'
            -- will throw an exception.
            -> Maybe BlockNo
            -- ^ finish after that many blocks, if 'Nothing' run continuously.
            -> FilePath
            -- ^ local socket dir
            -> Tracer IO String
            -> IO ()
runChairman ptcl nids securityParam maxBlockNo socketDir tracer = do

    (chainsVar :: ChainsVar IO blk) <- newTVarM
      (Map.fromList $ map (\coreNodeId -> (coreNodeId, AF.Empty Block.GenesisPoint)) nids)

    void $ flip mapConcurrently nids $ \coreNodeId ->
        let ProtocolInfo{pInfoConfig} = protocolInfo ptcl

        in createConnection
             coreNodeId
             chainsVar
             securityParam
             maxBlockNo
             tracer
             pInfoConfig
             socketDir

-- catch 'MuxError'; it will be thrown if a node shuts down closing the
-- connection.
handleMuxError
  :: Tracer IO String
  -> ChainsVar IO blk
  -> CoreNodeId
  -> MuxError
  -> IO ()
handleMuxError tracer chainsVar coreNodeId err = do
  traceWith tracer (show err)
  atomically $ modifyTVar chainsVar (Map.delete coreNodeId)

createConnection
  :: forall blk.
     ( RunNode blk
     , Condense blk
     , Condense (HeaderHash blk))
  => CoreNodeId
  -> ChainsVar IO blk
  -> SecurityParam
  -> Maybe BlockNo
  -> Tracer IO String
  -> NodeConfig (BlockProtocol blk)
  -> FilePath
  -> IO ()
createConnection
  coreNodeId
  chainsVar
  securityParam
  maxBlockNo
  tracer
  pInfoConfig
  socketDir = do
    addr <- localSocketAddrInfo (Just $ fromCoreNodeId coreNodeId) socketDir NoMkdirIfMissing
    connectTo
      nullTracer
      nullTracer
      (,)
      (localInitiatorNetworkApplication
        coreNodeId
        chainsVar
        securityParam
        maxBlockNo
        (showTracing tracer)
        nullTracer
        nullTracer
        pInfoConfig)
      Nothing
      addr
    `catch` handleMuxError tracer chainsVar coreNodeId


data ChairmanTrace blk
  = NotFoundCommonBlock [Point blk]
  -- ^ the common block was not found, present list of head points.  Might be
  -- empty if the chain was empty.
  | WitnessedConsensusAt (Point blk) [Point blk]
  -- ^ witness consensus at a given point.  The list is a list of tip points of
  -- each chain.

instance (Condense blk, Condense (HeaderHash blk)) => Show (ChairmanTrace blk) where
    show (NotFoundCommonBlock tips)
      = "not found common block" ++ condense tips
    show (WitnessedConsensusAt p tips)
      = mconcat
      [ "witnessed consensus at "
      , condense p
      , " current tips: "
      , condense tips
      ]


--
-- Shared State, and its API.
--


-- | Shared state between chain-sync clients.  Each chain-sync client will write to the
-- corresponding entry.
--
type ChainsVar m blk = StrictTVar m (Map CoreNodeId (AnchoredFragment blk))


-- | Add a single block to the chain.
--
addBlock
    :: forall blk m.
       ( MonadSTM m
       , HasHeader blk
       )
    => CoreNodeId
    -> ChainsVar m blk
    -> blk
    -> STM m ()
addBlock coreNodeId chainsVar blk =
    modifyTVar chainsVar (Map.adjust (AF.addBlock blk) coreNodeId)


data ChairmanError blk =
    NodeMisconduct [Point blk]
    -- ^ Nodes did not agree on a chain: we witnessed a fork longer than
    -- 'SecurityParam'.  The given points are tips of the chains when the fork
    -- was encountered.

instance (Condense blk, Condense (HeaderHash blk))
    => Show (ChairmanError blk) where
    show (NodeMisconduct blks) = "NodeMisconduct " ++ condense blks

instance ( Condense blk
         , Condense (HeaderHash blk)
         , Typeable blk
         ) => Exception (ChairmanError blk)


-- | Check if the oldest immutable tip agree, keep forks from the last common
-- block (including it).
--
checkAndPrune
    :: forall blk m.
       ( MonadSTM m
       , MonadThrow (STM m)
       , HasHeader blk
       , Condense blk
       , Condense (HeaderHash blk)
       )
    => ChainsVar m blk
    -> SecurityParam
    -> STM m (ChairmanTrace blk)
checkAndPrune chainsVar (SecurityParam securityParam) = do
    chains <- readTVar chainsVar
    case checkAndPrunePure chains of
      Left err -> throwM err
      Right (res, potentialForks) -> do
        writeTVar chainsVar $! potentialForks
        return res
  where
    checkAndPrunePure :: Map CoreNodeId (AnchoredFragment blk)
                 -> Either (ChairmanError blk)
                           ( ChairmanTrace blk
                           , Map CoreNodeId (AnchoredFragment blk)
                           )
    checkAndPrunePure chains =
      let tips :: [Point blk]
          tips = map AF.headPoint $ Map.elems chains

          -- Find intersection of all the chains; the CLI guarantees that
          -- there's at least one entry in @chains@, thus @fold1@ is safe here.
          common :: AnchoredFragment blk
          common =
            foldl1
              (\common' cf ->
                case AF.intersect common' cf of
                  Nothing                  -> AF.Empty Block.GenesisPoint
                  Just (common'', _, _, _) -> common'')
              chains
          -- Oldest common intersection point
          headPoint = AF.headPoint common

          -- Remove common chain fragment from all the chains
          -- to give potential forks.
          !potentialForks  =
            -- When the node has just started the common point will be the genesis point
            if headPoint == Block.GenesisPoint
            then chains
            else (\af -> case AF.intersect af (AF.Empty headPoint) of
                   Just (_, _, af', _) -> af'
                   -- we know that 'headPoint' is on each chain and thus this
                   -- case is impossible
                   Nothing             -> error "impossible happend"
                 )
             <$> chains

      in if maximum (AF.length <$> potentialForks) <= fromIntegral securityParam
        then if headPoint == Block.GenesisPoint
          then -- There is no intersection and forks are short;  This
               -- might happen when starting and the nodes have not yet
               -- found consensus.
               Right
                 ( NotFoundCommonBlock tips
                 , potentialForks
                 )
          else
               -- There is an intersection and all forks are shorter
               -- than security parameter k.
               Right ( WitnessedConsensusAt headPoint tips
                     , potentialForks
                     )
            -- There is a long fork i.e a fork longer than security parameter k.
        else Left (NodeMisconduct tips)

-- | Rollback a single block.  If the rollback point is not found, we simply
-- error.  It should never happen if the security parameter is set up correctly.
--
rollback
    :: forall blk m.
       ( MonadSTM m
       , HasHeader blk
       )
    => CoreNodeId
    -> ChainsVar m blk
    -> Point blk
    -> STM m ()
rollback coreNodeId chainsVar p =
    modifyTVar chainsVar (Map.adjust fn coreNodeId)
  where
    fn :: AnchoredFragment blk -> AnchoredFragment blk
    fn cf = case AF.rollback p cf of
      Nothing  -> AF.Empty Block.GenesisPoint
      Just cf' -> cf'


--
-- Chain-Sync client
--

-- | 'ChainSyncClient' which build chain fragment; on every roll forward it will
-- check if there is consensus on immutable chain.
--
chainSyncClient
  :: forall blk m.
     ( MonadSTM   m
     , MonadThrow (STM m)
     , MonadAsync m
     , HasHeader blk
     , Condense blk
     , Condense (HeaderHash blk)
     )
  => Tracer m (ChairmanTrace blk)
  -> CoreNodeId
  -> ChainsVar m blk
  -> SecurityParam
  -> Maybe BlockNo
  -> ChainSyncClient blk (Tip blk) m ()
chainSyncClient tracer coreNodeId chainsVar securityParam maxBlockNo = ChainSyncClient $ pure $
    -- Notify the core node about the our latest points at which we are
    -- synchronised.  This client is not persistent and thus it just
    -- synchronises from the genesis block.  A real implementation should send
    -- a list of points up to a point which is k blocks deep.
    SendMsgFindIntersect
      [Block.genesisPoint]
      ClientStIntersect {
        recvMsgIntersectFound    = \_ _ -> ChainSyncClient (pure $ clientStIdle Nothing),
        recvMsgIntersectNotFound = \  _ -> ChainSyncClient (pure $ clientStIdle Nothing)
      }
  where
    clientStIdle :: Maybe BlockNo
                 -- current point
                 -> ClientStIdle blk (Tip blk) m ()
    clientStIdle currentBlockNo =
      case (currentBlockNo, maxBlockNo) of
        (Just n, Just m) | n >= m
                         -> SendMsgDone ()
        _                -> SendMsgRequestNext clientStNext (pure clientStNext)

    clientStNext :: ClientStNext blk (Tip blk) m ()
    clientStNext = ClientStNext {
        recvMsgRollForward = \blk _tip -> ChainSyncClient $ do
          -- add block & check if there is consensus on immutable chain
          -- trace the decision or error
          res <- atomically $ do
            addBlock coreNodeId chainsVar blk
            checkAndPrune chainsVar securityParam
          traceWith tracer res
          let currentBlockNo = Just (Block.blockNo blk)
          pure $ clientStIdle currentBlockNo
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- rollback & check
          res <- atomically $ do
            rollback coreNodeId chainsVar point
            checkAndPrune chainsVar securityParam
          traceWith tracer res
          pure $ clientStIdle Nothing
      }

--
-- Client Application
--

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , Condense blk
     , Condense (HeaderHash blk)
     , MonadAsync m
     , MonadST    m
     , MonadTimer m
     , MonadThrow m
     , MonadThrow (STM m)
     )
  => CoreNodeId
  -> ChainsVar m blk
  -> SecurityParam
  -> Maybe BlockNo
  -> Tracer m (ChairmanTrace blk)
  -> Tracer m (TraceSendRecv (ChainSync blk (Tip blk)) peer DeserialiseFailure)
  -- ^ tracer which logs all chain-sync messages send and received by the client
  -- (see 'Ouroboros.Network.Protocol.ChainSync.Type' in 'ouroboros-network'
  -- package)
  -> Tracer m (TraceSendRecv (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)) peer DeserialiseFailure)
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> NodeConfig (BlockProtocol blk)
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication 'InitiatorApp peer NodeToClientProtocols
                                    m ByteString () Void)
localInitiatorNetworkApplication coreNodeId chainsVar securityParam maxBlockNo chairmanTracer chainSyncTracer localTxSubmissionTracer pInfoConfig =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData (nodeNetworkMagic (Proxy @blk) pInfoConfig))
      (DictVersion nodeToClientCodecCBORTerm)

  $ OuroborosInitiatorApplication $ \peer ptcl -> case ptcl of
      LocalTxSubmissionPtcl -> \channel -> do
        runPeer
          localTxSubmissionTracer
          localTxSubmissionCodec
          peer
          channel
          (localTxSubmissionClientPeer localTxSubmissionClientNull)

      ChainSyncWithBlocksPtcl -> \channel ->
        runPeer
          chainSyncTracer
          (localChainSyncCodec pInfoConfig)
          peer
          channel
          (chainSyncClientPeer $ chainSyncClient chairmanTracer coreNodeId chainsVar securityParam maxBlockNo)


--
-- Codecs
--

localTxSubmissionCodec
  :: forall m blk . (RunNode blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))
           DeserialiseFailure m ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    nodeEncodeGenTx
    nodeDecodeGenTx
    (nodeEncodeApplyTxError (Proxy @blk))
    (nodeDecodeApplyTxError (Proxy @blk))

localChainSyncCodec
  :: forall blk m.
     ( RunNode blk
     , MonadST    m
     )
  => NodeConfig (BlockProtocol blk)
  -> Codec (ChainSync blk (Tip blk))
           DeserialiseFailure m ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (nodeEncodeBlock pInfoConfig)
      (nodeDecodeBlock pInfoConfig)
      (Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
      (Block.encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))
