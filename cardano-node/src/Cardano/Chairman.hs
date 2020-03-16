{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Chairman (chairmanTest) where

import           Cardano.Prelude hiding (ByteString, STM, atomically, catch, option, show)
import           Prelude (String, error, show)

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad (void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Network.Mux (MuxError)

import           Ouroboros.Consensus.Block (BlockProtocol, GetHeader (..))
import           Ouroboros.Consensus.BlockchainTime.SlotLength
import           Ouroboros.Consensus.BlockchainTime.SlotLengths
import           Ouroboros.Consensus.Config
                   ( TopLevelConfig, configConsensus, configBlock )
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.LedgerDerivedInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Cardano

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Block (BlockNo, HasHeader, Point, Tip)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (WithOrigin(..), fromWithOrigin)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment, Anchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client hiding (SendMsgDone)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.NodeToClient

import           Cardano.Config.Types (SocketPath(..))


-- | The chairman checks for consensus and progress.
--
-- The chairman test is an integration test. It monitors a set of nodes and
-- checks that all the nodes agree on the chain, within a margin. It also
-- checks that enough blocks have been made.
--
-- Specifically in this case consensus is defined as follows: for all pairs
-- of chains, the intersection of each pair is within K blocks of each tip.
-- Progress is defined simply as each chain being at least of a certain length.
--
-- The consensus condition is checked incrementally as well as at the end, so
-- that failures can be detected as early as possible. The progress condition
-- is only checked at the end.
--
-- It is also possible to specify how many blocks should be validated.
--
chairmanTest :: RunNode blk
             => Tracer IO String
             -> Protocol blk (BlockProtocol blk)
             -> Maybe BlockNo
             -> Maybe DiffTime
             -> [SocketPath]
             -> IO ()
chairmanTest tracer ptcl maxBlockNo maxRunTime socketPaths = do

    -- The chairman runs until we hit the max block no, or max run time,
    -- or if both are @Nothing@ it runs indefinately, and the remaining
    -- checks are never performed.
    traceWith tracer ("Will run until block number " ++ show maxBlockNo')
    traceWith tracer ("Will run until timeout "      ++ show maxRunTime')

    -- Run the chairman and get the final snapshot of the chain from each node.
    chainsSnapshot <- runChairman
                        tracer
                        cfg securityParam
                        maxBlockNo' maxRunTime'
                        socketPaths

    traceWith tracer ("================== chairman results ==================")

    -- Test if we achieved consensus
    consensusSuccess <- either throwIO return $
                          consensusCondition securityParam chainsSnapshot

    traceWith tracer (show consensusSuccess)

    -- Test if we made adequate progress
    progressSuccess <- either throwIO return $
                         progressCondition progressThreshold consensusSuccess

    traceWith tracer (show progressSuccess)
    traceWith tracer ("================== chairman results ==================")

  where
    ProtocolInfo { pInfoConfig = cfg } = protocolInfo ptcl
    securityParam = protocolSecurityParam (configConsensus cfg)
    slotLength    = currentSlotLength (knownSlotLengths (configBlock cfg))

    (maxBlockNo', maxRunTime') = deriveRunLimits slotLength (maxBlockNo, maxRunTime)

    progressThreshold = fromMaybe 0 maxBlockNo'


-- | We can specify how long to run the chairman either by specifying the
-- number of blocks to run for, or the time, or both, or neither. If only
-- one is given, the other is derived.
deriveRunLimits :: SlotLength
                -> (Maybe BlockNo, Maybe DiffTime)
                -> (Maybe BlockNo, Maybe DiffTime)

-- If only the block number is specified, derive the timeout
deriveRunLimits slotLength (Just maxBlockNo, Nothing) =
    (Just maxBlockNo, Just maxRunTime)
  where
    maxRunTime = fromIntegral (Block.unBlockNo maxBlockNo)
               * getSlotLengthDiffTime slotLength

-- If only the timeout is specified, derive the block number
deriveRunLimits slotLength (Nothing, Just maxRunTime) =
    (Just maxBlockNo, Just maxRunTime)
  where
    maxBlockNo = Block.BlockNo $
                   floor (maxRunTime / getSlotLengthDiffTime slotLength)

-- Otherwise if both or none specified, leave them
deriveRunLimits _ x = x

getSlotLengthDiffTime :: SlotLength -> DiffTime
getSlotLengthDiffTime = realToFrac . getSlotLength

type ChainsSnapshot blk = Map PeerId (AnchoredFragment (Header blk))

type PeerId = SocketPath

data ConsensusSuccess blk =
     ConsensusSuccess
       -- Minimum of the maximum intersection points
       (Anchor (Header blk))
       -- Chain tip for each chain
       [(PeerId, Tip (Header blk))]
  deriving Show

data ConsensusFailure blk =
     ConsensusFailure
       -- Tip of two peer's chains that do not intersect within K blocks
       (PeerId, Tip (Header blk))
       (PeerId, Tip (Header blk))
       -- The tntersection point of two chains
       (Anchor (Header blk))
       SecurityParam
  deriving Show

instance HasHeader blk => Exception (ConsensusFailure blk) where
  displayException (ConsensusFailure (peerid1, tip1)
                                     (peerid2, tip2)
                                     intersection
                                     (SecurityParam securityParam)) =
    concat
      [ "consensus failure:\n"
      , "node at ", show peerid1, " has chain tip ", show tip1, "\n"
      , "node at ", show peerid2, " has chain tip ", show tip2, "\n"
      , "but their chain intersection is at ", show intersection, "\n"
      , "which is further back than the security param K ", show securityParam
      ]

-- | For this test we define consensus as follows: for all pairs of chains,
-- the intersection of each pair is within K blocks of each tip.
--
consensusCondition :: HasHeader (Header blk)
                   => SecurityParam
                   -> ChainsSnapshot blk
                   -> Either (ConsensusFailure blk)
                             (ConsensusSuccess blk)
consensusCondition (SecurityParam securityParam) chains =
    -- The (forkTooLong . chainForkPoints) predicate is not transitive.
    -- As a consequence, we need to check it between all the pairs of chains:
    let forks = [ ((peerid1, peerid2), chainForkPoints chain1 chain2)
                | (peerid1, chain1) <- Map.toList chains
                , (peerid2, chain2) <- Map.toList chains
                ]
     in case find (forkTooLong . snd) forks of
          Just ((peerid1, peerid2), (intersection, tip1, tip2)) ->
            Left $
              ConsensusFailure
                (peerid1, AF.anchorToTip tip1)
                (peerid2, AF.anchorToTip tip2)
                intersection
                (SecurityParam securityParam)
          Nothing ->
            Right $
              ConsensusSuccess
                -- the minimum intersection point:
                (minimumBy (comparing AF.anchorToBlockNo)
                           [ intersection | (_,(intersection,_,_)) <- forks ])
                -- all the chain tips:
                [ (peerid, AF.anchorToTip (AF.headAnchor chain))
                | (peerid, chain) <- Map.toList chains ]
  where
    chainForkPoints :: HasHeader (Header blk)
                    => AnchoredFragment (Header blk)
                    -> AnchoredFragment (Header blk)
                    -> (Anchor (Header blk), -- intersection
                        Anchor (Header blk), -- tip of c1
                        Anchor (Header blk)) -- tip of c2
    chainForkPoints chain1 chain2 =
      case AF.intersect chain1 chain2 of
        -- chains are anochored at the genesis, so their intersection is never
        -- empty
        Nothing -> error "chainChains: invariant violation"

        Just (_, _, extension1, extension2) ->
          (AF.anchor     extension1,
           AF.headAnchor extension1,
           AF.headAnchor extension2)

    forkTooLong :: (Anchor (Header blk), -- intersection
                    Anchor (Header blk), -- tip of chain1
                    Anchor (Header blk)) -- tip of chain2
                -> Bool
    forkTooLong (intersection, tip1, tip2) =
        -- If only one of len1, len2 is longer than the securityParam then it is
        -- still ok. That node can still recover by receiving a valid rollback
        -- instruction, but if both are longer, then we have a failure.
        forkLen tip1 > securityParam &&
        forkLen tip2 > securityParam
      where
        forkLen :: Anchor (Header blk) -> Word64
        forkLen tip =
          Block.unBlockNo $
            fromWithOrigin 0 (AF.anchorToBlockNo tip)
          - fromWithOrigin 0 (AF.anchorToBlockNo intersection)


data ProgressSuccess =
     ProgressSuccess
       BlockNo
  deriving Show

data ProgressFailure blk =
     ProgressFailure
       BlockNo -- minimum expected
       PeerId
       (Tip (Header blk))
  deriving Show

instance HasHeader blk => Exception (ProgressFailure blk) where
  displayException (ProgressFailure minBlockNo peerid tip) =
    concat
      [ "progress failure:\n"
      , "the node at ", show peerid, " has chain tip ", show tip, "\n"
      , "while the mininum expected block number is ", show minBlockNo
      ]

-- | Progress is defined as each chain being at least of a certain length.
--
progressCondition :: BlockNo
                  -> ConsensusSuccess blk
                  -> Either (ProgressFailure blk) ProgressSuccess
progressCondition minBlockNo (ConsensusSuccess _ tips) =
    case find (\(_,tip) -> Block.getTipBlockNo tip < At minBlockNo) tips of
      Just (peerid, tip) -> Left (ProgressFailure minBlockNo peerid tip)
      Nothing            -> Right (ProgressSuccess minBlockNo)


runChairman :: RunNode blk
            => Tracer IO String
            -> TopLevelConfig blk
            -> SecurityParam
            -- ^ security parameter, if a fork is deeper than it 'runChairman'
            -- will throw an exception.
            -> Maybe BlockNo
            -- ^ finish after this many blocks, if 'Nothing' run continuously.
            -> Maybe DiffTime
            -- ^ finish after this much time, if 'Nothing' run continuously.
            -> [SocketPath]
            -- ^ local socket dir
            -> IO (ChainsSnapshot blk)
runChairman tracer cfg securityParam
            maxBlockNo maxRunTime
            socketPaths = do

    let initialChains = Map.fromList [ (socketPath, AF.Empty AF.AnchorGenesis)
                                     | socketPath <- socketPaths]
    chainsVar <- newTVarM initialChains

    void $ timeout (fromMaybe (-1) maxRunTime) $
      withIOManager $ \iomgr ->
        forConcurrently_ socketPaths $ \sockPath ->
          createConnection
            tracer
            iomgr
            cfg
            securityParam
            maxBlockNo
            chainsVar
            sockPath

    atomically (readTVar chainsVar)

-- catch 'MuxError'; it will be thrown if a node shuts down closing the
-- connection.
handleMuxError
  :: Tracer IO String
  -> ChainsVar IO blk
  -> SocketPath
  -> MuxError
  -> IO ()
handleMuxError tracer chainsVar socketPath err = do
  traceWith tracer (show err)
  atomically $ modifyTVar chainsVar (Map.delete socketPath)

createConnection
  :: forall blk.
     RunNode blk
  => Tracer IO String
  -> AssociateWithIOCP
  -> TopLevelConfig blk
  -> SecurityParam
  -> Maybe BlockNo
  -> ChainsVar IO blk
  -> SocketPath
  -> IO ()
createConnection
  tracer
  iomgr
  cfg
  securityParam
  maxBlockNo
  chainsVar
  socketPath@(SocketFile path) =
      connectTo
        (localSnocket iomgr path)
        NetworkConnectTracers {
            nctMuxTracer       = nullTracer,
            nctHandshakeTracer = nullTracer
            }
        (localInitiatorNetworkApplication
            socketPath
            chainsVar
            securityParam
            maxBlockNo
            (showTracing tracer)
            nullTracer
            nullTracer
            cfg)
        path
        `catch` handleMuxError tracer chainsVar socketPath


--
-- Shared State, and its API.
--


-- | Shared state between chain-sync clients.  Each chain-sync client will write to the
-- corresponding entry.
--
type ChainsVar m blk = StrictTVar m (Map SocketPath (AnchoredFragment (Header blk)))


-- | Add a single block to the chain.
--
addBlock
    :: forall blk m.
       ( MonadSTM m
       , HasHeader (Header blk)
       , GetHeader blk
       )
    => SocketPath
    -> ChainsVar m blk
    -> blk
    -> STM m ()
addBlock sockPath chainsVar blk =
    modifyTVar chainsVar (Map.adjust (AF.addBlock (getHeader blk)) sockPath)


-- | Rollback a single block.  If the rollback point is not found, we simply
-- error.  It should never happen if the security parameter is set up correctly.
--
rollback
    :: forall blk m.
       ( MonadSTM m
       , HasHeader (Header blk)
       )
    => SocketPath
    -> ChainsVar m blk
    -> Point blk
    -> STM m ()
rollback sockPath chainsVar p =
    modifyTVar chainsVar (Map.adjust fn sockPath)
  where
    p' :: Point (Header blk)
    p' = coerce p

    fn :: AnchoredFragment (Header blk) -> AnchoredFragment (Header blk)
    fn cf = case AF.rollback p' cf of
      Nothing  -> error "rollback error: rollback beyond chain fragment"
      Just cf' -> cf'


--
-- Chain-Sync client
--

type ChairmanTrace blk = ConsensusSuccess blk

-- | 'ChainSyncClient' which build chain fragment; on every roll forward it will
-- check if there is consensus on immutable chain.
--
chainSyncClient
  :: forall blk m.
     ( MonadSTM   m
     , MonadThrow (STM m)
     , MonadAsync m
     , GetHeader blk
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => Tracer m (ChairmanTrace blk)
  -> SocketPath
  -> ChainsVar m blk
  -> SecurityParam
  -> Maybe BlockNo
  -> ChainSyncClient blk (Tip blk) m ()
chainSyncClient tracer sockPath chainsVar securityParam maxBlockNo = ChainSyncClient $ pure $
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
            addBlock sockPath chainsVar blk
            checkConsensus chainsVar securityParam
          traceWith tracer res
          let currentBlockNo = Just (Block.blockNo blk)
          pure $ clientStIdle currentBlockNo
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- rollback & check
          res <- atomically $ do
            rollback sockPath chainsVar point
            checkConsensus chainsVar securityParam
          traceWith tracer res
          pure $ clientStIdle Nothing
      }

-- | Check that all nodes agree with each other, within the security parameter.
--
checkConsensus
    :: forall blk m.
       ( MonadSTM m
       , MonadThrow (STM m)
       , HasHeader blk
       , HasHeader (Header blk)
       )
    => ChainsVar m blk
    -> SecurityParam
    -> STM m (ConsensusSuccess blk)
checkConsensus chainsVar securityParam = do
    chainsSnapshot <- readTVar chainsVar
    either throwM return $ consensusCondition securityParam chainsSnapshot


--
-- Client Application
--

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , MonadAsync m
     , MonadST    m
     , MonadTimer m
     , MonadThrow (STM m)
     )
  => SocketPath
  -> ChainsVar m blk
  -> SecurityParam
  -> Maybe BlockNo
  -> Tracer m (ChairmanTrace blk)
  -> Tracer m (TraceSendRecv (ChainSync blk (Tip blk)))
  -- ^ tracer which logs all chain-sync messages send and received by the client
  -- (see 'Ouroboros.Network.Protocol.ChainSync.Type' in 'ouroboros-network'
  -- package)
  -> Tracer m (TraceSendRecv (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> TopLevelConfig blk
  -> Versions NodeToClientVersion DictVersion
              (peer -> OuroborosApplication InitiatorApp ByteString m () Void)
localInitiatorNetworkApplication sockPath chainsVar securityParam maxBlockNo chairmanTracer chainSyncTracer localTxSubmissionTracer cfg =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData (nodeNetworkMagic (Proxy @blk) cfg))
      (DictVersion nodeToClientCodecCBORTerm) $ \_peerid ->

    nodeToClientProtocols $
      NodeToClientProtocols {
        localChainSyncProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              chainSyncTracer
              (localChainSyncCodec cfg)
              (chainSyncClientPeer $
                 chainSyncClient chairmanTracer sockPath chainsVar
                                 securityParam maxBlockNo)

      , localTxSubmissionProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              localTxSubmissionTracer
              localTxSubmissionCodec
              (localTxSubmissionClientPeer localTxSubmissionClientNull)
      }

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
  => TopLevelConfig blk
  -> Codec (ChainSync blk (Tip blk))
           DeserialiseFailure m ByteString
localChainSyncCodec cfg =
    codecChainSync
      (Block.wrapCBORinCBOR   (nodeEncodeBlock cfg))
      (Block.unwrapCBORinCBOR (nodeDecodeBlock cfg))
      (Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
      (Block.encodeTip   (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodeTip   (nodeDecodeHeaderHash (Proxy @blk)))
