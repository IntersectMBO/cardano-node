{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Network.Mux (MuxError, MuxMode(..))

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol(..))
import           Ouroboros.Consensus.Block (BlockProtocol, Header, CodecConfig, GetHeader (..))
import           Ouroboros.Consensus.BlockchainTime (SlotLength, getSlotLength)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (HasNetworkProtocolVersion (..),
                   supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)

import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Block (BlockNo, HasHeader, Point, Tip)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (WithOrigin(..), fromWithOrigin)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment, Anchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.ChainSync.Client
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
chairmanTest :: Tracer IO String
             -> SlotLength
             -> SecurityParam
             -> DiffTime
             -> Maybe BlockNo
             -> [SocketPath]
             -> SomeNodeClientProtocol
             -> NetworkMagic
             -> IO ()
chairmanTest tracer slotLength securityParam runningTime optionalProgressThreshold socketPaths someNodeClientProtocol nw = do

    traceWith tracer ("Will observe nodes for " ++ show runningTime)
    traceWith tracer ("Will require chain growth of " ++ show progressThreshold)

    SomeNodeClientProtocol (p :: ProtocolClient blk (BlockProtocol blk)) <- return $ someNodeClientProtocol

    -- Run the chairman and get the final snapshot of the chain from each node.
    chainsSnapshot <- runChairman
                        tracer
                        (pClientInfoCodecConfig $ protocolClientInfo p)
                        nw
                        securityParam
                        runningTime
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
    progressThreshold = deriveProgressThreshold
                          slotLength
                          runningTime
                          optionalProgressThreshold

-- | The caller specifies how long to run the chairman for and optionally a
-- chain growth progress threshold. If the threshold is not given, we can
-- derive a reasonable default from the running time and slot length.
--
deriveProgressThreshold :: SlotLength
                        -> DiffTime
                        -> Maybe BlockNo
                        -> BlockNo
deriveProgressThreshold _ _ (Just progressThreshold) = progressThreshold

-- If only the progress threshold is not specified, derive it from the running time
deriveProgressThreshold slotLength runningTime Nothing =
    Block.BlockNo (floor (runningTime / getSlotLengthDiffTime slotLength) - 2)


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
            -> CodecConfig blk
            -> NetworkMagic
            -> SecurityParam
            -- ^ security parameter, if a fork is deeper than it 'runChairman'
            -- will throw an exception.
            -> DiffTime
            -- ^ Run for this much time.
            -> [SocketPath]
            -- ^ local socket dir
            -> IO (ChainsSnapshot blk)
runChairman tracer cfg networkMagic securityParam runningTime socketPaths = do

    let initialChains = Map.fromList [ (socketPath, AF.Empty AF.AnchorGenesis)
                                     | socketPath <- socketPaths]
    chainsVar <- newTVarM initialChains

    void $ timeout runningTime $
      withIOManager $ \iomgr ->
        forConcurrently_ socketPaths $ \sockPath ->
          createConnection
            tracer
            iomgr
            cfg
            networkMagic
            sockPath
            chainsVar
            securityParam

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
  -> IOManager
  -> CodecConfig blk
  -> NetworkMagic
  -> SocketPath
  -> ChainsVar IO blk
  -> SecurityParam
  -> IO ()
createConnection
  tracer
  iomgr
  cfg
  networkMagic
  socketPath@(SocketPath path)
  chainsVar
  securityParam =
      connectTo
        (localSnocket iomgr path)
        NetworkConnectTracers {
            nctMuxTracer       = nullTracer,
            nctHandshakeTracer = nullTracer
            }
        (localInitiatorNetworkApplication
            (showTracing tracer)
            nullTracer
            nullTracer
            cfg
            networkMagic
            socketPath
            chainsVar
            securityParam)
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
     )
  => Tracer m (ChairmanTrace blk)
  -> SocketPath
  -> ChainsVar m blk
  -> SecurityParam
  -> ChainSyncClient blk (Tip blk) m ()
chainSyncClient tracer sockPath chainsVar securityParam = ChainSyncClient $ pure $
    -- Notify the core node about the our latest points at which we are
    -- synchronised.  This client is not persistent and thus it just
    -- synchronises from the genesis block.  A real implementation should send
    -- a list of points up to a point which is k blocks deep.
    SendMsgFindIntersect
      [Block.genesisPoint]
      ClientStIntersect {
        recvMsgIntersectFound    = \_ _ -> ChainSyncClient (pure clientStIdle),
        recvMsgIntersectNotFound = \  _ -> ChainSyncClient (pure clientStIdle)
      }
  where
    clientStIdle :: ClientStIdle blk (Tip blk) m ()
    clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

    clientStNext :: ClientStNext blk (Tip blk) m ()
    clientStNext = ClientStNext {
        recvMsgRollForward = \blk _tip -> ChainSyncClient $ do
          -- add block & check if there is consensus on immutable chain
          -- trace the decision or error
          res <- atomically $ do
            addBlock sockPath chainsVar blk
            checkConsensus chainsVar securityParam
          traceWith tracer res
          pure clientStIdle
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- rollback & check
          res <- atomically $ do
            rollback sockPath chainsVar point
            checkConsensus chainsVar securityParam
          traceWith tracer res
          pure clientStIdle
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
  :: forall blk m.
     ( RunNode blk
     , MonadAsync m
     , MonadST    m
     , MonadTimer m
     , MonadThrow (STM m)
     )
  => Tracer m (ChairmanTrace blk)
  -> Tracer m (TraceSendRecv (ChainSync blk (Tip blk)))
  -- ^ tracer which logs all chain-sync messages send and received by the client
  -- (see 'Ouroboros.Network.Protocol.ChainSync.Type' in 'ouroboros-network'
  -- package)
  -> Tracer m (TraceSendRecv (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -- ^ tracer which logs all local tx submission protocol messages send and
  -- received by the client (see 'Ouroboros.Network.Protocol.LocalTxSubmission.Type'
  -- in 'ouroboros-network' package).
  -> CodecConfig blk
  -> NetworkMagic
  -> SocketPath
  -> ChainsVar m blk
  -> SecurityParam
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication InitiatorMode LocalAddress ByteString m () Void)
localInitiatorNetworkApplication chairmanTracer chainSyncTracer
                                 localTxSubmissionTracer
                                 cfg networkMagic
                                 sockPath chainsVar securityParam =
    foldMapVersions
      (\(version, blockVersion) ->
        versionedNodeToClientProtocols
          version
          versionData
          (\_ _ -> protocols blockVersion))
      (Map.toList (supportedNodeToClientVersions proxy))
  where
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NodeToClientVersionData networkMagic

    protocols :: BlockNodeToClientVersion blk
              -> NodeToClientProtocols InitiatorMode ByteString m () Void
    protocols byronClientVersion  =
        NodeToClientProtocols {
          localChainSyncProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                chainSyncTracer
                cChainSyncCodec
                (chainSyncClientPeer $
                   chainSyncClient chairmanTracer sockPath chainsVar securityParam)

        , localTxSubmissionProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                localTxSubmissionTracer
                cTxSubmissionCodec
                localTxSubmissionPeerNull
        , localStateQueryProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cStateQueryCodec
                localStateQueryPeerNull
        }
      where
        Codecs { cChainSyncCodec
               , cTxSubmissionCodec
               , cStateQueryCodec
               } = clientCodecs cfg byronClientVersion
