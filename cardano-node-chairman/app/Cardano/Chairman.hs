{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Chairman (chairmanTest) where

import           Cardano.Prelude hiding (ByteString, STM, atomically, catch, option, show, throwIO)
import           Prelude (String, error, show)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SecurityParam

import           Ouroboros.Network.AnchoredFragment (Anchor, AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Protocol.ChainSync.Client

import           Cardano.Api
import           Cardano.Api.Byron
import           Cardano.Api.Shelley
import           Cardano.Node.Configuration.NodeAddress (SocketPath (..))

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
chairmanTest
  :: Tracer IO String
  -> NetworkId
  -> DiffTime
  -> BlockNo
  -> [SocketPath]
  -> AnyConsensusModeParams
  -> SecurityParam
  -> IO ()
chairmanTest tracer nw runningTime progressThreshold socketPaths
             (AnyConsensusModeParams cModeParams) secParam = do
  traceWith tracer ("Will observe nodes for " ++ show runningTime)
  traceWith tracer ("Will require chain growth of " ++ show progressThreshold)

  -- Run the chairman and get the final snapshot of the chain from each node.
  chainsSnapshot <-
    obtainGetHeader (consensusModeOnly cModeParams) $
     runChairman
      tracer
      nw
      runningTime
      socketPaths
      cModeParams
      secParam

  traceWith tracer "================== chairman results =================="

  -- Test if we achieved consensus
  consensusSuccess <- either throwIO return $
                        obtainHasHeader (consensusModeOnly cModeParams) $
                        consensusCondition (consensusModeOnly cModeParams) chainsSnapshot secParam

  traceWith tracer (show consensusSuccess)

  -- Test if we made adequate progress
  progressSuccess <- either throwIO return $
                       progressCondition progressThreshold consensusSuccess

  traceWith tracer (show progressSuccess)
  traceWith tracer "================== chairman results =================="

type PeerId = SocketPath

instance Exception ConsensusFailure where
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

consensusCondition
  :: ConsensusBlockForMode mode ~ blk
  => HasHeader (Header blk)
  => ConvertRawHash blk
  => ConsensusMode mode
  -> Map PeerId (AnchoredFragment (Header blk))
  -> SecurityParam
  -> Either ConsensusFailure ConsensusSuccess
consensusCondition cMode chains securityParam =
    -- The (forkTooLong . chainForkPoints) predicate is not transitive.
    -- As a consequence, we need to check it between all the pairs of chains:
    let forks =
          [ ((peerid1, peerid2), chainForkPoints chain1 chain2)
          | (peerid1, chain1) <- Map.toList chains
          , (peerid2, chain2) <- Map.toList chains
          ]
     in case find (forkTooLong . snd) forks of
          Just ((peerid1, peerid2), (intersection, tip1, tip2)) ->do
            let apiTip1 = fromConsensusTip cMode $ AF.anchorToTip tip1
                apiTip2 = fromConsensusTip cMode $ AF.anchorToTip tip2
                intersectChainPt = fromAnchor intersection

            Left $
              ConsensusFailure
                (peerid1, apiTip1)
                (peerid2, apiTip2)
                intersectChainPt
                securityParam
          Nothing ->
            Right $
              ConsensusSuccess
                -- the minimum intersection point:
                (fromAnchor
                   $ minimumBy (comparing AF.anchorToBlockNo)
                       [ intersection | (_,(intersection,_,_)) <- forks ])
                -- all the chain tips:
                [ (peerid, fromConsensusTip cMode $ AF.anchorToTip (AF.headAnchor chain))
                | (peerid, chain) <- Map.toList chains ]
  where
    chainForkPoints
      :: HasHeader (Header blk)
      => AnchoredFragment (Header blk)
      -> AnchoredFragment (Header blk)
      -> ( Anchor (Header blk) -- intersection
         , Anchor (Header blk) -- tip of c1
         , Anchor (Header blk) -- tip of c2
         )
    chainForkPoints chain1 chain2 =
      case AF.intersect chain1 chain2 of
        -- chains are anochored at the genesis, so their intersection is never
        -- empty
        Nothing -> error "chainChains: invariant violation"

        Just (_, _, extension1, extension2) ->
          ( AF.anchor     extension1
          , AF.headAnchor extension1
          , AF.headAnchor extension2
          )

    forkTooLong
      :: ( Anchor (Header blk) -- intersection
         , Anchor (Header blk) -- tip of chain1
         , Anchor (Header blk) -- tip of chain2
         )
      -> Bool
    forkTooLong (intersection, tip1, tip2) =
        -- If only one of len1, len2 is longer than the securityParam then it is
        -- still OK. That node can still recover by receiving a valid rollback
        -- instruction, but if both are longer, then we have a failure.
        forkLen tip1 >  maxRollbacks securityParam &&
        forkLen tip2 >  maxRollbacks securityParam
      where
        forkLen :: Anchor (Header blk) -> Word64
        forkLen tip =
          Block.unBlockNo $
            fromWithOrigin 0 (AF.anchorToBlockNo tip)
          - fromWithOrigin 0 (AF.anchorToBlockNo intersection)


data ConsensusSuccess = ConsensusSuccess
    -- Minimum of the maximum intersection points
    ChainPoint
    -- Chain tip for each chain
    [(PeerId, ChainTip)]
    deriving Show


data ConsensusFailure = ConsensusFailure
    -- Tip of two peer's chains that do not intersect within K blocks
    (PeerId, ChainTip)
    (PeerId, ChainTip)
    -- The intersection point of two chains
    ChainPoint
    SecurityParam
    deriving Show

fromAnchor :: forall blk. ConvertRawHash blk => Anchor (Header blk) -> ChainPoint
fromAnchor AF.AnchorGenesis = ChainPointAtGenesis
fromAnchor (AF.Anchor slot headerhash _blockNo) =
  let sbs = toShortRawHash (Proxy @blk) headerhash
  in ChainPoint slot $ HeaderHash sbs

newtype ProgressSuccess = ProgressSuccess BlockNo
  deriving Show

data ProgressFailure =
     ProgressFailure
       BlockNo -- minimum expected
       PeerId
       ChainTip
  deriving Show


instance Exception ProgressFailure where
  displayException (ProgressFailure minBlockNo peerid tip) =
    concat
      [ "progress failure:\n"
      , "the node at ", show peerid, " has chain tip ", show tip, "\n"
      , "while the mininum expected block number is ", show minBlockNo
      ]


-- | Progress is defined as each chain being at least of a certain length.
--
progressCondition :: BlockNo
                  -> ConsensusSuccess
                  -> Either ProgressFailure ProgressSuccess
progressCondition minBlockNo (ConsensusSuccess _ tips) = do
   case find (\(_, ct) -> getBlockNo ct < minBlockNo) tips of
    Just (peerid, tip) -> Left (ProgressFailure minBlockNo peerid tip)
    Nothing -> Right (ProgressSuccess minBlockNo)
 where
   getBlockNo :: ChainTip -> BlockNo
   getBlockNo (ChainTip _ _ bNum) = bNum
   getBlockNo ChainTipAtGenesis = 0

runChairman
  :: forall mode blk. ConsensusBlockForMode mode ~ blk
  => GetHeader (ConsensusBlockForMode mode)
  => Tracer IO String
  -> NetworkId
  -- ^ Security parameter, if a fork is deeper than it 'runChairman'
  -- will throw an exception.
  -> DiffTime
  -- ^ Run for this much time.
  -> [SocketPath]
  -- ^ Local socket directory
  -> ConsensusModeParams mode
  -> SecurityParam
  -> IO (Map SocketPath
             (AF.AnchoredSeq
                (WithOrigin SlotNo)
                (Anchor (Header blk))
                (Header blk)))
runChairman tracer networkId runningTime socketPaths cModeParams secParam = do
    let initialChains :: Map SocketPath (AF.AnchoredSeq (WithOrigin SlotNo) (Anchor (Header blk)) (Header blk))
        initialChains = Map.fromList
          [ (socketPath, AF.Empty AF.AnchorGenesis)
          | socketPath <- socketPaths]

    chainsVar <- newTVarIO initialChains

    void $ timeout runningTime $
        forConcurrently_ socketPaths $ \sockPath ->
          let localConnInfo = LocalNodeConnectInfo
                          { localConsensusModeParams = cModeParams
                          , localNodeNetworkId = networkId
                          , localNodeSocketPath = unSocketPath sockPath
                          }
              chairmanChainSyncClient = LocalChainSyncClient $ chainSyncClient (showTracing tracer) sockPath chainsVar cModeParams secParam
              protocolsInMode = LocalNodeClientProtocols
                { localChainSyncClient = chairmanChainSyncClient
                , localTxSubmissionClient = Nothing
                , localStateQueryClient = Nothing
                }
          in connectToLocalNode localConnInfo protocolsInMode

    atomically (readTVar chainsVar)

-- Shared State, and its API.

-- | Shared state between chain-sync clients.  Each chain-sync client will write to the
-- corresponding entry.
type ChainsVar m blk = StrictTVar m (Map SocketPath (AnchoredFragment (Header blk)))

-- | Add a single block to the chain.
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
rollback
  :: forall mode blk. ConsensusBlockForMode mode ~ blk
  => HasHeader (Header blk)
  => SocketPath
  -> StrictTVar IO (Map SocketPath (AnchoredFragment (Header (ConsensusBlockForMode mode))))
  -> ConsensusMode mode
  -> ChainPoint
  -> STM IO ()
rollback sockPath chainsVar cMode p =
  modifyTVar chainsVar (Map.adjust fn sockPath)
  where
    p' :: Point (Header (ConsensusBlockForMode mode))
    p' = coerce $ toConsensusPointInMode cMode p

    fn :: AnchoredFragment (Header (ConsensusBlockForMode mode))
       -> AnchoredFragment (Header (ConsensusBlockForMode mode))
    fn cf = case AF.rollback p' cf of
      Nothing  -> error "rollback error: rollback beyond chain fragment"
      Just cf' -> cf'

-- Chain-Sync client
type ChairmanTrace' = ConsensusSuccess

type ChainVar mode = StrictTVar IO (Map SocketPath (AnchoredFragment (Header (ConsensusBlockForMode mode))))

-- | 'chainSyncClient which build chain fragment; on every roll forward it will
-- check if there is consensus on immutable chain.
chainSyncClient
  :: forall mode. GetHeader (ConsensusBlockForMode mode)
  => Tracer IO ChairmanTrace'
  -> SocketPath
  -> ChainVar mode
  -> ConsensusModeParams mode
  -> SecurityParam
  -> ChainSyncClient (BlockInMode mode) ChainPoint ChainTip IO ()
chainSyncClient tracer sockPath chainsVar cModeP secParam = ChainSyncClient $ pure $
  -- Notify the core node about the our latest points at which we are
  -- synchronised.  This client is not persistent and thus it just
  -- synchronises from the genesis block.  A real implementation should send
  -- a list of points up to a point which is k blocks deep.
  SendMsgFindIntersect
    [fromConsensusPointInMode (consensusModeOnly cModeP) Block.genesisPoint]
    ClientStIntersect
    { recvMsgIntersectFound    = \_ _ -> ChainSyncClient $ pure clientStIdle
    , recvMsgIntersectNotFound = \  _ -> ChainSyncClient $ pure clientStIdle
    }
  where
    clientStIdle :: ClientStIdle (BlockInMode mode) ChainPoint ChainTip IO ()
    clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

    clientStNext :: ClientStNext (BlockInMode mode) ChainPoint ChainTip IO ()
    clientStNext = ClientStNext
      { recvMsgRollForward = \blk _tip -> ChainSyncClient $ do
          -- add block & check if there is consensus on immutable chain
          -- trace the decision or error
          res <- atomically $ do
            addBlock sockPath chainsVar $ toConsensusBlock blk
            obtainHasHeader (consensusModeOnly cModeP) $ checkConsensus (consensusModeOnly cModeP) chainsVar secParam
          traceWith tracer res
          pure clientStIdle
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- rollback & check
          res <- atomically $ do
            rollback sockPath chainsVar (consensusModeOnly cModeP) point
            obtainHasHeader (consensusModeOnly cModeP) $ checkConsensus (consensusModeOnly cModeP) chainsVar secParam
          traceWith tracer res
          pure clientStIdle
      }

-- Helpers

obtainHasHeader
  :: ConsensusBlockForMode mode ~ blk
  => ConsensusMode mode
  -> ((HasHeader (Header blk), ConvertRawHash (ConsensusBlockForMode mode)) => a)
  -> a
obtainHasHeader ByronMode f = f
obtainHasHeader ShelleyMode f = f
obtainHasHeader CardanoMode f = f

obtainGetHeader
  :: ConsensusMode mode
  -> ( (GetHeader (ConsensusBlockForMode mode)
       ) => a)
  -> a
obtainGetHeader ByronMode f = f
obtainGetHeader ShelleyMode f = f
obtainGetHeader CardanoMode f = f

-- | Check that all nodes agree with each other, within the security parameter.
checkConsensus
  :: HasHeader (Header (ConsensusBlockForMode mode))
  => ConvertRawHash (ConsensusBlockForMode mode)
  => ConsensusMode mode
  -> ChainVar mode
  -> SecurityParam
  -> STM IO ConsensusSuccess
checkConsensus cMode chainsVar secParam = do
  chainsSnapshot <- readTVar chainsVar
  either throwIO return $ consensusCondition cMode  chainsSnapshot secParam


