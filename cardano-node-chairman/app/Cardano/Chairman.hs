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

import           Cardano.Api

import           Cardano.Ledger.BaseTypes (unNonZero)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Network.AnchoredFragment (Anchor, AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Protocol.ChainSync.Client

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad (void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Data.Word (Word64)

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
chairmanTest :: ()
  => Tracer IO String
  -> NetworkId
  -> DiffTime
  -> BlockNo
  -> [SocketPath]
  -> ConsensusModeParams
  -> SecurityParam
  -> IO ()
chairmanTest tracer nw runningTime progressThreshold socketPaths cModeParams secParam = do
  traceWith tracer ("Will observe nodes for " ++ show runningTime)
  traceWith tracer ("Will require chain growth of " ++ show progressThreshold)

  -- Run the chairman and get the final snapshot of the chain from each node.
  chainsSnapshot <-
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
                        consensusCondition chainsSnapshot secParam

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

consensusCondition :: ()
  => Map PeerId (AnchoredFragment (Header (CardanoBlock StandardCrypto)))
  -> SecurityParam
  -> Either ConsensusFailure ConsensusSuccess
consensusCondition chains securityParam =
    -- The (forkTooLong . chainForkPoints) predicate is not transitive.
    -- As a consequence, we need to check it between all the pairs of chains:
    let forks =
          [ ((peerid1, peerid2), chainForkPoints chain1 chain2)
          | (peerid1, chain1) <- Map.toList chains
          , (peerid2, chain2) <- Map.toList chains
          ]
     in case List.find (forkTooLong . snd) forks of
          Just ((peerid1, peerid2), (intersection, tip1, tip2)) ->do
            let apiTip1 = AF.anchorToTip tip1
                apiTip2 = AF.anchorToTip tip2
                intersectChainPt = fromAnchor intersection

            Left $
              ConsensusFailure
                (peerid1, fromConsensusTip apiTip1)
                (peerid2, fromConsensusTip apiTip2)
                intersectChainPt
                securityParam
          Nothing ->
            Right $
              ConsensusSuccess
                -- the minimum intersection point:
                (fromAnchor
                   $ List.minimumBy (comparing AF.anchorToBlockNo)
                       [ intersection | (_,(intersection,_,_)) <- forks ])
                -- all the chain tips:
                [ (peerid, fromConsensusTip $ AF.anchorToTip (AF.headAnchor chain))
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
        -- chains are anchored at the genesis, so their intersection is never
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
        forkLen tip1 >  unNonZero (maxRollbacks securityParam) &&
        forkLen tip2 >  unNonZero (maxRollbacks securityParam)
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
      , "while the minimum expected block number is ", show minBlockNo
      ]


-- | Progress is defined as each chain being at least of a certain length.
--
progressCondition :: BlockNo
                  -> ConsensusSuccess
                  -> Either ProgressFailure ProgressSuccess
progressCondition minBlockNo (ConsensusSuccess _ tips) = do
   case List.find (\(_, ct) -> getBlockNo ct < minBlockNo) tips of
    Just (peerid, tip) -> Left (ProgressFailure minBlockNo peerid tip)
    Nothing -> Right (ProgressSuccess minBlockNo)
 where
   getBlockNo :: ChainTip -> BlockNo
   getBlockNo (ChainTip _ _ bNum) = bNum
   getBlockNo ChainTipAtGenesis = 0

runChairman ::
     Tracer IO String
  -> NetworkId
  -- ^ Security parameter, if a fork is deeper than it 'runChairman'
  -- will throw an exception.
  -> DiffTime
  -- ^ Run for this much time.
  -> [SocketPath]
  -- ^ Local socket directory
  -> ConsensusModeParams
  -> SecurityParam
  -> IO (Map SocketPath
             (AF.AnchoredSeq
                (WithOrigin SlotNo)
                (Anchor (Header (CardanoBlock StandardCrypto)))
                (Header (CardanoBlock StandardCrypto))))
runChairman tracer networkId runningTime socketPaths cModeParams secParam = do
    let initialChains :: Map
          SocketPath
          (AF.AnchoredSeq
            (WithOrigin SlotNo)
            (Anchor (Header (CardanoBlock StandardCrypto)))
            (Header (CardanoBlock StandardCrypto)))
        initialChains = Map.fromList
          [ (socketPath, AF.Empty AF.AnchorGenesis)
          | socketPath <- socketPaths]

    chainsVar <- newTVarIO initialChains

    void $ timeout runningTime $
        forConcurrently_ socketPaths $ \socketPath ->
          let localConnInfo = LocalNodeConnectInfo
                          { localConsensusModeParams = cModeParams
                          , localNodeNetworkId = networkId
                          , localNodeSocketPath = socketPath
                          }
              chairmanChainSyncClient = LocalChainSyncClient $
                chainSyncClient (showTracing tracer) socketPath chainsVar secParam
              protocolsInMode = LocalNodeClientProtocols
                { localChainSyncClient = chairmanChainSyncClient
                , localTxSubmissionClient = Nothing
                , localStateQueryClient = Nothing
                , localTxMonitoringClient = Nothing
                }
          in connectToLocalNode localConnInfo protocolsInMode

    readTVarIO chainsVar

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
rollback ::
     SocketPath
  -> StrictTVar IO (Map SocketPath (AnchoredFragment (Header (CardanoBlock StandardCrypto))))
  -> ChainPoint
  -> STM IO ()
rollback sockPath chainsVar p =
  modifyTVar chainsVar (Map.adjust fn sockPath)
  where
    p' :: Point (Header (CardanoBlock StandardCrypto))
    p' = toConsensusPointHF p

    fn :: AnchoredFragment (Header (CardanoBlock StandardCrypto))
       -> AnchoredFragment (Header (CardanoBlock StandardCrypto))
    fn cf = case AF.rollback p' cf of
      Nothing  -> error "rollback error: rollback beyond chain fragment"
      Just cf' -> cf'

-- Chain-Sync client
type ChairmanTrace' = ConsensusSuccess

type ChainVar = StrictTVar IO (Map SocketPath (AnchoredFragment (Header (CardanoBlock StandardCrypto))))

-- | 'chainSyncClient which build chain fragment; on every roll forward it will
-- check if there is consensus on immutable chain.
chainSyncClient
  :: Tracer IO ChairmanTrace'
  -> SocketPath
  -> ChainVar
  -> SecurityParam
  -> ChainSyncClient BlockInMode ChainPoint ChainTip IO ()
chainSyncClient tracer sockPath chainsVar secParam = ChainSyncClient $ pure $
  -- Notify the core node about the our latest points at which we are
  -- synchronised.  This client is not persistent and thus it just
  -- synchronises from the genesis block.  A real implementation should send
  -- a list of points up to a point which is k blocks deep.
  SendMsgFindIntersect
    [ChainPointAtGenesis]
    ClientStIntersect
    { recvMsgIntersectFound    = \_ _ -> ChainSyncClient $ pure clientStIdle
    , recvMsgIntersectNotFound = \  _ -> ChainSyncClient $ pure clientStIdle
    }
  where
    clientStIdle :: ClientStIdle BlockInMode ChainPoint ChainTip IO ()
    clientStIdle = SendMsgRequestNext (pure ()) clientStNext

    clientStNext :: ClientStNext BlockInMode ChainPoint ChainTip IO ()
    clientStNext = ClientStNext
      { recvMsgRollForward = \blk _tip -> ChainSyncClient $ do
          -- add block & check if there is consensus on immutable chain
          -- trace the decision or error
          res <- atomically $ do
            addBlock sockPath chainsVar $ toConsensusBlock blk
            checkConsensus chainsVar secParam
          traceWith tracer res
          pure clientStIdle
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- rollback & check
          res <- atomically $ do
            rollback sockPath chainsVar point
            checkConsensus chainsVar secParam
          traceWith tracer res
          pure clientStIdle
      }

-- Helpers

-- | Check that all nodes agree with each other, within the security parameter.
checkConsensus
  :: ChainVar
  -> SecurityParam
  -> STM IO ConsensusSuccess
checkConsensus chainsVar secParam = do
  chainsSnapshot <- readTVar chainsVar
  either throwIO return $ consensusCondition chainsSnapshot secParam
