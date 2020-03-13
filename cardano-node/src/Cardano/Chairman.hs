{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Chairman
  ( CurrentSlotEstimation(..)
  , runChairman
  ) where

import           Cardano.Prelude hiding (ByteString, STM, atomically, catch, option, show)
import           Prelude (String, error, show)

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad (void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Function (on)
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)
import           Data.Coerce (coerce)
import           Data.Typeable (Typeable, typeOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Network.Mux (MuxError)

import           Ouroboros.Consensus.Block (BlockProtocol, GetHeader (..))
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Network.Codec
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
import           Ouroboros.Network.Snocket (socketSnocket)

import           Cardano.Common.LocalSocket
import           Cardano.Config.Types (SocketPath)
import           Cardano.Slotting.Slot (SlotNo)

-- | Avoid depending on the complexities of 'BlockchainTime',
--   by providing an independent, hopefully simpler estimation.
class ( Condense (Header blk)
      , Condense (HeaderHash blk)
      , HasHeader (Header blk)
      , Typeable blk)
 => CurrentSlotEstimation blk where
  estimateCurrentSlot :: Protocol blk (BlockProtocol blk) -> IO SlotNo
  estimateCurrentSlot _ = throwIO (CurrentSlotEstimationUnavailable $ Proxy @blk)

instance {-# OVERLAPPABLE #-}
  ( Condense (Header blk)
  , Condense (HeaderHash blk)
  , HasHeader (Header blk)
  , Typeable blk)
  => CurrentSlotEstimation blk

-- | Run chairman: connect with all the core nodes.  Chairman will store the
-- forks from a common prefix.  If any of them is longer than the security
-- parameter it will throw an exception.
--
-- It is also possible to specify how many blocks should be validated.
--
runChairman :: forall blk.
               ( CurrentSlotEstimation blk
               , RunNode blk
               )
            => AssociateWithIOCP
            -> Protocol blk (BlockProtocol blk)
            -> SecurityParam
            -- ^ security parameter, if a fork is deeper than it 'runChairman'
            -- will throw an exception.
            -> Maybe BlockNo
            -- ^ finish after that many blocks, if 'Nothing' run continuously.
            -> [SocketPath]
            -- ^ local socket dir
            -> Tracer IO String
            -> IO ()
runChairman iocp ptcl securityParam maxBlockNo socketPaths tracer = do

    (chainsVar :: ChainsVar IO blk) <- newTVarM
      (Map.fromList $ map (\socketPath -> (socketPath, AF.Empty AF.AnchorGenesis)) socketPaths)

    void $ flip mapConcurrently socketPaths $ \sockPath ->
        let ProtocolInfo { pInfoConfig = cfg } = protocolInfo ptcl

        in createConnection
             chainsVar
             securityParam
             maxBlockNo
             tracer
             cfg
             iocp
             sockPath

    -- Perform the final test -- that sufficiently many blocks were made
    -- in the shortest agreed-upon chain.
    curSlot <- estimateCurrentSlot ptcl
    cpoints <- atomically $ checkChainsPairwise securityParam chainsVar

    case testChainDensity securityParam curSlot cpoints of
      Just e -> throwIO e
      _ -> pure ()

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
     ( RunNode blk
     , Condense (Header blk)
     , Condense (HeaderHash blk)
     )
  => ChainsVar IO blk
  -> SecurityParam
  -> Maybe BlockNo
  -> Tracer IO String
  -> TopLevelConfig blk
  -> AssociateWithIOCP
  -> SocketPath
  -> IO ()
createConnection
  chainsVar
  securityParam
  maxBlockNo
  tracer
  cfg
  iocp
  socketPath = do
      path <- localSocketPath socketPath
      connectTo
        (socketSnocket iocp)
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

-- | A tip where consensus was witnessed.
newtype ConsensusTip blk =
  ConsensusTip { unConsensusTip :: Tip blk }

data ChairmanTrace blk
  = WitnessedConsensus [ConsensusTip blk]
  -- ^ witness consensus at a given point.  The list is a list of tip points of
  -- each chain.

instance (Condense (HeaderHash blk)) => Show (ChairmanTrace blk) where
    show (WitnessedConsensus tips)
      = mconcat
      [ "witnessed consensus "
      , condense (Block.getTipPoint . unConsensusTip <$> tips)
      ]

data ChairmanError blk
  = ChainForksLongerThanK
      SecurityParam
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))
    -- ^ Nodes did not agree on a chain: we witnessed a fork longer than
    -- 'SecurityParam'.  The given points are tips of the chains when the fork
    -- was encountered.
  | NoChainIntersection
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))
    -- ^ Nodes returned completely diverging chains with zero intersection.
    -- This is a sign of node misconfiguration.
  | TooFewBlocksMade
      SlotNo
      Word64
      BlockNo
    -- ^ Too few blocks were made at the end of the 'SlotNo'-long run,
    -- the Word64 denoting our expectations, and 'BlockNo' the reality.
  | InvalidCurrentSlotEstimate
      (Tip blk)
      SlotNo
    -- ^ Our (SlotNo) estimate of the current slot (Tip) was invalid.
  | CurrentSlotEstimationUnavailable
      (Proxy blk)
    -- ^ Chain slot estimation unavailable for protocol.
  | SummaryFailure
      [ChairmanError blk]
    -- ^ Problems found during the Chairman's session.

instance ( Condense (Header blk)
         , Condense (HeaderHash blk)
         , HasHeader (Header blk)
         , Typeable blk)
    => Show (ChairmanError blk) where
    show (ChainForksLongerThanK k l r) =
      "Chain forks longer than 'k' (" ++ show k ++ "): "
      ++ condense l ++ ", " ++ condense r
    show (NoChainIntersection l r) =
      "Nodes posted diverging chains: "
      ++ condense (AF.headPoint l) ++ ", " ++ condense (AF.headPoint r)
    show (InvalidCurrentSlotEstimate tip (Block.SlotNo slot)) =
      "Chain progressed beyond expected slot #" ++ show slot
      ++ ", actual tip is: " ++ condense (Block.getTipPoint tip)
    show (TooFewBlocksMade (Block.SlotNo s) expect (Block.BlockNo actual)) =
      "Too few blocks made for a " ++ show s ++ "-slot run: expected "
      ++ show expect ++ ", got: " ++ show actual
    show (CurrentSlotEstimationUnavailable _) =
      "Current slot estimation unavailable for protocol of block "
      ++ show (typeOf $ Proxy @blk)
    show (SummaryFailure errs) =
      "Errors exposed during Chairman's session:\n  "
      ++ intercalate "\n  " (show <$> errs)

instance ( Condense (Header blk)
         , Condense (HeaderHash blk)
         , HasHeader (Header blk)
         , Typeable blk
         ) => Exception (ChairmanError blk)

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

getChains
  :: MonadSTM m
  => ChainsVar m blk -> STM m [AnchoredFragment (Header blk)]
getChains chainsVar = Map.elems <$> readTVar chainsVar

-- | Check if there is no illegitimate long fork.
--
checkChainsPairwise
 :: ( Condense (Header blk)
    , Condense (HeaderHash blk)
    , HasHeader (Header blk)
    , MonadSTM m
    , MonadThrow (STM m)
    , Typeable blk)
 => SecurityParam
 -> ChainsVar m blk
 -> STM m [ConsensusTip blk]
checkChainsPairwise securityParam chainsVar = do
  chains <- getChains chainsVar
  case computeInterchainRelation securityParam chains of
    ([], cpoints) -> pure cpoints
    (xs, _) -> throwM (SummaryFailure xs)

-- | For a given set of known consensus points, further determine
-- whether the chain density is satisfactory.
testChainDensity
  :: forall blk
  .  SecurityParam
  -> SlotNo
  -> [ConsensusTip blk]
  -> Maybe (ChairmanError blk)
testChainDensity _k curSlot cpoints
  | slotBehindTip (unConsensusTip furthestConsensus) curSlot
    = Just (InvalidCurrentSlotEstimate
             (unConsensusTip furthestConsensus)
             curSlot)
  | furthestAgreedBlockNo < Block.BlockNo expectedBlocks
    = Just (TooFewBlocksMade curSlot expectedBlocks furthestAgreedBlockNo)
  | otherwise
    = Nothing
 where
   expectedBlocks :: Word64
   expectedBlocks = Block.unSlotNo curSlot - 5

   furthestConsensus :: ConsensusTip blk
   furthestConsensus = minimumBy (tipOrder `on` unConsensusTip) cpoints

   furthestAgreedBlockNo :: BlockNo
   furthestAgreedBlockNo = case unConsensusTip furthestConsensus of
     -- Not entirely accurate, but an acceptable tradeoff, complexity-wise.
     Block.TipGenesis -> Block.BlockNo 0
     Block.Tip _ _ blkno -> blkno

   slotBehindTip :: Tip blk -> SlotNo -> Bool
   slotBehindTip Block.TipGenesis _ = False
   slotBehindTip (Block.Tip s' _ _) s = s' > s

   tipOrder :: Tip blk -> Tip blk -> Ordering
   tipOrder Block.TipGenesis Block.TipGenesis = EQ
   tipOrder Block.TipGenesis _ = LT
   tipOrder _ Block.TipGenesis = GT
   tipOrder (Block.Tip sl _ _) (Block.Tip sr _ _) = sl `compare` sr

-- | For a given set of chains fragments, perform a pairwise check for divergences:
--   1. whether each pair has a shared part, and
--   2. whether each pair has both intersection suffixes shorter than 'k'.
--   Note that this is only a local pair coherency check.
--
computeInterchainRelation
  :: forall blk. (HasHeader (Header blk))
  => SecurityParam
  -> [AnchoredFragment (Header blk)]
  -> ([ChairmanError blk], [ConsensusTip blk])
computeInterchainRelation k@(SecurityParam securityParam) chains =
    partitionEithers $
      uncurry chainRelation <$> [ (fr0, fr1)  | fr0 <- chains, fr1 <- chains ]
  where
    -- This property is not transitive (e.g. fr0 and fr1 are not long forks,
    -- and fr1 and fr2 are not long forks, but fr0 and fr2 are long forks).
    -- As a consequence, we need to check it between all the pairs of chains.
    chainRelation :: AnchoredFragment (Header blk)
             -> AnchoredFragment (Header blk)
             -> Either (ChairmanError blk) (ConsensusTip blk)
    chainRelation fr0 fr1 = case AF.intersect fr0 fr1 of
      -- chains are anchored at the genesis, so their intersection is never
      -- empty, for properly-configured nodes.
      Nothing -> Left $ NoChainIntersection fr0 fr1
      Just (p0, _, s0, s1) ->
        let s0len = fromIntegral (AF.length s0)
            s1len = fromIntegral (AF.length s1)
        in if s0len > securityParam && s1len > securityParam
             then Left $ ChainForksLongerThanK k s0 s1
             -- if only one of 's0len', 's1len' is greater than 'securityParam'
             -- then it is still ok. That node can still recover by receiving
             -- a valid rollback instruction.
             else Right $ ConsensusTip (AF.anchorToTip $ AF.anchor p0)

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
     , Condense (Header blk)
     , Condense (HeaderHash (Header blk))
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
            checkChainsPairwise securityParam chainsVar
          traceWith tracer (WitnessedConsensus res)
          let currentBlockNo = Just (Block.blockNo blk)
          pure $ clientStIdle currentBlockNo
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          -- rollback & check
          res <- atomically $ do
            rollback sockPath chainsVar point
            checkChainsPairwise securityParam chainsVar
          traceWith tracer (WitnessedConsensus res)
          pure $ clientStIdle Nothing
      }

--
-- Client Application
--

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , Condense (Header blk)
     , Condense (HeaderHash blk)
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
      NodeToClientProtocols
        { localChainSyncProtocol = InitiatorProtocolOnly $
                                     MuxPeer
                                       localTxSubmissionTracer
                                       localTxSubmissionCodec
                                       (localTxSubmissionClientPeer localTxSubmissionClientNull)
        , localTxSubmissionProtocol = InitiatorProtocolOnly $
                                        MuxPeer
                                          chainSyncTracer
                                          (localChainSyncCodec cfg)
                                          (chainSyncClientPeer $
                                             chainSyncClient chairmanTracer sockPath chainsVar
                                                             securityParam maxBlockNo)
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
