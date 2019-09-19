{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Chairman
  ( ConsensusValidationMode(..)
  , runChairman)
where

import           Cardano.Prelude hiding (ByteString, STM, atomically, catch, show)
import           Prelude (String, error, foldl1, show, unlines)

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad (void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Void (Void)
import qualified Data.Time as Time
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
import           Ouroboros.Consensus.BlockchainTime (SlotLength(..))
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
import           Ouroboros.Network.Block (HasHeader, HeaderHash, Point, SlotNo(..))
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Point as Point
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

data ConsensusValidationMode =
  -- | Define pass/fail as follows:
  -- - success is having maximum fork length under 'caMaximumForkLength'
  --   for 'caSlotsWithinTolerance' slots,
  -- - failure is inability to attain success within 'caTimeout'.
  MaxForkLengthForSlots
  { cvmMaximumForkLength :: !Int
  , cvmSlotsWithinTolerance :: !SlotNo
  }
  deriving Show

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
            -> SlotLength
            -> Time.UTCTime
            -> [CoreNodeId]
            -> NumCoreNodes
            -> SecurityParam
            -- ^ security parameter, if a fork is deeper than it 'runChairman'
            -- will throw an exception.
            -> Maybe ConsensusValidationMode
            -- ^ when specifid, terminate successfully when conditions specified by
            --   'ConsensusValidationMode' hold.
            -> FilePath
            -- ^ local socket dir
            -> Tracer IO String
            -> IO ()
runChairman ptcl slotLen startTime nids numCoreNodes securityParam consensusValidMode socketDir tracer = do

    now <- Time.getCurrentTime
    let valid = mkValidation slotLen startTime now nids
    validVar :: ValidVar IO blk <- newTVarM valid
    putStrLn $ unlines
      [ "Start time:            " <> show startTime
      , "Now:                   " <> show now
      , "Delta:                 " <> show (now `Time.diffUTCTime` startTime)
      , "Slot length:           " <> show (getSlotLength slotLen)
      , "Computed current slot: " <> show (unSlotNo $ vLastConsensusCriteriaViolation valid)
      ]

    addr <- localSocketAddrInfo (CoreId 0) socketDir NoMkdirIfMissing

    void $ flip mapConcurrently nids $ \coreNodeId ->
        let ProtocolInfo{pInfoConfig} =
              protocolInfo numCoreNodes
                           coreNodeId
                           ptcl

        in connectTo
            nullTracer
            nullTracer
            (,)
            (localInitiatorNetworkApplication
              coreNodeId
              validVar
              securityParam
              consensusValidMode
              (showTracing tracer)
              nullTracer
              nullTracer
              pInfoConfig)
            Nothing
            addr
          `catch` handleMuxError validVar coreNodeId
           -- The first thread that terminates normally, causes successful exit.
           >> exitSuccess
  where
    -- catch 'MuxError'; it will be thrown if a node shuts down closing the
    -- connection.
    handleMuxError :: ValidVar IO blk -> CoreNodeId -> MuxError -> IO ()
    handleMuxError validVar coreNodeId err = do
      traceWith tracer (show err)
      atomically $ modifyTVar validVar (adjustValidationChains $ Map.delete coreNodeId)


data ChairmanTrace blk
  = NotFoundCommonBlock [Point blk]
  -- ^ the common block was not found, present list of head points.  Might be
  -- empty if the chain was empty.
  | WitnessedConsensusAt (Point blk) [Point blk]
  -- ^ witness consensus at a given point.  The list is a list of tip points of
  -- each chain.
  | ValidationSuccess ConsensusValidationMode SlotNo (Validation blk)
  -- ^ consensus validation with 'ConsensusValidationMode'
  | Pass String

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
    show (ValidationSuccess vmode slot validation)
      = mconcat
      [ "validation success for mode "
      , show vmode
      , " after "
      , show slot
      , " slots, last violated on slot "
      , show $ vLastConsensusCriteriaViolation validation
      ]
    show (Pass s) = s

--
-- Shared State, and its API.
--


-- | Shared state between chain-sync clients.
--
data Validation blk = Validation
  { vLastConsensusCriteriaViolation :: !SlotNo
    -- ^ This is reset to the current point every time consensus quality
    -- acceptance criteria is violated (see 'ConsensusValidationMode').
  , vChains :: Map CoreNodeId (AnchoredFragment blk)
    -- ^ Each chain-sync client will write to the corresponding entry.
  }

mkValidation :: HasHeader blk => SlotLength -> Time.UTCTime -> Time.UTCTime -> [CoreNodeId] -> Validation blk
mkValidation slotDurMs startTime now nids =
  Validation
  { vLastConsensusCriteriaViolation = currentSlotNo
  , vChains = Map.fromList $ map (\coreNodeId -> (coreNodeId, AF.Empty Block.GenesisPoint)) nids
  } where elapsed :: Time.NominalDiffTime = now `Time.diffUTCTime` startTime
          -- TODO: use actual, not initial slot length from genesis.
          currentSlotNo = SlotNo . floor $ elapsed / getSlotLength slotDurMs

-- | Given a consensus validation mode, determine the updated slot number
--   it was last violated.
validationLastViolationSlot
  :: HasHeader blk
  => ConsensusValidationMode
  -> SlotNo
  -> Validation blk
  -> SlotNo
validationLastViolationSlot (MaxForkLengthForSlots maxForkLen _) curSlot v
  | maximum (AF.length <$> vChains v) > maxForkLen
  = curSlot
  | otherwise
  = vLastConsensusCriteriaViolation v

validationSuccess
  :: ConsensusValidationMode
  -> SlotNo
  -> Validation blk
  -> Bool
validationSuccess (MaxForkLengthForSlots _ slotsWithinTolerance) curSlot v
  = slotsWithinTolerance < slotsSinceViolation
  where slotsSinceViolation = curSlot - vLastConsensusCriteriaViolation v

type ValidVar m blk = StrictTVar m (Validation blk)

adjustValidationChains
  :: (Map CoreNodeId (AnchoredFragment blk) -> Map CoreNodeId (AnchoredFragment blk))
  -> (Validation blk -> Validation blk)
adjustValidationChains f v =
  v { vChains = f (vChains v) }

adjustValidationChain
  :: CoreNodeId
  -> (AnchoredFragment blk -> AnchoredFragment blk)
  -> (Validation blk -> Validation blk)
adjustValidationChain nodeId f =
  adjustValidationChains (Map.adjust f nodeId)

updateViolation :: SlotNo -> (Validation blk -> Validation blk)
updateViolation x v = v { vLastConsensusCriteriaViolation = x }

-- | Add a single block to the chain.
--
addBlock
    :: forall blk m.
       ( MonadSTM m
       , HasHeader blk
       )
    => CoreNodeId
    -> ValidVar m blk
    -> blk
    -> STM m ()
addBlock coreNodeId validVar blk =
    modifyTVar validVar (adjustValidationChain coreNodeId $ AF.addBlock blk)


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
    => ValidVar m blk
    -> SecurityParam
    -> STM m (ChairmanTrace blk)
checkAndPrune validVar (SecurityParam securityParam) = do
    v <- readTVar validVar
    case checkAndPrunePure (vChains v) of
      Left err -> throwM err
      Right (res, potentialForks) -> do
        let v'  = v  { vChains = potentialForks }
        writeTVar validVar $! v'
        pure res
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
    -> ValidVar m blk
    -> Point blk
    -> STM m ()
rollback coreNodeId validVar p =
    modifyTVar validVar (adjustValidationChain coreNodeId fn)
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
  -> ValidVar m blk
  -> SecurityParam
  -> Maybe ConsensusValidationMode
  -> ChainSyncClient blk (Point blk) m ()
chainSyncClient tracer coreNodeId validVar securityParam consensusValidMode = ChainSyncClient $ pure $
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
    clientStIdle :: ClientStIdle blk (Point blk) m ()
    clientStIdle =
      SendMsgRequestNext clientStNext (pure clientStNext)

    processValidation :: SlotNo -> ConsensusValidationMode -> m (ClientStIdle blk (Point blk) m ())
    processValidation currentSlot cvm = do
      v <- atomically $ do
        v <- readTVar validVar
        let v' = updateViolation (validationLastViolationSlot cvm currentSlot v) v
        writeTVar validVar $! v'
        pure v'
      traceWith tracer $ ValidationSuccess cvm currentSlot v
      if validationSuccess cvm currentSlot v
      then pure $ SendMsgDone ()
      else pure clientStIdle

    clientStNext :: ClientStNext blk (Point blk) m ()
    clientStNext = ClientStNext {
        recvMsgRollForward = \blk tip -> ChainSyncClient $ do
          -- add block & check if there is consensus on immutable chain
          -- trace the decision or error
          let currentSlot = Point.fromWithOrigin 0 $ Block.pointSlot tip
          traceWith tracer $ Pass $ "current slot: " <> show currentSlot
          res <- atomically $ do
            addBlock coreNodeId validVar blk
            checkAndPrune validVar securityParam
          traceWith tracer res
          fromMaybe (pure clientStIdle) $
            processValidation currentSlot <$> consensusValidMode
      , recvMsgRollBackward = \point tip -> ChainSyncClient $ do
          -- rollback & check
          let currentSlot = Point.fromWithOrigin 0 $ Block.pointSlot tip
          traceWith tracer $ Pass $ "current slot: " <> show currentSlot
          res <- atomically $ do
            rollback coreNodeId validVar point
            checkAndPrune validVar securityParam
          traceWith tracer res
          fromMaybe (pure clientStIdle) $
            processValidation currentSlot <$> consensusValidMode
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
  -> ValidVar m blk
  -> SecurityParam
  -> Maybe ConsensusValidationMode
  -> Tracer m (ChairmanTrace blk)
  -> Tracer m (TraceSendRecv (ChainSync blk (Point blk)) peer DeserialiseFailure)
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
localInitiatorNetworkApplication coreNodeId validVar securityParam consensusValidMode chairmanTracer chainSyncTracer localTxSubmissionTracer pInfoConfig =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = 0 })
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
          (chainSyncClientPeer $ chainSyncClient chairmanTracer coreNodeId validVar securityParam consensusValidMode)


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
  -> Codec (ChainSync blk (Point blk))
           DeserialiseFailure m ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (nodeEncodeBlock pInfoConfig)
      (nodeDecodeBlock pInfoConfig)
      (Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
      (Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk)))
      (Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk)))
