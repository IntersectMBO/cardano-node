{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.LedgerState.Extended
  ( -- * Initialization / Accumulation
    ExtLedgerCfg(..)
  , extLedgerCfgSecurityParam
  , ExtLedgerState(..)
  , initialExtLedgerState
  , applyBlock
  , ValidationMode(..)

    -- * Traversing the block chain
  , foldBlocks
  , chainSyncClientWithExtLedgerState
  , chainSyncClientPipelinedWithExtLedgerState

   -- * Errors
  , LedgerStateError(..)
  , FoldBlocksError(..)
  , GenesisConfigError(..)
  , InitialLedgerStateError(..)
  , renderLedgerStateError
  , renderFoldBlocksError
  , renderGenesisConfigError
  , renderInitialLedgerStateError
  )
  where

import           Prelude

import           Control.Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import           Data.Aeson as Aeson
import qualified Data.Aeson.Types as Data.Aeson.Types.Internal
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import           Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Short as BSS
import           Data.Foldable
import           Data.IORef
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.SOP.Strict (NP (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word
import qualified Data.Yaml as Yaml
import           Network.TypedProtocol.Pipelined (Nat (..))
import           System.FilePath

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.LedgerState (renderInitialLedgerStateError, renderLedgerStateError, renderFoldBlocksError, renderGenesisConfigError, LedgerStateError(ApplyBlockHashMismatch, ExtApplyBlockError, InvalidRollback), InitialLedgerStateError(ILSEConfigFile, ILSEGenesisFile, ILSELedgerConsensusConfig), FoldBlocksError(FoldBlocksInitialLedgerStateError, FoldBlocksApplyBlockError), GenesisConfigError(NEError, NEByronConfig, NEShelleyConfig, NEAlonzoConfig, NECardanoConfig), ValidationMode(QuickValidation, FullValidation))
import           Cardano.Api.IPC (ConsensusModeParams (..),
                   LocalChainSyncClient (LocalChainSyncClientPipelined),
                   LocalNodeClientProtocols (..), LocalNodeClientProtocolsInMode,
                   LocalNodeConnectInfo (..), connectToLocalNode)
import           Cardano.Api.Modes (CardanoMode, EpochSlots (..))
import           Cardano.Api.NetworkId (NetworkId (..), NetworkMagic (NetworkMagic))
import           Cardano.Api.Utils (textShow)
import qualified Cardano.Chain.Genesis
import qualified Cardano.Chain.Update
import           Cardano.Crypto (ProtocolMagicId (unProtocolMagicId), RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing
import qualified Cardano.Crypto.ProtocolMagic
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import qualified Cardano.Ledger.BaseTypes as Shelley.Spec
import qualified Cardano.Ledger.Credential as Shelley.Spec
import qualified Cardano.Ledger.Keys as Shelley.Spec
import qualified Cardano.Ledger.Shelley.Genesis as Shelley.Spec
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import qualified Cardano.Slotting.Slot as Slot
import qualified Ouroboros.Consensus.Block.Abstract as Consensus
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
import qualified Ouroboros.Consensus.Config as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as HFC
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import           Ouroboros.Consensus.Ledger.Basics (lrResult)
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import qualified Ouroboros.Consensus.Shelley.Eras as Shelley
import qualified Ouroboros.Consensus.Shelley.Node.Praos as Consensus
import qualified Ouroboros.Network.Block
import qualified Ouroboros.Network.Protocol.ChainSync.Client as CS
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined as CSP
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision

-- | Get the environment and initial ledger state.
initialExtLedgerState
  :: FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  ->  ExceptT InitialLedgerStateError IO (ExtLedgerCfg, ExtLedgerState)
  -- ^ The environment and initial ledger state
initialExtLedgerState networkConfigFile = do
  -- TODO Once support for querying the ledger config is added to the node, we
  -- can remove the networkConfigFile argument and much of the code in this
  -- module.
  config <- withExceptT ILSEConfigFile
                  (readNetworkConfig (NetworkConfigFile networkConfigFile))
  genesisConfig <- withExceptT ILSEGenesisFile (readCardanoGenesisConfig config)
  conf <- withExceptT ILSELedgerConsensusConfig (except (genesisConfigToExtLedgerCfg genesisConfig))
  let ledgerState = initExtLedgerStateVar genesisConfig
  return (conf, ledgerState)

-- | Apply a single block to the current ledger state.
applyBlock
  :: ExtLedgerCfg
  -- ^ The environment returned by @initialExtLedgerState@
  -> ExtLedgerState
  -- ^ The current ledger state
  -> ValidationMode
  -> Block era
  -- ^ Some block to apply
  -> Either LedgerStateError ExtLedgerState
  -- ^ The new ledger state (or an error).
applyBlock conf oldState validationMode block
  = applyBlock' conf oldState validationMode $ case block of
      ByronBlock byronBlock -> Consensus.BlockByron byronBlock
      ShelleyBlock blockEra shelleyBlock -> case blockEra of
        ShelleyBasedEraShelley -> Consensus.BlockShelley shelleyBlock
        ShelleyBasedEraAllegra -> Consensus.BlockAllegra shelleyBlock
        ShelleyBasedEraMary    -> Consensus.BlockMary shelleyBlock
        ShelleyBasedEraAlonzo  -> Consensus.BlockAlonzo shelleyBlock
        ShelleyBasedEraBabbage -> Consensus.BlockBabbage shelleyBlock

-- | Monadic fold over all blocks and ledger states. Stopping @k@ blocks before
-- the node's tip where @k@ is the security parameter.
foldBlocks
  :: forall a.
  FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> FilePath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> ValidationMode
  -> a
  -- ^ The initial accumulator state.
  -> (ExtLedgerCfg -> ExtLedgerState -> BlockInMode CardanoMode -> a -> IO a)
  -- ^ Accumulator function Takes:
  --
  --  * Environment (this is a constant over the whole fold).
  --  * The Ledger state (with block @i@ applied) at block @i@.
  --  * The Ledger events resulting from applying block @i@.
  --  * Block @i@.
  --  * The accumulator state at block @i - 1@.
  --
  -- And returns:
  --
  --  * The accumulator state at block @i@
  --
  -- Note: This function can safely assume no rollback will occur even though
  -- internally this is implemented with a client protocol that may require
  -- rollback. This is achieved by only calling the accumulator on states/blocks
  -- that are older than the security parameter, k. This has the side effect of
  -- truncating the last k blocks before the node's tip.
  -> ExceptT FoldBlocksError IO a
  -- ^ The final state
foldBlocks nodeConfigFilePath socketPath validationMode state0 accumulate = do
  -- NOTE this was originally implemented with a non-pipelined client then
  -- changed to a pipelined client for a modest speedup:
  --  * Non-pipelined: 1h  0m  19s
  --  * Pipelined:        46m  23s

  (extLedgerCfg, extLedgerState) <-
      withExceptT FoldBlocksInitialLedgerStateError (initialExtLedgerState nodeConfigFilePath)

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  errorIORef <- lift $ newIORef Nothing
  stateIORef <- lift $ newIORef state0

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig
        = (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
        . HFC.getPerEraLedgerConfig
        . HFC.hardForkLedgerConfigPerEra
        $ Consensus.topLevelConfigLedger
        $ Consensus.getExtLedgerCfg
        $ getExtLedgerCfg extLedgerCfg

      networkMagic
        = NetworkMagic
        $ unProtocolMagicId
        $ Cardano.Chain.Genesis.gdProtocolMagicId
        $ Cardano.Chain.Genesis.configGenesisData byronConfig

      networkId' = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic -> Testnet networkMagic

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * extLedgerCfgSecurityParam extLedgerCfg

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo CardanoMode
      connectInfo =
          LocalNodeConnectInfo {
            localConsensusModeParams = cardanoModeParams,
            localNodeNetworkId       = networkId',
            localNodeSocketPath      = socketPath
          }

  lift $ connectToLocalNode
    connectInfo
    (protocols stateIORef errorIORef extLedgerCfg extLedgerState)

  lift (readIORef errorIORef) >>= \case
    Just err -> throwE (FoldBlocksApplyBlockError err)
    Nothing -> lift $ readIORef stateIORef
  where

    protocols :: IORef a -> IORef (Maybe LedgerStateError) -> ExtLedgerCfg -> ExtLedgerState -> LocalNodeClientProtocolsInMode CardanoMode
    protocols stateIORef errorIORef extLedgerCfg extLedgerState =
        LocalNodeClientProtocols {
          localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50 stateIORef errorIORef extLedgerCfg extLedgerState),
          localTxSubmissionClient = Nothing,
          localStateQueryClient   = Nothing,
          localTxMonitoringClient = Nothing
        }

    -- | Defines the client side of the chain sync protocol.
    chainSyncClient :: Word32
                    -- ^ The maximum number of concurrent requests.
                    -> IORef a
                    -> IORef (Maybe LedgerStateError)
                    -- ^ Resulting error if any. Written to once on protocol
                    -- completion.
                    -> ExtLedgerCfg
                    -> ExtLedgerState
                    -> CSP.ChainSyncClientPipelined
                        (BlockInMode CardanoMode)
                        ChainPoint
                        ChainTip
                        IO ()
                    -- ^ Client returns maybe an error.
    chainSyncClient pipelineSize stateIORef errorIORef extLedgerCfg extLedgerState0
      = CSP.ChainSyncClientPipelined $ pure $ clientIdle_RequestMoreN Origin Origin Zero initialExtLedgerStateHistory
      where
          initialExtLedgerStateHistory = Seq.singleton (0, extLedgerState0, Origin)

          clientIdle_RequestMoreN
            :: WithOrigin BlockNo
            -> WithOrigin BlockNo
            -> Nat n -- Number of requests inflight.
            -> ExtLedgerStateHistory
            -> CSP.ClientPipelinedStIdle n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientIdle_RequestMoreN clientTip serverTip n knownExtLedgerStates
            = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
                Collect -> case n of
                  Succ predN -> CSP.CollectResponse Nothing (clientNextN predN knownExtLedgerStates)
                _ -> CSP.SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownExtLedgerStates)

          clientNextN
            :: Nat n -- Number of requests inflight.
            -> ExtLedgerStateHistory
            -> CSP.ClientStNext n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientNextN n knownExtLedgerStates =
            CSP.ClientStNext {
                CSP.recvMsgRollForward = \blockInMode@(BlockInMode block@(Block (BlockHeader slotNo _ currBlockNo) _) _era) serverChainTip -> do
                  let newExtLedgerStateE = applyBlock
                        extLedgerCfg
                        (maybe
                          (error "Impossible! Missing Ledger state")
                          (\(_, extLedgerState, _) -> extLedgerState)
                          (Seq.lookup 0 knownExtLedgerStates)
                        )
                        validationMode
                        block
                  case newExtLedgerStateE of
                    Left err -> clientIdle_DoneN n (Just err)
                    Right newExtLedgerState -> do
                      let (knownExtLedgerStates', committedStates) = pushExtLedgerState extLedgerCfg knownExtLedgerStates slotNo newExtLedgerState blockInMode
                          newClientTip = At currBlockNo
                          newServerTip = fromChainTip serverChainTip
                      forM_ committedStates $ \(_, extLedgerState, currBlockMay) -> case currBlockMay of
                          Origin -> return ()
                          At currBlock -> do
                            newState <- accumulate
                              extLedgerCfg
                              extLedgerState
                              currBlock
                              =<< readIORef stateIORef
                            writeIORef stateIORef newState
                      if newClientTip == newServerTip
                        then  clientIdle_DoneN n Nothing
                        else return (clientIdle_RequestMoreN newClientTip newServerTip n knownExtLedgerStates')
              , CSP.recvMsgRollBackward = \chainPoint serverChainTip -> do
                  let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                      newServerTip = fromChainTip serverChainTip
                      truncatedKnownExtLedgerStates = case chainPoint of
                          ChainPointAtGenesis -> initialExtLedgerStateHistory
                          ChainPoint slotNo _ -> rollBackExtLedgerStateHist knownExtLedgerStates slotNo
                  return (clientIdle_RequestMoreN newClientTip newServerTip n truncatedKnownExtLedgerStates)
              }

          clientIdle_DoneN
            :: Nat n -- Number of requests inflight.
            -> Maybe LedgerStateError -- Return value (maybe an error)
            -> IO (CSP.ClientPipelinedStIdle n (BlockInMode CardanoMode) ChainPoint ChainTip IO ())
          clientIdle_DoneN n errorMay = case n of
            Succ predN -> return (CSP.CollectResponse Nothing (clientNext_DoneN predN errorMay)) -- Ignore remaining message responses
            Zero -> do
              writeIORef errorIORef errorMay
              return (CSP.SendMsgDone ())

          clientNext_DoneN
            :: Nat n -- Number of requests inflight.
            -> Maybe LedgerStateError -- Return value (maybe an error)
            -> CSP.ClientStNext n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientNext_DoneN n errorMay =
            CSP.ClientStNext {
                CSP.recvMsgRollForward = \_ _ -> clientIdle_DoneN n errorMay
              , CSP.recvMsgRollBackward = \_ _ -> clientIdle_DoneN n errorMay
              }

          fromChainTip :: ChainTip -> WithOrigin BlockNo
          fromChainTip ct = case ct of
            ChainTipAtGenesis -> Origin
            ChainTip _ _ bno -> At bno

-- | Wrap a 'ChainSyncClient' with logic that tracks the ledger state.
chainSyncClientWithExtLedgerState
  :: forall m a.
     Monad m
  => ExtLedgerCfg
  -> ExtLedgerState
  -- ^ Initial ledger state
  -> ValidationMode
  -> CS.ChainSyncClient (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState)
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client to wrap. The block is annotated with a 'Either LedgerStateError
  -- ExtLedgerState'. This is either an error from validating a block or
  -- the current 'ExtLedgerState' from applying the current block. If we
  -- trust the node, then we generally expect blocks to validate. Also note that
  -- after a block fails to validate we may still roll back to a validated
  -- block, in which case the valid 'ExtLedgerState' will be passed here again.
  -> CS.ChainSyncClient (BlockInMode CardanoMode)
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client that acts just like the wrapped client but doesn't require the
  -- 'ExtLedgerState' annotation on the block type.
chainSyncClientWithExtLedgerState extLedgerCfg ledgerState0 validationMode (CS.ChainSyncClient clientTop)
  = CS.ChainSyncClient (goClientStIdle (Right initialExtLedgerStateHistory) <$> clientTop)
  where
    goClientStIdle
      :: Either LedgerStateError (History (Either LedgerStateError ExtLedgerState))
      -> CS.ClientStIdle (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState) ChainPoint ChainTip m a
      -> CS.ClientStIdle (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientStIdle history client = case client of
      CS.SendMsgRequestNext a b -> CS.SendMsgRequestNext (goClientStNext history a) (goClientStNext history <$> b)
      CS.SendMsgFindIntersect ps a -> CS.SendMsgFindIntersect ps (goClientStIntersect history a)
      CS.SendMsgDone a -> CS.SendMsgDone a

    -- This is where the magic happens. We intercept the blocks and rollbacks
    -- and use it to maintain the correct ledger state.
    goClientStNext
      :: Either LedgerStateError (History (Either LedgerStateError ExtLedgerState))
      -> CS.ClientStNext (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState) ChainPoint ChainTip m a
      -> CS.ClientStNext (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientStNext (Left err) (CS.ClientStNext recvMsgRollForward recvMsgRollBackward) = CS.ClientStNext
      (\blkInMode tip -> CS.ChainSyncClient $
            goClientStIdle (Left err) <$> CS.runChainSyncClient
                (recvMsgRollForward (blkInMode, Left err) tip)
      )
      (\point tip -> CS.ChainSyncClient $
            goClientStIdle (Left err) <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )
    goClientStNext (Right history) (CS.ClientStNext recvMsgRollForward recvMsgRollBackward) = CS.ClientStNext
      (\blkInMode@(BlockInMode blk@(Block (BlockHeader slotNo _ _) _) _) tip -> CS.ChainSyncClient $ let
          newExtLedgerStateE = case Seq.lookup 0 history of
            Nothing -> error "Impossible! History should always be non-empty"
            Just (_, Left err, _) -> Left err
            Just (_, Right oldExtLedgerState, _) -> applyBlock
                  extLedgerCfg
                  oldExtLedgerState
                  validationMode
                  blk
          (history', _) = pushExtLedgerState extLedgerCfg history slotNo newExtLedgerStateE blkInMode
          in goClientStIdle (Right history') <$> CS.runChainSyncClient
                (recvMsgRollForward (blkInMode, newExtLedgerStateE) tip)
      )
      (\point tip -> let
          oldestSlot = case history of
            _ Seq.:|> (s, _, _) -> s
            Seq.Empty -> error "Impossible! History should always be non-empty"
          history' = (\h -> if Seq.null h
                              then Left (InvalidRollback oldestSlot point)
                              else Right h)
                  $ case point of
                        ChainPointAtGenesis -> initialExtLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackExtLedgerStateHist history slotNo
        in CS.ChainSyncClient $ goClientStIdle history' <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )

    goClientStIntersect
      :: Either LedgerStateError (History (Either LedgerStateError ExtLedgerState))
      -> CS.ClientStIntersect (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState) ChainPoint ChainTip m a
      -> CS.ClientStIntersect (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientStIntersect history (CS.ClientStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CS.ClientStIntersect
      (\point tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectFound point tip)))
      (\tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectNotFound tip)))

    initialExtLedgerStateHistory :: History (Either LedgerStateError ExtLedgerState)
    initialExtLedgerStateHistory = Seq.singleton (0, Right ledgerState0, Origin)

-- | See 'chainSyncClientWithExtLedgerState'.
chainSyncClientPipelinedWithExtLedgerState
  :: forall m a.
     Monad m
  => ExtLedgerCfg
  -> ExtLedgerState
  -> ValidationMode
  -> CSP.ChainSyncClientPipelined
                        (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState)
                        ChainPoint
                        ChainTip
                        m
                        a
  -> CSP.ChainSyncClientPipelined
                        (BlockInMode CardanoMode)
                        ChainPoint
                        ChainTip
                        m
                        a
chainSyncClientPipelinedWithExtLedgerState extLedgerCfg ledgerState0 validationMode (CSP.ChainSyncClientPipelined clientTop)
  = CSP.ChainSyncClientPipelined (goClientPipelinedStIdle (Right initialExtLedgerStateHistory) Zero <$> clientTop)
  where
    goClientPipelinedStIdle
      :: Either LedgerStateError (History (Either LedgerStateError ExtLedgerState))
      -> Nat n
      -> CSP.ClientPipelinedStIdle n (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState) ChainPoint ChainTip m a
      -> CSP.ClientPipelinedStIdle n (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientPipelinedStIdle history n client = case client of
      CSP.SendMsgRequestNext a b -> CSP.SendMsgRequestNext (goClientStNext history n a) (goClientStNext history n <$> b)
      CSP.SendMsgRequestNextPipelined a ->  CSP.SendMsgRequestNextPipelined (goClientPipelinedStIdle history (Succ n) a)
      CSP.SendMsgFindIntersect ps a -> CSP.SendMsgFindIntersect ps (goClientPipelinedStIntersect history n a)
      CSP.CollectResponse a b -> case n of
        Succ nPrev -> CSP.CollectResponse ((fmap . fmap) (goClientPipelinedStIdle history n) a) (goClientStNext history nPrev b)
      CSP.SendMsgDone a -> CSP.SendMsgDone a

    -- This is where the magic happens. We intercept the blocks and rollbacks
    -- and use it to maintain the correct ledger state.
    goClientStNext
      :: Either LedgerStateError (History (Either LedgerStateError ExtLedgerState))
      -> Nat n
      -> CSP.ClientStNext n (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState) ChainPoint ChainTip m a
      -> CSP.ClientStNext n (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientStNext (Left err) n (CSP.ClientStNext recvMsgRollForward recvMsgRollBackward) = CSP.ClientStNext
      (\blkInMode tip ->
          goClientPipelinedStIdle (Left err) n <$> recvMsgRollForward
            (blkInMode, Left err) tip
      )
      (\point tip ->
          goClientPipelinedStIdle (Left err) n <$> recvMsgRollBackward point tip
      )
    goClientStNext (Right history) n (CSP.ClientStNext recvMsgRollForward recvMsgRollBackward) = CSP.ClientStNext
      (\blkInMode@(BlockInMode blk@(Block (BlockHeader slotNo _ _) _) _) tip -> let
          newExtLedgerStateE = case Seq.lookup 0 history of
            Nothing -> error "Impossible! History should always be non-empty"
            Just (_, Left err, _) -> Left err
            Just (_, Right oldExtLedgerState, _) -> applyBlock
                  extLedgerCfg
                  oldExtLedgerState
                  validationMode
                  blk
          (history', _) = pushExtLedgerState extLedgerCfg history slotNo newExtLedgerStateE blkInMode
        in goClientPipelinedStIdle (Right history') n <$> recvMsgRollForward
              (blkInMode, newExtLedgerStateE) tip
      )
      (\point tip -> let
          oldestSlot = case history of
            _ Seq.:|> (s, _, _) -> s
            Seq.Empty -> error "Impossible! History should always be non-empty"
          history' = (\h -> if Seq.null h
                              then Left (InvalidRollback oldestSlot point)
                              else Right h)
                  $ case point of
                        ChainPointAtGenesis -> initialExtLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackExtLedgerStateHist history slotNo
        in goClientPipelinedStIdle history' n <$> recvMsgRollBackward point tip
      )

    goClientPipelinedStIntersect
      :: Either LedgerStateError (History (Either LedgerStateError ExtLedgerState))
      -> Nat n
      -> CSP.ClientPipelinedStIntersect (BlockInMode CardanoMode, Either LedgerStateError ExtLedgerState) ChainPoint ChainTip m a
      -> CSP.ClientPipelinedStIntersect (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientPipelinedStIntersect history _ (CSP.ClientPipelinedStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CSP.ClientPipelinedStIntersect
      (\point tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectFound point tip)
      (\tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectNotFound tip)

    initialExtLedgerStateHistory :: History (Either LedgerStateError ExtLedgerState)
    initialExtLedgerStateHistory = Seq.singleton (0, Right ledgerState0, Origin)

{- HLINT ignore chainSyncClientPipelinedWithExtLedgerState "Use fmap" -}

-- | A history of k (security parameter) recent ledger states. The head is the
-- most recent item. Elements are:
--
-- * Slot number that a new block occurred
-- * The ledger state and events after applying the new block
-- * The new block
--
type ExtLedgerStateHistory = History ExtLedgerState
type History a = Seq (SlotNo, a, WithOrigin (BlockInMode CardanoMode))

-- | Add a new ledger state to the history
pushExtLedgerState
  :: ExtLedgerCfg       -- ^ Environment used to get the security param, k.
  -> History a          -- ^ History of k items.
  -> SlotNo             -- ^ Slot number of the new item.
  -> a                  -- ^ New item to add to the history
  -> BlockInMode CardanoMode
                        -- ^ The block that (when applied to the previous
                        -- item) resulted in the new item.
  -> (History a, History a)
  -- ^ ( The new history with the new item appended
  --   , Any existing items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
pushExtLedgerState cfg hist ix st block
  = Seq.splitAt
      (fromIntegral $ extLedgerCfgSecurityParam cfg + 1)
      ((ix, st, At block) Seq.:<| hist)

rollBackExtLedgerStateHist :: History a -> SlotNo -> History a
rollBackExtLedgerStateHist hist maxInc = Seq.dropWhileL ((> maxInc) . (\(x,_,_) -> x)) hist

--------------------------------------------------------------------------------
-- Everything below was copied/adapted from db-sync                           --
--------------------------------------------------------------------------------

genesisConfigToExtLedgerCfg
  :: GenesisConfig
  -> Either GenesisConfigError ExtLedgerCfg
genesisConfigToExtLedgerCfg
  -- enp
  genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg _
        | Cardano.Crypto.ProtocolMagic.unProtocolMagicId (Cardano.Chain.Genesis.configProtocolMagicId bCfg) /= Shelley.Spec.sgNetworkMagic (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (Cardano.Crypto.ProtocolMagic.unProtocolMagicId $ Cardano.Chain.Genesis.configProtocolMagicId bCfg)
                , " /= ", textShow (Shelley.Spec.sgNetworkMagic $ scConfig sCfg)
                ]
        | Cardano.Chain.Genesis.gdStartTime (Cardano.Chain.Genesis.configGenesisData bCfg) /= Shelley.Spec.sgSystemStart (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Cardano.Chain.Genesis.gdStartTime $ Cardano.Chain.Genesis.configGenesisData bCfg)
                , " /= ", textShow (Shelley.Spec.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            let
              topLevelConfig = Consensus.pInfoConfig (mkProtocolInfoCardano genCfg)
            in
              Right $ ExtLedgerCfg $ Consensus.ExtLedgerCfg topLevelConfig

readNetworkConfig :: NetworkConfigFile -> ExceptT Text IO NodeConfig
readNetworkConfig (NetworkConfigFile ncf) = do
    ncfg <- (except . parseNodeConfig) =<< readByteString ncf "node"
    return ncfg
      { ncByronGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncByronGenesisFile ncfg)
      , ncShelleyGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncShelleyGenesisFile ncfg)
      , ncAlonzoGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncAlonzoGenesisFile ncfg)
      }

data NodeConfig = NodeConfig
  { ncPBftSignatureThreshold :: !(Maybe Double)
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncAlonzoGenesisFile :: !GenesisFile
  , ncAlonzoGenesisHash :: !GenesisHashAlonzo
  , ncRequiresNetworkMagic :: !Cardano.Crypto.RequiresNetworkMagic
  , ncByronSoftwareVersion :: !Cardano.Chain.Update.SoftwareVersion
  , ncByronProtocolVersion :: !Cardano.Chain.Update.ProtocolVersion

  -- Per-era parameters for the hardfok transitions:
  , ncByronToShelley   :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Shelley.StandardShelley)
  , ncShelleyToAllegra :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Shelley.StandardAllegra)
  , ncAllegraToMary    :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Shelley.StandardMary)
  , ncMaryToAlonzo     :: !Consensus.TriggerHardFork
  , ncAlonzoToBabbage  :: !Consensus.TriggerHardFork
  }

instance FromJSON NodeConfig where
  parseJSON v =
      Aeson.withObject "NodeConfig" parse v
    where
      parse :: Object -> Data.Aeson.Types.Internal.Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ByronGenesisFile")
          <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
          <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
          <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
          <*> fmap GenesisFile (o .: "AlonzoGenesisFile")
          <*> fmap GenesisHashAlonzo (o .: "AlonzoGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseByronSoftwareVersion o
          <*> parseByronProtocolVersion o
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseShelleyHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseAllegraHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseMaryHardForkEpoch o)
          <*> parseAlonzoHardForkEpoch o
          <*> parseBabbageHardForkEpoch o

      parseByronProtocolVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.ProtocolVersion
      parseByronProtocolVersion o =
        Cardano.Chain.Update.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Minor"
          <*> o .: "LastKnownBlockVersion-Alt"

      parseByronSoftwareVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.SoftwareVersion
      parseByronSoftwareVersion o =
        Cardano.Chain.Update.SoftwareVersion
          <$> fmap Cardano.Chain.Update.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseShelleyHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseShelleyHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseAllegraHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseAllegraHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseMaryHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseMaryHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestMaryHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

      parseAlonzoHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseAlonzoHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestAlonzoHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 5 -- Mainnet default
          ]
      parseBabbageHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseBabbageHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestBabbageHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 7 -- Mainnet default
          ]

parseNodeConfig :: ByteString -> Either Text NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> Left $ "Error parsing node config: " <> textShow err
    Right nc -> Right nc

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

mkAdjustPath :: FilePath -> (FilePath -> FilePath)
mkAdjustPath nodeConfigFilePath fp = takeDirectory nodeConfigFilePath </> fp

readByteString :: FilePath -> Text -> ExceptT Text IO ByteString
readByteString fp cfgType = ExceptT $
  catch (Right <$> BS.readFile fp) $ \(_ :: IOException) ->
    return $ Left $ mconcat
      [ "Cannot read the ", cfgType, " configuration file at : ", Text.pack fp ]

initExtLedgerStateVar :: GenesisConfig -> ExtLedgerState
initExtLedgerStateVar genesisConfig = ExtLedgerState
  { clsState = Consensus.pInfoInitLedger protocolInfo
  }
  where
    protocolInfo = mkProtocolInfoCardano genesisConfig

newtype ExtLedgerState = ExtLedgerState
  { clsState :: Consensus.ExtLedgerState
                  (HFC.HardForkBlock
                    (Consensus.CardanoEras Consensus.StandardCrypto))
  }

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano
      !NodeConfig
      !Cardano.Chain.Genesis.Config
      !ShelleyConfig
      !AlonzoGenesis

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(Shelley.Spec.ShelleyGenesis Shelley.StandardShelley)
  , scGenesisHash :: !GenesisHashShelley
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype ExtLedgerStateDir = ExtLedgerStateDir
  {  unExtLedgerStateDir :: FilePath
  } deriving Show

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NetworkConfigFile = NetworkConfigFile
  { unNetworkConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

mkProtocolInfoCardano ::
  GenesisConfig ->
  Consensus.ProtocolInfo
    IO
    (HFC.HardForkBlock
            (Consensus.CardanoEras Consensus.StandardCrypto))
mkProtocolInfoCardano (GenesisCardano dnc byronGenesis shelleyGenesis alonzoGenesis)
  = Consensus.protocolInfoCardano
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = byronGenesis
            , Consensus.byronPbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> ncPBftSignatureThreshold dnc
            , Consensus.byronProtocolVersion = ncByronProtocolVersion dnc
            , Consensus.byronSoftwareVersion = ncByronSoftwareVersion dnc
            , Consensus.byronLeaderCredentials = Nothing
            , Consensus.byronMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedGenesis = scConfig shelleyGenesis
            , Consensus.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , Consensus.shelleyBasedLeaderCredentials = []
            }
          Consensus.ProtocolParamsShelley
            { Consensus.shelleyProtVer = shelleyProtVer dnc
            , Consensus.shelleyMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsAllegra
            { Consensus.allegraProtVer = shelleyProtVer dnc
            , Consensus.allegraMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsMary
            { Consensus.maryProtVer = shelleyProtVer dnc
            , Consensus.maryMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsAlonzo
            { Consensus.alonzoProtVer = shelleyProtVer dnc
            , Consensus.alonzoMaxTxCapacityOverrides  = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsBabbage
            { Consensus.babbageProtVer = shelleyProtVer dnc
            , Consensus.babbageMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          (ncByronToShelley dnc)
          (ncShelleyToAllegra dnc)
          (ncAllegraToMary dnc)
          (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis (ncMaryToAlonzo dnc))
          (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis (ncAlonzoToBabbage dnc))

shelleyPraosNonce :: ShelleyConfig -> Shelley.Spec.Nonce
shelleyPraosNonce sCfg = Shelley.Spec.Nonce (Cardano.Crypto.Hash.Class.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

shelleyProtVer :: NodeConfig -> Shelley.Spec.ProtVer
shelleyProtVer dnc =
  let bver = ncByronProtocolVersion dnc in
  Shelley.Spec.ProtVer
    (fromIntegral $ Cardano.Chain.Update.pvMajor bver)
    (fromIntegral $ Cardano.Chain.Update.pvMinor bver)

readCardanoGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO GenesisConfig
readCardanoGenesisConfig enc =
  GenesisCardano enc
    <$> readByronGenesisConfig enc
    <*> readShelleyGenesisConfig enc
    <*> readAlonzoGenesisConfig enc

readByronGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO Cardano.Chain.Genesis.Config
readByronGenesisConfig enc = do
  let file = unGenesisFile $ ncByronGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither
                $ Cardano.Crypto.Hashing.decodeAbstractHash (unGenesisHashByron $ ncByronGenesisHash enc)
  firstExceptT (NEByronConfig file)
                $ Cardano.Chain.Genesis.mkConfigFromFile (ncRequiresNetworkMagic enc) file genHash

readShelleyGenesisConfig
    :: NodeConfig
    -> ExceptT GenesisConfigError IO ShelleyConfig
readShelleyGenesisConfig enc = do
  let file = unGenesisFile $ ncShelleyGenesisFile enc
  firstExceptT (NEShelleyConfig file . renderShelleyGenesisError)
    $ readShelleyGenesis (GenesisFile file) (ncShelleyGenesisHash enc)

readAlonzoGenesisConfig
    :: NodeConfig
    -> ExceptT GenesisConfigError IO AlonzoGenesis
readAlonzoGenesisConfig enc = do
  let file = unGenesisFile $ ncAlonzoGenesisFile enc
  firstExceptT (NEAlonzoConfig file . renderAlonzoGenesisError)
    $ readAlonzoGenesis (GenesisFile file) (ncAlonzoGenesisHash enc)

readShelleyGenesis
    :: GenesisFile -> GenesisHashShelley
    -> ExceptT ShelleyGenesisError IO ShelleyConfig
readShelleyGenesis (GenesisFile file) expectedGenesisHash = do
    content <- handleIOExceptT (ShelleyGenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashShelley (Cardano.Crypto.Hash.Class.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (ShelleyGenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
    pure $ ShelleyConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT ShelleyGenesisError IO ()
    checkExpectedGenesisHash actual =
      if actual /= expectedGenesisHash
        then left (ShelleyGenesisHashMismatch actual expectedGenesisHash)
        else pure ()

data ShelleyGenesisError
     = ShelleyGenesisReadError !FilePath !Text
     | ShelleyGenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
     | ShelleyGenesisDecodeError !FilePath !Text
     deriving Show

renderShelleyGenesisError :: ShelleyGenesisError -> Text
renderShelleyGenesisError sge =
    case sge of
      ShelleyGenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      ShelleyGenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Shelley genesis file: the actual hash is ", renderHash (unGenesisHashShelley actual)
          , ", but the expected Shelley genesis hash given in the node "
          , "configuration file is ", renderHash (unGenesisHashShelley expected), "."
          ]

      ShelleyGenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

readAlonzoGenesis
    :: GenesisFile -> GenesisHashAlonzo
    -> ExceptT AlonzoGenesisError IO AlonzoGenesis
readAlonzoGenesis (GenesisFile file) expectedGenesisHash = do
    content <- handleIOExceptT (AlonzoGenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashAlonzo (Cardano.Crypto.Hash.Class.hashWith id content)
    checkExpectedGenesisHash genesisHash
    firstExceptT (AlonzoGenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
  where
    checkExpectedGenesisHash :: GenesisHashAlonzo -> ExceptT AlonzoGenesisError IO ()
    checkExpectedGenesisHash actual =
      if actual /= expectedGenesisHash
        then left (AlonzoGenesisHashMismatch actual expectedGenesisHash)
        else pure ()

data AlonzoGenesisError
     = AlonzoGenesisReadError !FilePath !Text
     | AlonzoGenesisHashMismatch !GenesisHashAlonzo !GenesisHashAlonzo -- actual, expected
     | AlonzoGenesisDecodeError !FilePath !Text
     deriving Show

renderAlonzoGenesisError :: AlonzoGenesisError -> Text
renderAlonzoGenesisError sge =
    case sge of
      AlonzoGenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      AlonzoGenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Alonzo genesis file: the actual hash is ", renderHash (unGenesisHashAlonzo actual)
          , ", but the expected Alonzo genesis hash given in the node "
          , "configuration file is ", renderHash (unGenesisHashAlonzo expected), "."
          ]

      AlonzoGenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

renderHash :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString -> Text
renderHash h = Text.decodeUtf8 $ Base16.encode (Cardano.Crypto.Hash.Class.hashToBytes h)

newtype StakeCred
  = StakeCred { _unStakeCred :: Shelley.Spec.Credential 'Shelley.Spec.Staking Consensus.StandardCrypto }
  deriving (Eq, Ord)

newtype ExtLedgerCfg = ExtLedgerCfg
    { getExtLedgerCfg :: Consensus.ExtLedgerCfg (HFC.HardForkBlock (Consensus.CardanoEras Consensus.StandardCrypto))
    }

extLedgerCfgSecurityParam :: ExtLedgerCfg -> Word64
extLedgerCfgSecurityParam (ExtLedgerCfg extLedgerCfg) = k
  where
    Consensus.SecurityParam k
      = HFC.hardForkConsensusConfigK
      $ Consensus.topLevelConfigProtocol
      $ Consensus.getExtLedgerCfg extLedgerCfg

-- The function 'tickThenReapply' does zero validation, so add minimal
-- validation ('blockPrevHash' matches the tip hash of the 'ExtLedgerState'). This
-- was originally for debugging but the check is cheap enough to keep.
applyBlock'
  :: ExtLedgerCfg
  -> ExtLedgerState
  -> ValidationMode
  -> HFC.HardForkBlock (Consensus.CardanoEras Consensus.StandardCrypto)
  -> Either LedgerStateError ExtLedgerState
applyBlock' (ExtLedgerCfg config) oldState validationMode block = do
  let stateOld = clsState oldState
  case validationMode of
    FullValidation -> tickThenApply config block stateOld
    QuickValidation -> tickThenReapplyCheckHash config block stateOld

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenReapplyCheckHash
    :: Consensus.ExtLedgerCfg (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto))
    -> Consensus.CardanoBlock Consensus.StandardCrypto
    -> Consensus.ExtLedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto))
    -> Either LedgerStateError ExtLedgerState
tickThenReapplyCheckHash cfg block lsb =
  if Consensus.blockPrevHash block == Consensus.ledgerTipHash (Consensus.ledgerState lsb)
    then Right . ExtLedgerState . lrResult
          $ Consensus.tickThenReapplyLedgerResult cfg block lsb
    else Left $ ApplyBlockHashMismatch
              $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow (Slot.unSlotNo $ Slot.fromWithOrigin (Slot.SlotNo 0) (Consensus.ledgerTipSlot $ Consensus.ledgerState lsb))
                  , " hash ", renderByteArray (unChainHash (Consensus.ledgerTipHash $ Consensus.ledgerState lsb))
                  , " but block previous hash is "
                  , renderByteArray (unChainHash $ Consensus.blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray (BSS.fromShort . HFC.getOneEraHash $ Ouroboros.Network.Block.blockHash block), "."
                  ]
 where
    renderByteArray :: ByteArrayAccess bin => bin -> Text
    renderByteArray = Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

    unChainHash :: Ouroboros.Network.Block.ChainHash (Consensus.CardanoBlock era) -> ByteString
    unChainHash ch =
      case ch of
        Ouroboros.Network.Block.GenesisHash -> "genesis"
        Ouroboros.Network.Block.BlockHash bh -> BSS.fromShort (HFC.getOneEraHash bh)

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenApply
    :: Consensus.LedgerCfg (Consensus.ExtLedgerState (HFC.HardForkBlock (Consensus.CardanoEras Consensus.StandardCrypto)))
    -> Consensus.CardanoBlock Consensus.StandardCrypto
    -> Consensus.ExtLedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto))
    -> Either LedgerStateError ExtLedgerState
tickThenApply cfg block lsb =
    either (Left . ExtApplyBlockError) (Right . ExtLedgerState . lrResult)
        $ runExcept
        $ Consensus.tickThenApplyLedgerResult cfg block lsb
