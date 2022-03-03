{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.LedgerState
  ( -- * Initialization / Accumulation
    Env(..)
  , envSecurityParam
  , LedgerState
      ( ..
      , LedgerStateByron
      , LedgerStateShelley
      , LedgerStateAllegra
      , LedgerStateMary
      , LedgerStateAlonzo
      )
  , initialLedgerState
  , applyBlock
  , ValidationMode(..)
  , applyBlockWithEvents

    -- * Traversing the block chain
  , foldBlocks
  , chainSyncClientWithLedgerState
  , chainSyncClientPipelinedWithLedgerState

   -- * Errors
  , LedgerStateError(..)
  , FoldBlocksError(..)
  , GenesisConfigError(..)
  , InitialLedgerStateError(..)
  , renderLedgerStateError
  , renderFoldBlocksError
  , renderGenesisConfigError
  , renderInitialLedgerStateError

  -- * Leadership schedule
  , LeadershipError(..)
  , constructGlobals
  , currentEpochEligibleLeadershipSlots
  , nextEpochEligibleLeadershipSlots
  )
  where

import           Prelude

import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson
import qualified Data.Aeson.Types as Data.Aeson.Types.Internal
import           Data.Bifunctor
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import           Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Short as BSS
import           Data.Foldable
import           Data.IORef
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.SOP.Strict (NP (..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Sharing (FromSharedCBOR, Interns, Share)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word
import qualified Data.Yaml as Yaml
import           GHC.Records (HasField (..))
import           System.FilePath

import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.IPC (ConsensusModeParams (..),
                   LocalChainSyncClient (LocalChainSyncClientPipelined),
                   LocalNodeClientProtocols (..), LocalNodeClientProtocolsInMode,
                   LocalNodeConnectInfo (..), connectToLocalNode)
import           Cardano.Api.KeysPraos
import           Cardano.Api.LedgerEvent (LedgerEvent, toLedgerEvent)
import           Cardano.Api.Modes (CardanoMode, EpochSlots (..))
import           Cardano.Api.NetworkId (NetworkId (..), NetworkMagic (NetworkMagic))
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query (CurrentEpochState (..), ProtocolState,
                   SerialisedCurrentEpochState (..), decodeCurrentEpochState, decodeProtocolState)
import           Cardano.Binary (FromCBOR)
import qualified Cardano.Chain.Genesis
import qualified Cardano.Chain.Update
import           Cardano.Crypto (ProtocolMagicId (unProtocolMagicId), RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing
import qualified Cardano.Crypto.ProtocolMagic
import qualified Cardano.Crypto.VRF as Crypto
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import qualified Cardano.Ledger.BHeaderView as Ledger
import           Cardano.Ledger.BaseTypes (Globals (..), UnitInterval, (⭒))
import qualified Cardano.Ledger.BaseTypes as Shelley.Spec
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as Shelley.Spec
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Shelley.Spec
import qualified Cardano.Ledger.Shelley.API as ShelleyAPI
import qualified Cardano.Protocol.TPraos.API as TPraos
import qualified Cardano.Ledger.Shelley.Genesis as Shelley.Spec
import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Tick
import           Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo.API as Slot
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import qualified Cardano.Slotting.Slot as Slot
import           Control.State.Transition
import           Network.TypedProtocol.Pipelined (Nat (..))
import qualified Ouroboros.Consensus.Block.Abstract as Consensus
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
import qualified Ouroboros.Consensus.Config as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as HFC
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import           Ouroboros.Consensus.Ledger.Basics (LedgerResult (lrEvents), lrResult)
import qualified Ouroboros.Consensus.Ledger.Extended as Ledger
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import qualified Ouroboros.Consensus.Shelley.Eras as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapLedgerEvent (WrapLedgerEvent))
import qualified Ouroboros.Network.Block
import qualified Ouroboros.Network.Protocol.ChainSync.Client as CS
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined as CSP
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as TPraos

data InitialLedgerStateError
  = ILSEConfigFile Text
  -- ^ Failed to read or parse the network config file.
  | ILSEGenesisFile GenesisConfigError
  -- ^ Failed to read or parse a genesis file linked from the network config file.
  | ILSELedgerConsensusConfig GenesisConfigError
  -- ^ Failed to derive the Ledger or Consensus config.

renderInitialLedgerStateError :: InitialLedgerStateError -> Text
renderInitialLedgerStateError ilse = case ilse of
  ILSEConfigFile err ->
    "Failed to read or parse the network config file: " <> err
  ILSEGenesisFile err ->
    "Failed to read or parse a genesis file linked from the network config file: "
    <> renderGenesisConfigError err
  ILSELedgerConsensusConfig err ->
    "Failed to derive the Ledger or Consensus config: "
    <> renderGenesisConfigError err

data LedgerStateError
  = ApplyBlockHashMismatch Text
  -- ^ When using QuickValidation, the block hash did not match the expected
  -- block hash after applying a new block to the current ledger state.
  | ApplyBlockError (Consensus.HardForkLedgerError (Consensus.CardanoEras Consensus.StandardCrypto))
  -- ^ When using FullValidation, an error occurred when applying a new block
  -- to the current ledger state.
  | InvalidRollback
  -- ^ Encountered a rollback larger than the security parameter.
      SlotNo     -- ^ Oldest known slot number that we can roll back to.
      ChainPoint -- ^ Rollback was attempted to this point.
  deriving (Show)

renderLedgerStateError :: LedgerStateError -> Text
renderLedgerStateError = \case
  ApplyBlockHashMismatch err -> "Applying a block did not result in the expected block hash: " <> err
  ApplyBlockError hardForkLedgerError -> "Applying a block resulted in an error: " <> textShow hardForkLedgerError
  InvalidRollback oldestSupported rollbackPoint ->
      "Encountered a rollback larger than the security parameter. Attempted to roll back to "
      <> textShow rollbackPoint
      <> ", but oldest supported slot is "
      <> textShow oldestSupported

-- | Get the environment and initial ledger state.
initialLedgerState
  :: FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  ->  ExceptT InitialLedgerStateError IO (Env, LedgerState)
  -- ^ The environment and initial ledger state
initialLedgerState networkConfigFile = do
  -- TODO Once support for querying the ledger config is added to the node, we
  -- can remove the networkConfigFile argument and much of the code in this
  -- module.
  config <- withExceptT ILSEConfigFile
                  (readNetworkConfig (NetworkConfigFile networkConfigFile))
  genesisConfig <- withExceptT ILSEGenesisFile (readCardanoGenesisConfig config)
  env <- withExceptT ILSELedgerConsensusConfig (except (genesisConfigToEnv genesisConfig))
  let ledgerState = initLedgerStateVar genesisConfig
  return (env, ledgerState)

-- | Apply a single block to the current ledger state.
applyBlock
  :: Env
  -- ^ The environment returned by @initialLedgerState@
  -> LedgerState
  -- ^ The current ledger state
  -> ValidationMode
  -> Block era
  -- ^ Some block to apply
  -> Either LedgerStateError (LedgerState, [LedgerEvent])
  -- ^ The new ledger state (or an error).
applyBlock env oldState validationMode block
  = applyBlock' env oldState validationMode $ case block of
      ByronBlock byronBlock -> Consensus.BlockByron byronBlock
      ShelleyBlock blockEra shelleyBlock -> case blockEra of
        ShelleyBasedEraShelley -> Consensus.BlockShelley shelleyBlock
        ShelleyBasedEraAllegra -> Consensus.BlockAllegra shelleyBlock
        ShelleyBasedEraMary    -> Consensus.BlockMary shelleyBlock
        ShelleyBasedEraAlonzo  -> Consensus.BlockAlonzo shelleyBlock

pattern LedgerStateByron
  :: Ledger.LedgerState Byron.ByronBlock
  -> LedgerState
pattern LedgerStateByron st <- LedgerState (Consensus.LedgerStateByron st)

pattern LedgerStateShelley
  :: Ledger.LedgerState (Shelley.ShelleyBlock (Shelley.ShelleyEra Shelley.StandardCrypto))
  -> LedgerState
pattern LedgerStateShelley st <- LedgerState  (Consensus.LedgerStateShelley st)

pattern LedgerStateAllegra
  :: Ledger.LedgerState (Shelley.ShelleyBlock (Shelley.AllegraEra Shelley.StandardCrypto))
  -> LedgerState
pattern LedgerStateAllegra st <- LedgerState  (Consensus.LedgerStateAllegra st)

pattern LedgerStateMary
  :: Ledger.LedgerState (Shelley.ShelleyBlock (Shelley.MaryEra Shelley.StandardCrypto))
  -> LedgerState
pattern LedgerStateMary st <- LedgerState  (Consensus.LedgerStateMary st)

pattern LedgerStateAlonzo
  :: Ledger.LedgerState (Shelley.ShelleyBlock (Shelley.AlonzoEra Shelley.StandardCrypto))
  -> LedgerState
pattern LedgerStateAlonzo st <- LedgerState  (Consensus.LedgerStateAlonzo st)

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary
           , LedgerStateAlonzo #-}

data FoldBlocksError
  = FoldBlocksInitialLedgerStateError InitialLedgerStateError
  | FoldBlocksApplyBlockError LedgerStateError

renderFoldBlocksError :: FoldBlocksError -> Text
renderFoldBlocksError fbe = case fbe of
  FoldBlocksInitialLedgerStateError err -> renderInitialLedgerStateError err
  FoldBlocksApplyBlockError err -> "Failed when applying a block: " <> renderLedgerStateError err

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
  -> (Env -> LedgerState -> [LedgerEvent] -> BlockInMode CardanoMode -> a -> IO a)
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

  (env, ledgerState) <- withExceptT FoldBlocksInitialLedgerStateError
                            (initialLedgerState nodeConfigFilePath)

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
        $ envLedgerConfig env

      networkMagic
        = NetworkMagic
        $ unProtocolMagicId
        $ Cardano.Chain.Genesis.gdProtocolMagicId
        $ Cardano.Chain.Genesis.configGenesisData byronConfig

      networkId' = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic -> Testnet networkMagic

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

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
    (protocols stateIORef errorIORef env ledgerState)

  lift (readIORef errorIORef) >>= \case
    Just err -> throwE (FoldBlocksApplyBlockError err)
    Nothing -> lift $ readIORef stateIORef
  where

    protocols :: IORef a -> IORef (Maybe LedgerStateError) -> Env -> LedgerState -> LocalNodeClientProtocolsInMode CardanoMode
    protocols stateIORef errorIORef env ledgerState =
        LocalNodeClientProtocols {
          localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50 stateIORef errorIORef env ledgerState),
          localTxSubmissionClient = Nothing,
          localStateQueryClient   = Nothing
        }

    -- | Defines the client side of the chain sync protocol.
    chainSyncClient :: Word32
                    -- ^ The maximum number of concurrent requests.
                    -> IORef a
                    -> IORef (Maybe LedgerStateError)
                    -- ^ Resulting error if any. Written to once on protocol
                    -- completion.
                    -> Env
                    -> LedgerState
                    -> CSP.ChainSyncClientPipelined
                        (BlockInMode CardanoMode)
                        ChainPoint
                        ChainTip
                        IO ()
                    -- ^ Client returns maybe an error.
    chainSyncClient pipelineSize stateIORef errorIORef env ledgerState0
      = CSP.ChainSyncClientPipelined $ pure $ clientIdle_RequestMoreN Origin Origin Zero initialLedgerStateHistory
      where
          initialLedgerStateHistory = Seq.singleton (0, (ledgerState0, []), Origin)

          clientIdle_RequestMoreN
            :: WithOrigin BlockNo
            -> WithOrigin BlockNo
            -> Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> CSP.ClientPipelinedStIdle n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientIdle_RequestMoreN clientTip serverTip n knownLedgerStates
            = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
                Collect -> case n of
                  Succ predN -> CSP.CollectResponse Nothing (clientNextN predN knownLedgerStates)
                _ -> CSP.SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownLedgerStates)

          clientNextN
            :: Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> CSP.ClientStNext n (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
          clientNextN n knownLedgerStates =
            CSP.ClientStNext {
                CSP.recvMsgRollForward = \blockInMode@(BlockInMode block@(Block (BlockHeader slotNo _ currBlockNo) _) _era) serverChainTip -> do
                  let newLedgerStateE = applyBlock
                        env
                        (maybe
                          (error "Impossible! Missing Ledger state")
                          (\(_,(ledgerState, _),_) -> ledgerState)
                          (Seq.lookup 0 knownLedgerStates)
                        )
                        validationMode
                        block
                  case newLedgerStateE of
                    Left err -> clientIdle_DoneN n (Just err)
                    Right newLedgerState -> do
                      let (knownLedgerStates', committedStates) = pushLedgerState env knownLedgerStates slotNo newLedgerState blockInMode
                          newClientTip = At currBlockNo
                          newServerTip = fromChainTip serverChainTip
                      forM_ committedStates $ \(_, (ledgerState, ledgerEvents), currBlockMay) -> case currBlockMay of
                          Origin -> return ()
                          At currBlock -> do
                            newState <- accumulate
                              env
                              ledgerState
                              ledgerEvents
                              currBlock
                              =<< readIORef stateIORef
                            writeIORef stateIORef newState
                      if newClientTip == newServerTip
                        then  clientIdle_DoneN n Nothing
                        else return (clientIdle_RequestMoreN newClientTip newServerTip n knownLedgerStates')
              , CSP.recvMsgRollBackward = \chainPoint serverChainTip -> do
                  let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                      newServerTip = fromChainTip serverChainTip
                      truncatedKnownLedgerStates = case chainPoint of
                          ChainPointAtGenesis -> initialLedgerStateHistory
                          ChainPoint slotNo _ -> rollBackLedgerStateHist knownLedgerStates slotNo
                  return (clientIdle_RequestMoreN newClientTip newServerTip n truncatedKnownLedgerStates)
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
chainSyncClientWithLedgerState
  :: forall m a.
     Monad m
  => Env
  -> LedgerState
  -- ^ Initial ledger state
  -> ValidationMode
  -> CS.ChainSyncClient (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client to wrap. The block is annotated with a 'Either LedgerStateError
  -- LedgerState'. This is either an error from validating a block or
  -- the current 'LedgerState' from applying the current block. If we
  -- trust the node, then we generally expect blocks to validate. Also note that
  -- after a block fails to validate we may still roll back to a validated
  -- block, in which case the valid 'LedgerState' will be passed here again.
  -> CS.ChainSyncClient (BlockInMode CardanoMode)
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client that acts just like the wrapped client but doesn't require the
  -- 'LedgerState' annotation on the block type.
chainSyncClientWithLedgerState env ledgerState0 validationMode (CS.ChainSyncClient clientTop)
  = CS.ChainSyncClient (goClientStIdle (Right initialLedgerStateHistory) <$> clientTop)
  where
    goClientStIdle
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> CS.ClientStIdle (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CS.ClientStIdle (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientStIdle history client = case client of
      CS.SendMsgRequestNext a b -> CS.SendMsgRequestNext (goClientStNext history a) (goClientStNext history <$> b)
      CS.SendMsgFindIntersect ps a -> CS.SendMsgFindIntersect ps (goClientStIntersect history a)
      CS.SendMsgDone a -> CS.SendMsgDone a

    -- This is where the magic happens. We intercept the blocks and rollbacks
    -- and use it to maintain the correct ledger state.
    goClientStNext
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> CS.ClientStNext (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
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
          newLedgerStateE = case Seq.lookup 0 history of
            Nothing -> error "Impossilbe! History should always be non-empty"
            Just (_, Left err, _) -> Left err
            Just (_, Right (oldLedgerState, _), _) -> applyBlock
                  env
                  oldLedgerState
                  validationMode
                  blk
          (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
          in goClientStIdle (Right history') <$> CS.runChainSyncClient
                (recvMsgRollForward (blkInMode, newLedgerStateE) tip)
      )
      (\point tip -> let
          oldestSlot = case history of
            _ Seq.:|> (s, _, _) -> s
            Seq.Empty -> error "Impossilbe! History should always be non-empty"
          history' = (\h -> if Seq.null h
                              then Left (InvalidRollback oldestSlot point)
                              else Right h)
                  $ case point of
                        ChainPointAtGenesis -> initialLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
        in CS.ChainSyncClient $ goClientStIdle history' <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )

    goClientStIntersect
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> CS.ClientStIntersect (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CS.ClientStIntersect (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientStIntersect history (CS.ClientStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CS.ClientStIntersect
      (\point tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectFound point tip)))
      (\tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectNotFound tip)))

    initialLedgerStateHistory :: History (Either LedgerStateError LedgerStateEvents)
    initialLedgerStateHistory = Seq.singleton (0, Right (ledgerState0, []), Origin)

-- | See 'chainSyncClientWithLedgerState'.
chainSyncClientPipelinedWithLedgerState
  :: forall m a.
     Monad m
  => Env
  -> LedgerState
  -> ValidationMode
  -> CSP.ChainSyncClientPipelined
                        (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
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
chainSyncClientPipelinedWithLedgerState env ledgerState0 validationMode (CSP.ChainSyncClientPipelined clientTop)
  = CSP.ChainSyncClientPipelined (goClientPipelinedStIdle (Right initialLedgerStateHistory) Zero <$> clientTop)
  where
    goClientPipelinedStIdle
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> Nat n
      -> CSP.ClientPipelinedStIdle n (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
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
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> Nat n
      -> CSP.ClientStNext n (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
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
          newLedgerStateE = case Seq.lookup 0 history of
            Nothing -> error "Impossilbe! History should always be non-empty"
            Just (_, Left err, _) -> Left err
            Just (_, Right (oldLedgerState, _), _) -> applyBlock
                  env
                  oldLedgerState
                  validationMode
                  blk
          (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
        in goClientPipelinedStIdle (Right history') n <$> recvMsgRollForward
              (blkInMode, newLedgerStateE) tip
      )
      (\point tip -> let
          oldestSlot = case history of
            _ Seq.:|> (s, _, _) -> s
            Seq.Empty -> error "Impossilbe! History should always be non-empty"
          history' = (\h -> if Seq.null h
                              then Left (InvalidRollback oldestSlot point)
                              else Right h)
                  $ case point of
                        ChainPointAtGenesis -> initialLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
        in goClientPipelinedStIdle history' n <$> recvMsgRollBackward point tip
      )

    goClientPipelinedStIntersect
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> Nat n
      -> CSP.ClientPipelinedStIntersect (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CSP.ClientPipelinedStIntersect (BlockInMode CardanoMode                                                      ) ChainPoint ChainTip m a
    goClientPipelinedStIntersect history _ (CSP.ClientPipelinedStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CSP.ClientPipelinedStIntersect
      (\point tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectFound point tip)
      (\tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectNotFound tip)

    initialLedgerStateHistory :: History (Either LedgerStateError LedgerStateEvents)
    initialLedgerStateHistory = Seq.singleton (0, Right (ledgerState0, []), Origin)

{- HLINT ignore chainSyncClientPipelinedWithLedgerState "Use fmap" -}

-- | A history of k (security parameter) recent ledger states. The head is the
-- most recent item. Elements are:
--
-- * Slot number that a new block occurred
-- * The ledger state and events after applying the new block
-- * The new block
--
type LedgerStateHistory = History LedgerStateEvents
type History a = Seq (SlotNo, a, WithOrigin (BlockInMode CardanoMode))

-- | Add a new ledger state to the history
pushLedgerState
  :: Env                -- ^ Environement used to get the security param, k.
  -> History a          -- ^ History of k items.
  -> SlotNo             -- ^ Slot number of the new item.
  -> a                  -- ^ New item to add to the history
  -> BlockInMode CardanoMode
                        -- ^ The block that (when applied to the previous
                        -- item) resulted in the new item.
  -> (History a, History a)
  -- ^ ( The new history with the new item appended
  --   , Any exisiting items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
pushLedgerState env hist ix st block
  = Seq.splitAt
      (fromIntegral $ envSecurityParam env + 1)
      ((ix, st, At block) Seq.:<| hist)

rollBackLedgerStateHist :: History a -> SlotNo -> History a
rollBackLedgerStateHist hist maxInc = Seq.dropWhileL ((> maxInc) . (\(x,_,_) -> x)) hist

--------------------------------------------------------------------------------
-- Everything below was copied/adapted from db-sync                           --
--------------------------------------------------------------------------------

genesisConfigToEnv
  :: GenesisConfig
  -> Either GenesisConfigError Env
genesisConfigToEnv
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
            Right $ Env
                  { envLedgerConfig = Consensus.topLevelConfigLedger topLevelConfig
                  , envProtocolConfig = Consensus.topLevelConfigProtocol topLevelConfig
                  }

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

initLedgerStateVar :: GenesisConfig -> LedgerState
initLedgerStateVar genesisConfig = LedgerState
  { clsState = Ledger.ledgerState $ Consensus.pInfoInitLedger protocolInfo
  }
  where
    protocolInfo = mkProtocolInfoCardano genesisConfig

newtype LedgerState = LedgerState
  { clsState :: Ledger.LedgerState
                  (HFC.HardForkBlock
                    (Consensus.CardanoEras Consensus.StandardCrypto))
  }

type LedgerStateEvents = (LedgerState, [LedgerEvent])

toLedgerStateEvents ::
  LedgerResult
    ( Shelley.LedgerState
        (HFC.HardForkBlock (Consensus.CardanoEras Shelley.StandardCrypto))
    )
    ( Shelley.LedgerState
        (HFC.HardForkBlock (Consensus.CardanoEras Shelley.StandardCrypto))
    ) ->
  LedgerStateEvents
toLedgerStateEvents lr = (ledgerState, ledgerEvents)
  where
    ledgerState = LedgerState (lrResult lr)
    ledgerEvents = mapMaybe (toLedgerEvent
      . WrapLedgerEvent @(HFC.HardForkBlock (Consensus.CardanoEras Shelley.StandardCrypto)))
      $ lrEvents lr


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

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
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
          (ncByronToShelley dnc)
          (ncShelleyToAllegra dnc)
          (ncAllegraToMary dnc)
          (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis (ncMaryToAlonzo dnc))

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

data GenesisConfigError
  = NEError !Text
  | NEByronConfig !FilePath !Cardano.Chain.Genesis.ConfigurationError
  | NEShelleyConfig !FilePath !Text
  | NEAlonzoConfig !FilePath !Text
  | NECardanoConfig !Text

renderGenesisConfigError :: GenesisConfigError -> Text
renderGenesisConfigError ne =
  case ne of
    NEError t -> "Error: " <> t
    NEByronConfig fp ce ->
      mconcat
        [ "Failed reading Byron genesis file ", textShow fp, ": ", textShow ce
        ]
    NEShelleyConfig fp txt ->
      mconcat
        [ "Failed reading Shelley genesis file ", textShow fp, ": ", txt
        ]
    NEAlonzoConfig fp txt ->
      mconcat
        [ "Failed reading Alonzo genesis file ", textShow fp, ": ", txt
        ]
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   ", err
        ]

data LookupFail
  = DbLookupBlockHash !ByteString
  | DbLookupBlockId !Word64
  | DbLookupMessage !Text
  | DbLookupTxHash !ByteString
  | DbLookupTxOutPair !ByteString !Word16
  | DbLookupEpochNo !Word64
  | DbLookupSlotNo !Word64
  | DbMetaEmpty
  | DbMetaMultipleRows
  deriving (Eq, Show)

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

textShow :: Show a => a -> Text
textShow = Text.pack . show

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

data Env = Env
  { envLedgerConfig :: HFC.HardForkLedgerConfig (Consensus.CardanoEras Shelley.StandardCrypto)
  , envProtocolConfig :: TPraos.ConsensusConfig (HFC.HardForkProtocol (Consensus.CardanoEras Shelley.StandardCrypto))
  }

envSecurityParam :: Env -> Word64
envSecurityParam env = k
  where
    Consensus.SecurityParam k
      = HFC.hardForkConsensusConfigK
      $ envProtocolConfig env

-- | How to do validation when applying a block to a ledger state.
data ValidationMode
  -- | Do all validation implied by the ledger layer's 'applyBlock`.
  = FullValidation
  -- | Only check that the previous hash from the block matches the head hash of
  -- the ledger state.
  | QuickValidation

-- The function 'tickThenReapply' does zero validation, so add minimal
-- validation ('blockPrevHash' matches the tip hash of the 'LedgerState'). This
-- was originally for debugging but the check is cheap enough to keep.
applyBlock'
  :: Env
  -> LedgerState
  -> ValidationMode
  ->  HFC.HardForkBlock
            (Consensus.CardanoEras Consensus.StandardCrypto)
  -> Either LedgerStateError LedgerStateEvents
applyBlock' env oldState validationMode block = do
  let config = envLedgerConfig env
      stateOld = clsState oldState
  case validationMode of
    FullValidation -> tickThenApply config block stateOld
    QuickValidation -> tickThenReapplyCheckHash config block stateOld

applyBlockWithEvents
  :: Env
  -> LedgerState
  -> Bool
  -- ^ True to validate
  ->  HFC.HardForkBlock
            (Consensus.CardanoEras Consensus.StandardCrypto)
  -> Either LedgerStateError LedgerStateEvents
applyBlockWithEvents env oldState enableValidation block = do
  let config = envLedgerConfig env
      stateOld = clsState oldState
  if enableValidation
    then tickThenApply config block stateOld
    else tickThenReapplyCheckHash config block stateOld

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenReapplyCheckHash
    :: HFC.HardForkLedgerConfig
        (Consensus.CardanoEras Shelley.StandardCrypto)
    -> Consensus.CardanoBlock Consensus.StandardCrypto
    -> Shelley.LedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto))
    -> Either LedgerStateError LedgerStateEvents
tickThenReapplyCheckHash cfg block lsb =
  if Consensus.blockPrevHash block == Ledger.ledgerTipHash lsb
    then Right . toLedgerStateEvents
          $ Ledger.tickThenReapplyLedgerResult cfg block lsb
    else Left $ ApplyBlockHashMismatch $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow
                      $ Slot.unSlotNo
                      $ Slot.fromWithOrigin
                          (Slot.SlotNo 0)
                          (Ledger.ledgerTipSlot lsb)
                  , " hash "
                  , renderByteArray
                      $ unChainHash
                      $ Ledger.ledgerTipHash lsb
                  , " but block previous hash is "
                  , renderByteArray (unChainHash $ Consensus.blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray
                      $ BSS.fromShort
                      $ HFC.getOneEraHash
                      $ Ouroboros.Network.Block.blockHash block
                  , "."
                  ]

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenApply
    :: HFC.HardForkLedgerConfig
        (Consensus.CardanoEras Shelley.StandardCrypto)
    -> Consensus.CardanoBlock Consensus.StandardCrypto
    -> Shelley.LedgerState
        (HFC.HardForkBlock
            (Consensus.CardanoEras Shelley.StandardCrypto))
    -> Either LedgerStateError LedgerStateEvents
tickThenApply cfg block lsb
  = either (Left . ApplyBlockError) (Right . toLedgerStateEvents)
  $ runExcept
  $ Ledger.tickThenApplyLedgerResult cfg block lsb

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

unChainHash :: Ouroboros.Network.Block.ChainHash (Consensus.CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    Ouroboros.Network.Block.GenesisHash -> "genesis"
    Ouroboros.Network.Block.BlockHash bh -> BSS.fromShort (HFC.getOneEraHash bh)

data LeadershipError = LeaderErrDecodeLedgerStateFailure
                     | LeaderErrDecodeProtocolStateFailure
                     | LeaderErrDecodeProtocolEpochStateFailure
                     | LeaderErrGenesisSlot
                     | LeaderErrStakePoolHasNoStake PoolId
                     | LeaderErrStakeDistribUnstable
                         SlotNo
                         -- ^ Current slot
                         SlotNo
                         -- ^ Stable after
                         SlotNo
                         -- ^ Stability window size
                         SlotNo
                         -- ^ Predicted last slot of the epoch
                     | LeaderErrSlotRangeCalculationFailure Text
                     | LeaderErrCandidateNonceStillEvolving
                     deriving Show

instance Error LeadershipError where
  displayError LeaderErrDecodeLedgerStateFailure =
    "Failed to successfully decode ledger state"
  displayError LeaderErrDecodeProtocolStateFailure =
    "Failed to successfully decode protocol state"
  displayError LeaderErrGenesisSlot =
    "Leadership schedule currently cannot be calculated from genesis"
  displayError (LeaderErrStakePoolHasNoStake poolId) =
    "The stake pool: " <> show poolId <> " has no stake"
  displayError LeaderErrDecodeProtocolEpochStateFailure =
    "Failed to successfully decode the current epoch state"
  displayError (LeaderErrStakeDistribUnstable curSlot stableAfterSlot stabWindow predictedLastSlot) =
    "The current stake distribution is currently unstable and therefore we cannot predict " <>
    "the following epoch's leadership schedule. Please wait until : " <> show stableAfterSlot <>
    " before running the leadership-schedule command again. \nCurrent slot: " <> show curSlot <>
    " \nStability window: " <> show stabWindow <>
    " \nCalculated last slot of current epoch: " <> show predictedLastSlot
  displayError (LeaderErrSlotRangeCalculationFailure e) =
    "Error while calculating the slot range: " <> Text.unpack e
  displayError LeaderErrCandidateNonceStillEvolving = "Candidate nonce is still evolving"

nextEpochEligibleLeadershipSlots
  :: HasField "_d" (Core.PParams (ShelleyLedgerEra era)) UnitInterval
  => Ledger.Era (ShelleyLedgerEra era)
  => Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Spec.Credential 'Shelley.Spec.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
  => ShelleyBasedEra era
  -> ShelleyGenesis Shelley.StandardShelley
  -> SerialisedCurrentEpochState era
  -- ^ We need the mark stake distribution in order to predict
  --   the following epoch's leadership schedule
  -> ProtocolState era
  -> PoolId
  -- ^ Potential slot leading stake pool
  -> SigningKey VrfKey
  -- ^ VRF signing key of the stake pool
  -> ProtocolParameters
  -> EpochInfo (Either Text)
  -> (ChainTip, EpochNo)
  -> Either LeadershipError (Set SlotNo)
nextEpochEligibleLeadershipSlots sbe sGen serCurrEpochState ptclState
                 poolid@(StakePoolKeyHash poolHash) (VrfSigningKey vrfSkey) pParams
                 eInfo (cTip, currentEpoch) = do

  (_, currentEpochLastSlot) <- first LeaderErrSlotRangeCalculationFailure
                                 $ Slot.epochInfoRange eInfo currentEpoch

  rOfInterest <- first LeaderErrSlotRangeCalculationFailure
                  $ Slot.epochInfoRange eInfo (currentEpoch + 1)


  -- First we check if we are within 3k/f slots of the end of the current epoch.
  -- Why? Because the stake distribution is stable at this point.
  -- k is the security parameter
  -- f is the active slot coefficient
  let stabilityWindowR :: Rational
      stabilityWindowR = fromIntegral (3 * sgSecurityParam sGen) / Shelley.Spec.unboundRational (sgActiveSlotsCoeff sGen)
      stabilityWindowSlots :: SlotNo
      stabilityWindowSlots = fromIntegral @Word64 $ floor $ fromRational @Double stabilityWindowR
      stableStakeDistribSlot = currentEpochLastSlot - stabilityWindowSlots


  case cTip of
    ChainTipAtGenesis -> Left LeaderErrGenesisSlot
    ChainTip tip _ _ ->
      if tip > stableStakeDistribSlot
      then return ()
      else Left $ LeaderErrStakeDistribUnstable tip stableStakeDistribSlot stabilityWindowSlots currentEpochLastSlot

  -- Let's do a nonce check. The candidate nonce and the evolving nonce should not be equal.
  chainDepState <- first (const LeaderErrDecodeProtocolStateFailure)
                     $ decodeProtocolState ptclState

  -- We need the candidate nonce, the previous epoch's last block header hash
  -- and the extra entropy from the protocol parameters. We then need to combine them
  -- with the (⭒) operator.
  let TPraos.PrtclState _ evolvingNonce candidateNonce = TPraos.csProtocol chainDepState

  when (evolvingNonce == candidateNonce)
   $ Left LeaderErrCandidateNonceStillEvolving

  -- Get the previous epoch's last block header hash nonce
  let Tick.TicknState _ prevEpochLastBlkHeaderHashNonce = TPraos.csTickn chainDepState
      extraEntropy = toLedgerNonce $ protocolParamExtraPraosEntropy pParams
      nextEpochsNonce = candidateNonce ⭒ prevEpochLastBlkHeaderHashNonce ⭒ extraEntropy

  -- Then we get the "mark" snapshot. This snapshot will be used for the next
  -- epoch's leadership schedule.
  CurrentEpochState cEstate <- first (const LeaderErrDecodeProtocolEpochStateFailure)
                                 $ obtainDecodeEpochStateConstraints sbe
                                 $ decodeCurrentEpochState serCurrEpochState

  let markSnapshotPoolDistr = ShelleyAPI.unPoolDistr . ShelleyAPI.calculatePoolDistr . ShelleyAPI._pstakeMark
                                $ obtainIsStandardCrypto sbe $ ShelleyAPI.esSnapshots cEstate


  relativeStake <- maybe (Left $ LeaderErrStakePoolHasNoStake poolid)
                         (Right . ShelleyAPI.individualPoolStake) $ Map.lookup poolHash markSnapshotPoolDistr

  return $ isLeadingSlots sbe rOfInterest nextEpochsNonce pParams vrfSkey relativeStake f
 where
  globals = constructGlobals sGen eInfo pParams

  f :: Shelley.Spec.ActiveSlotCoeff
  f = activeSlotCoeff globals


-- | Return slots a given stake pool operator is leading.
-- See Leader Value Calculation in the Shelley ledger specification.
-- We need the certified natural value from the VRF, active slot coefficient
-- and the stake proportion of the stake pool.
isLeadingSlots
  :: Crypto.Signable v Shelley.Spec.Seed
  => Crypto.VRFAlgorithm v
  => Crypto.ContextVRF v ~ ()
  => HasField "_d" (Core.PParams (ShelleyLedgerEra era)) UnitInterval
  => ShelleyBasedEra era
  -> (SlotNo, SlotNo) -- ^ Slot range of interest
  -> Consensus.Nonce
  -> ProtocolParameters
  -> Crypto.SignKeyVRF v
  -> Rational -- ^ Stake pool relative stake
  -> Shelley.Spec.ActiveSlotCoeff
  -> Set SlotNo
isLeadingSlots sbe (firstSlotOfEpoch, lastSlotofEpoch) eNonce pParams vrfSkey
             stakePoolStake activeSlotCoeff' =
  let certified s = certifiedNaturalValue s eNonce vrfSkey
      pp = toLedgerPParams sbe pParams
      slotRangeOfInterest = Set.fromList [firstSlotOfEpoch .. lastSlotofEpoch]

      isLeader s = not (Ledger.isOverlaySlot firstSlotOfEpoch (getField @"_d" pp) s)
                 && TPraos.checkLeaderValue (certified s)
                                            stakePoolStake activeSlotCoeff'
  in Set.filter isLeader slotRangeOfInterest
 where
  certifiedNaturalValue
    :: Crypto.Signable v Shelley.Spec.Seed
    => Crypto.VRFAlgorithm v
    => Crypto.ContextVRF v ~ ()
    => SlotNo
    -> Consensus.Nonce
    -> Crypto.SignKeyVRF v
    -> Crypto.OutputVRF v
  certifiedNaturalValue slot epochNonce vrfSkey' =
   Crypto.certifiedOutput
   $ Crypto.evalCertified () (TPraos.mkSeed TPraos.seedL slot epochNonce) vrfSkey'

obtainIsStandardCrypto
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (Ledger.Crypto ledgerera ~ Shelley.StandardCrypto => a)
  -> a
obtainIsStandardCrypto ShelleyBasedEraShelley f = f
obtainIsStandardCrypto ShelleyBasedEraAllegra f = f
obtainIsStandardCrypto ShelleyBasedEraMary    f = f
obtainIsStandardCrypto ShelleyBasedEraAlonzo  f = f


obtainDecodeEpochStateConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (( FromCBOR (Core.PParams ledgerera)
      , FromCBOR (State (Core.EraRule "PPUP" ledgerera))
      , FromCBOR (Core.Value ledgerera)
      , FromSharedCBOR (Core.TxOut ledgerera)
      ) => a) -> a
obtainDecodeEpochStateConstraints ShelleyBasedEraShelley f = f
obtainDecodeEpochStateConstraints ShelleyBasedEraAllegra f = f
obtainDecodeEpochStateConstraints ShelleyBasedEraMary    f = f
obtainDecodeEpochStateConstraints ShelleyBasedEraAlonzo  f = f

-- | Return the slots at which a particular stake pool operator is
-- expected to mint a block.
currentEpochEligibleLeadershipSlots
  :: ShelleyLedgerEra era ~ ledgerera
  => Ledger.Era ledgerera
  => HasField "_d" (Core.PParams ledgerera) UnitInterval
  => Crypto.Signable (Crypto.VRF (Ledger.Crypto ledgerera)) Shelley.Spec.Seed
  => Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Shelley.Spec.Credential 'Shelley.Spec.Staking (Ledger.Crypto (ShelleyLedgerEra era)))
  => Ledger.Crypto ledgerera ~ Shelley.StandardCrypto
  => ShelleyBasedEra era
  -> ShelleyGenesis Shelley.StandardShelley
  -> EpochInfo (Either Text)
  -> ProtocolParameters
  -> ProtocolState era
  -> PoolId
  -> SigningKey VrfKey
  -> SerialisedCurrentEpochState era
  -> EpochNo -- ^ Current EpochInfo
  -> Either LeadershipError (Set SlotNo)
currentEpochEligibleLeadershipSlots sbe sGen eInfo pParams ptclState
                        poolid@(StakePoolKeyHash poolHash) (VrfSigningKey vrkSkey)
                        serCurrEpochState currentEpoch = do

  chainDepState <- first (const LeaderErrDecodeProtocolStateFailure)
                     $ decodeProtocolState ptclState

  -- We use the current epoch's nonce for the current leadership schedule
  -- calculation because the TICKN transition updates the epoch nonce
  -- at the start of the epoch.
  let Tick.TicknState epochNonce _ = TPraos.csTickn chainDepState

  currentEpochRange <- first LeaderErrSlotRangeCalculationFailure
                         $ Slot.epochInfoRange eInfo currentEpoch

  CurrentEpochState cEstate <- first (const LeaderErrDecodeProtocolEpochStateFailure)
                                 $ obtainDecodeEpochStateConstraints sbe
                                 $ decodeCurrentEpochState serCurrEpochState

  -- We need the "set" stake distribution (distribution of the previous epoch)
  -- in order to calculate the leadership schedule of the current epoch.
  let setSnapshotPoolDistr = ShelleyAPI.unPoolDistr . ShelleyAPI.calculatePoolDistr
                                . ShelleyAPI._pstakeSet . obtainIsStandardCrypto sbe
                                $ ShelleyAPI.esSnapshots cEstate

  relativeStake <- maybe (Left $ LeaderErrStakePoolHasNoStake poolid)
                         (Right . ShelleyAPI.individualPoolStake)
                         (Map.lookup poolHash setSnapshotPoolDistr)

  Right $ isLeadingSlots sbe currentEpochRange epochNonce pParams vrkSkey relativeStake f

 where
  globals = constructGlobals sGen eInfo pParams

  f :: Shelley.Spec.ActiveSlotCoeff
  f = activeSlotCoeff globals

constructGlobals
  :: ShelleyGenesis Shelley.StandardShelley
  -> EpochInfo (Either Text)
  -> ProtocolParameters
  -> Globals
constructGlobals sGen eInfo pParams =
  let majorPParamsVer = fst $ protocolParamProtocolVersion pParams
  in Shelley.Spec.mkShelleyGlobals sGen eInfo majorPParamsVer
