{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import qualified Cardano.Api.IPC as IPC
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Network.TypedProtocol.Pipelined (Nat (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (ChainSyncClientPipelined),
                   ClientPipelinedStIdle (CollectResponse, SendMsgDone, SendMsgRequestNextPipelined),
                   ClientStNext (..))
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import           System.Environment (getArgs)


-- TODO move this module into cardano-api
import           Cardano.Api
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import           Control.Monad (when)
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           NewApiStuff
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley


main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketPath : _ <- getArgs
  blockCount <- foldBlocks
    configFilePath
    socketPath
    (0 :: Int) -- We just use a count of the blocks as the current state
    (\_env
      ledgerState
      (IPC.BlockInMode (Block (BlockHeader _slotNo _blockHeaderHash (BlockNo blockNoI)) _transactions) _era)
      blockCount -> do
        case ledgerState of
            LedgerStateShelley (Shelley.ShelleyLedgerState shelleyTipWO _ _) -> case shelleyTipWO of
              Origin -> putStrLn "."
              At (Shelley.ShelleyTip _ _ hash) -> print hash
            _ -> when (blockNoI `mod` 100 == 0) (print blockNoI)
        return (blockCount + 1)
    )

  putStrLn $ "Processed " ++ show blockCount ++ " blocks"
  return ()

-- Non-pipelined version took: 1h  0m  19s
-- Pipelined version took:        46m  23s

-- | Monadic fold over all blocks and ledger states.
foldBlocks
  :: forall a.
  FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> FilePath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> a
  -- ^ The initial accumulator state.
  -> (Env -> LedgerState -> IPC.BlockInMode IPC.CardanoMode -> a -> IO a)
  -- ^ Accumulator function Takes:
  --  * Environment (this is a constant over the whole fold)
  --  * The current Ledger state (with the current block applied)
  --  * The current Block
  --  * The previous state
  --
  -- And this should return the new state.
  --
  -- Note: This function can safely assume no rollback will occur even though
  -- internally this is implemented with a client protocol that may require
  -- rollback. This is achieved by only calling the accumulator on states/blocks
  -- that are older than the security parameter, k. This has the side effect of
  -- truncating the last k blocks before the node's tip.
  -> IO a
  -- ^ The final state
foldBlocks nodeConfigFilePath socketPath state0 accumulate = do
  (env, ledgerState) <- initialLedgerState nodeConfigFilePath

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  stateIORef <- newIORef state0

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  IPC.connectToLocalNode
    connectInfo
    (protocols stateIORef env ledgerState)

  readIORef stateIORef
  where
  connectInfo :: IPC.LocalNodeConnectInfo IPC.CardanoMode
  connectInfo =
      IPC.LocalNodeConnectInfo {
        IPC.localConsensusModeParams = IPC.CardanoModeParams (IPC.EpochSlots 21600),
        IPC.localNodeNetworkId       = Mainnet,
        IPC.localNodeSocketPath      = socketPath
      }

  protocols :: IORef a -> Env -> LedgerState -> IPC.LocalNodeClientProtocolsInMode IPC.CardanoMode
  protocols stateIORef env ledgerState =
      IPC.LocalNodeClientProtocols {
        IPC.localChainSyncClient    = IPC.LocalChainSyncClientPipelined (chainSyncClient 50 stateIORef env ledgerState),
        IPC.localTxSubmissionClient = Nothing,
        IPC.localStateQueryClient   = Nothing
      }

  -- | Add a new ledger state to the history
  pushLedgerState :: Env -> LedgerStateHistory -> SlotNo -> LedgerState -> IPC.BlockInMode IPC.CardanoMode
    -> (LedgerStateHistory, LedgerStateHistory)
    -- ^ ( The new history with the new state appended
    --   , Any exisiting ledger states that are now past the security parameter
    --      and hence can no longer be rolled back.
    --   )
  pushLedgerState env hist ix st block
    = Seq.splitAt
        (fromIntegral $ envSecurityParam env + 1)
        ((ix, st, At block) Seq.:<| hist)

  rollBackLedgerStateHist :: LedgerStateHistory -> SlotNo -> LedgerStateHistory
  rollBackLedgerStateHist hist maxInc = Seq.dropWhileL ((> maxInc) . (\(x,_,_) -> x)) hist

  -- | Defines the client side of the chain sync protocol.
  chainSyncClient :: Word32
                  -- ^ The maximum number of concurrent requests.IORef a
                  -> IORef a
                  -> Env
                  -> LedgerState
                  -> ChainSyncClientPipelined
                      (IPC.BlockInMode IPC.CardanoMode)
                      ChainPoint
                      ChainTip
                      IO ()
  chainSyncClient pipelineSize stateIORef env ledgerState0
    = ChainSyncClientPipelined $ pure $ clientIdle_RequestMoreN Origin Origin Zero initialLedgerStateHistory
    where
        initialLedgerStateHistory = Seq.singleton (0, ledgerState0, Origin) -- TODO is the initial ledger state at slot 0?

        pushLedgerState' = pushLedgerState env

        clientIdle_RequestMoreN
          :: WithOrigin BlockNo
          -> WithOrigin BlockNo
          -> Nat n
          -> LedgerStateHistory
          -> ClientPipelinedStIdle n (IPC.BlockInMode IPC.CardanoMode) ChainPoint ChainTip IO ()
        clientIdle_RequestMoreN clientTip serverTip n knownLedgerStates
          = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
              Collect -> case n of
                Succ predN -> CollectResponse Nothing (clientNextN predN knownLedgerStates)
              _ -> SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownLedgerStates)

        clientNextN
          :: Nat n
          -> LedgerStateHistory
          -> ClientStNext n (IPC.BlockInMode IPC.CardanoMode) ChainPoint ChainTip IO ()
        clientNextN n knownLedgerStates =
          ClientStNext {
              recvMsgRollForward = \blockInMode@(IPC.BlockInMode block@(Block (BlockHeader slotNo _ currBlockNo) _) _era) serverChainTip -> do
                let newLedgerState = applyBlock env (fromMaybe (error "Impossible! Missing Ledger state") . fmap (\(_,x,_) -> x) $ Seq.lookup 0 knownLedgerStates) block
                    (knownLedgerStates', committedStates) = pushLedgerState' knownLedgerStates slotNo newLedgerState blockInMode
                    newClientTip = At currBlockNo
                    newServerTip = fromChainTip serverChainTip
                forM_ committedStates $ \(_, currLedgerState, currBlockMay) -> case currBlockMay of
                    Origin -> return ()
                    At currBlock -> do
                      newState <- accumulate env currLedgerState currBlock =<< readIORef stateIORef
                      writeIORef stateIORef newState
                if newClientTip == newServerTip
                  then  clientIdle_DoneN n
                  else return (clientIdle_RequestMoreN newClientTip newServerTip n knownLedgerStates')
            , recvMsgRollBackward = \chainPoint serverChainTip -> do
                putStrLn "Rollback"
                let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                    newServerTip = fromChainTip serverChainTip
                    truncatedKnownLedgerStates = case chainPoint of
                        ChainPointAtGenesis -> initialLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackLedgerStateHist knownLedgerStates slotNo
                return (clientIdle_RequestMoreN newClientTip newServerTip n truncatedKnownLedgerStates)
            }

        clientIdle_DoneN
          :: Nat n
          -> IO (ClientPipelinedStIdle n (IPC.BlockInMode IPC.CardanoMode) ChainPoint ChainTip IO ())
        clientIdle_DoneN n = case n of
          Succ predN -> do
            putStrLn "Chain Sync: done! (Ignoring remaining responses)"
            return $ CollectResponse Nothing (clientNext_DoneN predN) -- Ignore remaining message responses
          Zero -> do
            putStrLn "Chain Sync: done!"
            return $ SendMsgDone ()

        clientNext_DoneN
          :: Nat n
          -> ClientStNext n (IPC.BlockInMode IPC.CardanoMode) ChainPoint ChainTip IO ()
        clientNext_DoneN n =
          ClientStNext {
              recvMsgRollForward = \_ _ -> clientIdle_DoneN n
            , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
            }

        fromChainTip :: ChainTip -> WithOrigin BlockNo
        fromChainTip ct = case ct of
          ChainTipAtGenesis -> Origin
          ChainTip _ _ bno -> At bno

type LedgerStateHistory = Seq (SlotNo, LedgerState, WithOrigin (IPC.BlockInMode IPC.CardanoMode))
