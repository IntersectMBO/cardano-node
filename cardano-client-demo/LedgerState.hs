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
import           Ouroboros.Network.Protocol.ChainSync.Client
import           System.Environment (getArgs)


-- TODO move this module into cardano-api
import           Cardano.Api
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import           Control.Monad (when)
import           Data.Foldable
import           Data.IORef
import           NewApiStuff
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley


main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketPath : _ <- getArgs
  () <- foldBlocks
    configFilePath
    socketPath
    ()
    (\_env
      ledgerState
      (IPC.BlockInMode (Block (BlockHeader _slotNo _blockHeaderHash (BlockNo blockNoI)) _transactions) _era)
      () -> case ledgerState of
            LedgerStateShelley (Shelley.ShelleyLedgerState shelleyTipWO _ _) -> case shelleyTipWO of
              Origin -> putStrLn "."
              At (Shelley.ShelleyTip _ _ hash) -> print hash
            _ -> when (blockNoI `mod` 100 == 0) (print blockNoI)
    )
  return ()



-- | Monadic fold over all blocks and ledger states.
foldBlocks
  :: forall a.
  FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> FilePath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> a
  -- ^ Initial state
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
        IPC.localChainSyncClient    = IPC.LocalChainSyncClient (chainSyncClient stateIORef env ledgerState),
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
  chainSyncClient :: IORef a
                  -> Env
                  -> LedgerState
                  -> ChainSyncClient
                      (IPC.BlockInMode IPC.CardanoMode)
                      ChainPoint
                      ChainTip
                      IO ()
  chainSyncClient stateIORef env ledgerState0 = ChainSyncClient $ clientStIdle initialLedgerStateHistory -- TODO is the initial ledger state at slot 0?
    where
        initialLedgerStateHistory = Seq.singleton (0, ledgerState0, Origin)

        pushLedgerState' = pushLedgerState env
        clientStIdle :: LedgerStateHistory -- Known Ledger states. Must be complete up to and including the most recently received Block's SlotNo.
                    -> IO (ClientStIdle (IPC.BlockInMode IPC.CardanoMode)
                                    ChainPoint ChainTip IO ())
        clientStIdle knownLedgerStates = do
          -- putStrLn "Chain Sync: requesting next"
          return $ SendMsgRequestNext
            -- There's more to get immediately
            (clientStNext knownLedgerStates)

            -- The node is asking us to wait. This is because we reached the
            -- tip. We can certainly carry on here, but for this demo we are
            -- going to stop when we hit the current chain tip.
            clientDone

        clientStNext :: LedgerStateHistory -- ^ Known Ledger states. Must be complete up to the current BlockNo.
                    -> ClientStNext (IPC.BlockInMode IPC.CardanoMode)
                                    ChainPoint ChainTip IO ()
        clientStNext knownLedgerStates =
          ClientStNext {
              recvMsgRollForward = \blockInMode@(IPC.BlockInMode block@(Block (BlockHeader slotNo _ _) _) _era) _tip ->
                let
                  newLedgerState = applyBlock env (fromMaybe (error "Impossible! Missing Ledger state") . fmap (\(_,x,_) -> x) $ Seq.lookup 0 knownLedgerStates) block
                  (knownLedgerStates', committedStates) = pushLedgerState' knownLedgerStates slotNo newLedgerState blockInMode
                in ChainSyncClient $ do
                    forM_ committedStates $ \(_, currLedgerState, currBlockMay) -> case currBlockMay of
                        Origin -> return ()
                        At currBlock -> do
                          newState <- accumulate env currLedgerState currBlock =<< readIORef stateIORef
                          writeIORef stateIORef newState
                    clientStIdle knownLedgerStates'

            , recvMsgRollBackward = \chainPoint _ -> ChainSyncClient $ do
                  let truncatedKnownLedgerStates = case chainPoint of
                          ChainPointAtGenesis -> initialLedgerStateHistory
                          ChainPoint slotNo _ -> rollBackLedgerStateHist knownLedgerStates slotNo
                  clientStIdle truncatedKnownLedgerStates
            }

        -- We're still in the "Next" state here, but we've decided to stop
        -- as soon as we get the reply, no matter which reply.
        clientDone :: IO (ClientStNext (IPC.BlockInMode IPC.CardanoMode)
                                    ChainPoint ChainTip IO ())
        clientDone = do
          putStrLn "Chain Sync: done!"
          return $ ClientStNext {
            recvMsgRollForward  = \_ _ -> ChainSyncClient (pure (SendMsgDone ())),
            recvMsgRollBackward = \_ _ -> ChainSyncClient (pure (SendMsgDone ()))
          }

type LedgerStateHistory = Seq (SlotNo, LedgerState, WithOrigin (IPC.BlockInMode IPC.CardanoMode))
