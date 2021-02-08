{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
import           System.FilePath ((</>))


-- TODO move this module into cardano-api
import           Cardano.Api
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import           Control.Monad (when)
import           NewApiStuff
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley


main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketDir : _ <- getArgs
  let socketPath = socketDir </> "node.sock"
  (env, ledgerState) <- initialLedgerState configFilePath

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  IPC.connectToLocalNode
    (connectInfo socketPath)
    (protocols env ledgerState)
  where
  connectInfo :: FilePath -> IPC.LocalNodeConnectInfo IPC.CardanoMode
  connectInfo socketPath =
      IPC.LocalNodeConnectInfo {
        IPC.localConsensusModeParams = IPC.CardanoModeParams (IPC.EpochSlots 21600),
        IPC.localNodeNetworkId       = Mainnet,
        IPC.localNodeSocketPath      = socketPath
      }

  protocols :: Env -> LedgerState -> IPC.LocalNodeClientProtocolsInMode IPC.CardanoMode
  protocols env ledgerState =
      IPC.LocalNodeClientProtocols {
        IPC.localChainSyncClient    = IPC.LocalChainSyncClient (chainSyncClient env ledgerState),
        IPC.localTxSubmissionClient = Nothing,
        IPC.localStateQueryClient   = Nothing
      }

type LedgerStateHistory = Seq (SlotNo, LedgerState)

pushLedgerState :: Env -> LedgerStateHistory -> SlotNo -> LedgerState -> LedgerStateHistory
pushLedgerState env hist ix st = Seq.take (fromIntegral $ envSecurityParam env) ((ix, st) Seq.:<| hist)

rollBackLedgerStateHist :: LedgerStateHistory -> SlotNo -> LedgerStateHistory
rollBackLedgerStateHist hist maxInc = Seq.dropWhileL ((> maxInc) . fst) hist

-- | Defines the client side of the chain sync protocol.
chainSyncClient :: Env
                -> LedgerState
                -> ChainSyncClient
                     (IPC.BlockInMode IPC.CardanoMode)
                     ChainPoint
                     ChainTip
                     IO ()
chainSyncClient env ledgerState0 = ChainSyncClient $ clientStIdle (Seq.singleton (0, ledgerState0)) -- TODO is the initial ledger state at slot 0?
  where
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
            recvMsgRollForward = \(IPC.BlockInMode block@(Block (BlockHeader slotNo _ (BlockNo blockNoI)) _) _era) _tip ->
              let
                newLedgerState = applyBlock env (fromMaybe (error "Impossible! Missing Ledger state") . fmap snd $ Seq.lookup 0 knownLedgerStates) block
                knownLedgerStates' = pushLedgerState' knownLedgerStates slotNo newLedgerState
              in ChainSyncClient $ do
                  case newLedgerState of
                    LedgerStateShelley (Shelley.ShelleyLedgerState shelleyTipWO _ _) -> case shelleyTipWO of
                      Origin -> putStrLn "."
                      At (Shelley.ShelleyTip _ _ hash) -> print hash
                    _ -> when (blockNoI `mod` 100 == 0) (print blockNoI)
                  clientStIdle knownLedgerStates'

          , recvMsgRollBackward = \chainPoint _ -> ChainSyncClient $ do
                let truncatedKnownLedgerStates = case chainPoint of
                        ChainPointAtGenesis -> Seq.singleton (0, ledgerState0)
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
