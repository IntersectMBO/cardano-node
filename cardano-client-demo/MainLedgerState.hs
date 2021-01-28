{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Cardano.Api (Block (..), BlockHeader (BlockHeader), BlockNo (BlockNo), ByronEra,
                   ChainPoint (..), ChainSyncClient, ChainTip (ChainTip), EraInMode (..), Hash,
                   NetworkId (Mainnet), SlotNo, TxOut (..))
import qualified Cardano.Api.Block as Block
import qualified Cardano.Api.IPC as IPC
import           Cardano.Binary (Raw)
import           Cardano.Chain.Block (BlockValidationMode (..))
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as Byron (EpochSlots (..))
import           Cardano.Chain.UTxO (TxValidationMode (..))
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron

-- TODO: Export this via Cardano.Api
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Network.TypedProtocol.Pipelined (N (..), Nat (..), natToInt, unsafeIntToNat)
import           Ouroboros.Network.Protocol.ChainSync.Client

import           Control.Monad (when)
import           Control.Monad.Except
import           Data.Kind
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Time
import qualified GHC.TypeLits as GHC
import           System.Environment (getArgs)
import           System.FilePath ((</>))


-- TODO move this module into cardano-api
import           Cardano.Chain.Common
import           NewApiStuff

main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketDir : _ <- getArgs
  let socketPath = socketDir </> "node.sock"
  ledgerState <- initialLedgerState configFilePath RequiresMagic

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  IPC.connectToLocalNode
    (connectInfo socketPath)
    (protocols ledgerState)
  where
  connectInfo :: FilePath -> IPC.LocalNodeConnectInfo IPC.CardanoMode
  connectInfo socketPath =
      IPC.LocalNodeConnectInfo {
        IPC.localConsensusModeParams = IPC.CardanoModeParams (IPC.EpochSlots 21600),
        IPC.localNodeNetworkId       = Mainnet,
        IPC.localNodeSocketPath      = socketPath
      }

  protocols :: LedgerState ByronEra -> IPC.LocalNodeClientProtocolsInMode IPC.CardanoMode
  protocols ledgerState =
      IPC.LocalNodeClientProtocols {
        IPC.localChainSyncClient    = Just (Right (chainSyncClient ledgerState)),
        IPC.localTxSubmissionClient = Nothing,
        IPC.localStateQueryClient   = Nothing
      }

-- | Defines the client side of the chain sync protocol.
chainSyncClient :: LedgerState ByronEra
                -> ChainSyncClient
                     (IPC.BlockInMode IPC.CardanoMode)
                     ChainPoint
                     ChainTip
                     IO ()
chainSyncClient initialLedgerState = ChainSyncClient $ clientStIdle Map.empty
  where
      clientStIdle :: Map SlotNo (LedgerState ByronEra) -- Known Ledger states. Must be complete up to and including the most recently received Block's SlotNo.
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

      clientStNext :: Map SlotNo (LedgerState ByronEra) -- ^ Known Ledger states. Must be complete up to the current BlockNo.
                   -> ClientStNext (IPC.BlockInMode IPC.CardanoMode)
                                  ChainPoint ChainTip IO ()
      clientStNext knownLedgerStates =
        ClientStNext {
            recvMsgRollForward = \(IPC.BlockInMode block@(Block (BlockHeader slotNo _ blockNo@(BlockNo blockNoI)) _) era) _tip -> case era of
              ByronEraInCardanoMode -> ChainSyncClient $ do
                let prevLedgerState = maybe initialLedgerState snd  (Map.lookupLT slotNo knownLedgerStates)
                    currLedgerState = applyBlock prevLedgerState block
                    knownLedgerStates' = Map.insert slotNo currLedgerState knownLedgerStates
                when (blockNoI `mod` 1000 == 0) $ do
                  printLedgerState currLedgerState
                clientStIdle knownLedgerStates'

          , recvMsgRollBackward = \chainPoint _ -> ChainSyncClient $ do
                let truncatedKnownLedgerStates = case chainPoint of
                        ChainPointAtGenesis -> Map.empty
                        ChainPoint slotNo _ -> case Map.splitLookup slotNo knownLedgerStates of
                          (x, _, _) -> x
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

      printLedgerState :: LedgerState ByronEra -> IO ()
      printLedgerState _ = putStrLn "TODO printLedgerState"
