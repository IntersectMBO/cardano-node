{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import           Cardano.Api
import           Cardano.Api.ChainSync.Client
import qualified Cardano.Chain.Slotting as Byron (EpochSlots (..))
import           Cardano.Slotting.Slot
import qualified Ouroboros.Consensus.Cardano.Block as C
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as C
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Types as C
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as TSP

import           Control.Monad (when)
import           Control.Monad.Trans.Except
import           Data.Kind
import           Data.Proxy
import qualified Data.SOP as SOP
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Word (Word32)
import qualified GHC.TypeLits as GHC
import           System.Environment (getArgs)
import           System.FilePath ((</>))

-- | Connects to a local cardano node, requests the blocks and prints out some
-- information from each ledger state. To run this, you must first start a local
-- node e.g.:
--
--     $ cabal run cardano-node:cardano-node -- run \
--        --config        configuration/cardano/mainnet-config.json \
--        --topology      configuration/cardano/mainnet-topology.json \
--        --database-path db \
--        --socket-path   db/node.sock \
--        --host-addr     127.0.0.1 \
--        --port          3001 \
--
-- Then run this with the path to the config file and the node.sock:
--
--     $ cabal run cardano-client-demo:chain-sync-client-with-ledger-state -- \
--          configuration/cardano/mainnet-config.json \
--          db/node.sock
--
main :: IO ()
main = do
  -- Get config and socket path from CLI argument.
  configFilePath : socketPath : xs <- getArgs
  byronSlotLength <- case xs of
        byronSlotLengthStr : _ -> return (read byronSlotLengthStr)
        _ -> do
          let l = 21600
          putStrLn $ "Using default byron slots per epoch: " <> show l
          return l

  -- Use 'chainSyncClientWithLedgerState' to support ledger state.
  Right (env, initialLedgerState) <- runExceptT $ initialLedgerState configFilePath
  let client = chainSyncClientWithLedgerState
        env
        initialLedgerState
        FullValidation
        chainSyncClient

      protocols :: LocalNodeClientProtocolsInMode CardanoMode
      protocols =
          LocalNodeClientProtocols {
            localChainSyncClient    = LocalChainSyncClient client,
            localTxSubmissionClient = Nothing,
            localStateQueryClient   = Nothing
          }

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  connectToLocalNode
    (connectInfo socketPath)
    protocols
  where
  connectInfo :: FilePath -> LocalNodeConnectInfo CardanoMode
  connectInfo socketPath =
      LocalNodeConnectInfo {
        localConsensusModeParams = CardanoModeParams (Byron.EpochSlots 21600),
        localNodeNetworkId       = Mainnet,
        localNodeSocketPath      = socketPath
      }

-- | Defines the client side of the chain sync protocol.
chainSyncClient
  :: ChainSyncClient
        (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
        ChainPoint
        ChainTip
        IO
        ()
chainSyncClient = ChainSyncClient $ do
  startTime <- getCurrentTime
  let
    clientStIdle :: ClientStIdle (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
                                 ChainPoint ChainTip IO ()
    clientStIdle = SendMsgRequestNext
        clientStNext
        (pure clientStNext)

    clientStNext :: ClientStNext (BlockInMode CardanoMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
                                 ChainPoint ChainTip IO ()
    clientStNext =
      ClientStNext {
          recvMsgRollForward =
            \( BlockInMode block@(Block (BlockHeader _ _ (BlockNo blockNo)) _) _
             , ledgerStateE
             )
             _tip ->
              ChainSyncClient $ case ledgerStateE of
                Left err -> do
                  putStrLn $ "Ledger state error: " <> T.unpack (renderLedgerStateError err)
                  return (SendMsgDone ())
                Right (LedgerState (C.HardForkLedgerState (C.HardForkState ledgerState)), _) -> do
                  when (blockNo `mod` 1000 == 0) $ do
                    printLedgerState ledgerState
                    now <- getCurrentTime
                    let elapsedTime = realToFrac (now `diffUTCTime` startTime) :: Float
                        rate = fromIntegral blockNo / elapsedTime
                    putStrLn $ "Rate = " ++ show rate ++ " blocks/second"
                  return clientStIdle
        , recvMsgRollBackward = \_ _ -> ChainSyncClient $ do
            putStrLn "Rollback!"
            return clientStIdle
        }

    printLedgerState :: TSP.Telescope (SOP.K C.Past) (C.Current C.LedgerState) xs -> IO ()
    printLedgerState ls = case ls of
      TSP.TZ (C.Current bound _) -> putStrLn $ "curren't era bounds: " <> show bound
      TSP.TS _ ls' -> printLedgerState ls'
  return clientStIdle

