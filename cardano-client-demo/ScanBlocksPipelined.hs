{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Cardano.Api
import           Cardano.Api.ChainSync.ClientPipelined
import qualified Cardano.Chain.Slotting as Byron (EpochSlots (..))
import           Cardano.Slotting.Slot

import           Control.Monad (when)
import           Data.Kind
import           Data.Proxy
import           Data.Type.Queue
import           Data.Type.Nat
import           Data.Time
import           Data.Word (Word32)
import qualified GHC.TypeLits as GHC
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined

-- | Connects to a local cardano node, requests the blocks and prints out the
-- number of transactions. To run this, you must first start a local node e.g.:
--
--     $ cabal run cardano-node:cardano-node -- run \
--        --config        configuration/cardano/mainnet-config.json \
--        --topology      configuration/cardano/mainnet-topology.json \
--        --database-path db \
--        --socket-path   db/node.sock \
--        --host-addr     127.0.0.1 \
--        --port          3001 \
--
-- Then run this with the path to the directory containing node.sock:
--
--     $ cabal run cardano-client-demo:scan-blocks-pipelined -- db
--
main :: IO ()
main = do
  -- Get cocket path from CLI argument.
  socketDir:_ <- getArgs
  let socketPath = socketDir </> "node.sock"

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

  protocols :: LocalNodeClientProtocolsInMode CardanoMode
  protocols =
      LocalNodeClientProtocols {
        localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50),
        localTxSubmissionClient = Nothing,
        localStateQueryClient   = Nothing,
        localTxMonitoringClient = Nothing
      }


-- | Defines the pipelined client side of the chain sync protocol.
chainSyncClient
  :: Word32
  -- ^ The maximum number of concurrent requests.
  -> ChainSyncClientPipelined
        (BlockInMode CardanoMode)
        ChainPoint
        ChainTip
        IO
        ()
chainSyncClient pipelineSize = ChainSyncClientPipelined $ do
  startTime <- getCurrentTime
  let
    clientIdle_RequestMoreN :: WithOrigin BlockNo -> WithOrigin BlockNo
                            -> SingQueueF F q -> ClientPipelinedStIdle (BlockInMode CardanoMode)
                                              ChainPoint ChainTip q IO ()
    clientIdle_RequestMoreN clientTip serverTip q = case pipelineDecisionMax pipelineSize (queueFDepthNat q) clientTip serverTip  of
      Collect -> case q of
        SingConsF FCanAwait q' -> CollectResponse Nothing (clientNextN q')
        SingConsF FMustReply q' -> CollectResponse Nothing (clientNextN q')
      _ -> SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (q |> FCanAwait))

    clientNextN :: SingQueueF F q -> ClientStNext (BlockInMode CardanoMode)
                                     ChainPoint ChainTip q IO ()
    clientNextN q =
      ClientStNext {
          recvMsgRollForward = \(BlockInMode block@(Block (BlockHeader _ _ currBlockNo@(BlockNo blockNo)) _) _) serverChainTip -> do
            let newClientTip = At currBlockNo
                newServerTip = fromChainTip serverChainTip
            when (blockNo `mod` 1000 == 0) $ do
              printBlock block
              now <- getCurrentTime
              let elapsedTime = realToFrac (now `diffUTCTime` startTime) :: Double
                  rate = fromIntegral blockNo / elapsedTime
              putStrLn $ "Rate = " ++ show rate ++ " blocks/second"
            if newClientTip == newServerTip
              then  clientIdle_DoneN q
              else return (clientIdle_RequestMoreN newClientTip newServerTip q)
        , recvMsgRollBackward = \_ serverChainTip -> do
            putStrLn "Rollback"
            let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                newServerTip = fromChainTip serverChainTip
            return (clientIdle_RequestMoreN newClientTip newServerTip q)
        }

    clientIdle_DoneN :: SingQueueF F q -> IO (ClientPipelinedStIdle (BlockInMode CardanoMode)
                                              ChainPoint ChainTip q IO ())
    clientIdle_DoneN q = case q of
      SingConsF FCanAwait q' -> do
        putStrLn "Chain Sync: done! (Ignoring remaining responses)"
        return $ CollectResponse Nothing (clientNext_DoneN q') -- Ignore remaining message responses
      SingConsF FMustReply q' -> do
        putStrLn "Chain Sync: done! (Ignoring remaining responses)"
        return $ CollectResponse Nothing (clientNext_DoneN q') -- Ignore remaining message responses
      SingEmptyF -> do
        putStrLn "Chain Sync: done!"
        return $ SendMsgDone ()

    clientNext_DoneN :: SingQueueF F q -> ClientStNext (BlockInMode CardanoMode)
                                          ChainPoint ChainTip q IO ()
    clientNext_DoneN n =
      ClientStNext {
          recvMsgRollForward = \_ _ -> clientIdle_DoneN n
        , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
        }

    printBlock :: Block era -> IO ()
    printBlock (Block (BlockHeader _ _ currBlockNo) transactions)
      = putStrLn $ show currBlockNo ++ " transactions: " ++ show (length transactions)

    fromChainTip :: ChainTip -> WithOrigin BlockNo
    fromChainTip ct = case ct of
      ChainTipAtGenesis -> Origin
      ChainTip _ _ bno -> At bno

  return (clientIdle_RequestMoreN Origin Origin SingEmptyF)
