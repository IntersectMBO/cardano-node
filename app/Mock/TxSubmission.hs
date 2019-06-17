{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Mock.TxSubmission (
      command'
    , parseMockTx
    , handleTxSubmission
    , spawnMempoolListener
    ) where

import           Codec.Serialise (decode, hPutSerialise)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Except
import           Control.Tracer
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Options.Applicative
import           System.IO (IOMode (..))

import           Ouroboros.Consensus.Crypto.Hash (ShortHash)
import qualified Ouroboros.Consensus.Crypto.Hash as H
import           Ouroboros.Consensus.Demo.Run
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Node   (NodeKernel (..))
import           Ouroboros.Consensus.Util.CBOR (Decoder (..), initDecoderIO)
import           Ouroboros.Consensus.Util.Condense

import           NamedPipe
import           Topology

{-------------------------------------------------------------------------------
  Parsers for the mock UTxO model
-------------------------------------------------------------------------------}

parseMockTx :: Parser Mock.Tx
parseMockTx = mkTx
    <$> many parseMockTxIn
    <*> many parseMockTxOut
  where
    mkTx :: [Mock.TxIn] -> [Mock.TxOut] -> Mock.Tx
    mkTx ins = Mock.Tx (Set.fromList ins)

parseMockTxIn :: Parser Mock.TxIn
parseMockTxIn = (,)
    <$> strOption (mconcat [
            long "txin"
          , help "Hash of the input transaction. Single hex char."
          ])
    <*> option auto (mconcat [
            long "txix"
          , help "Index of the output in the specified transaction"
          ])

parseMockTxOut :: Parser Mock.TxOut
parseMockTxOut = (,)
    <$> strOption (mconcat [
            long "address"
          , help "Address to transfer to"
          ])
    <*> option auto (mconcat [
            long "amount"
          , help "Amount to transfer"
          ])


{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

handleTxSubmission :: TopologyInfo -> Mock.Tx -> IO ()
handleTxSubmission tinfo tx = do
    topoE <- readTopologyFile (topologyFile tinfo)
    case topoE of
         Left e  -> error e
         Right t ->
             case M.lookup (node tinfo) (toNetworkMap t) of
                  Nothing -> error "Target node not found."
                  Just _  -> submitTx (node tinfo) tx

submitTx :: NodeId -> Mock.Tx -> IO ()
submitTx n tx = do
    withTxPipe n WriteMode False $ \h -> hPutSerialise h tx
    putStrLn $ "The Id for this transaction is: " <> condense (H.hash @ShortHash tx)

-- | Auxiliary to 'spawnMempoolListener'
readIncomingTx :: RunDemo blk
               => Tracer IO String
               -> NodeKernel IO NodeId blk
               -> Decoder IO
               -> IO ()
readIncomingTx tracer kernel Decoder{..} = forever $ do
    newTx :: Mock.Tx <- decodeNext decode
    rejected <- addTxs (getMempool kernel) [demoMockTx (getNodeConfig kernel) newTx]
    traceWith tracer $
      (if null rejected then "Accepted" else "Rejected") <>
      " transaction: " <> show newTx

-- | Listen for transactions coming a named pipe and add them to the mempool
spawnMempoolListener :: RunDemo blk
                     => Tracer IO String
                     -> NodeId
                     -> NodeKernel IO NodeId blk
                     -> IO (Async.Async ())
spawnMempoolListener tracer myNodeId kernel = do
    Async.async $ do
        -- Apparently I have to pass 'ReadWriteMode' here, otherwise the
        -- node will die prematurely with a (DeserialiseFailure 0 "end of input")
        -- error.
        withTxPipe myNodeId ReadWriteMode True $ \h -> do
            let getChunk = BS.hGetSome h 1024
            readIncomingTx tracer kernel =<< initDecoderIO getChunk
