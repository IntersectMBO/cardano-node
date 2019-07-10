{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TxSubmission (
      command'
    , parseMockTx
    , handleTxSubmission

    , localSocketFilePath
    , localSocketAddrInfo
    ) where

import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Options.Applicative

import qualified Codec.Serialise as Serialise (encode, decode)
import           Network.Socket as Socket

import           Control.Monad (forever, unless)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Cardano.Crypto.Hash (ShortHash)
import qualified Cardano.Crypto.Hash as H

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Util.Condense

import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Codec.Cbor
import           Network.Mux.Interface
import           Ouroboros.Network.Block (Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Client
                   (ChainSyncClient(..), chainSyncClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.NodeToClient

import           Cardano.Node.CLI

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
  Main logic
-------------------------------------------------------------------------------}

handleTxSubmission :: forall blk.
                      RunDemo blk
                   => DemoProtocol blk
                   -> TopologyInfo
                   -> Mock.Tx
                   -> Tracer IO String
                   -> IO ()
handleTxSubmission ptcl tinfo mocktx tracer = do
    topoE <- readTopologyFile (topologyFile tinfo)
    t@(NetworkTopology nodeSetups) <-
      case topoE of
        Left e  -> fail e
        Right t -> return t

    unless (node tinfo `M.member` toNetworkMap t) $
      fail "Target node not found."

    nid <- case node tinfo of
      CoreId nid -> return nid
      RelayId{}  -> fail "Only core nodes are supported targets"

    let ProtocolInfo{pInfoConfig} =
          protocolInfo (NumCoreNodes (length nodeSetups))
                       (CoreNodeId nid)
                       ptcl

        tx :: GenTx blk
        tx = demoMockTx pInfoConfig mocktx

    traceWith tracer $
      "The Id for this transaction is: " <> condense (H.hash @ShortHash mocktx)

    submitTx pInfoConfig (node tinfo) tx tracer


submitTx :: RunDemo blk
         => NodeConfig (BlockProtocol blk)
         -> NodeId
         -> GenTx blk
         -> Tracer IO String
         -> IO ()
submitTx pInfoConfig nodeId tx tracer =
    connectTo
      (muxLocalInitiatorNetworkApplication tracer pInfoConfig tx)
      Nothing
      addr
  where
    addr = localSocketAddrInfo (localSocketFilePath nodeId)

muxLocalInitiatorNetworkApplication
  :: forall blk m.
     (RunDemo blk, MonadST m, MonadThrow m, MonadTimer m)
  => Tracer m String
  -> NodeConfig (BlockProtocol blk)
  -> GenTx blk
  -> Versions NodeToClientVersion DictVersion
              (MuxApplication InitiatorApp NodeToClientProtocols
                              m ByteString () Void)
muxLocalInitiatorNetworkApplication tracer pInfoConfig tx =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = 0 })
      (DictVersion nodeToClientCodecCBORTerm)

  $ MuxInitiatorApplication $ \ptcl -> case ptcl of
      LocalTxSubmissionPtcl -> \channel -> do
        traceWith tracer ("Submitting transaction: " {-++ show tx-})
        result <- runPeer
                    nullTracer -- (contramap show tracer)
                    localTxSubmissionCodec
                    channel
                    (localTxSubmissionClientPeer
                       (pure (txSubmissionClientSingle tx)))
        case result of
          Nothing  -> traceWith tracer "Transaction accepted"
          Just msg -> traceWith tracer ("Transaction rejected: " ++ msg)

      ChainSyncWithBlocksPtcl -> \channel ->
        runPeer
          nullTracer
          (localChainSyncCodec @blk pInfoConfig)
          channel
          (chainSyncClientPeer chainSyncClientNull)


-- | A 'LocalTxSubmissionClient' that submits exactly one transaction, and then
-- disconnects, returning the confirmation or rejection.
--
txSubmissionClientSingle
  :: forall tx reject m.
     Applicative m
  => tx
  -> LocalTxSubmissionClient tx reject m (Maybe reject)
txSubmissionClientSingle tx =
    SendMsgSubmitTx tx $ \mreject ->
      pure (SendMsgDone mreject)

chainSyncClientNull
  :: MonadTimer m
  => ChainSyncClient blk (Point blk) m a
chainSyncClientNull =
    ChainSyncClient blockForever
  where
    blockForever = forever (threadDelay 3600)

localTxSubmissionCodec
  :: (RunDemo blk, MonadST m)
  => Codec (LocalTxSubmission (GenTx blk) String)
           DeserialiseFailure m ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    demoEncodeGenTx
    demoDecodeGenTx
    Serialise.encode
    Serialise.decode

localChainSyncCodec
  :: (RunDemo blk, MonadST m)
  => NodeConfig (BlockProtocol blk)
  -> Codec (ChainSync blk (Point blk))
           DeserialiseFailure m ByteString
localChainSyncCodec pInfoConfig =
    codecChainSync
      (demoEncodeBlock pInfoConfig)
      (demoDecodeBlock pInfoConfig)
      (Block.encodePoint demoEncodeHeaderHash)
      (Block.decodePoint demoDecodeHeaderHash)


localSocketFilePath :: NodeId -> FilePath
localSocketFilePath (CoreId  n) = "node-core-" ++ show n ++ ".socket"
localSocketFilePath (RelayId n) = "node-relay-" ++ show n ++ ".socket"

localSocketAddrInfo :: FilePath -> Socket.AddrInfo
localSocketAddrInfo socketPath =
    Socket.AddrInfo
      []
      Socket.AF_UNIX
      Socket.Stream
      Socket.defaultProtocol
      (Socket.SockAddrUnix socketPath)
      Nothing
