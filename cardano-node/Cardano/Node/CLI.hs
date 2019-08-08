{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.CLI (
    -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , fromProtocol
    -- * CLI
  , NodeCLIArguments(..)
  , TopologyInfo(..)
  , Command(..)
  , TraceOptions (..)
  , ConsensusTraceOptions
  , ProtocolTraceOptions
  , nodeParser
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (read)

import           Data.Functor.Const (Const (..))
import qualified Data.IP as IP
import           Data.Semigroup ((<>))
import           Network.Socket (PortNumber)
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers' (..))

import           Cardano.Node.Topology (NodeAddress (..), TopologyInfo (..))
import           Cardano.Node.TxSubmission (parseMockTx)

import           Cardano.Node.ConfigCLI

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data NodeCLIArguments = NodeCLIArguments {
    systemStart  :: !SystemStart
  , slotDuration :: !SlotLength
  , commonCLI    :: !CommonCLI
  , command      :: !Command
  }


type ConsensusTraceOptions = Consensus.Tracers' () ()    (Const Bool)
type ProtocolTraceOptions  = ProtocolTracers'   () () () (Const Bool)

-- | Tracing options. Each option enables a tracer which adds verbosity to the
-- log output.
data TraceOptions = TraceOptions
  { traceChainDB         :: !Bool
    -- ^ By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer
  , traceConsensus       :: ConsensusTraceOptions
  , traceProtocols       :: ProtocolTraceOptions
  , traceIpSubscription  :: !Bool
  , traceDnsSubscription :: !Bool
  , traceDnsResolver     :: !Bool
  }

parseConsensusTraceOptions :: Parser ConsensusTraceOptions
parseConsensusTraceOptions = Consensus.Tracers
  <$> (Const <$> parseTraceChainSyncClient)
  <*> (Const <$> parseTraceChainSyncServer)
  <*> (Const <$> parseTraceBlockFetchDecisions)
  <*> (Const <$> parseTraceBlockFetchClient)
  <*> (Const <$> parseTraceBlockFetchServer)
  <*> (Const <$> parseTraceTxInbound)
  <*> (Const <$> parseTraceTxOutbound)
  <*> (Const <$> parseTraceLocalTxSubmissionServer)
  <*> (Const <$> parseTraceMempool)
  <*> (Const <$> parseTraceForge)

parseProtocolTraceOptions :: Parser ProtocolTraceOptions
parseProtocolTraceOptions = ProtocolTracers
  <$> (Const <$> parseTraceChainSyncProtocol)
  <*> (Const <$> parseTraceBlockFetchProtocol)
  <*> (Const <$> parseTraceTxSubmissionProtocol)
  <*> (Const <$> parseTraceLocalChainSyncProtocol)
  <*> (Const <$> parseTraceLocalTxSubmissionProtocol)

parseTraceOptions :: Parser TraceOptions
parseTraceOptions = TraceOptions
  <$> parseTraceChainDB
  <*> parseConsensusTraceOptions
  <*> parseProtocolTraceOptions
  <*> parseTraceIpSubscription
  <*> parseTraceDnsSubscription
  <*> parseTraceDnsResolver

data Command =
    SimpleNode  TopologyInfo NodeAddress Protocol ViewMode TraceOptions
  | TxSubmitter TopologyInfo Mock.Tx     Protocol
  | TraceAcceptor

nodeParser :: Parser NodeCLIArguments
nodeParser = NodeCLIArguments
    <$> parseSystemStart
    <*> parseSlotDuration
    <*> parseCommonCLI
    <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode
        <$> parseTopologyInfo
        <*> parseNodeAddress
        <*> parseProtocol
        <*> parseViewMode
        <*> parseTraceOptions
  , command' "submit" "Submit a transaction." $
      TxSubmitter <$> parseTopologyInfo <*> parseMockTx <*> parseProtocol
  , command' "trace-acceptor" "Spawn an acceptor." $
      pure TraceAcceptor
  ]

parseHostAddr :: Parser IP.IP
parseHostAddr =
    option (read <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "The ipv6 or ipv4 address"
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

parseTopologyInfo :: Parser TopologyInfo
parseTopologyInfo = TopologyInfo <$> parseNodeId <*> parseTopologyFile

parseTraceChainDB :: Parser Bool
parseTraceChainDB =
    switch (
         long "trace-chain-db"
      <> help "Verbose tracer of ChainDB."
    )

parseTraceChainSyncClient :: Parser Bool
parseTraceChainSyncClient  =
    switch (
         long "trace-chain-sync-client"
      <> help "Trace ChainSync client."
    )

parseTraceChainSyncServer :: Parser Bool
parseTraceChainSyncServer  =
    switch (
         long "trace-chain-sync-server"
      <> help "Trace ChainSync server."
    )

parseTraceBlockFetchDecisions :: Parser Bool
parseTraceBlockFetchDecisions =
    switch (
         long "trace-block-fetch-decisions"
      <> help "Trace BlockFetch decisions made by the BlockFetch client."
    )
parseTraceBlockFetchClient :: Parser Bool
parseTraceBlockFetchClient  =
    switch (
         long "trace-block-fetch-client"
      <> help "Trace BlockFetch client."
    )

parseTraceBlockFetchServer :: Parser Bool
parseTraceBlockFetchServer  =
    switch (
         long "trace-block-fetch-server"
      <> help "Trace BlockFetch server."
    )

parseTraceTxInbound :: Parser Bool
parseTraceTxInbound =
    switch (
         long "trace-tx-inbound"
      <> help "Trace TxSubmission server (inbound transactions)."
    )

parseTraceTxOutbound :: Parser Bool
parseTraceTxOutbound =
    switch (
         long "trace-tx-outbound"
      <> help "Trace TxSubmission client (outbound transactions)."
    )

parseTraceLocalTxSubmissionServer :: Parser Bool
parseTraceLocalTxSubmissionServer =
    switch (
         long "trace-local-tx-submission-server"
      <> help "Trace local TxSubmission server."
    )

parseTraceMempool :: Parser Bool
parseTraceMempool =
    switch (
         long "trace-mempool"
      <> help "Trace mempool."
    )

parseTraceForge :: Parser Bool
parseTraceForge =
    switch (
         long "trace-forge"
      <> help "Trace block forging."
    )

parseTraceChainSyncProtocol :: Parser Bool
parseTraceChainSyncProtocol =
    switch (
         long "trace-chain-sync-protocol"
      <> help "Trace ChainSync protocol messages."
    )

parseTraceBlockFetchProtocol :: Parser Bool
parseTraceBlockFetchProtocol =
    switch (
         long "trace-block-fetch-protocol"
      <> help "Trace BlockFetch protocol messages."
    )

parseTraceTxSubmissionProtocol :: Parser Bool
parseTraceTxSubmissionProtocol =
    switch (
         long "trace-tx-submission-protocol"
      <> help "Trace TxSubmission protocol messages."
    )

parseTraceLocalChainSyncProtocol :: Parser Bool
parseTraceLocalChainSyncProtocol =
    switch (
         long "trace-local-chain-sync-protocol"
      <> help "Trace local ChainSync protocol messages."
    )

parseTraceLocalTxSubmissionProtocol :: Parser Bool
parseTraceLocalTxSubmissionProtocol =
    switch (
         long "trace-local-tx-submission-protocol"
      <> help "Trace local TxSubmission protocol messages."
    )

parseTraceIpSubscription :: Parser Bool
parseTraceIpSubscription =
    switch (
         long "trace-ip-subscription"
      <> help "Trace IP Subscription messages."
    )

parseTraceDnsSubscription :: Parser Bool
parseTraceDnsSubscription =
    switch (
         long "trace-dns-subscription"
      <> help "Trace DNS Subscription messages."
    )

parseTraceDnsResolver :: Parser Bool
parseTraceDnsResolver =
    switch (
         long "trace-dns-resolver"
      <> help "Trace DNS Resolver messages."
    )
