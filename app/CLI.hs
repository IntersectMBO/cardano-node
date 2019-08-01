{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module CLI (
    -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , fromProtocol
    -- * CLI
  , NodeCLIArguments(..)
  , TopologyInfo(..)
  , Command(..)
  , TraceOptions (..)
  , nodeParser
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Data.Semigroup ((<>))
import qualified Data.IP as IP
import           Options.Applicative
import           Network.Socket (PortNumber)

import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.Ledger.Mock as Mock

import           Topology (NodeAddress (..), TopologyInfo (..))
import           TxSubmission (command', parseMockTx)

import           Cardano.Node.CLI

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data NodeCLIArguments = NodeCLIArguments {
    systemStart                   :: !SystemStart
  , slotDuration                  :: !SlotLength
  , commonCLI                     :: !CommonCLI
  , command                       :: !Command
  }


-- | Tracing options.  Each option enables a tracer which adds verbosity to the
-- log output.
--
data TraceOptions = TraceOptions {
      traceChainDB        :: !Bool
    -- ^ by default we use 'readableChainDB' tracer, if on this it will use more
    -- verbose tracer
    , traceChainSync      :: !Bool
    -- ^ trace chain syn messages
    , traceTxSubmission   :: !Bool
    -- ^ trace tx submission messages
    , traceFetchDecisions :: !Bool
    -- ^ trace fetch decisions; it links to 'decisionTracer' in 'NodeParams'
    , traceFetchClient    :: !Bool
    -- ^ trace fetch client; it links to 'fetchClientTracer' in 'NodeParams'
    , traceTxInbound      :: !Bool
    -- ^ trace tx-submission server; it link to 'txInboundTracer' in 'NodeParams'
    , traceTxOutbound     :: !Bool
    -- ^ trace tx-submission client; it link to 'txOutboundTracer' in 'NodeParams'
    }


parseTraceOptions :: Parser TraceOptions
parseTraceOptions =
        TraceOptions
    <$> parseTraceChainDB
    <*> parseTraceChainSync
    <*> parseTraceTxSubmission
    <*> parseTraceFetchDecisions
    <*> parseTraceFetchClient
    <*> parseTraceTxInbound
    <*> parseTraceTxOutbound

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
      <> help "Verbose tracer of chain db."
    )

parseTraceChainSync :: Parser Bool
parseTraceChainSync =
    switch (
         long "trace-chain-sync"
      <> help "Trace chain sync protocol messages."
    )

parseTraceTxSubmission :: Parser Bool
parseTraceTxSubmission =
    switch (
         long "trace-tx-submission"
      <> help "Trace tx-submission protocol messages."
    )

parseTraceFetchDecisions :: Parser Bool
parseTraceFetchDecisions =
    switch (
         long "trace-fetch-decisions"
      <> help "Trace fetch decisions done by fetch client."
    )

parseTraceFetchClient :: Parser Bool
parseTraceFetchClient =
    switch (
         long "trace-fetch-client"
      <> help "Trace fetch client."
    )

parseTraceTxInbound :: Parser Bool
parseTraceTxInbound =
    switch (
         long "trace-tx-inbound"
      <> help "Trace tx-submission server (inbound transaction)."
    )

parseTraceTxOutbound :: Parser Bool
parseTraceTxOutbound =
    switch (
         long "trace-tx-outbound"
      <> help "Trace tx-submission client (outbound transactions)."
    )
