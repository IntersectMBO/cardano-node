{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.LogTypes
  ( AsyncBenchmarkControl (..)
  , BenchTracers (..)
  , EnvConsts (..)
  , NodeToNodeSubmissionTrace (..)
  , SendRecvConnect
  , SendRecvTxSubmission2
  , SubmissionSummary (..)
  , TraceBenchTxSubmit (..)
  ) where

import           Cardano.Api

import           Cardano.Benchmarking.OuroborosImports
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Version as Version
import           Cardano.Logging
import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Cardano.TxGenerator.PlutusContext (PlutusBudgetSummary)
import           Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..))
import           Cardano.TxGenerator.Types (TPSRate)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId)
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion, RemoteConnectionId)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)

import           Prelude

import qualified Codec.CBOR.Term as CBOR
import qualified Control.Concurrent.Async as Async (Async)
import qualified Control.Concurrent.STM as STM (TVar)
import           Data.Text
import           Data.Time.Clock (DiffTime, NominalDiffTime)
import           GHC.Generics
import qualified Network.Mux as Mux

data AsyncBenchmarkControl =
  AsyncBenchmarkControl
  { abcFeeder   :: Async.Async ()
  -- ^ The thread to feed transactions, also called a throttler.
  , abcWorkers  :: [Async.Async ()]
  -- ^ The per-node transaction submission threads.
  , abcSummary  :: IO SubmissionSummary
  -- ^ IO action to emit a summary.
  , abcShutdown :: IO ()
  -- ^ IO action to shut down the feeder thread.
  }

data EnvConsts =
  EnvConsts
  { envIOManager  :: IOManager
  , envThreads    :: STM.TVar (Maybe AsyncBenchmarkControl)
  -- ^ The reference needs to be a constant, but the referred-to data
  --   (`AsyncBenchmarkControl`) needs to be able to be initialized.
  --   This could in principle be an `IORef` instead of a `STM.TVar`.
  , envNixSvcOpts :: Maybe NixServiceOptions
  -- ^ There are situations `NixServiceOptions` won't be available and
  --   defaults will have to be used.
  , benchTracers  :: STM.TVar (Maybe BenchTracers)
  -- ^ This also needs to be accessible to the signal handlers.
  }

data BenchTracers =
  BenchTracers
  { btTxSubmit_   :: Trace IO (TraceBenchTxSubmit TxId)
  , btConnect_    :: Trace IO SendRecvConnect
  , btSubmission2_:: Trace IO SendRecvTxSubmission2
  , btN2N_        :: Trace IO NodeToNodeSubmissionTrace
  }

data TraceBenchTxSubmit txid
  = TraceTxGeneratorVersion Version.Version
  | TraceBenchTxSubRecv [txid]
  -- ^ Received from generator.
  | TraceBenchTxSubStart [txid]
  -- ^ The @txid@ has been submitted to `TxSubmission2`
  --   protocol peer.
  | SubmissionClientReplyTxIds [txid]
  -- ^ Announcing txids in response for server's request.
  | TraceBenchTxSubServReq [txid]
  -- ^ Request for @tx@ received from `TxSubmission2` protocol
  --   peer.
  | SubmissionClientDiscardAcknowledged [txid]
  -- ^ An ack (window moved over) received for these transactions.
  | TraceBenchTxSubServDrop [txid]
  -- ^ Transactions the server implicitly dropped.
  | SubmissionClientUnAcked [txid]
  -- ^ Transactions outstanding.
  | TraceBenchTxSubServUnav [txid]
  -- ^ Transactions requested, but unavailable in the outstanding set.
  | TraceBenchTxSubServFed [txid] Int
  -- ^ Transactions fed by the feeder, accompanied by sequence number.
  | TraceBenchTxSubServCons [txid]
  -- ^ Transactions consumed by a submitter.
  | TraceBenchTxSubIdle
  -- ^ Remote peer requested new transactions but none were
  --   available, generator not keeping up?
  | TraceBenchTxSubRateLimit DiffTime
  -- ^ Rate limiter bit, this much delay inserted to keep within
  --   configured rate.
  | TraceBenchTxSubSummary SubmissionSummary
  -- ^ SubmissionSummary.
  | TraceBenchTxSubDebug String
  | TraceBenchTxSubError Text
  | TraceBenchPlutusBudgetSummary PlutusBudgetSummary
  -- ^ PlutusBudgetSummary.
  deriving stock (Show, Generic)

data SubmissionSummary
  = SubmissionSummary
      { ssTxSent        :: !Sent
      , ssTxUnavailable :: !Unav
      , ssElapsed       :: !NominalDiffTime
      , ssEffectiveTps  :: !TPSRate
      , ssThreadwiseTps :: ![TPSRate]
      , ssFailures      :: ![String]
      }
  deriving stock (Show, Generic)
instance ToJSON SubmissionSummary

data NodeToNodeSubmissionTrace
  = ReqIdsBlocking  Ack Req
  | IdsListBlocking Int
  | ReqIdsNonBlocking Ack Req
  | IdsListNonBlocking Int
  | ReqTxs          Int
  | TxList          Int
  | EndOfProtocol
  deriving stock (Show, Generic)

type SendRecvTxSubmission2 = TraceSendRecv (TxSubmission2 (GenTxId CardanoBlock) (GenTx CardanoBlock))

type SendRecvConnect = Mux.WithBearer
                         RemoteConnectionId
                         (TraceSendRecv (Handshake
                                           NodeToNodeVersion
                                           CBOR.Term))
