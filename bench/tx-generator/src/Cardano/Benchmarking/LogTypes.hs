{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.LogTypes
  ( BenchTracers(..)
  , NodeToNodeSubmissionTrace(..)
  , SendRecvConnect
  , SendRecvTxSubmission2
  , SubmissionSummary(..)
  , TraceBenchTxSubmit(..)
  ) where

import           Prelude

import           Data.Text
import           Data.Time.Clock (DiffTime, NominalDiffTime)

import           GHC.Generics


import           Cardano.Api
import qualified Codec.CBOR.Term as CBOR

import           "contra-tracer" Control.Tracer

import           Network.Mux (WithMuxBearer (..))

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()


import           Cardano.Benchmarking.OuroborosImports
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId)
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion, RemoteConnectionId)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Version as Version
import           Cardano.TxGenerator.PlutusContext (PlutusBudgetSummary)
import           Cardano.TxGenerator.Types (TPSRate)

data BenchTracers =
  BenchTracers
  { btTxSubmit_   :: Tracer IO (TraceBenchTxSubmit TxId)
  , btConnect_    :: Tracer IO SendRecvConnect
  , btSubmission2_:: Tracer IO SendRecvTxSubmission2
  , btN2N_        :: Tracer IO NodeToNodeSubmissionTrace
  }

data TraceBenchTxSubmit txid
  = TraceTxGeneratorVersion Version.Version
  | TraceBenchTxSubRecv [txid]
  -- ^ Received from generator.
  | TraceBenchTxSubStart [txid]
  -- ^ The @txid@ has been submitted to `TxSubmission`
  --   protocol peer.
  | SubmissionClientReplyTxIds [txid]
  -- ^ Announcing txids in response for server's request.
  | TraceBenchTxSubServReq [txid]
  -- ^ Request for @tx@ received from `TxSubmission` protocol
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
      { ssThreadName    :: !String
      , ssTxSent        :: !Sent
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

type SendRecvConnect = WithMuxBearer
                         RemoteConnectionId
                         (TraceSendRecv (Handshake
                                           NodeToNodeVersion
                                           CBOR.Term))
