{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.Tracer
  ( BenchTracers(..)
  , NodeToNodeSubmissionTrace(..)
  , SendRecvConnect
  , SendRecvTxSubmission2
  , SubmissionSummary(..)
  , TraceBenchTxSubmit(..)
  , TraceLowLevelSubmit(..)
  , createLoggingLayerTracers
  , createTracers
  , createDebugTracers
  , initTracers
  ) where


import           Data.Aeson (ToJSON (..), encode, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSL (unpack)
import qualified Data.Text as T
import           Data.Time.Clock (DiffTime, NominalDiffTime, getCurrentTime)
import           Prelude (Show (..), String)

import           Control.Tracer (debugTracer)

import           Cardano.Api
import qualified Codec.CBOR.Term as CBOR

import           Cardano.Prelude hiding (TypeError, show)


import           Cardano.BM.Data.Tracer (trStructured)
import           Cardano.BM.Tracing
import           Network.Mux (WithMuxBearer (..))


import           Cardano.Node.Configuration.Logging (LOContent (..), LoggingLayer (..))
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
import qualified Data.Aeson.KeyMap as KeyMap

data BenchTracers =
  BenchTracers
  { btBase_       :: Trace  IO Text
  , btTxSubmit_   :: Tracer IO (TraceBenchTxSubmit TxId)
  , btConnect_    :: Tracer IO SendRecvConnect
  , btSubmission2_:: Tracer IO SendRecvTxSubmission2
  , btLowLevel_   :: Tracer IO TraceLowLevelSubmit
  , btN2N_        :: Tracer IO NodeToNodeSubmissionTrace
  }

createTracers :: Tracer IO String -> BenchTracers
createTracers baseTr = initTracers tr tr
  where
    tr = contramap (\(_,t) -> BSL.unpack $ encode t) baseTr

createDebugTracers :: BenchTracers
createDebugTracers = createTracers debugTracer

createLoggingLayerTracers :: LoggingLayer -> BenchTracers
createLoggingLayerTracers loggingLayer
  = initTracers baseTrace tr
   where
     baseTrace = llBasicTrace loggingLayer
     tr = llAppendName loggingLayer "cli" baseTrace

initTracers :: Trace IO Text -> Trace IO Text -> BenchTracers
initTracers baseTrace tr =
  BenchTracers
    { btBase_        = baseTrace
    , btTxSubmit_    = benchTracer
    , btConnect_     = connectTracer
    , btSubmission2_ = submitTracer
    , btLowLevel_    = lowLevelSubmitTracer
    , btN2N_         = n2nSubmitTracer
    }
 where
  tr' :: Trace IO Text
  tr' = appendName "generate-txs" tr

  benchTracer :: Tracer IO (TraceBenchTxSubmit TxId)
  benchTracer = toLogObjectVerbose (appendName "benchmark" tr')

  connectTracer :: Tracer IO SendRecvConnect
  connectTracer = toLogObjectVerbose (appendName "connect" tr')

  submitTracer :: Tracer IO SendRecvTxSubmission2
  submitTracer = toLogObjectVerbose (appendName "submit" tr')

  lowLevelSubmitTracer :: Tracer IO TraceLowLevelSubmit
  lowLevelSubmitTracer = toLogObjectMinimal (appendName "llSubmit" tr')

  n2nSubmitTracer :: Tracer IO NodeToNodeSubmissionTrace
  n2nSubmitTracer = toLogObjectMinimal (appendName "submitN2N" tr')

{-------------------------------------------------------------------------------
  Overall benchmarking trace
-------------------------------------------------------------------------------}
data TraceBenchTxSubmit txid
  = TraceBenchTxSubRecv [txid]
  -- ^ Received from generator.
  | TraceBenchTxSubStart [txid]
  -- ^ The @txid@ has been submitted to `TxSubmission`
  --   protocol peer.
  | TraceBenchTxSubServAnn [txid]
  -- ^ Announcing txids in response for server's request.
  | TraceBenchTxSubServReq [txid]
  -- ^ Request for @tx@ received from `TxSubmission` protocol
  --   peer.
  | TraceBenchTxSubServAck [txid]
  -- ^ An ack (window moved over) received for these transactions.
  | TraceBenchTxSubServDrop [txid]
  -- ^ Transactions the server implicitly dropped.
  | TraceBenchTxSubServOuts [txid]
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
  deriving stock (Show)

instance Transformable Text IO (TraceBenchTxSubmit TxId) where
  -- transform to JSON Object
  trTransformer = trStructured

instance HasSeverityAnnotation (TraceBenchTxSubmit TxId)
instance HasPrivacyAnnotation  (TraceBenchTxSubmit TxId)

-- | Summary of a tx submission run.
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

{-------------------------------------------------------------------------------
  N2N submission trace
-------------------------------------------------------------------------------}
data NodeToNodeSubmissionTrace
  = ReqIdsBlocking  Ack Req
  | IdsListBlocking Int

  | ReqIdsPrompt    Ack Req
  | IdsListPrompt   Int

  | ReqTxs          Int
  | TxList          Int

  | EndOfProtocol

instance ToObject NodeToNodeSubmissionTrace where
  toObject MinimalVerbosity = const mempty -- do not log
  toObject _ = \case
    ReqIdsBlocking  (Ack ack) (Req req) ->
                               mconcat [ "kind" .= A.String "ReqIdsBlocking"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListBlocking sent    -> mconcat [ "kind" .= A.String "IdsListBlocking"
                                        , "sent" .= A.toJSON sent ]
    ReqIdsPrompt    (Ack ack) (Req req) ->
                               mconcat [ "kind" .= A.String "ReqIdsPrompt"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListPrompt   sent    -> mconcat [ "kind" .= A.String "IdsListPrompt"
                                        , "sent" .= A.toJSON sent ]
    EndOfProtocol           -> mconcat [ "kind" .= A.String "EndOfProtocol" ]
    ReqTxs          req     -> mconcat [ "kind" .= A.String "ReqTxs"
                                        , "req"  .= A.toJSON req ]
    TxList          sent    -> mconcat [ "kind" .= A.String "TxList"
                                        , "sent" .= A.toJSON sent ]


instance HasSeverityAnnotation NodeToNodeSubmissionTrace
instance HasPrivacyAnnotation  NodeToNodeSubmissionTrace
instance Transformable Text IO NodeToNodeSubmissionTrace where
  trTransformer = trStructured

{-------------------------------------------------------------------------------
  Low-level tracer
-------------------------------------------------------------------------------}
data TraceLowLevelSubmit
  = TraceLowLevelSubmitting
  -- ^ Submitting transaction.
  | TraceLowLevelAccepted
  -- ^ The transaction has been accepted.
  | TraceLowLevelRejected String
  -- ^ The transaction has been rejected, with corresponding error message.
  deriving stock (Show)

instance ToObject TraceLowLevelSubmit where
  toObject MinimalVerbosity _ = mempty -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceLowLevelSubmitting -> mconcat ["kind" .= A.String "TraceLowLevelSubmitting"]
      TraceLowLevelAccepted   -> mconcat ["kind" .= A.String "TraceLowLevelAccepted"]
      TraceLowLevelRejected m -> mconcat [ "kind" .= A.String "TraceLowLevelRejected"
                                          , "message" .= A.String (T.pack m)
                                          ]
  toObject MaximalVerbosity t =
    case t of
      TraceLowLevelSubmitting ->
        mconcat [ "kind" .= A.String "TraceLowLevelSubmitting"
                 ]
      TraceLowLevelAccepted ->
        mconcat [ "kind" .= A.String "TraceLowLevelAccepted"
                 ]
      TraceLowLevelRejected errMsg ->
        mconcat [ "kind"   .= A.String "TraceLowLevelRejected"
                 , "errMsg" .= A.String (T.pack errMsg)
                 ]

instance HasSeverityAnnotation TraceLowLevelSubmit
instance HasPrivacyAnnotation TraceLowLevelSubmit

instance (MonadIO m) => Transformable Text m TraceLowLevelSubmit where
  -- transform to JSON Object
  trTransformer = trStructured

{-------------------------------------------------------------------------------
  SendRecvTxSubmission
-------------------------------------------------------------------------------}
type SendRecvTxSubmission2 = TraceSendRecv (TxSubmission2 (GenTxId CardanoBlock) (GenTx CardanoBlock))

instance Transformable Text IO SendRecvTxSubmission2 where
  -- transform to JSON Object
  trTransformer verb tr = Tracer $ \arg -> do
    currentTime <- getCurrentTime
    let
      obj = toObject verb arg
      updatedObj =
        if obj == mempty
          then obj
          else
            -- Add a timestamp in 'ToObject'-representation.
            KeyMap.insert "time" (A.String (T.pack . show $ currentTime)) obj
      tracer = if obj == mempty then nullTracer else tr
    meta <- mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
    traceWith tracer (mempty, LogObject mempty meta (LogStructured updatedObj))

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}
instance HasSeverityAnnotation TxId
instance HasPrivacyAnnotation  TxId

instance ToObject TxId where
  toObject MinimalVerbosity _    = mempty -- do not log
  toObject NormalVerbosity _     = mconcat [ "kind" .= A.String "GenTxId"]
  toObject MaximalVerbosity txid = mconcat [ "kind" .= A.String "GenTxId"
                                            , "txId" .= toJSON txid
                                            ]

instance Transformable Text IO TxId where
  trTransformer = trStructured

type SendRecvConnect = WithMuxBearer
                         RemoteConnectionId
                         (TraceSendRecv (Handshake
                                           NodeToNodeVersion
                                           CBOR.Term))

instance ToObject (TraceBenchTxSubmit TxId) where
  toObject MinimalVerbosity _ = mempty -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceBenchTxSubRecv _      -> mconcat ["kind" .= A.String "TraceBenchTxSubRecv"]
      TraceBenchTxSubStart _     -> mconcat ["kind" .= A.String "TraceBenchTxSubStart"]
      TraceBenchTxSubServAnn _   -> mconcat ["kind" .= A.String "TraceBenchTxSubServAnn"]
      TraceBenchTxSubServReq _   -> mconcat ["kind" .= A.String "TraceBenchTxSubServReq"]
      TraceBenchTxSubServAck _   -> mconcat ["kind" .= A.String "TraceBenchTxSubServAck"]
      TraceBenchTxSubServDrop _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServDrop"]
      TraceBenchTxSubServOuts _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServOuts"]
      TraceBenchTxSubServUnav _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServUnav"]
      TraceBenchTxSubServFed _ _ -> mconcat ["kind" .= A.String "TraceBenchTxSubServFed"]
      TraceBenchTxSubServCons _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServCons"]
      TraceBenchTxSubIdle        -> mconcat ["kind" .= A.String "TraceBenchTxSubIdle"]
      TraceBenchTxSubRateLimit _ -> mconcat ["kind" .= A.String "TraceBenchTxSubRateLimit"]
      TraceBenchTxSubSummary _   -> mconcat ["kind" .= A.String "TraceBenchTxSubSummary"]
      TraceBenchTxSubDebug _     -> mconcat ["kind" .= A.String "TraceBenchTxSubDebug"]
      TraceBenchTxSubError _     -> mconcat ["kind" .= A.String "TraceBenchTxSubError"]
  toObject MaximalVerbosity t =
    case t of
      TraceBenchTxSubRecv txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubRecv"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubStart txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubStart"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServAnn txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServAnn"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServReq txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServReq"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServAck txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServAck"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServDrop txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServDrop"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServOuts txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServOuts"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServUnav txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServUnav"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServFed txIds ix ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServFed"
                 , "txIds" .= toJSON txIds
                 , "index" .= toJSON ix
                 ]
      TraceBenchTxSubServCons txIds ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubServCons"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubIdle ->
        mconcat [ "kind" .= A.String "TraceBenchTxSubIdle"
                 ]
      TraceBenchTxSubRateLimit limit ->
        mconcat [ "kind"  .= A.String "TraceBenchTxSubRateLimit"
                 , "limit" .= toJSON limit
                 ]
      TraceBenchTxSubSummary summary ->
        mconcat [ "kind"    .= A.String "TraceBenchTxSubSummary"
                 , "summary" .= toJSON summary
                 ]
      TraceBenchTxSubDebug s ->
        mconcat [ "kind" .= A.String "TraceBenchTxSubDebug"
                 , "msg"  .= A.String (T.pack s)
                 ]
      TraceBenchTxSubError s ->
        mconcat [ "kind" .= A.String "TraceBenchTxSubError"
                 , "msg"  .= A.String s
                 ]
