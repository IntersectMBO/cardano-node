{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.LegacyTracer
  ( createLoggingLayerTracers
  , initDefaultLegacyTracers
  , createDebugTracers
  ) where


import           Data.Aeson (ToJSON (..), encode, (.=))
import qualified Data.Aeson as A

import qualified Data.ByteString.Lazy.Char8 as BSL (unpack)
import qualified Data.Text as T

import           Data.Time.Clock (getCurrentTime)
import           Prelude (Show (..), String)

import           "contra-tracer" Control.Tracer (debugTracer)

import           Cardano.Api

import           Cardano.Prelude hiding (TypeError, show)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem (mapLogObject)
import           Cardano.BM.Data.Tracer (trStructured)
import           Cardano.BM.Data.Output
import           Cardano.BM.Setup (setupTrace_)
import           Cardano.BM.Tracing

import           Cardano.Node.Configuration.Logging (LOContent (..), LoggingLayer (..))
import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Cardano.Benchmarking.OuroborosImports

import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.Types


import qualified Data.Aeson.KeyMap as KeyMap

createLoggingLayerTracers :: LoggingLayer -> BenchTracers
createLoggingLayerTracers loggingLayer
  = initTracers tr
   where
     baseTrace = llBasicTrace loggingLayer
     tr = llAppendName loggingLayer "cli" baseTrace

createDebugTracers :: BenchTracers
createDebugTracers = createTracers debugTracer

initDefaultLegacyTracers :: IO BenchTracers
initDefaultLegacyTracers = do
  c <- defaultConfigStdout
  CM.setDefaultBackends c [KatipBK ]
  CM.setSetupBackends c [KatipBK ]
  CM.setDefaultBackends c [KatipBK ]
  CM.setSetupScribes c [ ScribeDefinition {
                              scName = "cli"
                            , scFormat = ScJson
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            , scRotation = Nothing
                            }
                         ]
  CM.setScribes c "cardano.cli" (Just ["StdoutSK::cli"])
  (tr :: Trace IO String, _switchboard) <- setupTrace_ c "cardano"
  let
    (bt :: Trace IO T.Text)  = contramap (second $ mapLogObject T.unpack) $ appendName "cli" tr
  return $ initTracers bt

createTracers :: Tracer IO String -> BenchTracers
createTracers baseTr = initTracers tr
  where
    tr = contramap (\(_,t) -> BSL.unpack $ encode t) baseTr

initTracers :: Trace IO Text -> BenchTracers
initTracers tr =
  BenchTracers
    { btTxSubmit_    = benchTracer
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
  n2nSubmitTracer = toLogObjectVerbose (appendName "submitN2N" tr')

instance Transformable Text IO (TraceBenchTxSubmit TxId) where
  -- transform to JSON Object
  trTransformer = trStructured

instance HasSeverityAnnotation (TraceBenchTxSubmit TxId)
instance HasPrivacyAnnotation  (TraceBenchTxSubmit TxId)

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
