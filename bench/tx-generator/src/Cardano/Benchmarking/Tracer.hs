{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Benchmarking.Tracer
  ( initTracers
  , initDefaultTracers
  , initNullTracers
  )
where

import           "contra-tracer" Control.Tracer (Tracer (..))
import           GHC.Generics

import           Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Kind
import qualified Data.Map.Strict as Map

import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock

import           Ouroboros.Network.IOManager (IOManager)
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.TraceObject

import           Cardano.Api
import           Cardano.Logging
import           Cardano.Node.Startup

import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Version as Version

generatorTracer ::
     LogFormatting a
  => (a -> Namespace)
  -> Text
  -> Maybe (Trace IO FormattedMessage)
  -> Maybe (Trace IO FormattedMessage)
  -> IO (Trace IO a)
generatorTracer namesFor tracerName mbTrStdout mbTrForward = do
  forwardTrace <- case mbTrForward of
                        Nothing -> mempty
                        Just trForward -> forwardFormatter Nothing trForward
  stdoutTrace  <- case mbTrStdout of
                        Nothing -> mempty
                        Just trForward -> machineFormatter Nothing trForward
  let tr = forwardTrace <> stdoutTrace
  tr'  <- withDetailsFromConfig tr
  pure $ withNamesAppended namesFor
          $ appendName tracerName
              tr'

initNullTracers :: BenchTracers
initNullTracers = BenchTracers
    { btTxSubmit_    = Tracer ignore
    , btConnect_     = Tracer ignore
    , btSubmission2_ = Tracer ignore
    , btN2N_         = Tracer ignore
    }
  where ignore _ = return ()

initDefaultTracers :: IO BenchTracers
initDefaultTracers = do
  mbStdoutTracer <- fmap Just standardTracer
  let mbForwardingTracer = Nothing
  benchTracer <-  generatorTracer singletonName "benchmark"  mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig benchTracerDocumented [benchTracer]
  n2nSubmitTracer <- generatorTracer singletonName "submitN2N"  mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig nodeToNodeSubmissionTraceDocumented [n2nSubmitTracer]
  connectTracer <- generatorTracer singletonName "connect"  mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig sendRecvConnectDocumented [connectTracer]
  submitTracer <- generatorTracer namesForSubmission2 "submit"  mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig submission2Documented [submitTracer]

  return $ BenchTracers
    { btTxSubmit_    = Tracer (traceWith benchTracer)
    , btConnect_     = Tracer (traceWith connectTracer)
    , btSubmission2_ = Tracer (traceWith submitTracer)
    , btN2N_         = Tracer (traceWith n2nSubmitTracer)
    }


initTracers ::
     IOManager
  -> NetworkId
  -> FilePath
  -> IO BenchTracers
initTracers iomgr networkId tracerSocket = do
  (forwardingTracer :: Trace IO FormattedMessage, dpTracer :: Trace IO DataPoint) <- do
          (forwardSink :: ForwardSink TraceObject, dpStore) <- initForwarding iomgr initialTraceConfig (toNetworkMagic networkId)
                                                                 Nothing $ Just (tracerSocket, Initiator)
          pure (forwardTracer forwardSink, dataPointTracer dpStore)
  mbStdoutTracer <- fmap Just standardTracer
  let mbForwardingTracer = Just forwardingTracer
  benchTracer <-  generatorTracer singletonName "benchmark" mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig benchTracerDocumented [benchTracer]
  n2nSubmitTracer <- generatorTracer singletonName "submitN2N" mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig nodeToNodeSubmissionTraceDocumented [n2nSubmitTracer]
  connectTracer <- generatorTracer singletonName "connect" mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig sendRecvConnectDocumented [connectTracer]
  submitTracer <- generatorTracer namesForSubmission2 "submit" mbStdoutTracer mbForwardingTracer
  configureTracers initialTraceConfig submission2Documented [submitTracer]
  -- Now we need to provide "Nodeinfo" DataPoint, to forward generator's name
  -- to the acceptor application (for example, 'cardano-tracer').
  nodeInfoTracer <- mkDataPointTracer dpTracer (const ["NodeInfo"])
  prepareGenInfo >>= traceWith nodeInfoTracer

  traceWith benchTracer $ TraceTxGeneratorVersion Version.txGeneratorVersion
--  traceWith st $ show $ TraceTxGeneratorVersion Version.txGeneratorVersion
  return $ BenchTracers
    { btTxSubmit_    = Tracer (traceWith benchTracer)
    , btConnect_     = Tracer (traceWith connectTracer)
    , btSubmission2_ = Tracer (traceWith submitTracer)
    , btN2N_         = Tracer (traceWith n2nSubmitTracer)
    }
 where
  prepareGenInfo = do
    now <- getCurrentTime
    return $ NodeInfo
      { niName            = "TxGenerator"
      , niProtocol        = "N/A"
      , niVersion         = _compilerVersion
      , niCommit          = _gitRev
      , niStartTime       = now
      , niSystemStartTime = now
      }
  Version{_compilerVersion, _gitRev} = Version.txGeneratorVersion

initialTraceConfig :: TraceConfig
initialTraceConfig = TraceConfig {
      tcOptions = Map.fromList
          [ ([], [ConfSeverity (SeverityF Nothing)])
          , initConf "benchmark"
          , initConf "submitN2N"
          , initConf "connect"
          , initConf "submit"
          , (["llSubmit"], [ConfDetail DMinimal])
          ]
    , tcForwarder = defaultForwarder
    , tcNodeName = Nothing
    , tcPeerFrequency = Just 2000 -- Every 2 seconds
    , tcResourceFrequency = Just 1000 -- Every second
    }
  where
    initConf :: Text -> (Namespace, [ConfigOption])
    initConf tr = ([tr], [ConfDetail DMaximum])

singletonName :: (ConstructorName f, Generic a, Rep a ~ D1 c f) => a -> [Text]
singletonName a = [ genericName a ]

genericName :: (ConstructorName f, Generic a, Rep a ~ D1 c f) => a -> Text
genericName x = Text.pack $ constructorName $ unM1 $ from x

class ConstructorName f where
  constructorName :: f p -> String

instance (ConstructorName f, ConstructorName g) => ConstructorName (f :+: g) where
  constructorName (L1 x) = constructorName x
  constructorName (R1 x) = constructorName x
instance (Constructor ('MetaCons n f r)) => ConstructorName (C1 ('MetaCons n f r) x) where
  constructorName = conName

genericConstructorsOf :: forall a c f. (Rep a ~ D1 c f, ConstructorsOf f) => Proxy a -> [Text]
genericConstructorsOf _ = map Text.pack $ constructorsOf (Proxy :: Proxy f)

class ConstructorsOf (f :: Type -> Type ) where
  constructorsOf :: Proxy f -> [String]

instance (ConstructorsOf f, ConstructorsOf g) => ConstructorsOf (f :+: g) where
  constructorsOf _ = constructorsOf (Proxy :: Proxy f) ++ constructorsOf (Proxy :: Proxy g)

instance (Constructor ('MetaCons n f r)) => ConstructorsOf (C1 ('MetaCons n f r) x) where
  constructorsOf _ = [ conName @('MetaCons n f r) undefined ]

instance LogFormatting (TraceBenchTxSubmit TxId) where
  forHuman = Text.pack . show
  forMachine DMinimal _ = mempty
  forMachine DNormal t = mconcat [ "kind" .= A.String (genericName t) ]
  forMachine DDetailed t = forMachine DMaximum t
  forMachine DMaximum t = case t of
    TraceTxGeneratorVersion v -> mconcat [ "kind" .= A.String "TraceTxGeneratorVersion" ] <> Version.toJsonLogMsg v
    TraceBenchTxSubRecv txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubRecv"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubStart txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubStart"
              , "txIds" .= toJSON txIds
              ]
    SubmissionClientReplyTxIds txIds ->
      mconcat [ "kind"  .= A.String "SubmissionClientReplyTxIds"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServReq txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServReq"
              , "txIds" .= toJSON txIds
              ]
    SubmissionClientDiscardAcknowledged txIds ->
      mconcat [ "kind"  .= A.String "SubmissionClientDiscardAcknowledged"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServDrop txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServDrop"
              , "txIds" .= toJSON txIds
              ]
    SubmissionClientUnAcked txIds ->
      mconcat [ "kind"  .= A.String "SubmissionClientUnAcked"
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
              , "msg"  .= A.String (Text.pack s)
              ]
    TraceBenchTxSubError s ->
      mconcat [ "kind" .= A.String "TraceBenchTxSubError"
              , "msg"  .= A.String s
              ]

benchTracerDocumented :: Documented (TraceBenchTxSubmit TxId)
benchTracerDocumented
  = Documented $ map (emptyDoc2 "benchmark") $ genericConstructorsOf (Proxy :: Proxy (TraceBenchTxSubmit x))

instance LogFormatting NodeToNodeSubmissionTrace where
  forHuman = Text.pack . show
  forMachine _verb = \case
    ReqIdsBlocking (Ack ack) (Req req) -> KeyMap.fromList
      [ "kind" .= A.String "ReqIdsBlocking"
      , "ack"  .= A.toJSON ack
      , "req"  .= A.toJSON req ]
    IdsListBlocking sent -> KeyMap.fromList
      [ "kind" .= A.String "IdsListBlocking"
      , "sent" .= A.toJSON sent ]
    ReqIdsNonBlocking (Ack ack) (Req req) -> KeyMap.fromList
      [ "kind" .= A.String "ReqIdsNonBlocking"
      , "ack"  .= A.toJSON ack
      , "req"  .= A.toJSON req ]
    IdsListNonBlocking sent -> KeyMap.fromList
      [ "kind" .= A.String "IdsListNonBlocking"
      , "sent" .= A.toJSON sent ]
    EndOfProtocol -> KeyMap.fromList [ "kind" .= A.String "EndOfProtocol" ]
    ReqTxs req -> KeyMap.fromList
       [ "kind" .= A.String "ReqTxs"
       , "req"  .= A.toJSON req ]
    TxList sent -> KeyMap.fromList
       [ "kind" .= A.String "TxList"
       , "sent" .= A.toJSON sent ]

nodeToNodeSubmissionTraceDocumented :: Documented NodeToNodeSubmissionTrace
nodeToNodeSubmissionTraceDocumented
  = Documented $ map (emptyDoc2 "submitN2N") $ genericConstructorsOf (Proxy :: Proxy NodeToNodeSubmissionTrace)

instance LogFormatting SendRecvConnect where
  forHuman = Text.pack . show
  forMachine _ _ = KeyMap.fromList [ "kind" .= A.String "SendRecvConnect" ]

sendRecvConnectDocumented :: Documented SendRecvConnect
sendRecvConnectDocumented = Documented
  [ emptyDoc ["connect"]
  ]

instance LogFormatting SendRecvTxSubmission2 where
  forHuman = Text.pack . show
  forMachine _ _ = KeyMap.fromList [ "kind" .= A.String "SendRecvTxSubmission2" ]

namesForSubmission2 :: SendRecvTxSubmission2 ->  [Text]
namesForSubmission2 _ = []

submission2Documented :: Documented SendRecvTxSubmission2
submission2Documented = Documented
  [ emptyDoc ["submission2"]
  ]

emptyDoc :: Namespace -> DocMsg a
emptyDoc ns = DocMsg ns [] "ToDo: write benchmark tracer docs"

emptyDoc2 :: Text -> Text -> DocMsg a
emptyDoc2 n1 n2 = DocMsg [n1, n2] [] "ToDo: write benchmark tracer docs"
