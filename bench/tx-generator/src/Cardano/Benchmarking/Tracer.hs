{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  ( initDefaultTracers
  ) where

import           "contra-tracer" Control.Tracer (Tracer (..))
import           GHC.Generics

import           Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Kind
import qualified Data.Map as Map

import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api
import           Cardano.Logging

import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Version as Version

generatorTracer :: LogFormatting a => (a -> Namespace) -> Text -> Trace IO FormattedMessage -> IO (Trace IO a)
generatorTracer namesFor tracerName tr = do
  tr'   <- machineFormatter Nothing tr
  tr''  <- withDetailsFromConfig tr'
  pure $ withNamesAppended namesFor
          $ appendName tracerName
              tr''

initDefaultTracers :: IO BenchTracers
initDefaultTracers = do
  st <-  standardTracer
  benchTracer <-  generatorTracer singletonName "benchmark" st
  configureTracers initialTraceConfig benchTracerDocumented [benchTracer]
  n2nSubmitTracer <- generatorTracer singletonName "submitN2N" st
  configureTracers initialTraceConfig nodeToNodeSubmissionTraceDocumented [n2nSubmitTracer]
  connectTracer <- generatorTracer singletonName "connect" st
  configureTracers initialTraceConfig sendRecvConnectDocumented [connectTracer]
  submitTracer <- generatorTracer namesForSubmission2 "submit" st
  configureTracers initialTraceConfig submission2Documented [submitTracer]

  return $ BenchTracers
    { btTxSubmit_    = Tracer (traceWith benchTracer)
    , btConnect_     = Tracer (traceWith connectTracer)
    , btSubmission2_ = Tracer (traceWith submitTracer)
    , btN2N_         = Tracer (traceWith n2nSubmitTracer)
    }

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
