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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Benchmarking.Tracer
  ( initNullTracers
  , initTxGenTracers
  )
where

import           GHC.Generics

import           Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Kind
import qualified Data.Map.Strict as Map

import           Control.Monad (forM, guard)
import           Data.List (find)
import           Data.Maybe (fromMaybe)
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

pattern TracerNameBench     :: Text
pattern TracerNameBench     = "Benchmark"
pattern TracerNameSubmit    :: Text
pattern TracerNameSubmit    = "Submit"
pattern TracerNameSubmitN2N :: Text
pattern TracerNameSubmitN2N = "SubmitN2N"
pattern TracerNameConnect   :: Text
pattern TracerNameConnect   = "Connect"

generatorTracer ::
     (LogFormatting a, MetaTrace a)
  => Text
  -> Maybe (Trace IO FormattedMessage)
  -> Maybe (Trace IO FormattedMessage)
  -> IO (Trace IO a)
generatorTracer tracerName mbTrStdout mbTrForward = do
  forwardTrace <- case mbTrForward of
                        Nothing -> mempty
                        Just trForward -> forwardFormatter Nothing trForward
  stdoutTrace  <- case mbTrStdout of
                        Nothing -> mempty
                        Just trForward -> machineFormatter Nothing trForward
  let tr = forwardTrace <> stdoutTrace
  tr'  <- withDetailsFromConfig tr
  pure $ withInnerNames $ appendPrefixName tracerName tr'

initNullTracers :: BenchTracers
initNullTracers = BenchTracers
    { btTxSubmit_    = mempty
    , btConnect_     = mempty
    , btSubmission2_ = mempty
    , btN2N_         = mempty
    }

-- if the first argument isJust, we assume we have a socket path
-- and want to use trace-dispatcher, so we'll create a forwarding tracer
initTxGenTracers :: Maybe (IOManager, NetworkId, FilePath) -> IO BenchTracers
initTxGenTracers mbForwarding = do
  mbStdoutTracer <- fmap Just standardTracer
  mbForwardingTracer <- prepareForwardingTracer
  confState       <- emptyConfigReflection

  let
    mkTracer :: (LogFormatting a, MetaTrace a)
                  => Text
                  -> Maybe (Trace IO FormattedMessage)
                  -> Maybe (Trace IO FormattedMessage)
                  -> IO (Trace IO a)
    mkTracer namespace mbStdoutTracer' mbForwardingTracer'
      | isPrefixSilent namespace = pure mempty
      | otherwise = do
        tracer <-  generatorTracer namespace mbStdoutTracer' mbForwardingTracer'
        configureTracers confState initialTraceConfig [tracer]
        pure tracer

  benchTracer     <- mkTracer TracerNameBench     mbStdoutTracer mbForwardingTracer
  n2nSubmitTracer <- mkTracer TracerNameSubmitN2N mbStdoutTracer mbForwardingTracer
  connectTracer   <- mkTracer TracerNameConnect   mbStdoutTracer mbForwardingTracer
  submitTracer    <- mkTracer TracerNameSubmit    mbStdoutTracer mbForwardingTracer

  traceWith benchTracer (TraceTxGeneratorVersion Version.txGeneratorVersion)

  return $ BenchTracers
    { btTxSubmit_    = benchTracer
    , btConnect_     = connectTracer
    , btSubmission2_ = submitTracer
    , btN2N_         = n2nSubmitTracer
    }
 where
  prepareForwardingTracer :: IO (Maybe (Trace IO FormattedMessage))
  prepareForwardingTracer = forM mbForwarding $
    \(iomgr, networkId, tracerSocket) -> do
        let forwardingConf = fromMaybe defaultForwarder (tcForwarder initialTraceConfig)
        (forwardSink :: ForwardSink TraceObject, dpStore) <-
          initForwarding iomgr forwardingConf (toNetworkMagic networkId) Nothing $ Just (tracerSocket, Initiator)

        -- we need to provide NodeInfo DataPoint, to forward generator's name
        -- to the acceptor application (for example, 'cardano-tracer').
        let
          dpt :: Trace IO DataPoint
          dpt = dataPointTracer dpStore
        nodeInfoTracer <- mkDataPointTracer dpt
        prepareGenInfo >>= traceWith nodeInfoTracer

        pure $ forwardTracer forwardSink

  prepareGenInfo :: IO NodeInfo
  prepareGenInfo =
    let Version{_compilerVersion, _gitRev} = Version.txGeneratorVersion
    in do
      now <- getCurrentTime
      return $ NodeInfo
        { niName            = "TxGenerator"
        , niProtocol        = "N/A"
        , niVersion         = _compilerVersion
        , niCommit          = _gitRev
        , niStartTime       = now
        , niSystemStartTime = now
        }

isPrefixSilent :: Text -> Bool
isPrefixSilent namespace =
  fromMaybe False $ do
    guard $ not $ Text.null namespace
    prefix <- find (any (namespace `Text.isPrefixOf`)) (Map.keys opts)
    conf <- prefix `Map.lookup` opts
    pure $ configSilent `elem` conf
  where
    TraceConfig{tcOptions = opts} = initialTraceConfig

configSilent :: ConfigOption
configSilent = ConfSeverity (SeverityF Nothing)

initialTraceConfig :: TraceConfig
initialTraceConfig = TraceConfig {
      tcOptions = Map.fromList
          [ ([], [configSilent])
          , setMaxDetail TracerNameBench
          , ([TracerNameSubmitN2N], [configSilent])
          , setMaxDetail TracerNameConnect
          , setMaxDetail TracerNameSubmit
          ]
    , tcForwarder = Just defaultForwarder
    , tcNodeName = Nothing
    , tcPeerFrequency = Just 2000 -- Every 2 seconds
    , tcResourceFrequency = Just 1000 -- Every second
    }
  where
    setMaxDetail :: Text -> ([Text], [ConfigOption])
    setMaxDetail tr = ([tr], [ConfDetail DMaximum])

genericName :: (ConstructorName f, Generic a, Rep a ~ D1 c f) => a -> Text
genericName x = Text.pack $ constructorName $ unM1 $ from x

class ConstructorName f where
  constructorName :: f p -> String

instance (ConstructorName f, ConstructorName g) => ConstructorName (f :+: g) where
  constructorName (L1 x) = constructorName x
  constructorName (R1 x) = constructorName x
instance (Constructor ('MetaCons n f r)) => ConstructorName (C1 ('MetaCons n f r) x) where
  constructorName = conName


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
    TraceBenchPlutusBudgetSummary summary ->
      mconcat [ "kind"    .= A.String "TraceBenchPlutusBudgetSummary"
              , "summary" .= toJSON summary
              ]

instance MetaTrace (TraceBenchTxSubmit TxId) where
    namespaceFor TraceTxGeneratorVersion {} = Namespace [] ["TxGeneratorVersion"]
    namespaceFor TraceBenchTxSubRecv {} = Namespace [] ["BenchTxSubRecv"]
    namespaceFor TraceBenchTxSubStart {} = Namespace [] ["BenchTxSubStart"]
    namespaceFor SubmissionClientReplyTxIds {} = Namespace [] ["SubmissionClientReplyTxIds"]
    namespaceFor TraceBenchTxSubServReq {} = Namespace [] ["BenchTxSubServReq"]
    namespaceFor SubmissionClientDiscardAcknowledged {} = Namespace [] ["SubmissionClientDiscardAcknowledged"]
    namespaceFor TraceBenchTxSubServDrop {} = Namespace [] ["BenchTxSubServDrop"]
    namespaceFor SubmissionClientUnAcked {} = Namespace [] ["SubmissionClientUnAcked"]
    namespaceFor TraceBenchTxSubServUnav {} = Namespace [] ["BenchTxSubServUnav"]
    namespaceFor TraceBenchTxSubServFed {} = Namespace [] ["BenchTxSubServFed"]
    namespaceFor TraceBenchTxSubServCons {} = Namespace [] ["BenchTxSubServCons"]
    namespaceFor TraceBenchTxSubIdle {} = Namespace [] ["BenchTxSubIdle"]
    namespaceFor TraceBenchTxSubRateLimit {} = Namespace [] ["BenchTxSubRateLimit"]
    namespaceFor TraceBenchTxSubSummary {} = Namespace [] ["BenchTxSubSummary"]
    namespaceFor TraceBenchTxSubDebug {} = Namespace [] ["BenchTxSubDebug"]
    namespaceFor TraceBenchTxSubError {} = Namespace [] ["BenchTxSubError"]
    namespaceFor TraceBenchPlutusBudgetSummary {} = Namespace [] ["BenchPlutusBudgetSummary"]

    severityFor _ _ = Just Info

    documentFor _ = Just ""

    allNamespaces = [
          Namespace [] ["TxGeneratorVersion"]
        , Namespace [] ["BenchTxSubRecv"]
        , Namespace [] ["BenchTxSubStart"]
        , Namespace [] ["SubmissionClientReplyTxIds"]
        , Namespace [] ["BenchTxSubServReq"]
        , Namespace [] ["SubmissionClientDiscardAcknowledged"]
        , Namespace [] ["BenchTxSubServDrop"]
        , Namespace [] ["SubmissionClientUnAcked"]
        , Namespace [] ["BenchTxSubServUnav"]
        , Namespace [] ["BenchTxSubServFed"]
        , Namespace [] ["BenchTxSubServCons"]
        , Namespace [] ["BenchTxSubIdle"]
        , Namespace [] ["BenchTxSubRateLimit"]
        , Namespace [] ["BenchTxSubSummary"]
        , Namespace [] ["BenchTxSubDebug"]
        , Namespace [] ["BenchTxSubError"]
        , Namespace [] ["BenchPlutusBudgetSummary"]
        ]

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


instance MetaTrace NodeToNodeSubmissionTrace where
    namespaceFor ReqIdsBlocking {} = Namespace [] ["ReqIdsBlocking"]
    namespaceFor IdsListBlocking {} = Namespace [] ["IdsListBlocking"]
    namespaceFor ReqIdsNonBlocking {} = Namespace [] ["ReqIdsNonBlocking"]
    namespaceFor IdsListNonBlocking {} = Namespace [] ["IdsListNonBlocking"]
    namespaceFor EndOfProtocol {} = Namespace [] ["EndOfProtocol"]
    namespaceFor ReqTxs {} = Namespace [] ["ReqTxs"]
    namespaceFor TxList {} = Namespace [] ["TxList"]

    severityFor _ _ = Just Info

    documentFor _ = Just ""

    allNamespaces = [
          Namespace [] ["ReqIdsBlocking"]
        , Namespace [] ["IdsListBlocking"]
        , Namespace [] ["ReqIdsNonBlocking"]
        , Namespace [] ["IdsListNonBlocking"]
        , Namespace [] ["EndOfProtocol"]
        , Namespace [] ["ReqTxs"]
        , Namespace [] ["TxList"]
        ]

instance LogFormatting SendRecvConnect where
  forHuman = Text.pack . show
  forMachine _ _ = KeyMap.fromList [ "kind" .= A.String "SendRecvConnect" ]

instance MetaTrace SendRecvConnect where
    namespaceFor _ = Namespace [] ["ReqIdsBlocking"]
    severityFor _ _ = Just Info

    documentFor _ = Just ""

    allNamespaces = [
          Namespace [] ["SendRecvConnect"]
        ]

instance LogFormatting SendRecvTxSubmission2 where
  forHuman = Text.pack . show
  forMachine _ _ = KeyMap.fromList [ "kind" .= A.String "SendRecvTxSubmission2" ]

instance MetaTrace SendRecvTxSubmission2 where
    namespaceFor _ = Namespace [] ["SendRecvTxSubmission2"]
    severityFor _ _ = Just Info

    documentFor _ = Just ""

    allNamespaces = [
          Namespace [] ["SendRecvTxSubmission2"]
        ]
