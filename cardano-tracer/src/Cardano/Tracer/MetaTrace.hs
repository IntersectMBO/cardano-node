{-# OPTIONS_GHC -Wno-partial-fields #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracer.MetaTrace
  ( module Cardano.Tracer.MetaTrace
  , Trace, SeverityF (..), SeverityS (..)
  , traceWith
  ) where

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (NodeId (..), NodeName)

import           Data.Aeson hiding (Error)
import qualified Data.Aeson as AE
import qualified Data.Map.Strict as Map
import           Data.Text as T (Text, pack)
import qualified System.IO as Sys



rtViewConfigWarning :: Text
rtViewConfigWarning = "RTView requested in config but cardano-tracer was built without it"

data TracerTrace
  -- | Static information about the build.
  = TracerBuildInfo
    { ttBuiltWithRTView      :: Bool
    }
  | TracerParamsAre
    { ttConfigPath           :: FilePath
    , ttStateDir             :: Maybe FilePath
    , ttMinLogSeverity       :: Maybe SeverityS }
  | TracerConfigIs
    { ttConfig               :: TracerConfig
    , ttWarnRTViewMissing    :: Bool
    }
  | TracerInitStarted
  | TracerInitEventQueues
  | TracerInitDone
  | TracerAddNewNodeIdMapping
    { ttBimapping :: !(NodeId, NodeName)
    }
  | TracerStartedLogRotator
  | TracerStartedPrometheus
    { ttPrometheusEndpoint   :: Endpoint
    }
  | TracerStartedMonitoring
    { ttMonitoringEndpoint   :: Endpoint
    , ttMonitoringType       :: Text
    }
  | TracerStartedAcceptors
    { ttAcceptorsAddr        :: Network }
  | TracerStartedRTView
  | TracerStartedReforwarder
  | TracerSockListen
    { ttListenAt             :: FilePath }
  | TracerSockIncoming
    { ttConnectionIncomingAt :: FilePath
    , ttAddr                 :: Text }
  | TracerSockConnecting
    { ttConnectingTo         :: FilePath }
  | TracerSockConnected
    { ttConnectedTo          :: FilePath }
  | TracerShutdownInitiated
  | TracerShutdownHistBackup
  | TracerShutdownComplete
  | TracerError
    { ttError                :: Text }
  | TracerResource
    { ttResource             :: ResourceStats }
  | TracerForwardingInterrupted
    { ttConnection           :: HowToConnect
    , ttMessage              :: String
    }
  deriving Show


instance LogFormatting TracerTrace where
  forHuman t@TracerConfigIs{ttWarnRTViewMissing = True} =
      rtViewConfigWarning <> ": " <> forHuman t {ttWarnRTViewMissing = False}
  forHuman (TracerForwardingInterrupted howToConnect msg) =
      T.pack $ "connection with " <> show howToConnect <> " failed: " <> msg
  forHuman _ = ""

  forMachine _dtal = \case
    TracerBuildInfo{..} -> mconcat
      [ "builtWithRTView" .= ttBuiltWithRTView
      , "kind"            .= AE.String "TracerBuildInfo"
      ]
    TracerParamsAre{..} -> mconcat
      [ "configPath"     .= ttConfigPath
      , "stateDir"       .= ttStateDir
      , "minLogSeverity" .= ttMinLogSeverity
      , "kind"           .= AE.String "TracerParamsAre"
      ]
    TracerConfigIs{..} -> mconcat $
      [ "config"            .= ttConfig
      , "kind"              .= AE.String "TracerConfigIs" ] ++
      [ "warnRTViewMissing" .= rtViewConfigWarning
      | ttWarnRTViewMissing
      ]
    TracerInitStarted -> mconcat
      [ "kind" .= AE.String "TracerInitStarted"
      ]
    TracerInitEventQueues -> mconcat
      [ "kind" .= AE.String "TracerInitEventQueues"
      ]
    TracerInitDone -> mconcat
      [ "kind" .= AE.String "TracerInitDone"
      ]
    TracerAddNewNodeIdMapping (NodeId nodeId, nodeName) -> mconcat
      [ "kind"     .= AE.String "TracerAddNewNodeIdMapping"
      , "nodeId"   .= AE.String nodeId
      , "nodeName" .= AE.String nodeName
      ]
    TracerStartedLogRotator -> mconcat
      [ "kind" .= AE.String "TracerStartedLogRotator"
      ]
    TracerStartedPrometheus{..} -> mconcat
      [ "kind"     .= AE.String "TracerStartedPrometheus"
      , "endpoint" .= ttPrometheusEndpoint
      ]
    TracerStartedMonitoring{..} -> mconcat
      [ "kind"     .= AE.String "TracerStartedMonitoring"
      , "endpoint" .= ttMonitoringEndpoint
      , "type"     .= ttMonitoringType
      ]
    TracerStartedAcceptors{..} -> mconcat
      [ "kind"          .= AE.String "TracerStartedAcceptors"
      , "AcceptorsAddr" .= ttAcceptorsAddr
      ]
    TracerStartedRTView -> mconcat
      [ "kind" .= AE.String "TracerStartedRTView"
      ]
    TracerStartedReforwarder -> mconcat
      [ "kind" .= AE.String "TracerStartedReforwarder"
      ]
    TracerSockListen{..} -> mconcat
      [ "kind"     .= AE.String "TracerSockListen"
      , "listenAt" .= ttListenAt
      ]
    TracerSockIncoming{..} -> mconcat
      [ "kind"                 .= AE.String "TracerSockIncoming"
      , "connectionIncomingAt" .= ttConnectionIncomingAt
      , "addr"                 .= ttAddr
      ]
    TracerSockConnecting{..} -> mconcat
      [ "kind"                 .= AE.String "TracerSockConnecting"
      , "connectionIncomingAt" .= ttConnectingTo
      ]
    TracerSockConnected{..} -> mconcat
      [ "kind"        .= AE.String "TracerSockConnected"
      , "connectedTo" .= ttConnectedTo
      ]
    TracerShutdownInitiated -> mconcat
      [ "kind" .= AE.String "TracerShutdownInitiated"
      ]
    TracerShutdownHistBackup -> mconcat
      [ "kind" .= AE.String "TracerShutdownHistBackup"
      ]
    TracerShutdownComplete -> mconcat
      [ "kind" .= AE.String "TracerShutdownComplete"
      ]
    TracerError{..} -> mconcat
      [ "kind"  .= AE.String "TracerError"
      , "error" .= ttError
      ]
    TracerResource{..} -> forMachine _dtal ttResource
    TracerForwardingInterrupted{..} -> mconcat
      [ "kind"    .= AE.String "TracerForwardingInterrupted"
      , "conn"    .= ttConnection
      , "message" .= ttMessage
      ]

instance MetaTrace TracerTrace where
    namespaceFor TracerBuildInfo {} = Namespace [] ["BuildInfo"]
    namespaceFor TracerParamsAre {} = Namespace [] ["ParamsAre"]
    namespaceFor TracerConfigIs {} = Namespace [] ["ConfigIs"]
    namespaceFor TracerInitStarted = Namespace [] ["InitStart"]
    namespaceFor TracerInitEventQueues = Namespace [] ["EventQueues"]
    namespaceFor TracerInitDone = Namespace [] ["InitDone"]
    namespaceFor TracerAddNewNodeIdMapping {} = Namespace [] ["AddNewNodeIdMapping"]
    namespaceFor TracerStartedLogRotator = Namespace [] ["StartedLogRotator"]
    namespaceFor TracerStartedPrometheus{} = Namespace [] ["StartedPrometheus"]
    namespaceFor TracerStartedMonitoring{} = Namespace [] ["StartedMonitoring"]
    namespaceFor TracerStartedAcceptors {} = Namespace [] ["StartedAcceptors"]
    namespaceFor TracerStartedRTView = Namespace [] ["StartedRTView"]
    namespaceFor TracerStartedReforwarder = Namespace [] ["StartedReforwarder"]
    namespaceFor TracerSockListen {} = Namespace [] ["SockListen"]
    namespaceFor TracerSockIncoming {} = Namespace [] ["SockIncoming"]
    namespaceFor TracerSockConnecting {} = Namespace [] ["SockConnecting"]
    namespaceFor TracerSockConnected {} = Namespace [] ["SockConnected"]
    namespaceFor TracerShutdownInitiated = Namespace [] ["ShutdownInitiated"]
    namespaceFor TracerShutdownHistBackup = Namespace [] ["ShutdownHistBackup"]
    namespaceFor TracerShutdownComplete = Namespace [] ["ShutdownComplete"]
    namespaceFor TracerError {} = Namespace [] ["Error"]
    namespaceFor TracerResource {} = Namespace [] ["Resources"]
    namespaceFor TracerForwardingInterrupted {} = Namespace [] ["ForwardingInterrupted"]

    severityFor (Namespace _ ["BuildInfo"]) _ = Just Info
    severityFor (Namespace _ ["ParamsAre"]) _ = Just Warning
    severityFor (Namespace _ ["ConfigIs"]) _ = Just Warning
    severityFor (Namespace _ ["InitStart"]) _ = Just Info
    severityFor (Namespace _ ["EventQueues"]) _ = Just Info
    severityFor (Namespace _ ["InitDone"]) _ = Just Info
    severityFor (Namespace _ ["AddNewNodeIdMapping"]) _ = Just Info
    severityFor (Namespace _ ["StartedLogRotator"]) _ = Just Info
    severityFor (Namespace _ ["StartedPrometheus"]) _ = Just Info
    severityFor (Namespace _ ["StartedMonitoring"]) _ = Just Info
    severityFor (Namespace _ ["StartedAcceptors"]) _ = Just Info
    severityFor (Namespace _ ["StartedRTView"]) _ = Just Info
    severityFor (Namespace _ ["StartedReforwarder"]) _ = Just Info
    severityFor (Namespace _ ["SockListen"]) _ = Just Info
    severityFor (Namespace _ ["SockIncoming"]) _ = Just Info
    severityFor (Namespace _ ["SockConnecting"]) _ = Just Info
    severityFor (Namespace _ ["SockConnected"]) _ = Just Info
    severityFor (Namespace _ ["ShutdownInitiated"]) _ = Just Warning
    severityFor (Namespace _ ["ShutdownHistBackup"]) _ = Just Info
    severityFor (Namespace _ ["ShutdownComplete"]) _ = Just Warning
    severityFor (Namespace _ ["Error"]) _ = Just Error
    severityFor (Namespace _ ["Resources"]) _ = Just Info
    severityFor (Namespace _ ["ForwardingInterrupted"]) _ = Just Warning
    severityFor _ _ = Nothing

    documentFor _ = Just ""

    allNamespaces = [
        Namespace [] ["BuildInfo"]
      , Namespace [] ["ParamsAre"]
      , Namespace [] ["ConfigIs"]
      , Namespace [] ["InitStart"]
      , Namespace [] ["EventQueues"]
      , Namespace [] ["InitDone"]
      , Namespace [] ["AddNewNodeIdMapping"]
      , Namespace [] ["StartedLogRotator"]
      , Namespace [] ["StartedPrometheus"]
      , Namespace [] ["StartedMonitoring"]
      , Namespace [] ["StartedAcceptors"]
      , Namespace [] ["StartedRTView"]
      , Namespace [] ["StartedReforwarder"]
      , Namespace [] ["SockListen"]
      , Namespace [] ["SockIncoming"]
      , Namespace [] ["SockConnecting"]
      , Namespace [] ["SockConnected"]
      , Namespace [] ["ShutdownInitiated"]
      , Namespace [] ["ShutdownHistBackup"]
      , Namespace [] ["ShutdownComplete"]
      , Namespace [] ["Error"]
      , Namespace [] ["Resources"]
      , Namespace [] ["ForwardingInterrupted"]
      ]

stderrShowTracer :: Trace IO TracerTrace
stderrShowTracer =  contramapM'
    (either (const $ pure ()) (Sys.hPrint Sys.stderr) . snd)

mkTracerTracer :: SeverityF -> IO (Trace IO TracerTrace)
mkTracerTracer defSeverity = do
  standardTracer
    >>= machineFormatter
    >>= filterSeverityFromConfig
    >>= \t ->
          let finalTracer = withNames ["Tracer"] (withSeverity t)
          in configTracerTracer defSeverity finalTracer >> pure finalTracer

configTracerTracer :: SeverityF -> Trace IO TracerTrace -> IO ()
configTracerTracer defSeverity tr = do
  configReflection <- emptyConfigReflection
  configureTracers configReflection initialTraceConfig [tr]
 where
   initialTraceConfig :: TraceConfig
   initialTraceConfig =
     TraceConfig
     { tcForwarder         = Nothing
     , tcApplicationName   = Nothing
     , tcPeriodic          = Map.empty
     , tcMetricsPrefix     = Nothing
     , tcOptions = Map.fromList
                   [ ([],         [ConfSeverity defSeverity])
                   , (["Tracer"], [ConfDetail DMaximum])
                   ]
     }
