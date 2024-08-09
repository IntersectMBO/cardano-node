{-# OPTIONS_GHC -Wno-partial-fields #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

import           Data.Aeson hiding (Error)
import qualified Data.Aeson as AE
import           Data.Function
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import qualified System.IO as Sys

data TracerTrace
  -- | Static information about the build.
  = TracerBuildInfo
    { ttBuiltWithRTView      :: !Bool
    }
  | TracerParamsAre
    { ttConfigPath           :: !FilePath
    , ttStateDir             :: !(Maybe FilePath)
    , ttMinLogSeverity       :: !(Maybe SeverityS) }
  | TracerConfigIs
    { ttConfig               :: !TracerConfig
    , ttWarnRTViewMissing    :: !Bool
    }
  | TracerInitStarted
  | TracerInitEventQueues
  | TracerInitDone
  | TracerStartedLogRotator
  | TracerStartedPrometheus
    { ttPrometheusEndpoint   :: !Endpoint
    }
  | TracerStartedMonitoring
    { ttMonitoringEndpoint   :: !Endpoint
    , ttMonitoringType       :: !Text
    }
  | TracerStartedAcceptors
    { ttAcceptorsAddr        :: !Network }
  | TracerStartedRTView
  | TracerStartedReforwarder
  | TracerSockListen
    { ttListenAt             :: !FilePath }
  | TracerSockIncoming
    { ttConnectionIncomingAt :: !FilePath
    , ttAddr                 :: !Text }
  | TracerSockConnecting
    { ttConnectingTo         :: !FilePath }
  | TracerSockConnected
    { ttConnectedTo          :: !FilePath }
  | TracerShutdownInitiated
  | TracerShutdownHistBackup
  | TracerShutdownComplete
  | TracerError
    { ttError                :: !Text }
  | TracerResource
    { ttResource             :: !ResourceStats }
  deriving (Generic, Show)

instance ToJSON TracerTrace where
  toEncoding :: TracerTrace -> Encoding
  toEncoding = \case
    TracerBuildInfo{..} -> concatPairs
      [ "BuiltWithRTView" .= ttBuiltWithRTView
      , "kind"            .= txt "TracerBuildInfo"
      ]
    TracerParamsAre{..} -> concatPairs
      [ "ConfigPath"     .= ttConfigPath
      , "StateDir"       .= ttStateDir
      , "MinLogSeverity" .= ttMinLogSeverity
      , "kind"           .= txt "TracerParamsAre"
      ]
    TracerConfigIs{..} -> concatPairs $
      [ "Config"            .= ttConfig
      , "kind"              .= txt "TracerConfigIs" ] ++
      [ "WarnRTViewMissing" .= txt "RTView requested in config but cardano-tracer was built without it."
      | ttWarnRTViewMissing
      ]
    TracerInitStarted -> concatPairs
      [ "kind" .= txt "TracerInitStarted"
      ]
    TracerInitEventQueues -> concatPairs
      [ "kind" .= txt "TracerInitEventQueues"
      ]
    TracerInitDone -> concatPairs
      [ "kind" .= txt "TracerInitDone"
      ]
    TracerStartedLogRotator -> concatPairs
      [ "kind" .= txt "TracerStartedLogRotator"
      ]
    TracerStartedPrometheus{..} -> concatPairs
      [ "kind"     .= txt "TracerStartedPrometheus"
      , "endpoint" .= ttPrometheusEndpoint
      ]
    TracerStartedMonitoring{..} -> concatPairs
      [ "kind"     .= txt "TracerStartedMonitoring"
      , "endpoint" .= ttMonitoringEndpoint
      , "type"     .= ttMonitoringType
      ]
    TracerStartedAcceptors{..} -> concatPairs
      [ "kind"          .= txt "TracerStartedAcceptors"
      , "AcceptorsAddr" .= ttAcceptorsAddr
      ]
    TracerStartedRTView -> concatPairs
      [ "kind" .= txt "TracerStartedRTView"
      ]
    TracerStartedReforwarder -> concatPairs
      [ "kind" .= txt "TracerStartedReforwarder"
      ]
    TracerSockListen{..} -> concatPairs
      [ "kind"     .= txt "TracerSockListen"
      , "ListenAt" .= ttListenAt
      ]
    TracerSockIncoming{..} -> concatPairs
      [ "kind"                 .= txt "TracerSockIncoming"
      , "ConnectionIncomingAt" .= ttConnectionIncomingAt
      , "Addr"                 .= ttAddr
      ]
    TracerSockConnecting{..} -> concatPairs
      [ "kind"                 .= txt "TracerSockConnecting"
      , "ConnectionIncomingAt" .= ttConnectingTo
      ]
    TracerSockConnected{..} -> concatPairs
      [ "kind"        .= txt "TracerSockConnected"
      , "ConnectedTo" .= ttConnectedTo
      ]
    TracerShutdownInitiated -> concatPairs
      [ "kind" .= txt "TracerShutdownInitiated"
      ]
    TracerShutdownHistBackup -> concatPairs
      [ "kind" .= txt "TracerShutdownHistBackup"
      ]
    TracerShutdownComplete -> concatPairs
      [ "kind" .= txt "TracerShutdownComplete"
      ]
    TracerError{..} -> concatPairs
      [ "kind"  .= txt "TracerError"
      , "Error" .= ttError
      ]
    TracerResource{..} -> concatPairs
      [ "kind"     .= txt "TracerResource"
      , "Resource" .= ttResource
      ]
   where
    txt :: Text -> Text
    txt = id
    concatPairs :: [Series] -> Encoding
    concatPairs = pairs . mconcat

  toJSON = AE.genericToJSON jsonEncodingOptions

jsonEncodingOptions :: AE.Options
jsonEncodingOptions = AE.defaultOptions
  { AE.fieldLabelModifier     = drop 2
  , AE.tagSingleConstructors  = True
  , AE.sumEncoding =
    AE.TaggedObject
    { AE.tagFieldName = "kind"
    , AE.contentsFieldName = "contents"
    }
  }

instance LogFormatting TracerTrace where
  forHuman t@TracerConfigIs{ttWarnRTViewMissing = True} = T.pack $
    unlines
      [ show t ++ ": RTView requested in config but cardano-tracer was built without it."
      , "Enable with `-f +rtview`."
      ]
  forHuman t = T.pack (show t)

  forMachine DMinimal  _ = mempty
  forMachine DNormal   _ = mempty
  forMachine DDetailed t = forMachine DMaximum t
  forMachine DMaximum  t = case AE.toJSON t of
                             AE.Object x -> x
                             _ -> error "Impossible"

instance MetaTrace TracerTrace where
    namespaceFor TracerBuildInfo {} = Namespace [] ["BuildInfo"]
    namespaceFor TracerParamsAre {} = Namespace [] ["ParamsAre"]
    namespaceFor TracerConfigIs {} = Namespace [] ["ConfigIs"]
    namespaceFor TracerInitStarted = Namespace [] ["InitStart"]
    namespaceFor TracerInitEventQueues = Namespace [] ["EventQueues"]
    namespaceFor TracerInitDone = Namespace [] ["InitDone"]
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
    namespaceFor TracerResource {} = Namespace [] ["Resource"]

    severityFor (Namespace _ ["ParamsAre"]) _ = Just Warning
    severityFor (Namespace _ ["ConfigIs"]) _ = Just Warning
    severityFor (Namespace _ ["InitStart"]) _ = Just Info
    severityFor (Namespace _ ["EventQueues"]) _ = Just Info
    severityFor (Namespace _ ["InitDone"]) _ = Just Info
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
    severityFor (Namespace _ ["Resource"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor _ = Just ""

    allNamespaces = [
        Namespace [] ["ParamsAre"]
      , Namespace [] ["ConfigIs"]
      , Namespace [] ["InitStart"]
      , Namespace [] ["EventQueues"]
      , Namespace [] ["InitDone"]
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
      , Namespace [] ["Resource"]
      ]

stderrShowTracer :: Trace IO TracerTrace
stderrShowTracer =  contramapM'
    (either (const $ pure ()) (Sys.hPrint Sys.stderr) . snd)

stderrTracer :: Trace IO FormattedMessage
stderrTracer =
  contramapM'
    (either (const $ pure ()) (Sys.hPutStrLn Sys.stderr . T.unpack . render) . snd)
 where
   render = \case
      FormattedHuman _ x -> x
      FormattedMachine x -> x
      _ -> ""

mkTracerTracer :: SeverityF -> IO (Trace IO TracerTrace)
mkTracerTracer defSeverity = do
  base     :: Trace IO FormattedMessage <- standardTracer
  metaBase :: Trace IO TracerTrace      <-
    machineFormatter Nothing base
    >>= withDetailsFromConfig
  let tr = metaBase
           & withInnerNames
           & appendPrefixName "Tracer"
           & withSeverity
  configReflection <- emptyConfigReflection
  configureTracers configReflection initialTraceConfig [tr]
  pure tr
 where
   initialTraceConfig :: TraceConfig
   initialTraceConfig =
     TraceConfig
     { tcForwarder         = Nothing
     , tcNodeName          = Nothing
     , tcPeerFrequency     = Nothing
     , tcResourceFrequency = Nothing
     , tcMetricsPrefix     = Nothing
     , tcOptions = Map.fromList
                   [ ([],         [ConfSeverity defSeverity])
                   , (["Tracer"], [ConfDetail DMaximum])
                   ]
     }
