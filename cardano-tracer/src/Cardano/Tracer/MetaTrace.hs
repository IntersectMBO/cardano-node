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
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module Cardano.Tracer.MetaTrace
  ( module Cardano.Tracer.MetaTrace
  , Trace, SeverityF (..), SeverityS (..)
  , traceWith
  ) where

import "trace-dispatcher" Control.Tracer qualified as T
import Data.Aeson (ToJSON (..), (.=), Encoding, pairs)
import Data.Aeson qualified as AE
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import System.IO qualified as Sys

import Cardano.Logging

import Cardano.Tracer.Configuration

import Cardano.Logging.Resources

data TracerTrace
  = TracerParamsAre
    { ttConfigPath           :: !FilePath
    , ttStateDir             :: !(Maybe FilePath)
    , ttMinLogSeverity       :: !(Maybe SeverityS) }
  | TracerConfigIs
    { ttConfig               :: !TracerConfig }
  | TracerInitStarted
  | TracerInitEventQueues
  | TracerInitDone
  | TracerStartedLogRotator
  | TracerStartedPrometheus
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
    TracerParamsAre{..} -> pairs
        ("ConfigPath" .= ttConfigPath 
      <> "StateDir" .= ttStateDir 
      <> "MinLogSeverity" .= ttMinLogSeverity 
      <> "kind" .= ("TracerParamsAre" :: Text))
    TracerConfigIs{..} -> pairs
        ("Config" .= ttConfig 
      <> "kind" .= ("TracerConfigIs" :: Text))
    TracerInitStarted -> pairs
        ("kind" .= ("TracerInitStarted" :: Text))
    TracerInitEventQueues -> pairs
        ("kind" .= ("TracerInitEventQueues" :: Text))
    TracerInitDone -> pairs
        ("kind" .= ("TracerInitDone" :: Text))
    TracerStartedLogRotator -> pairs
        ("kind" .= ("TracerStartedLogRotator" :: Text))
    TracerStartedPrometheus -> pairs
        ("kind" .= ("TracerStartedPrometheus" :: Text))
    TracerStartedAcceptors{..} -> pairs
        ("kind" .= ("TracerStartedAcceptors" :: Text)
      <> "AcceptorsAddr" .= ttAcceptorsAddr)
    TracerStartedRTView -> pairs
        ("kind" .= ("TracerStartedRTView" :: Text))
    TracerStartedReforwarder -> pairs
        ("kind" .= ("TracerStartedReforwarder" :: Text))
    TracerSockListen{..} -> pairs
        ("kind" .= ("TracerSockListen" :: Text)
      <> "ListenAt" .= ttListenAt)
    TracerSockIncoming{..} -> pairs
        ("kind" .= ("TracerSockIncoming" :: Text)
      <> "ConnectionIncomingAt" .= ttConnectionIncomingAt
      <> "Addr" .= ttAddr)
    TracerSockConnecting{..} -> pairs
        ("kind" .= ("TracerSockConnecting" :: Text)
      <> "ConnectionIncomingAt" .= ttConnectingTo)
    TracerSockConnected{..} -> pairs
        ("kind" .= ("TracerSockConnected" :: Text)
      <> "ConnectedTo" .= ttConnectedTo)
    TracerShutdownInitiated -> pairs
        ("kind" .= ("TracerShutdownInitiated" :: Text))
    TracerShutdownHistBackup -> pairs
        ("kind" .= ("TracerShutdownHistBackup" :: Text))
    TracerShutdownComplete -> pairs
        ("kind" .= ("TracerShutdownComplete" :: Text))
    TracerError{..} -> pairs
        ("kind" .= ("TracerError" :: Text)
      <> "Error" .= ttError)
  toJSON = AE.genericToJSON jsonEncodingOptions

-- {"ConfigPath":"config.json","MinLogSeverity":null,"StateDir":null,"kind":"TracerParamsAre}"


--   toJSON = AE.genericToJSON jsonEncodingOptions
--   toEncoding = AE.genericToEncoding jsonEncodingOptions
-- instance ToJSON TracerTrace where
--   toJSON = AE.genericToJSON jsonEncodingOptions

-- instance ToJSON Person where
--     -- this generates a Value
--     toJSON (Person name age) =
--         object ["name" .= name, "age" .= age]

--     -- this encodes directly to a bytestring Builder
--     toEncoding (Person name age) =

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
  forHuman = T.pack . show
  forMachine DMinimal  _ = mempty
  forMachine DNormal   _ = mempty
  forMachine DDetailed t = forMachine DMaximum t
  forMachine DMaximum  t = case AE.toJSON t of
                             AE.Object x -> x
                             _ -> error "Impossible"

instance MetaTrace TracerTrace where
    namespaceFor TracerParamsAre {} = Namespace [] ["ParamsAre"]
    namespaceFor TracerConfigIs {} = Namespace [] ["ConfigIs"]
    namespaceFor TracerInitStarted = Namespace [] ["InitStart"]
    namespaceFor TracerInitEventQueues = Namespace [] ["EventQueues"]
    namespaceFor TracerInitDone = Namespace [] ["InitDone"]
    namespaceFor TracerStartedLogRotator = Namespace [] ["StartedLogRotator"]
    namespaceFor TracerStartedPrometheus = Namespace [] ["StartedPrometheus"]
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
      ]

stderrShowTracer :: Trace IO TracerTrace
stderrShowTracer =  Trace $ T.arrow $ T.emit
    (either (const $ pure ()) (Sys.hPrint Sys.stderr) . snd)

stderrTracer :: Trace IO FormattedMessage
stderrTracer =
  Trace $ T.arrow $ T.emit $
    either (const $ pure ()) (Sys.hPutStrLn Sys.stderr . T.unpack . render) . snd
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
     , tcOptions = Map.fromList
                   [ ([],         [ConfSeverity defSeverity])
                   , (["Tracer"], [ConfDetail DMaximum])
                   ]
     }
