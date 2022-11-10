{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracer.MetaTrace
  ( module Cardano.Tracer.MetaTrace
  , Trace, SeverityF (..), SeverityS (..)
  , traceWith
  ) where

import qualified "trace-dispatcher" Control.Tracer as T
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as AE
import Data.Function
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import qualified System.IO as Sys

import Cardano.Logging

import Cardano.Tracer.Configuration


ctorTracerTrace :: TracerTrace -> Text
ctorTracerTrace = \case
  TracerParamsAre{}            -> "TracerParamsAre"
  TracerConfigIs{}             -> "TracerConfigIs"
  TracerInitStarted{}          -> "TracerInitStarted"
  TracerInitEventQueues{}      -> "TracerInitEventQueues"
  TracerInitDone{}             -> "TracerInitDone"
  TracerSockListen{}           -> "TracerSockListen"
  TracerSockIncoming{}         -> "TracerSockIncoming"
  TracerSockConnecting{}       -> "TracerSockConnecting"
  TracerSockConnected{}        -> "TracerSockConnected"
  TracerShutdownInitiated{}    -> "TracerShutdownInitiated"
  TracerShutdownHistBackup{}   -> "TracerShutdownHistBackup"
  TracerShutdownComplete{}     -> "TracerShutdownComplete"
  TracerError{}                -> "TracerError"

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
  deriving (Generic, Show)

instance ToJSON TracerTrace where
  toJSON = AE.genericToJSON jsonEncodingOptions
  toEncoding = AE.genericToEncoding jsonEncodingOptions

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
  forMachine DNormal   t = mconcat [ "kind" .= AE.String (ctorTracerTrace t) ]
  forMachine DDetailed t = forMachine DMaximum t
  forMachine DMaximum  t = case AE.toJSON t of
                             AE.Object x -> x
                             _ -> error "Impossible"

stderrShowTracer :: Trace IO TracerTrace
stderrShowTracer =
  Trace $ T.arrow $ T.emit $
    either (const $ pure ()) (Sys.hPrint Sys.stderr) . snd

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
           & withNamesAppended ((:[]) . ctorTracerTrace)
           & appendName "Tracer"
           & withSeverity
               (\case
                   TracerParamsAre{}         -> Warning
                   TracerConfigIs{}          -> Warning
                   TracerShutdownInitiated{} -> Warning
                   TracerShutdownComplete{}  -> Warning
                   TracerError{}             -> Error
                   _                         -> Info)
  configureTracers initialTraceConfig trDoc [tr]
  pure tr
 where
   initialTraceConfig :: TraceConfig
   initialTraceConfig =
     TraceConfig
     { tcForwarder         = defaultForwarder
     , tcNodeName          = Nothing
     , tcPeerFrequency     = Nothing
     , tcResourceFrequency = Nothing
     , tcOptions = Map.fromList
                   [ ([],         [ConfSeverity defSeverity])
                   , (["Tracer"], [ConfDetail DMaximum])
                   ]
     }
   trDoc :: Documented TracerTrace
   trDoc = Documented []
