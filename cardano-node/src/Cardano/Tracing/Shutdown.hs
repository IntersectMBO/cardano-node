{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.Shutdown () where

import           Data.Text (Text)
import           Prelude (IO)

import           Cardano.BM.Data.Tracer (HasTextFormatter (..), trStructuredText)
import           Cardano.BM.Tracing (HasPrivacyAnnotation (..), HasSeverityAnnotation (..),
                   Severity (..), ToObject (..), Transformable (..))
import           Cardano.Logging (LogFormatting (..))
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Tracing.Compat
import           Cardano.Node.Tracing.Tracers.Shutdown

instance HasPrivacyAnnotation  ShutdownTrace
instance HasSeverityAnnotation ShutdownTrace where
  getSeverityAnnotation _ = Warning

instance Transformable Text IO ShutdownTrace where
  trTransformer = trStructuredText

instance HasTextFormatter ShutdownTrace where
  formatText a _ = ppShutdownTrace a

instance  ToObject ShutdownTrace where
  toObject verb = forMachine (toDetailLevel verb)
