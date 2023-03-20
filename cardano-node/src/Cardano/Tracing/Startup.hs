{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tracing.Startup where

import           Data.Text (Text)
import           Prelude

import           Cardano.Logging (LogFormatting (..))
import           Cardano.Node.Startup
import           Cardano.Node.Tracing.Compat
import           Cardano.Node.Tracing.Tracers.Startup
import           Cardano.Tracing.OrphanInstances.Network ()

import           Cardano.BM.Data.Tracer (HasTextFormatter (..), trStructuredText)
import           Cardano.BM.Tracing (HasPrivacyAnnotation (..), HasSeverityAnnotation (..),
                   Severity (..), ToObject (..), Transformable (..))

import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                   BlockNodeToNodeVersion)


instance HasSeverityAnnotation (StartupTrace blk) where
    getSeverityAnnotation (StartupSocketConfigError _) = Error
    getSeverityAnnotation NetworkConfigUpdate = Notice
    getSeverityAnnotation (NetworkConfigUpdateError _) = Error
    getSeverityAnnotation NetworkConfigUpdateUnsupported = Warning
    getSeverityAnnotation P2PWarning = Warning
    getSeverityAnnotation P2PWarningDevelopementNetworkProtocols = Warning
    getSeverityAnnotation WarningDevelopmentNodeToNodeVersions {} = Warning
    getSeverityAnnotation WarningDevelopmentNodeToClientVersions {} = Warning
    getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (StartupTrace blk)

instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
      => Transformable Text IO (StartupTrace blk) where
  trTransformer = trStructuredText

instance HasTextFormatter (StartupTrace blk) where
  formatText a _ = ppStartupInfoTrace a

instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
        => ToObject (StartupTrace blk) where
  toObject verb = forMachine (toDetailLevel verb)
