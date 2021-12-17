{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Tracing.Startup where

import           Prelude

import           Data.Text (Text)

import           Cardano.Logging (LogFormatting (..))
import           Cardano.Node.Startup
import           Cardano.TraceDispatcher.Compat
import           Cardano.TraceDispatcher.Tracers.Startup
import           Cardano.Tracing.OrphanInstances.Network ()

import           Cardano.BM.Tracing (HasPrivacyAnnotation (..),
                   HasSeverityAnnotation (..), Severity (..), ToObject (..),
                   Transformable (..))
import           Cardano.BM.Data.Tracer (HasTextFormatter (..),
                   trStructuredText)

import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (BlockNodeToClientVersion, BlockNodeToNodeVersion)


instance HasSeverityAnnotation (StartupTrace blk) where
    getSeverityAnnotation (StartupSocketConfigError _) = Error
    getSeverityAnnotation (NetworkConfigUpdateError _) = Error
    getSeverityAnnotation P2PWarning = Warning
    getSeverityAnnotation P2PWarningDevelopementNetworkProtocols = Warning
    getSeverityAnnotation WarningDevelopmentNetworkProtocols {} = Warning
    getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (StartupTrace blk)

instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
      => Transformable Text IO (StartupTrace blk) where
  trTransformer = trStructuredText

instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
        => HasTextFormatter (StartupTrace blk) where
  formatText a _ = ppStartupInfoTrace a

instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
        => ToObject (StartupTrace blk) where
  toObject verb = forMachine (toDetailLevel verb)
