module Cardano.Node.Tracing.Consistency
  ( checkNodeTraceConfiguration
  , checkNodeTraceConfigurationWith
  , DocRun
  ) where

import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.Documentation (DocRun (..), docTracersFirstPhase)

import           Hermod.Tracing
import           Hermod.Tracing.DocuGenerator (DocTracer (..))


-- | Check the configuration in the given file against the tracers the node
-- declares. An empty return list means everything is well.
checkNodeTraceConfiguration ::
     FilePath
  -> IO NSWarnings
checkNodeTraceConfiguration configFileName = do
  run <- docTracersFirstPhase Nothing
  checkNodeTraceConfigurationWith run configFileName

-- | Check the configuration in the given file against an already computed
-- documentation run (from 'docTracersFirstPhase'), so that callers checking
-- several configurations can share a single documentation pass. The run's
-- own warnings (missing severities, missing documentation) are included.
checkNodeTraceConfigurationWith ::
     DocRun
  -> FilePath
  -> IO NSWarnings
checkNodeTraceConfigurationWith run configFileName = do
  w1 <- checkTraceConfiguration
          (FromFile configFileName)
          defaultCardanoConfig
          (drNamespaces run)
  pure $ w1 <> dtWarnings (drDocTracer run)
