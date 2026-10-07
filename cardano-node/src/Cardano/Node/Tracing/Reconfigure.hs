{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Re-reading the trace options of the node's configuration file and applying
-- them to the traces the node is already running, so that an operator can
-- change severities, detail levels and stdout/forwarder routing with a
-- @SIGHUP@ instead of a restart.
--
-- What cannot be reloaded, because it is decided once when the tracing system
-- is built: whether forwarding is enabled at all and its socket and queue
-- parameters, the PrometheusSimple listener and its DoS parameters, the
-- metrics prefix, the EKG store, the periodic tracer intervals, the data
-- points, the set of traces itself, and the frequency of a limiter that has
-- already been created.
module Cardano.Node.Tracing.Reconfigure
  ( TracingReconfigure (..)
  , mkTracingReconfigure
  ) where

import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Tracing.Registry (ApplyTraceConfig (..))

import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import           Control.Exception (try)
import qualified Control.Exception as Exception
import           Data.Text (Text, pack)

import           Hermod.Tracing (TraceConfig)
import           Hermod.Tracing.API.Tracer (Tracer, traceWith)
import           Hermod.Tracing.ConfigurationParser (ConfigSource (..),
                   readConfigurationWithDefault)

-- | The node's handle for reloading the trace options at runtime.
newtype TracingReconfigure = TracingReconfigure
  { -- | Re-read the trace options and apply them. Never throws: a
    -- configuration that cannot be read or cannot be applied leaves the
    -- running one in force.
    reconfigureTracing :: IO ()
  }

-- | The 'MVar' is both the mutex and the last configuration known to apply
-- cleanly. A mutex is required: each @SIGHUP@ handler runs on a freshly forked
-- thread, and two interleaved configuration passes over one trace throw
-- (hermod detects a trace that was not reset before being reconfigured).
-- Holding the configuration in the same 'MVar' makes "store it only on
-- success" the same step as releasing the lock.
mkTracingReconfigure
  :: forall blk
   . Tracer IO (StartupTrace blk)
     -- ^ the node's startup tracer; note it is itself one of the traces being
     -- reconfigured
  -> FilePath
     -- ^ the node's configuration file, re-read on every reload
  -> TraceConfig
     -- ^ the configuration applied at start-up: the first "last good"
  -> ApplyTraceConfig
  -> IO TracingReconfigure
mkTracingReconfigure startupTr configFile initialConfig ApplyTraceConfig{..} = do
    stateVar :: MVar TraceConfig <- newMVar initialConfig
    pure $ TracingReconfigure $ modifyMVar_ stateVar reload
  where
    reload :: TraceConfig -> IO TraceConfig
    reload lastGood = do
      -- Emitted under the configuration still in force, so it is visible
      -- whenever the Startup namespace was visible before the reload.
      traceWith startupTr TraceConfigUpdate

      -- Read before applying: a malformed or missing file must leave the
      -- running traces untouched.
      read' <- try @Exception.SomeException $
                 readConfigurationWithDefault (FromFile configFile) lastGood
      case read' of
        Left err -> do
          report $ "could not be read: " <> displayed err
          pure lastGood
        Right newConfig -> do
          applied <- try @Exception.SomeException (applyTraceConfig newConfig)
          case applied of
            Right _warnings -> do
              -- Subject to the configuration just applied, so a reload that
              -- silences Startup suppresses this. The unconditional record is
              -- hermod's own Reflection.TracerInfoConfig.
              traceWith startupTr TraceConfigUpdated
              pure newConfig
            Left err -> do
              rollback lastGood err
              pure lastGood

    -- Restore first and report second: a half-applied pass can leave the
    -- startup tracer itself discarding everything, which would swallow the
    -- report. Every pass begins by resetting each trace, so a restore cannot
    -- be refused for any configuration that applied cleanly before.
    rollback :: TraceConfig -> Exception.SomeException -> IO ()
    rollback lastGood err = do
      restored <- try @Exception.SomeException (restoreTraceConfig lastGood)
      report $ case restored of
        Right () ->
          "could not be applied, the previous one is back in force: "
          <> displayed err
        Left err' ->
          "could not be applied (" <> displayed err
          <> ") and restoring the previous one failed as well ("
          <> displayed err'
          <> "); tracing is degraded until the next SIGHUP or a restart"

    report :: Text -> IO ()
    report what =
      traceWith startupTr . TraceConfigUpdateError $
        "the trace configuration " <> what

    displayed :: Exception.SomeException -> Text
    displayed = pack . Exception.displayException
