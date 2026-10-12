{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The trace options can be re-read and applied to traces that are already
-- running. These properties drive the registry the node itself builds, so they
-- cover every declared trace rather than a hand-made one.
module Test.Cardano.Tracing.NewTracing.Reconfigure (tests) where

import           Cardano.Network.NodeToClient (LocalAddress)
import           Cardano.Network.NodeToNode (RemoteAddress)
import           Cardano.Node.Orphans ()
import           Cardano.Node.Queries ()
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Tracing (Tracers (..))
import           Cardano.Node.Tracing.DefaultTraceConfig (defaultCardanoConfig)
import           Cardano.Node.Tracing.Reconfigure (TracingReconfigure (..), mkTracingReconfigure)
import           Cardano.Node.Tracing.Registry (ApplyTraceConfig (..), Backends (..), Purpose (..),
                   applier, newRegistry)
import           Cardano.Node.Tracing.Tracers (buildNodeTracers)
import           Cardano.Node.Tracing.Tracers.HasIssuer ()
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import           Control.Exception (IOException, catch, finally)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as IO
import           System.IO (hClose, openTempFile)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H

import           Hermod.Tracing (ConfigOption (..), FormattedMessage (..), SeverityF (..),
                   SeverityS (..), TraceConfig (..), traceWith)
import           Hermod.Tracing.API.Tracer (Tracer, mkTracer)

type Blk = CardanoBlock StandardCrypto

type Tracers' = Tracers RemoteAddress LocalAddress Blk IO

-- | A terminal backend that keeps what reaches it, rendered.
capturing :: IORef [Text] -> Tracer IO FormattedMessage
capturing ref = mkTracer $ \msg ->
  atomicModifyIORef' ref (\msgs -> (dropTimestamp (rendered msg) : msgs, ()))
 where
  rendered = \case
    FormattedHuman _ t   -> t
    FormattedMachine t   -> t
    FormattedMetrics ms  -> T.pack (show ms)
    FormattedForwarder _ -> "<forwarded>"
    FormattedCBOR _      -> "<cbor>"

  -- Every line is stamped with the moment it was emitted, so two runs are
  -- only comparable from the namespace onwards.
  dropTimestamp t =
    let (_, fromNs) = T.breakOn "\"ns\":" t
    in if T.null fromNs then t else fromNs

-- | The startup tracer the reload reports through, classified at trace time:
-- 'StartupTrace' has no 'Show' instance to annotate with.
reportSink :: IORef [Text] -> Tracer IO (StartupTrace Blk)
reportSink ref = mkTracer $ \case
    TraceConfigUpdate        -> push "update"
    TraceConfigUpdated       -> push "updated"
    TraceConfigUnchanged     -> push "unchanged"
    TraceConfigUpdateError e -> push ("error: " <> e)
    _                        -> pure ()
  where
    push t = atomicModifyIORef' ref (\ts -> (t : ts, ()))

-- | A unique configuration file for a reload to read, removed afterwards.
-- @hedgehog-extras@' 'moduleWorkspace' wants @MonadResource@, which
-- @PropertyT IO@ does not provide.
withConfigFile :: (FilePath -> IO a) -> IO a
withConfigFile act = do
  tmp <- IO.getTemporaryDirectory
  (path, h) <- openTempFile tmp "reconfigure-config.json"
  hClose h
  -- one of the cases below removes the file itself
  act path `finally` (IO.removeFile path `catch` \e -> ignore (e :: IOException))
 where
  ignore _ = pure ()

-- | The node's registry over a capturing stdout backend, plus the applier.
nodeRegistry :: IO (IORef [Text], Tracers', ApplyTraceConfig)
nodeRegistry = do
  ref <- newIORef []
  registry <- newRegistry ForRuntime Backends
    { bkStdout    = capturing ref
    , bkForward   = mempty
    , bkEKG       = Nothing
    , bkDataPoint = mempty
    }
  tracers <- buildNodeTracers @Blk registry
  apply <- applier registry
  pure (ref, tracers, apply)

-- | Raise the Startup namespace to Info, so that 'StartupDBValidation' (Info,
-- by the catch-all in its 'MetaTrace' instance) passes the severity filter;
-- 'defaultCardanoConfig' sets the root to Notice, which hides it.
startupVisible :: TraceConfig -> TraceConfig
startupVisible cfg = cfg
  { tcOptions = Map.insert ["Startup"] [ConfSeverity (SeverityF (Just Info))] (tcOptions cfg) }

-- | Apply a configuration, then emit one Startup message and report what the
-- backend saw. The capture is cleared after applying, because applying traces
-- the tracer info and the effective configuration through the same backend.
probe :: ApplyTraceConfig -> Tracers' -> IORef [Text] -> TraceConfig -> IO [Text]
probe apply tracers ref cfg = do
  _warnings <- applyTraceConfig apply cfg
  writeIORef ref []
  traceWith (startupTracer tracers) StartupDBValidation
  readIORef ref

-- | Reconfiguration must change what the backend sees, and must be reversible:
-- A then B then A puts the original behaviour back, rather than latching.
prop_reconfigureChangesOutput :: Property
prop_reconfigureChangesOutput = H.withTests 1 $ H.withShrinks 0 $ H.property $ do
  (a1, b, a2) <- liftIO $ do
    (ref, tracers, apply) <- nodeRegistry
    a1 <- probe apply tracers ref defaultCardanoConfig
    b  <- probe apply tracers ref (startupVisible defaultCardanoConfig)
    a2 <- probe apply tracers ref defaultCardanoConfig
    pure (a1, b, a2)
  H.annotateShow (a1, b, a2)
  -- a root severity of Notice hides an Info message
  a1 === []
  -- Startup raised to Info shows it
  H.assert (any ("StartupDBValidation" `T.isInfixOf`) b)
  -- and going back is a real revert
  a2 === a1

-- | A configuration that cannot be read leaves the running one in force. Each
-- case must end with the traces behaving exactly as before the attempt, and
-- with the failure reported.
prop_malformedConfigIsNoOp :: Property
prop_malformedConfigIsNoOp = H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    results <- liftIO $ withConfigFile $ \cfgFile -> do
      (ref, tracers, apply) <- nodeRegistry
      reported <- newIORef []
      -- start from a configuration under which the probe message is visible
      before <- probe apply tracers ref (startupVisible defaultCardanoConfig)
      reconf <- mkTracingReconfigure @Blk (reportSink reported) cfgFile
                  defaultCardanoConfig (startupVisible defaultCardanoConfig) apply

      let attempt :: IO () -> IO ([Text], [Text])
          attempt prepare = do
            prepare
            writeIORef reported []
            reconfigureTracing reconf
            writeIORef ref []
            traceWith (startupTracer tracers) StartupDBValidation
            (,) <$> readIORef ref <*> (reverse <$> readIORef reported)

      invalidYaml <- attempt (T.writeFile cfgFile "{ this is not: [valid")
      badEnum     <- attempt (T.writeFile cfgFile
                       "{\"TraceOptions\":{\"\":{\"severity\":\"Nonsense\"}}}")
      -- a missing file is why readConfiguration' must not be used here: it
      -- silently substitutes a minimal configuration
      missing     <- attempt (IO.removeFile cfgFile)
      pure (before, [invalidYaml, badEnum, missing])

    let (before, attempts) = results
    H.annotateShow results
    -- every attempt is a complete no-op for the traces ...
    mapM_ (\(out, _) -> out === before) attempts
    -- ... and every attempt reports the failure
    mapM_ (\(_, seen) -> H.assert (any ("error: " `T.isPrefixOf`) seen)) attempts

-- | Two reconfigurations at once must serialise: hermod throws when a trace is
-- reconfigured without being reset first, which is exactly what interleaved
-- passes do -- and two @SIGHUP@s milliseconds apart each run on their own
-- forked thread.
prop_concurrentReconfigureIsSerialised :: Property
prop_concurrentReconfigureIsSerialised = H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    (reported, concurrentOut, singleOut) <- liftIO $ withConfigFile $ \cfgFile -> do
      T.writeFile cfgFile "{\"TraceOptions\":{\"Startup\":{\"severity\":\"Info\"}}}"
      seen <- newIORef []

      (ref, tracers, apply) <- nodeRegistry
      reconf <- mkTracingReconfigure @Blk (reportSink seen) cfgFile
                  defaultCardanoConfig defaultCardanoConfig apply

      -- both threads blocked on the same gate, so the passes really do overlap
      gate  <- newEmptyMVar
      done1 <- newEmptyMVar
      done2 <- newEmptyMVar
      let pass done = forkIO $
            (readMVar gate >> reconfigureTracing reconf) `finally` putMVar done ()
      _ <- pass done1
      _ <- pass done2
      putMVar gate ()
      takeMVar done1
      takeMVar done2

      writeIORef ref []
      traceWith (startupTracer tracers) StartupDBValidation
      concurrentOut <- readIORef ref

      -- the same reload once, on its own registry, for comparison
      (ref', tracers', apply') <- nodeRegistry
      reconf' <- mkTracingReconfigure @Blk (reportSink seen) cfgFile
                   defaultCardanoConfig defaultCardanoConfig apply'
      reconfigureTracing reconf'
      writeIORef ref' []
      traceWith (startupTracer tracers') StartupDBValidation
      singleOut <- readIORef ref'

      (\ts -> (reverse ts, concurrentOut, singleOut)) <$> readIORef seen

    H.annotateShow (reported, concurrentOut, singleOut)
    -- no pass raced with the other
    H.assert (not (any raced reported))
    -- the file raises Startup to Info, so the reload has an observable effect
    H.assert (any ("StartupDBValidation" `T.isInfixOf`) singleOut)
    -- and the outcome is the outcome of a single pass
    concurrentOut === singleOut
 where
  raced t =
    "not reset before reconfiguration" `T.isInfixOf` t
      || "Inconsistent trace configuration" `T.isInfixOf` t

-- | A reload whose trace options equal the ones in force must not apply them
-- again: applying resets every trace and reports the reflection and the
-- effective configuration, so the backend would see it. A later change must
-- still be applied.
prop_unchangedConfigIsNotReapplied :: Property
prop_unchangedConfigIsNotReapplied = H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    results <- liftIO $ withConfigFile $ \cfgFile -> do
      (ref, _tracers, apply) <- nodeRegistry
      reported <- newIORef []
      _ <- applyTraceConfig apply defaultCardanoConfig
      reconf <- mkTracingReconfigure @Blk (reportSink reported) cfgFile
                  defaultCardanoConfig defaultCardanoConfig apply

      -- what one reload reports, and what reached the backend meanwhile
      let reloadWith :: Text -> IO ([Text], [Text])
          reloadWith contents = do
            T.writeFile cfgFile contents
            writeIORef ref []
            writeIORef reported []
            reconfigureTracing reconf
            (,) <$> (reverse <$> readIORef reported) <*> readIORef ref

          startupInfo  = "{\"TraceOptions\":{\"Startup\":{\"severity\":\"Info\"}}}"
          startupDebug = "{\"TraceOptions\":{\"Startup\":{\"severity\":\"Debug\"}}}"

      first  <- reloadWith startupInfo
      same   <- reloadWith startupInfo
      change <- reloadWith startupDebug
      pure (first, same, change)

    let ((firstSeen, firstOut), (sameSeen, sameOut), (changeSeen, changeOut)) = results
    H.annotateShow results
    -- the first reload changes the options, so it applies them
    firstSeen === ["update", "updated"]
    H.assert (not (null firstOut))
    -- the same file again is recognised as unchanged and nothing is applied
    sameSeen === ["update", "unchanged"]
    sameOut === []
    -- a real change after that is applied again
    changeSeen === ["update", "updated"]
    H.assert (not (null changeOut))

-- | Removing the last override and reloading goes back to the defaults: an
-- empty set of trace options must not keep the options in force.
prop_emptyOptionsRevertToDefaults :: Property
prop_emptyOptionsRevertToDefaults = H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    results <- liftIO $ withConfigFile $ \cfgFile -> do
      (ref, tracers, apply) <- nodeRegistry
      reported <- newIORef []
      _ <- applyTraceConfig apply defaultCardanoConfig
      reconf <- mkTracingReconfigure @Blk (reportSink reported) cfgFile
                  defaultCardanoConfig defaultCardanoConfig apply

      -- what one reload reports, and whether the probe message is visible after it
      let reloadWith :: Text -> IO ([Text], [Text])
          reloadWith contents = do
            T.writeFile cfgFile contents
            writeIORef reported []
            reconfigureTracing reconf
            writeIORef ref []
            traceWith (startupTracer tracers) StartupDBValidation
            (,) <$> (reverse <$> readIORef reported) <*> readIORef ref

      overridden <- reloadWith "{\"TraceOptions\":{\"Startup\":{\"severity\":\"Info\"}}}"
      emptied    <- reloadWith "{\"TraceOptions\":{}}"
      pure (overridden, emptied)

    let ((overriddenSeen, overriddenOut), (emptiedSeen, emptiedOut)) = results
    H.annotateShow results
    -- the override raises Startup to Info, so the probe is visible
    overriddenSeen === ["update", "updated"]
    H.assert (any ("StartupDBValidation" `T.isInfixOf`) overriddenOut)
    -- without it the defaults apply again, and the root's Notice hides the probe
    emptiedSeen === ["update", "updated"]
    emptiedOut === []

tests :: MonadIO m => m Bool
tests = H.checkSequential $ H.Group "Tracing reconfiguration"
  [ ( "reconfiguration changes what the backend sees, reversibly"
    , prop_reconfigureChangesOutput )
  , ( "an unreadable configuration is a no-op and is reported"
    , prop_malformedConfigIsNoOp )
  , ( "concurrent reconfigurations are serialised"
    , prop_concurrentReconfigureIsSerialised )
  , ( "unchanged trace options are not applied again"
    , prop_unchangedConfigIsNotReapplied )
  , ( "empty trace options revert to the defaults"
    , prop_emptyOptionsRevertToDefaults )
  ]
