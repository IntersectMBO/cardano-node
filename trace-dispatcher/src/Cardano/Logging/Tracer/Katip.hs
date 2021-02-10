{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Katip where

import           Control.Concurrent (myThreadId)
import           Control.Concurrent.STM
import           Control.Monad (join, void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Control.Tracer.Arrow as TA
import           Data.Foldable as FT
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef,
                     writeIORef)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import           Katip
import           Katip.Core (ScribeHandle (..), WorkerMessage (..),
                     mkThreadIdText, tryWriteTBQueue)
import           Katip.Scribes.Handle (ioLogEnv)
import           System.IO (stdout)

import           Cardano.Logging.Types

stdoutObjectKatipTracer :: (MonadIO m, LogItem a) => m (Trace m a)
stdoutObjectKatipTracer = do
    env <- liftIO $ ioLogEnv (\ _ -> pure True) V3
    pure $ withKatipLogEnv env katipTracer

stdoutJsonKatipTracer :: (MonadIO m, LogItem a) => m (Trace m a)
stdoutJsonKatipTracer = do
    env <- liftIO $ ioLogEnvJson (\ _ -> pure True) V3
    pure $ withKatipLogEnv env katipTracer

-- | Sets severities for the messages in this trace based on the selector function
withKatipLogEnv :: Monad m
  => LogEnv
  -> T.Tracer m (LoggingContextKatip, Either TraceConfig a)
  -> Trace m a
withKatipLogEnv le = T.contramap (\ (lc,e) -> (LoggingContextKatip lc le, e))

--- | A standard Katip tracer
katipTracer :: (MonadIO m, LogItem a)
  => T.Tracer m (LoggingContextKatip, Either TraceConfig a)
katipTracer =  T.arrow $ T.emit $ uncurry output
  where
    output LoggingContextKatip {..} (Right a) =
                      logItem'
                          a
                          (Namespace (lcContext lk))
                          (fromMaybe InfoS (lcSeverity lk))
                          ""
                          lkLogEnv
    output LoggingContextKatip {..} (Left a) = pure () -- TODO


-------------------------------------------------------------------------------
-- | Log with everything, including a source code location. This is
-- very low level and you typically can use 'logT' in its place.
logItem'
    :: (LogItem a, MonadIO m)
    => a
    -> Namespace
    -> Severity
    -> LogStr
    -> LogEnv
    -> m ()
logItem' a ns sev msg le@LogEnv{..} = do
    item <- liftIO $ (Item <$> pure _logEnvApp
          <*> pure _logEnvEnv
          <*> pure sev
          <*> (mkThreadIdText <$> myThreadId)
          <*> pure _logEnvHost
          <*> pure _logEnvPid
          <*> pure a
          <*> pure msg
          <*> _logEnvTimer
          <*> pure (_logEnvApp <> ns)
          <*> pure Nothing)
    logKatipItem' le item

-- | Log already constructed 'Item'. This is the lowest level function that other log*
--   functions use.
--   It can be useful when implementing centralised logging services.
logKatipItem'
    :: (LogItem a, MonadIO m)
    => LogEnv
    -> Item a
    -> m ()
logKatipItem' LogEnv{..} item = do
    liftIO $
      FT.forM_ (M.elems _logEnvScribes) $ \ ScribeHandle {..} -> do
        whenM (scribePermitItem shScribe item) $
          void $ atomically $ tryWriteTBQueue shChan (NewItem item)
  where
    whenM :: Monad m => m Bool -> m () -> m ()
    whenM p m = p >>= flip when m

ioLogEnvJson :: PermitFunc -> Verbosity -> IO LogEnv
ioLogEnvJson permit verb = do
  le <- initLogEnv "io" "io"
  lh <- mkHandleScribeWithFormatter jsonFormat ColorIfTerminal stdout permit verb
  registerScribe "stdout" lh defaultScribeSettings le
