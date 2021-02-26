{-# LANGUAGE FlexibleInstances   #-}
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
import qualified Data.Aeson as AE
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

import qualified Cardano.Logging.Types as LT

data LoggingContextKatip = LoggingContextKatip {
    lk       :: LT.LoggingContext
  , lkLogEnv :: LogEnv
}

instance LogItem AE.Object where
  payloadKeys _ _ = AllKeys

asKatipSeverity :: LT.SeverityS -> Severity
asKatipSeverity LT.Debug     = DebugS
asKatipSeverity LT.Info      = InfoS
asKatipSeverity LT.Notice    = NoticeS
asKatipSeverity LT.Warning   = WarningS
asKatipSeverity LT.Error     = ErrorS
asKatipSeverity LT.Critical  = CriticalS
asKatipSeverity LT.Alert     = AlertS
asKatipSeverity LT.Emergency = EmergencyS

stdoutObjectKatipTracer :: (MonadIO m, LT.Logging a) => m (LT.Trace m a)
stdoutObjectKatipTracer = do
    env <- liftIO $ ioLogEnv (\ _ -> pure True) V3
    pure $ withKatipLogEnv env katipTracer

stdoutJsonKatipTracer :: (MonadIO m, LT.Logging a) => m (LT.Trace m a)
stdoutJsonKatipTracer = do
    env <- liftIO $ ioLogEnvJson (\ _ -> pure True) V3
    pure $ withKatipLogEnv env katipTracer

-- | Sets severities for the messages in this trace based on the selector function
withKatipLogEnv :: Monad m
  => LogEnv
  -> T.Tracer m (LoggingContextKatip, Maybe LT.TraceControl, a)
  -> LT.Trace m a
withKatipLogEnv le = T.contramap (\ (lc,mbC, e) -> (LoggingContextKatip lc le, mbC, e))

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

--- | A standard Katip tracer
katipTracer :: (MonadIO m, LT.Logging a)
  => T.Tracer m (LoggingContextKatip, Maybe LT.TraceControl, a)
katipTracer =  T.arrow $ T.emit $ uncurry3 output
  where
    output LoggingContextKatip {..} Nothing a =
                      logItem'
                          (LT.forMachine (fromMaybe LT.DRegular (LT.lcDetails lk)) a)
                          (Namespace (LT.lcContext lk))
                          (asKatipSeverity (fromMaybe LT.Info (LT.lcSeverity lk)))
                          ""
                          lkLogEnv
    output LoggingContextKatip {..} (Just c) a = pure () -- TODO


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
    item <- liftIO $ pure (Item _logEnvApp)
          <*> pure _logEnvEnv
          <*> pure sev
          <*> (mkThreadIdText <$> myThreadId)
          <*> pure _logEnvHost
          <*> pure _logEnvPid
          <*> pure a
          <*> pure msg
          <*> _logEnvTimer
          <*> pure (_logEnvApp <> ns)
          <*> pure Nothing
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
