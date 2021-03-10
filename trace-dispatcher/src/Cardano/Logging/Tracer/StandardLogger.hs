{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.StandardLogger (
    standardMachineTracer
  , standardHumanTracer
) where

import           Control.Concurrent (forkIO, myThreadId)
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BS
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, stripPrefix)
import qualified Data.Text.Array as TA
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder as TB
import           Data.Time (UTCTime (..), defaultTimeLocale, formatTime,
                     getCurrentTime)
import           Data.Time.Format.ISO8601 (FormatExtension (BasicFormat),
                     calendarFormat, dayAndTimeFormat, formatShow, iso8601Show,
                     timeOfDayFormat)
import           GHC.Conc (ThreadId)
import           Network.HostName (getHostName)


import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import qualified Control.Tracer as T

-- | Do we log to stdout or to a file?
data LogTarget = LogStdout | LogFile FilePath
  deriving (Eq, Show)

-- | The state of a standard tracer
data StandardTracerState a =  StandardTracerState {
    stRunning :: Maybe (InChan Text, OutChan Text, ThreadId)
  , stTarget  :: LogTarget
}

emptyStandardTracerState :: Text -> StandardTracerState a
emptyStandardTracerState name = StandardTracerState Nothing LogStdout

standardMachineTracer :: forall a m. (MonadIO m, LogFormatting a)
  => Text
  -> Maybe (DetailLevel -> a -> AE.Object)
  -> m (Trace m a)
standardMachineTracer tracerName mbFormatter = do
    stateRef <- liftIO $ newIORef (emptyStandardTracerState tracerName)
    hostname <- liftIO getHostName
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output stateRef hostname)
  where
    output ::
         IORef (StandardTracerState a)
      -> String
      -> LoggingContext
      -> Maybe TraceControl
      -> a
      -> m ()
    output stateRef _ LoggingContext {..} (Just Reset) a = liftIO $ do
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> initLogging stateRef
        Just _  -> pure ()
    output stateRef hostName lc@LoggingContext {..} Nothing a = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> do
          msg <- formatMachine mbFormatter (stTarget st == LogStdout) lc hostName a
          writeChan inChannel msg
        Nothing                -> pure ()
    output _ _ lk (Just c@Document {}) a =
       docIt (StandardBackend tracerName) Machine (lk, Just c, a)
    output stateRef _ LoggingContext {..} _ a = pure ()

formatMachine :: LogFormatting a =>
     Maybe (DetailLevel -> a -> AE.Object)
  -> Bool
  -> LoggingContext
  -> String
  -> a
  -> IO Text
formatMachine mbFormatter withColor LoggingContext {..} hostname obj = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = fromMaybe Info lcSeverity
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ns       = colorBySeverity
                    withColor
                    severity
                    $ mconcat (intersperse (singleton '.')
                      (fromString hostname : map fromText lcNamespace
                      <> [fromString (show severity) , fromText tid] ))
      ts       = fromString $ formatTime defaultTimeLocale "%F %T" time
      payload  = case mbFormatter of
                  Just form -> form (fromMaybe DRegular lcDetails) obj
                  Nothing   -> forMachine (fromMaybe DRegular lcDetails) obj
      pb       = fromText $ decodeUtf8 $ BS.toStrict $ AE.encode payload
  pure $ toStrict
          $ toLazyText
            $ mconcat (map squareBrackets [ns, ts]) <> pb
  where
    squareBrackets :: Builder -> Builder
    squareBrackets b = TB.singleton '[' <> b <> TB.singleton ']'

standardHumanTracer :: forall a m. (MonadIO m, LogFormatting a)
  => Text
  -> Maybe (a -> Text)
  -> m (Trace m a)
standardHumanTracer tracerName mbFormatter = do
    stateRef <- liftIO $ newIORef (emptyStandardTracerState tracerName)
    hostname <- liftIO getHostName
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output stateRef hostname)
  where
    output ::
         IORef (StandardTracerState a)
      -> String
      -> LoggingContext
      -> Maybe TraceControl
      -> a
      -> m ()
    output stateRef _ LoggingContext {..} (Just Reset) a = liftIO $ do
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> initLogging stateRef
        Just _  -> pure ()
    output stateRef hostName lc@LoggingContext {..} Nothing a = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> do
          msg <- formatHuman mbFormatter (stTarget st == LogStdout) lc hostName a
          writeChan inChannel msg
        Nothing                -> pure ()
    output _ _ lk (Just c@Document {}) a =
       docIt (StandardBackend tracerName) Machine (lk, Just c, a)
    output stateRef _ LoggingContext {..} _ a = pure ()

formatHuman :: LogFormatting a =>
     Maybe (a -> Text)
  -> Bool
  -> LoggingContext
  -> String
  -> a
  -> IO Text
formatHuman mbFormatter withColor LoggingContext {..} hostname obj = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = fromMaybe Info lcSeverity
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ns       = colorBySeverity
                    withColor
                    severity
                    $ mconcat (intersperse (singleton '.')
                      (fromString hostname : map fromText lcNamespace
                      <> [fromString (show severity) , fromText tid] ))
      ts       = fromString $ formatTime defaultTimeLocale "%F %T" time
      payload  = case mbFormatter of
                  Just form -> form obj
                  Nothing   -> forHuman obj
      pb       = fromText $ decodeUtf8 $ BS.toStrict $ AE.encode payload
  pure $ toStrict
          $ toLazyText
            $ mconcat (map squareBrackets [ns, ts]) <> pb
  where
    squareBrackets :: Builder -> Builder
    squareBrackets b = TB.singleton '[' <> b <> TB.singleton ']'


-- | Color a text message based on `Severity`. `Error` and more severe errors
-- are colored red, `Warning` is colored yellow, and all other messages are
-- rendered in the default color.
colorBySeverity :: Bool -> SeverityS -> Builder -> Builder
colorBySeverity withColor severity msg = case severity of
  Emergency -> red msg
  Alert     -> red msg
  Critical  -> red msg
  Error     -> red msg
  Warning   -> yellow msg
  _         -> msg
  where
    red = colorize "31"
    yellow = colorize "33"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s

initLogging :: IORef (StandardTracerState a) -> IO ()
initLogging stateRef = do
  (inChan, outChan) <- newChan 2048
  threadId <- forkIO $ forever $ do
    state <- readIORef stateRef
    msg   <- readChan outChan
    case stTarget state of
        LogFile f -> do
                        TIO.appendFile f msg
                        TIO.appendFile f "\n"
        LogStdout -> TIO.putStrLn msg
  modifyIORef stateRef (\ st ->
    st {stRunning = Just (inChan, outChan, threadId)})

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
