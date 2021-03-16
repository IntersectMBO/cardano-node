{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}


{-# LANGUAGE PartialTypeSignatures #-}
module Cardano.Logging.Formatter (
    humanFormatter
  , metricsFormatter
  , machineFormatter
) where

import qualified Control.Tracer as T
import           Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BS
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, stripPrefix)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder as TB
import           Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import           Cardano.Logging.Types
import           Control.Concurrent (myThreadId)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Network.HostName

-- | Format this trace as metrics
metricsFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Trace m FormattedMessage
  -> m (Trace m a)
metricsFormatter (Trace tr) = do
  let trr = mkTracer
  pure $ Trace (T.arrow trr)
 where
    mkTracer = T.emit $
      \ (lc, mbC, v) ->
        let metrics =  asMetrics v
        in T.traceWith tr (lc, mbC, Metrics metrics)

-- | Format this trace for human readability
-- The boolean value tells, if this representation is for the console and should be colored
-- The text argument gives the application name which is prepended to the namespace
humanFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Bool
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
humanFormatter withColor application (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \ (lc, mbC, v) -> do
        text <- liftIO $ formatContextHuman withColor hn application lc (forHuman v)
        T.traceWith tr (lc, mbC, Human text)

formatContextHuman ::
     Bool
  -> String
  -> Text
  -> LoggingContext
  -> Text
  -> IO Text
formatContextHuman withColor hostname application LoggingContext {..}  txt = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = fromMaybe Info lcSeverity
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ns       = colorBySeverity
                    withColor
                    severity
                    $ mconcat (intersperse (singleton '.')
                      (fromString hostname :
                        (map fromText (application : lcNamespace)
                          <> [fromString (show severity) , fromText tid])))
      ts       = fromString $ formatTime defaultTimeLocale "%F %T" time
  pure $ toStrict
          $ toLazyText
            $ mconcat (map squareBrackets [ns, ts]) <> singleton ' ' <> fromText txt
  where
    squareBrackets :: Builder -> Builder
    squareBrackets b = TB.singleton '[' <> b <> TB.singleton ']'

-- | Format this trace for machine readability
-- The detail level give a hint to the formatter
-- The text argument gives the application name which is prepended to the namespace
machineFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => DetailLevel
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
machineFormatter detailLevel application (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \ (lc, mbC, v) -> do
        obj <- liftIO $ formatContextMachine hn application lc (forMachine detailLevel v)
        T.traceWith tr (lc, mbC, Machine (decodeUtf8 (BS.toStrict (AE.encode obj))))

formatContextMachine ::
     String
  -> Text
  -> LoggingContext
  -> AE.Object
  -> IO AE.Value
formatContextMachine hostname application LoggingContext {..} obj = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = (pack . show) (fromMaybe Info lcSeverity)
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ns       = application : lcNamespace
      ts       = pack $ formatTime defaultTimeLocale "%F %T" time
  pure $ AE.object [  "at"      .= ts
                    , "ns"      .= ns
                    , "sev"     .= severity
                    , "thread"  .= tid
                    , "host"    .= hostname
                    , "message" .= obj]

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
