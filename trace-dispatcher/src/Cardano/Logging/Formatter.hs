{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cardano.Logging.Formatter (
    humanFormatter
  , metricsFormatter
  , machineFormatter
  , forwardFormatter
  , preFormatted
) where

import qualified Control.Tracer as T
import           Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString.Lazy as BS
import           Data.Functor.Contravariant
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, stripPrefix, replace)
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
  => Text
  -> Trace m FormattedMessage
  -> Trace m a
metricsFormatter application (Trace tr) =
  let trr = mkTracer
  in Trace (T.arrow trr)
 where
    mkTracer = T.emit $
      \ case
        (lc, Right v) ->
          let metrics = asMetrics v
          in T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                            , Right (FormattedMetrics metrics))
        (lc, Left ctrl) ->
          T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                            , Left ctrl)

-- | Format this trace as TraceObject for the trace forwarder
forwardFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
forwardFormatter condApplication (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \ case
        (lc, Right v) -> do
          thid <- liftIO myThreadId
          time <- liftIO getCurrentTime
          let fh = forHuman v
              details = fromMaybe DNormal (lcDetails lc)
              fm = forMachine details v
              nlc = lc { lcNamespace = case condApplication of
                                                  Just app -> app : lcNamespace lc
                                                  Nothing  -> lcNamespace lc}
              to = TraceObject {
                      toHuman     = if fh == "" then Nothing else Just fh
                    , toMachine   = if fm == mempty then Nothing else
                                    Just
                                      $ replace "\\" "\\\\"
                                      $ decodeUtf8 (BS.toStrict (AE.encode fm))
                    , toNamespace = lcNamespace nlc
                    , toSeverity  = fromMaybe Info (lcSeverity lc)
                    , toDetails   = fromMaybe DNormal (lcDetails lc)
                    , toTimestamp = time
                    , toHostname  = hn
                    , toThreadId  = (pack . show) thid
                  }
          T.traceWith tr ( nlc
                         , Right (FormattedForwarder to))
        (lc, Left ctrl) -> do
          T.traceWith tr (lc { lcNamespace = case condApplication of
                                              Just app -> app : lcNamespace lc
                                              Nothing  -> lcNamespace lc}
                            , Left ctrl)

-- | Format this trace for human readability
-- The boolean value tells, if this representation is for the console and should be colored
-- The text argument gives the application name which is prepended to the namespace
humanFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Bool
  -> Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
humanFormatter withColor condApplication (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \ case
        (lc, Right v) -> do
          let fh = forHuman v
          text <- liftIO $ formatContextHuman withColor hn condApplication lc fh
          T.traceWith tr (lc { lcNamespace = case condApplication of
                                              Just app -> app : lcNamespace lc
                                              Nothing  -> lcNamespace lc}
                             , Right (FormattedHuman withColor text))
        (lc, Left ctrl) -> do
          T.traceWith tr (lc { lcNamespace = case condApplication of
                                              Just app -> app : lcNamespace lc
                                              Nothing  -> lcNamespace lc}
                             , Left ctrl)

formatContextHuman ::
     Bool
  -> String
  -> Maybe Text
  -> LoggingContext
  -> Text
  -> IO Text
formatContextHuman withColor hostname condApplication LoggingContext {..}  txt = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = fromMaybe Info lcSeverity
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ts       = fromString $ formatTime defaultTimeLocale "%F %H:%M:%S%4Q" time
      ns       = colorBySeverity
                    withColor
                    severity
                    $ fromString hostname
                      <> singleton ':'
                      <> mconcat (intersperse (singleton '.')
                          (case condApplication of
                            Just app -> map fromText (app : lcNamespace)
                            Nothing  -> map fromText lcNamespace))

      tadd     = fromText " ("
                  <> fromString (show severity)
                  <> singleton ','
                  <> fromText tid
                  <> fromText ") "
  pure $ toStrict
          $ toLazyText
            $ squareBrackets ts
              <> singleton ' '
              <> squareBrackets ns
              <> tadd
              <> fromText txt
  where
    squareBrackets :: Builder -> Builder
    squareBrackets b = singleton '[' <> b <> singleton ']'

-- | Format this trace for machine readability
-- The detail level give a hint to the formatter
-- The text argument gives the application name which is prepended to the namespace
machineFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
machineFormatter condApplication (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \case
        (lc, Right v) -> do
          let detailLevel = fromMaybe DNormal (lcDetails lc)
          obj <- liftIO $ formatContextMachine hn condApplication lc (forMachine detailLevel v)
          T.traceWith tr (lc { lcNamespace = case condApplication of
                                              Just app -> app : lcNamespace lc
                                              Nothing  -> lcNamespace lc}
                         , Right (FormattedMachine (decodeUtf8 (BS.toStrict
                                (AE.encodingToLazyByteString obj)))))
        (lc, Left c) -> do
          T.traceWith tr (lc { lcNamespace = case condApplication of
                                              Just app -> app : lcNamespace lc
                                              Nothing  -> lcNamespace lc}
                         , Left c)

formatContextMachine ::
     String
  -> Maybe Text
  -> LoggingContext
  -> AE.Object
  -> IO AE.Encoding
formatContextMachine hostname condApplication LoggingContext {..} obj = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = (pack . show) (fromMaybe Info lcSeverity)
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ns       = mconcat (intersperse (singleton '.')
                     (case condApplication of
                       Just app -> map fromText (app : lcNamespace)
                       Nothing  -> map fromText lcNamespace))
      ts       = pack $ formatTime defaultTimeLocale "%F %H:%M:%S%4QZ" time
  pure $ AE.pairs $    "at"      .= ts
                    <> "ns"      .= toStrict (toLazyText ns)
                    <> "data"    .= obj
                    <> "sev"     .= severity
                    <> "thread"  .= tid
                    <> "host"    .= hostname


-- | Transform this trace to a preformatted message, so that double serialization
-- is avoided
preFormatted ::
  (  LogFormatting a
  ,  Monad m)
  => [BackendConfig]
  -> Trace m (PreFormatted a)
  -> Trace m a
preFormatted backends tr@(Trace tr')=
  if Forwarder `elem` backends
    then if elem (Stdout HumanFormatUncoloured) backends
            || elem (Stdout HumanFormatColoured) backends
      then contramap (\msg -> PreFormatted msg (Just (forHuman msg)) Nothing) tr
      else if Stdout MachineFormat `elem` backends
        then Trace $ T.contramap
              (\case
                  (lc, Right msg) ->
                    let dtal = fromMaybe DNormal (lcDetails lc)
                    in (lc, Right (PreFormatted msg Nothing
                                    (Just (forMachine dtal msg))))
                  (lc, Left ctrl) -> (lc, Left ctrl))
              tr'
        else contramap (\msg -> PreFormatted msg Nothing Nothing) tr
    else contramap (\msg -> PreFormatted msg Nothing Nothing) tr

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
