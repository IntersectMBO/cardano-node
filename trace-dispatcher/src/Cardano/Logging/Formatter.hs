{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Formatter (
    metricsFormatter
  , preFormatted
  , forwardFormatter
  , forwardFormatter'
  , machineFormatter
  , machineFormatter'
  , humanFormatter
  , humanFormatter'
) where

import           Cardano.Logging.Trace (contramapM)
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (showT)

import           Control.Concurrent (myThreadId)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE
import           Data.Functor.Contravariant
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, intercalate, pack, stripPrefix)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import           Network.HostName


encodingToText :: AE.Encoding -> Text
{-# INLINE encodingToText #-}
encodingToText = toStrict . decodeUtf8 . AE.encodingToLazyByteString

-- | Format this trace as metrics
metricsFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Trace m FormattedMessage
  -> Trace m a
metricsFormatter (Trace tr) = Trace $
    T.contramap
      (\ case
        (lc, Right v) ->
          let metrics = asMetrics v
          in (lc, Right (FormattedMetrics metrics))
        (lc, Left ctrl) ->
             (lc, Left ctrl))
      tr

-- | Transform this trace to a preformatted message, so that double serialization
-- is avoided
preFormatted ::
  (  LogFormatting a
  ,  MonadIO m)
  => [BackendConfig]
  -> Trace m (PreFormatted a)
  -> m (Trace m a)
preFormatted backends' (Trace tr) = do
  hostname <- liftIO getHostName
  contramapM (Trace tr)
    (\case
      (lc, Right msg) -> do
        time     <- liftIO getCurrentTime
        threadId <- liftIO myThreadId
        let ns' = lcNSPrefix lc ++ lcNSInner lc
            threadText = showT threadId
            threadTextShortened =
              fromMaybe threadText (stripPrefix "ThreadId " threadText)
            details = fromMaybe DNormal (lcDetails lc)
            condForHuman = if elem (Stdout HumanFormatUncoloured) backends'
                          || elem (Stdout HumanFormatColoured) backends'
                          || elem Forwarder backends'
                          then case forHuman msg of
                                  "" -> Nothing
                                  txt -> Just txt
                          else Nothing
            machineFormatted = forMachine details msg

        pure (lc, Right (PreFormatted
                          { pfMessage = msg
                          , pfForHuman = condForHuman
                          , pfForMachine = machineFormatted
                          , pfTimestamp = timeFormatted time
                          , pfTime = time
                          , pfNamespace = ns'
                          , pfHostname = hostname
                          , pfThreadId = threadTextShortened
                          }))
      (lc, Left ctrl) ->
        pure (lc, Left ctrl))

-- | Format this trace as TraceObject for the trace forwarder
forwardFormatter'
  :: forall a m .
     MonadIO m
  => Maybe Text
  -> Trace m FormattedMessage
  -> Trace m (PreFormatted a)
forwardFormatter' condPrefix (Trace tr) = Trace $
  contramap
    (\ case
      (lc, Right v) ->
            let ns = case condPrefix of
                                  Just app -> app : pfNamespace v
                                  Nothing -> pfNamespace v
                machineObj = AE.pairs $
                      "at"       .= pfTime v
                    <> "ns"      .= ns
                    <> "data"    .= pfForMachine v
                    <> "sev"     .= fromMaybe Info (lcSeverity lc)
                    <> "thread"  .= pfThreadId v
                    <> "host"    .= pfHostname v
                to = TraceObject {
                    toHuman     = pfForHuman v
                  , toMachine   = encodingToText machineObj
                  , toNamespace = ns
                  , toSeverity  = fromMaybe Info (lcSeverity lc)
                  , toDetails   = fromMaybe DNormal (lcDetails lc)
                  , toTimestamp = pfTime v
                  , toHostname  = pfHostname v
                  , toThreadId  = pfThreadId v
                }
            in (lc, Right (FormattedForwarder to))
      (lc, Left ctrl) -> (lc, Left ctrl))
      tr

-- | Format this trace as TraceObject for the trace forwarder
machineFormatter'
  :: forall a m .
     MonadIO m
  => Maybe Text
  -> Trace m FormattedMessage
  -> Trace m (PreFormatted a)
machineFormatter' condPrefix (Trace tr) = Trace $
  contramap
    (\ case
      (lc, Right v) ->
        let ns = case condPrefix of
                                  Just app -> app : pfNamespace v
                                  Nothing -> pfNamespace v
            machineObj = AE.pairs $
                   "at"      .= pfTime v
                <> "ns"      .= intercalate "." ns
                <> "data"    .= pfForMachine v
                <> "sev"     .= fromMaybe Info (lcSeverity lc)
                <> "thread"  .= pfThreadId v
                <> "host"    .= pfHostname v
        in (lc, Right (FormattedMachine (encodingToText machineObj)))
      (lc, Left ctrl) -> (lc, Left ctrl))
      tr

-- | Format this trace in human readable style
humanFormatter'
  :: forall a m .
     MonadIO m
  => Bool
  -> Maybe Text
  -> Trace m FormattedMessage
  -> Trace m (PreFormatted a)
humanFormatter' withColor condPrefix (Trace tr) =
  Trace $
      contramap
        (\ case
          (lc, Right v) ->
              let sev      = fromMaybe Info (lcSeverity lc)
                  ns       = fromString (pfHostname v)
                                <> singleton ':'
                                <> fromText
                                    (intercalate "."
                                      (case condPrefix of
                                          Just app -> app : pfNamespace v
                                          Nothing  ->pfNamespace v))
                  prePart  = squareBrackets (fromText (pfTimestamp v))
                                <> squareBrackets ns
                                <> roundBrackets
                                    (fromString (show sev)
                                    <> singleton ','
                                    <> fromText (pfThreadId v))
                  dataPart = fromMaybe
                                (encodingToText (AE.pairs ("data" .= pfForMachine v)))
                                (pfForHuman v)
                  forHuman'' = toStrict
                                $ toLazyText
                                  (colorBySeverity withColor sev prePart
                                    <> singleton ' '
                                    <> fromText dataPart)
                  in (lc, Right (FormattedHuman withColor forHuman''))
          (lc, Left ctrl) -> (lc, Left ctrl))
          tr

squareBrackets :: Builder -> Builder
squareBrackets b = singleton '[' <> b <> singleton ']'

roundBrackets :: Builder -> Builder
roundBrackets b = singleton '(' <> b <> singleton ')'

-- | Color a text message based on `Severity`. `Error` and more severe errors
-- are colored red, `Warning` is colored yellow, and all other messages are
-- rendered in the default color.
colorBySeverity :: Bool -> SeverityS -> Builder -> Builder
colorBySeverity withColor severity' msg =
  if withColor
    then case severity' of
            Emergency -> red msg
            Alert     -> red msg
            Critical  -> red msg
            Error     -> red msg
            Warning   -> yellow msg
            Notice    -> magenta msg
            Info      -> blue msg
            Debug     -> msg
    else msg
  where
    red = colorize "31"
    yellow = colorize "33"
    magenta = colorize "35"
    blue = colorize "34"
    colorize c msg' = "\ESC[" <> c <> "m" <> msg' <> "\ESC[0m"

timeFormatted :: UTCTime -> Text
timeFormatted = pack . formatTime defaultTimeLocale "%F %H:%M:%S%4QZ"

humanFormatter
  :: forall a m .
     MonadIO m
  => LogFormatting a
  => Bool
  -> Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
humanFormatter withColor condPrefix tr = do
    let tr' = humanFormatter' withColor condPrefix tr
    preFormatted [Stdout (if withColor
                            then HumanFormatColoured
                            else HumanFormatUncoloured)] tr'

machineFormatter
  :: forall a m .
     (MonadIO m
  ,  LogFormatting a)
  => Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
machineFormatter condPrefix tr = do
    let tr' = machineFormatter' condPrefix tr
    preFormatted [Stdout MachineFormat] tr'

forwardFormatter
  :: forall a m .
     MonadIO m
  => LogFormatting a
  => Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
forwardFormatter condPrefix tr = do
    let tr' = forwardFormatter' condPrefix tr
    preFormatted [Stdout MachineFormat, Stdout HumanFormatColoured] tr'
