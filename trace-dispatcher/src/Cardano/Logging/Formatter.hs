{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.Formatter (
    metricsFormatter
  , preFormatted
  , forwardFormatter
  , forwardFormatter'
  , machineFormatter
  , machineFormatter'
  , cborFormatter
  , cborFormatter'
  , humanFormatter
  , humanFormatter'
) where

import           Cardano.Logging.Trace (contramapM)
import           Cardano.Logging.Types
import           Cardano.Logging.Types.TraceMessage

import           Codec.Serialise (serialise)
import           Control.Concurrent (myThreadId)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString.Lazy as BL (toStrict)
import           Data.Functor.Contravariant
import           Data.Maybe (fromMaybe)
import           Data.Text as T (Text, intercalate, null, pack)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder as TB
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import           Network.HostName
import           System.IO.Unsafe (unsafePerformIO)


-- If the hostname in the logs should be anything different from the system reported hostname,
-- a new field would need to be added to PreFormatted to carry a new hostname argument to preFormatted.
hostname :: Text
{-# NOINLINE hostname #-}
hostname = unsafePerformIO $ T.pack <$> getHostName


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
  => Bool
  -> Trace m PreFormatted
  -> m (Trace m a)
preFormatted withForHuman =
  flip contramapM
    (\case
      (lc, Right msg) -> do
        time     <- liftIO getCurrentTime
        threadId <- liftIO myThreadId
        let
            pf = PreFormatted
              { pfTime              = time
              , pfNamespace         = intercalate "." (lcNSPrefix lc ++ lcNSInner lc)
              , pfThreadId          = T.pack $ drop 9 $ show threadId                           -- drop "ThreadId " prefix
              , pfForHuman          = if withForHuman then (let txt = forHuman msg in if T.null txt then Nothing else Just txt) else Nothing
              , pfForMachineObject  = forMachine (fromMaybe DNormal (lcDetails lc)) msg
              }
        pure (lc, Right pf)

      (lc, Left ctrl) ->
        pure (lc, Left ctrl)
    )

-- | Format this trace as TraceObject for the trace forwarder
forwardFormatter'
  :: forall m .
     MonadIO m
  => Trace m FormattedMessage
  -> Trace m PreFormatted
forwardFormatter' (Trace tr) = Trace $
  contramap
    (\ case
      (lc, Right v) ->
            let
              jsonObj = TraceMessage
                { tmsgAt      = pfTime v
                , tmsgNS      = pfNamespace v
                , tmsgData    = pfForMachineObject v
                , tmsgSev     = fromMaybe Info $ lcSeverity lc
                , tmsgThread  = pfThreadId v
                , tmsgHost    = hostname
                }
              to = TraceObject
                { toHuman     = pfForHuman v
                , toMachine   = (toStrict . decodeUtf8 . AE.encode) jsonObj
                -- backwards compatible to not break ForwardingV_1 protocol' type: value used to be segmented (["name", "space"])
                , toNamespace = [pfNamespace v]
                , toSeverity  = fromMaybe Info (lcSeverity lc)
                , toDetails   = fromMaybe DNormal (lcDetails lc)
                , toTimestamp = pfTime v
                , toHostname  = hostname
                , toThreadId  = pfThreadId v
                }
            in (lc, Right (FormattedForwarder to))
      (lc, Left ctrl) -> (lc, Left ctrl))
      tr

-- | Format this trace as TraceObject for machine-readable text output (JSON)
machineFormatter'
  :: forall m .
     MonadIO m
  => Trace m FormattedMessage
  -> Trace m PreFormatted
machineFormatter' (Trace tr) = Trace $
  contramap
    (\ case
      (lc, Right v) ->
        let
          msg = TraceMessage
            { tmsgAt      = pfTime v
            , tmsgNS      = pfNamespace v
            , tmsgData    = pfForMachineObject v
            , tmsgSev     = fromMaybe Info $ lcSeverity lc
            , tmsgThread  = pfThreadId v
            , tmsgHost    = hostname
            }
        in (lc, Right (FormattedMachine (toStrict . decodeUtf8 $ AE.encode msg)))
      (lc, Left ctrl) -> (lc, Left ctrl))
      tr

-- | Format this trace in binary serialisation (CBOR)
cborFormatter'
  :: forall m .
     MonadIO m
  => Trace m FormattedMessage
  -> Trace m PreFormatted
cborFormatter' (Trace tr) = Trace $
  contramap
    (\ case
      (lc, Right v) ->
        let
          cborObj = TraceMessage
            { tmsgAt      = pfTime v
            , tmsgNS      = pfNamespace v
            , tmsgData    = pfForMachineObject v
            , tmsgSev     = fromMaybe Info $ lcSeverity lc
            , tmsgThread  = pfThreadId v
            , tmsgHost    = hostname
            }
        in (lc, Right (FormattedCBOR $ BL.toStrict $ serialise cborObj))
      (lc, Left ctrl) -> (lc, Left ctrl))
      tr

-- | Format this trace in human readable text output
humanFormatter'
  :: forall m .
     MonadIO m
  => Bool
  -> Trace m FormattedMessage
  -> Trace m PreFormatted
humanFormatter' withColor (Trace tr) =
  Trace $
      contramap
        (\ case
          (lc, Right v) ->
              let sev      = fromMaybe Info (lcSeverity lc)
                  ns       = fromText hostname
                                <> singleton ':'
                                <> fromText (pfNamespace v)
                  showTime = formatTime defaultTimeLocale "%F %H:%M:%S%4QZ"
                  prePart  = squareBrackets (fromString $ showTime $ pfTime v)
                                <> squareBrackets ns
                                <> roundBrackets
                                    (fromString (show sev)
                                    <> singleton ','
                                    <> fromText (pfThreadId v))
                  dataPart = fromMaybe
                                (toStrict . decodeUtf8 . AE.encodingToLazyByteString $ 
                                  AE.pairs ("data" .= pfForMachineObject v)
                                )
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

humanFormatter
  :: forall a m .
     MonadIO m
  => LogFormatting a
  => Bool
  -> Trace m FormattedMessage
  -> m (Trace m a)
humanFormatter withColor =
  preFormatted True . humanFormatter' withColor

machineFormatter
  :: forall a m .
     (MonadIO m
  ,  LogFormatting a)
  => Trace m FormattedMessage
  -> m (Trace m a)
machineFormatter =
  preFormatted False . machineFormatter'

cborFormatter
  :: forall a m .
     (MonadIO m
  ,  LogFormatting a)
  => Trace m FormattedMessage
  -> m (Trace m a)
cborFormatter =
  preFormatted False . cborFormatter'

forwardFormatter
  :: forall a m .
     MonadIO m
  => LogFormatting a
  => Trace m FormattedMessage
  -> m (Trace m a)
forwardFormatter =
  preFormatted True . forwardFormatter'
