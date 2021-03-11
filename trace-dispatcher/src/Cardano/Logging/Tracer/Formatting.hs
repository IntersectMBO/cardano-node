{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Formatting (
    formatHuman
  , formatMachine
  , formatIt
) where

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

formatHuman :: LogFormatting a =>
     Maybe (a -> Text)
  -> a
  -> Text
formatHuman (Just f) msg = f msg
formatHuman Nothing msg  = forHuman msg

formatMachine :: LogFormatting a =>
     Maybe (DetailLevel -> a ->AE.Object)
  -> Maybe DetailLevel
  -> a
  -> Text
formatMachine (Just f) dl msg =
  let obj = f (fromMaybe DRegular dl) msg
  in decodeUtf8 $ BS.toStrict $ AE.encode obj
formatMachine Nothing dl msg  =
  let obj = forMachine (fromMaybe DRegular dl) msg
  in decodeUtf8 $ BS.toStrict $ AE.encode obj

formatIt ::
     Bool
  -> LoggingContext
  -> String
  -> Text
  -> IO Text
formatIt withColor LoggingContext {..} hostname txt = do
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
  pure $ toStrict
          $ toLazyText
            $ mconcat (map squareBrackets [ns, ts]) <> singleton ' ' <> fromText txt
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
