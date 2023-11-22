{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Test.Tracer (
    testTracer
  , formattedMsgAsText
  ) where

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Text (Text, pack)

import           Cardano.Logging

testTracer :: MonadIO m
  => IORef [FormattedMessage]
  -> m (Trace m FormattedMessage)
testTracer ioRef = liftIO $
    pure $ Trace $ arrow $ emit output
  where
    output (LoggingContext{}, Right msg) = liftIO $ do
      modifyIORef ioRef (msg :)
    output (LoggingContext{}, _) = pure ()


formattedMsgAsText :: FormattedMessage -> Text
formattedMsgAsText (FormattedHuman _ text) = text
formattedMsgAsText (FormattedMachine text) = text
formattedMsgAsText (FormattedMetrics metrics) = pack (show metrics)
formattedMsgAsText (FormattedForwarder traceObj) = toMachine traceObj
