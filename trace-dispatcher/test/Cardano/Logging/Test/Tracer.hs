{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Test.Tracer (
    testTracer
  , formattedMsgAsText
  ) where

import           Cardano.Logging.Types
import           Cardano.Prelude hiding (head)
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

data LoggingMessage a = LoggingMessage {
      at :: Time
    , ns :: Namespace
    , dataX :: a
    , sev :: SeverityS
    , thread :: Text
    , host :: Text
  } deriving (Eq, Ord, Show, Generic)

