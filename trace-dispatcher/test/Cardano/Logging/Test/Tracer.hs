{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Test.Tracer (
    testTracer
  , formattedMsgAsText
  , LoggingMessage (..)
  , testLoggingMessageEq
  , testLoggingMessagesEq
  ) where

import           Cardano.Logging.Types
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON (..), Object, decodeStrict, withObject, (.:))
import           Data.IORef
import           Data.Text (Text, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (UTCTime)

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

data LoggingMessage = LoggingMessage {
      at :: UTCTime
    , ns :: Text
    , dataX :: Object
    , sev :: SeverityS
    , thread :: Text
    , host :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON LoggingMessage where
    parseJSON = withObject "LoggingMessage" $ \v -> LoggingMessage
        <$> v .: "at"
        <*> v .: "ns"
        <*> v .: "data"
        <*> v .: "sev"
        <*> v .: "thread"
        <*> v .: "host"

testLoggingMessageEq :: Text -> Text -> Bool
testLoggingMessageEq t1 t2 =
    let lm1 = (decodeStrict . encodeUtf8) t1
        lm2 = (decodeStrict . encodeUtf8) t2
    in  case (lm1, lm2) of
          (Just (LoggingMessage _at1 ns1 dataX1 sev1 _thread1 _host1),
              Just (LoggingMessage _at2 ns2 dataX2 sev2 _thread2 _host2)) ->
                ns1 == ns2 && dataX1 == dataX2 && sev1 == sev2
          _ -> False

testLoggingMessagesEq :: [Text] -> [Text] -> Bool
testLoggingMessagesEq [] [] = True
testLoggingMessagesEq (a : atl) (b : btl) =
  testLoggingMessageEq a b && testLoggingMessagesEq atl btl
testLoggingMessagesEq _ _ = False
