{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Test.Tracer (
    testTracer
  , formattedMsgAsText
  , LoggingMessage (..)
  , testLoggingMessageEq
  , testLoggingMessagesEq
  ) where

import           Cardano.Logging

import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON (..), Object, decodeStrict, withObject, (.:))
import           Data.IORef
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (UTCTime)


testTracer :: MonadIO m
  => IORef [FormattedMessage]
  -> m (Trace m FormattedMessage)
testTracer ioRef = liftIO $
    pure $ Trace $ arrow $ emit output
  where
    output (LoggingContext{}, Right msg) = liftIO $ do
      modifyIORef ioRef (msg :)
    output (lc, c@(Left TCDocument {})) =
       docIt
        (Stdout MachineFormat)
        (lc, c)
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

testLoggingMessageEq :: Text -> Text -> IO Bool
testLoggingMessageEq t1 t2 =
    let lm1 = (decodeStrict . encodeUtf8) t1 :: Maybe LoggingMessage
        lm2 = (decodeStrict . encodeUtf8) t2 :: Maybe LoggingMessage
    in  case (lm1, lm2) of
          (Just (LoggingMessage _at1 ns1 dataX1 sev1 _thread1 _host1),
              Just (LoggingMessage _at2 ns2 dataX2 sev2 _thread2 _host2)) ->
                let res = ns1 == ns2 && dataX1 == dataX2 && sev1 == sev2
                in if not res
                    then do
                      putStrLn ("Failed ns1: " ++ show ns1 ++ " ns2: " ++ show ns2 ++
                                " dataX1: " ++ show dataX1 ++ " dataX2: " ++ show dataX2 ++
                                " sev1: " ++ show sev1 ++ " sev2: " ++ show sev2)
                      pure False
                    else pure True
          _ -> do
            putStrLn ("Failed t1:" ++ unpack t1 ++ " t2: " ++ unpack t2
                      ++ " lm1 " ++ show lm1 ++ " lm2 " ++ show lm2)
            pure False

testLoggingMessagesEq :: [Text] -> [Text] -> IO Bool
testLoggingMessagesEq [] [] = pure True
testLoggingMessagesEq (a : atl) (b : btl) = do
    resl <- testLoggingMessageEq a b
    if resl
      then do
        testLoggingMessagesEq atl btl
      else do
        putStrLn ("Failed a: " ++ unpack a ++ " b: " ++ unpack b)
        pure False
testLoggingMessagesEq _ _ = do
    putStrLn "number differs"
    pure False
