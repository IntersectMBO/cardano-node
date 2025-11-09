{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Test.Tracer (
    testTracer
  , formattedMsgAsText
  , testLoggingMessageEq
  , testLoggingMessagesEq
  ) where

import           Cardano.Logging
import           Cardano.Logging.Types.TraceMessage

import           Control.Monad.IO.Class
import           Data.Aeson (decodeStrict)
import           Data.Function (on)
import           Data.IORef
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)


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
formattedMsgAsText (FormattedCBOR _) = error "FormattedMessage.FormattedCBOR currently has no Text representation"

testLoggingMessageEq :: Text -> Text -> IO Bool
testLoggingMessageEq t1 t2 =
    let lm1 = (decodeStrict . encodeUtf8) t1 :: Maybe TraceMessage
        lm2 = (decodeStrict . encodeUtf8) t2 :: Maybe TraceMessage
    in  case (lm1, lm2) of
          (Just parse1, Just parse2) ->
            let
              constraints =
                [ (==) `on` tmsgNS
                , (==) `on` tmsgData
                , (==) `on` tmsgSev
                ]
              allConstraintsHold = all (\check -> check parse1 parse2) constraints
            in if not allConstraintsHold
                then do
                  putStrLn $ "Failed ns1: " ++ show (tmsgNS parse1) ++ " ns2: " ++ show (tmsgNS parse2) ++
                             " / data1: " ++ show (tmsgData parse1) ++ " data2: " ++ show (tmsgData parse1) ++
                             " / sev1: " ++ show (tmsgSev parse1) ++ " sev2: " ++ show (tmsgSev parse2)
                  pure False
                else pure True
          _ -> do
            putStrLn $ "Failed t1:" ++ unpack t1 ++ " t2: " ++ unpack t2 ++
                       " / lm1 " ++ show lm1 ++ " lm2 " ++ show lm2
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
