{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Unit.Configuration (
    testConfig
  , testConfigResult
) where

import           Cardano.Logging
import           Cardano.Logging.Test.Tracer

import           Control.Monad.IO.Class
import qualified Data.Aeson as AE
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

newtype TestMessage = TestMessage Text
  deriving Show

instance LogFormatting TestMessage where
  forHuman (TestMessage text)         = text
  forMachine _verb (TestMessage text) =
         KeyMap.fromList
           [ "kind" AE..= AE.String "TestMessage"
           , "text" AE..= AE.String text
           ]

instance MetaTrace TestMessage where
  namespaceFor (TestMessage _text) = Namespace [] ["TestMessage"]
  severityFor (Namespace _ ["TestMessage"]) _ = Just Info
  privacyFor  (Namespace _ ["TestMessage"]) _ = Just Public
  documentFor (Namespace _ ["TestMessage"]) = Just "Just a test"
  metricsDocFor (Namespace _ ["TestMessage"]) = []
  allNamespaces = [Namespace [] ["TestMessage"]]

tracers :: MonadIO m => IORef [FormattedMessage] -> m (Trace m TestMessage, Trace m TestMessage, Trace m TestMessage)
tracers testTracerRef = do
  t <- testTracer testTracerRef
  t0 <- machineFormatter Nothing t
  t1 <- withInnerNames . appendPrefixName "tracer1" <$> filterSeverityFromConfig t0
  t2 <- withInnerNames . appendPrefixName "tracer2" <$> filterSeverityFromConfig t0
  t3 <- withInnerNames . appendPrefixName "tracer3" <$> filterSeverityFromConfig t0
  pure (t1, t2, t3)

config1 :: TraceConfig
config1 = TraceConfig {
      tcOptions = Map.fromList
          [ ([], [ConfSeverity (SeverityF Nothing)])
          , (["tracer1","TestMessage"], [ConfSeverity (SeverityF (Just Error))])
          , (["tracer2","TestMessage"], [ConfSeverity (SeverityF (Just Critical))])
          , (["tracer3","TestMessage"], [ConfSeverity (SeverityF (Just Info))])
          ]
    , tcForwarder = Just TraceOptionForwarder {
        tofConnQueueSize = 100
      , tofDisconnQueueSize = 1000
      , tofVerbosity = Minimum
      }
    , tcNodeName = Nothing
    , tcPeerFrequency = Nothing
    , tcResourceFrequency = Nothing
    }

config2 :: TraceConfig
config2 = TraceConfig {
      tcOptions = Map.fromList
        [ ([], [ConfSeverity (SeverityF (Just Info))])
        , (["tracer2","TestMessage"], [ConfSeverity (SeverityF (Just Warning))])
        , (["tracer3","TestMessage"], [ConfSeverity (SeverityF (Just Warning))])
        ]
    , tcForwarder = Just TraceOptionForwarder {
        tofConnQueueSize = 100
      , tofDisconnQueueSize = 1000
      , tofVerbosity = Minimum
      }
    , tcNodeName = Just "node-1"
    , tcPeerFrequency = Nothing
    , tcResourceFrequency = Nothing
    }

testConfig' ::
     TraceConfig
  -> Trace IO TestMessage
  -> Trace IO TestMessage
  -> Trace IO TestMessage
  -> IO ()
testConfig' tc t1 t2 t3 = do
    confState <- emptyConfigReflection
    configureTracers confState tc [t1, t2, t3]
    traceWith (setSeverity Critical t1) (TestMessage "Now setting config")
    traceWith
      (setSeverity Error t1)
      (TestMessage "1: show with config1 and config2")
    traceWith
      (setSeverity Info t1)
      (TestMessage "2: show not with config1 but with config2")
    traceWith
      (setSeverity Notice t3)
      (TestMessage "3: show with config1 but not with config2")
    traceWith
      (setSeverity Warning t2)
      (TestMessage "4: show not with config1 but with config2")
    traceWith
      (setSeverity Info t2)
      (TestMessage "5: never show")

testConfig :: IO [Text]
testConfig = do
  testTracerRef <- newIORef []
  (t1, t2, t3) <- tracers testTracerRef
  testConfig' config1 t1 t2 t3
  testConfig' config2 t1 t2 t3
  msgs <- reverse <$> readIORef testTracerRef
  let res = map formattedMsgAsText msgs
  -- print res
  pure res

testConfigResult :: [Text]
testConfigResult = [
   "{\"at\":\"2023-11-27T20:58:43.900681509Z\",\"ns\":\"tracer1.TestMessage\",\"data\":{\"kind\":\"TestMessage\",\"text\":\"Now setting config\"},\"sev\":\"Critical\",\"thread\":\"501\",\"host\":\"deusXmachina\"}"
  ,"{\"at\":\"2023-11-27T20:58:43.900689522Z\",\"ns\":\"tracer1.TestMessage\",\"data\":{\"kind\":\"TestMessage\",\"text\":\"1: show with config1 and config2\"},\"sev\":\"Error\",\"thread\":\"501\",\"host\":\"deusXmachina\"}"
  ,"{\"at\":\"2023-11-27T20:58:43.900697892Z\",\"ns\":\"tracer3.TestMessage\",\"data\":{\"kind\":\"TestMessage\",\"text\":\"3: show with config1 but not with config2\"},\"sev\":\"Notice\",\"thread\":\"501\",\"host\":\"deusXmachina\"}"
  ,"{\"at\":\"2023-11-27T20:58:43.900763175Z\",\"ns\":\"tracer1.TestMessage\",\"data\":{\"kind\":\"TestMessage\",\"text\":\"Now setting config\"},\"sev\":\"Critical\",\"thread\":\"501\",\"host\":\"deusXmachina\"}"
  ,"{\"at\":\"2023-11-27T20:58:43.900769378Z\",\"ns\":\"tracer1.TestMessage\",\"data\":{\"kind\":\"TestMessage\",\"text\":\"1: show with config1 and config2\"},\"sev\":\"Error\",\"thread\":\"501\",\"host\":\"deusXmachina\"}"
  ,"{\"at\":\"2023-11-27T20:58:43.900773848Z\",\"ns\":\"tracer1.TestMessage\",\"data\":{\"kind\":\"TestMessage\",\"text\":\"2: show not with config1 but with config2\"},\"sev\":\"Info\",\"thread\":\"501\",\"host\":\"deusXmachina\"}"
  ,"{\"at\":\"2023-11-27T20:58:43.900781456Z\",\"ns\":\"tracer2.TestMessage\",\"data\":{\"kind\":\"TestMessage\",\"text\":\"4: show not with config1 but with config2\"},\"sev\":\"Warning\",\"thread\":\"501\",\"host\":\"deusXmachina\"}"
  ]
