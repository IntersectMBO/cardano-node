{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Configuration (
    testConfig
  , testMessageDocumented
) where

import           Control.Monad.IO.Class
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import           Data.Text (Text)

import           Cardano.Logging

newtype TestMessage = TestMessage Text
  deriving Show

instance LogFormatting TestMessage where
  forHuman (TestMessage text)         = text
  forMachine _verb (TestMessage text) =
         HM.fromList
           [ "kind" AE..= AE.String "TestMessage"
           , "text" AE..= AE.String text
           ]

testMessageDocumented :: Documented TestMessage
testMessageDocumented = Documented
  [ DocMsg (TestMessage "dummy") [] "just a text"
  ]

tracers :: MonadIO m => m (Trace m TestMessage, Trace m TestMessage)
tracers  = do
  t <- standardTracer
  t0 <- humanFormatter True "cardano" t
  t1 <- appendName "tracer1" <$> filterSeverityFromConfig t0
  t2 <- appendName "tracer2" <$> filterSeverityFromConfig t0
  pure (t1, t2)

config1 :: TraceConfig
config1 = TraceConfig {
      tcOptions = Map.fromList
          [ ([], [ConfSeverity (SeverityF Nothing)])
          , (["tracer1"], [ConfSeverity (SeverityF (Just Error))])
          , (["tracer2"], [ConfSeverity (SeverityF (Just Critical))])
          , (["tracer2","bubble"], [ConfSeverity (SeverityF (Just Info))])
          ]
    , tcForwarder = TraceOptionForwarder {
        tofAddress = LocalSocket "forwarder.log"
      , tofMode = Responder
      , tofConnQueueSize = 100
      , tofDisconnQueueSize = 1000
      , tofVerbosity = Minimum
      }
    , tcNodeName = Nothing
    , tcPeerFreqency = Nothing
    , tcResourceFreqency = Nothing
    }

config2 :: TraceConfig
config2 = TraceConfig {
      tcOptions = Map.fromList
        [ ([], [ConfSeverity (SeverityF (Just Info))])
        , (["tracer2"], [ConfSeverity (SeverityF (Just Warning))])
        , (["tracer2","bubble"], [ConfSeverity (SeverityF (Just Debug))])
        ]
    , tcForwarder = TraceOptionForwarder {
        tofAddress = LocalSocket "forwarder.log"
      , tofMode = Responder
      , tofConnQueueSize = 100
      , tofDisconnQueueSize = 1000
      , tofVerbosity = Minimum
      }
    , tcNodeName = Just "node-1"
    , tcPeerFreqency = Nothing
    , tcResourceFreqency = Nothing
    }

testConfig' :: TraceConfig -> Trace IO TestMessage -> Trace IO TestMessage -> IO ()
testConfig' tc t1 t2 = do
    let bubbleTracer = appendName "bubble" t2
    configureTracers tc testMessageDocumented [t1, t2]
    traceWith (setSeverity Critical t1) (TestMessage "Now setting config")
    traceWith
      (setSeverity Error t1)
      (TestMessage "1: show with config1 and config2")
    traceWith
      (setSeverity Info t1)
      (TestMessage "2: show not with config1 but with config2")
    traceWith
      (setSeverity Notice bubbleTracer)
      (TestMessage "3: show with config1 but not with config2")
    traceWith
      (setSeverity Warning t2)
      (TestMessage "4: show not with config1 but with config2")
    traceWith
      (setSeverity Info t2)
      (TestMessage "5: never show")

testConfig :: IO ()
testConfig = do
  (t1, t2) <- tracers
  testConfig' config1 t1 t2
  testConfig' config2 t1 t2
