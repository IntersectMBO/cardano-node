module Examples.Configuration where

import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Katip
import           Katip.Scribes.Handle (ioLogEnv)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Cardano.Logging
import           Examples.TestObjects

tracer1 :: MonadIO m => m (Trace m LO)
tracer1  = fmap
            (appendName "tracer1")
            (filterSeverityFromConfig =<< stdoutJsonKatipTracer)

tracer2 :: MonadIO m => m (Trace m LO)
tracer2  = fmap
            (appendName "tracer2")
            (filterSeverityFromConfig =<< stdoutObjectKatipTracer)

config1 :: TraceConfig
config1 = TraceConfig {
    tcName = "Config1"
  , tcOptions = Map.fromList
          [([], [CoSeverity SilenceF]),
          (["tracer1"], [CoSeverity ErrorF]),
          (["tracer2"], [CoSeverity CriticalF]),
          (["tracer2","bubble"], [CoSeverity InfoF])]
    }

config2 :: TraceConfig
config2 = TraceConfig {
    tcName = "Config2"
  , tcOptions = Map.fromList [([], [CoSeverity InfoF]),
          (["tracer2"], [CoSeverity WarningF]),
          (["tracer2","bubble"], [CoSeverity WarningF])]
    }

testConfig' :: TraceConfig -> IO ()
testConfig' tc = do
    t1 <- tracer1
    t2 <- tracer2
    let bubbleTracer = appendName "bubble" t2
    configureTracers tc [t1, t2, bubbleTracer]
    traceWith (setSeverity CriticalS t1) (LO2 "Now setting config")
    traceWith
      (setSeverity ErrorS t1)
      (LO2 "1: show with config1 and config2")
    traceWith
      (setSeverity InfoS t1)
      (LO2 "2: show not with config1 but with config2")
    traceWith
      (setSeverity NoticeS bubbleTracer)
      (LO2 "3: show with config1 but not with config2")
    traceWith
      (setSeverity WarningS t2)
      (LO2 "4: show not with config1 but with config2")
    traceWith
      (setSeverity InfoS t2)
      (LO2 "5: never show")

testConfig = do
  t1 <- tracer1
  testConfig' config1
  testConfig' config2

{-
>>> config1
{"at":"2021-02-16T14:15:15.351679768Z","env":"io","ns":["io","tracer1"],"data":{"tag":"LO2","comment":"Now setting config"},"app":["io"],"msg":"","pid":"5127","loc":null,"host":"yupanqui-PC","sev":"Critical","thread":"126"}

{"at":"2021-02-16T14:15:15.351679768Z","env":"io","ns":["io","tracer1"],"data":{"tag":"LO2","comment":"1: show with config1 and config2"},"app":["io"],"msg":"","pid":"5127","loc":null,"host":"yupanqui-PC","sev":"Error","thread":"126"}

[2021-02-16 14:15:15][io.tracer2.bubble][Notice][yupanqui-PC][PID 5127][ThreadId 126][tag:LO2][comment:3: show with config1 but not with config2]

>>> config2
{"at":"2021-02-16T14:15:15.352147647Z","env":"io","ns":["io","tracer1"],"data":{"tag":"LO2","comment":"Now setting config"},"app":["io"],"msg":"","pid":"5127","loc":null,"host":"yupanqui-PC","sev":"Critical","thread":"126"}

{"at":"2021-02-16T14:15:15.352147647Z","env":"io","ns":["io","tracer1"],"data":{"tag":"LO2","comment":"1: show with config1 and config2"},"app":["io"],"msg":"","pid":"5127","loc":null,"host":"yupanqui-PC","sev":"Error","thread":"126"}

{"at":"2021-02-16T14:15:15.352147647Z","env":"io","ns":["io","tracer1"],"data":{"tag":"LO2","comment":"2: show not with config1 but with config2"},"app":["io"],"msg":"","pid":"5127","loc":null,"host":"yupanqui-PC","sev":"Info","thread":"126"}

[2021-02-16 14:15:15][io.tracer2][Warning][yupanqui-PC][PID 5127][ThreadId 126][tag:LO2][comment:4: show not with config1 but with config2]

-}
