{-# LANGUAGE FlexibleContexts #-}
module Examples.Documentation (
  docTracers
) where

import qualified Data.Text.IO as T

import           Cardano.Logging
import           Examples.TestObjects

docTracers :: IO ()
docTracers = do
  t <- standardTracer
  t1' <- humanFormatter True (Just "cardano") t
  let t1 = withSeverityTraceForgeEvent
                (appendName "node" t1')
  t2' <- machineFormatter (Just "cardano") t
  let t2 = withSeverityTraceForgeEvent
                (appendName "node" t2')
  bl <- documentMarkdown traceForgeEventDocu [t1, t2]
  res <- buildersToText bl emptyTraceConfig
  T.writeFile "/tmp/Testdocu.md" res
