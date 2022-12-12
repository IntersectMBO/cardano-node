{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}



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
  let t1 :: Trace IO (TraceForgeEvent LogBlock) = withSeverity severityFor
                (appendName "node" t1')
  t2' <- machineFormatter (Just "cardano") t
  let t2  :: Trace IO (TraceForgeEvent LogBlock) = withSeverity severityFor
                (appendName "node" t2')
  bl <- documentMarkdown [t1, t2]
  res <- buildersToText bl emptyTraceConfig
  T.writeFile "/tmp/Testdocu.md" res
