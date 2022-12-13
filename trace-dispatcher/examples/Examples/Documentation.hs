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
  t1' <- humanFormatter True Nothing t
  let t1 :: Trace IO (TraceForgeEvent LogBlock) =
            withSeverity
              $ withPrivacy
                $ withDetails
                  $ appendOuterName "node" t1'
  t2' <- machineFormatter Nothing t
  let t2  :: Trace IO (TraceForgeEvent LogBlock) =
            withSeverity
              $ withPrivacy
                $ withDetails
                   $ appendOuterName "node" t2'
  bl <- documentTracer [t1, t2]
  res <- docuResultsToText bl emptyTraceConfig
  T.writeFile "/tmp/Testdocu.md" res
