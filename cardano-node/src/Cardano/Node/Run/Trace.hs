{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Run.Trace
  ( checkLiveViewRequiredTracers
  ) where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Cardano.Tracing.Config (TraceOptions (..), TraceSelection (..))

checkLiveViewRequiredTracers :: TraceOptions -> IO ()
checkLiveViewRequiredTracers traceConfig = do
  reqTracers <- case traceConfig of
                  TracingOn TraceSelection{ traceBlockFetchDecisions,traceChainDB
                                          , traceForge,traceMempool} ->
                    return [traceBlockFetchDecisions, traceChainDB, traceForge, traceMempool]
                  TracingOff ->
                    return [False]

  if all (== True) reqTracers
  then pure ()
  else do putTextLn "for full functional 'LiveView', please turn on the following \
                    \tracers in the configuration file: TraceBlockFetchDecisions, \
                    \TraceChainDb, TraceForge & TraceMempool"

          putTextLn "     (press enter to continue)"
          _ <- getLine
          pure ()
