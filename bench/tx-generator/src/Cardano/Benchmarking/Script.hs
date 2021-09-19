{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Cardano.Benchmarking.Script
  ( Script
  , runScript
  , parseScriptFile
  )
where

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class

import           Ouroboros.Network.NodeToClient (IOManager)
import           Cardano.Node.Configuration.Logging (shutdownLoggingLayer)

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (parseScriptFile)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Types

type Script = [Action]

runScript :: Script -> IOManager -> IO (Either Error ())
runScript script iom = runActionM (forM_ script action) iom >>= \case
  (Right a  , s ,  ()) -> do
    cleanup s shutDownLogging
    threadDelay 10_000_000
    return $ Right a
  (Left err , s  , ()) -> do
    cleanup s (traceError (show err) >> shutDownLogging)
    threadDelay 10_000_000
    return $ Left err
 where
  cleanup s a = void $ runActionMEnv s a iom

shutDownLogging :: ActionM ()
shutDownLogging = do
  ll <- get LoggingLayer
  traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ do
    threadDelay (200*1000)
    shutdownLoggingLayer ll
