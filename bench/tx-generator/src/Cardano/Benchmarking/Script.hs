{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Cardano.Benchmarking.Script
  ( Script
  , runScript
  , parseScriptFileAeson
  )
where

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           System.Mem (performGC)

import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (parseScriptFileAeson)
import           Cardano.Benchmarking.Script.Core (setProtocolParameters)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Types

type Script = [Action]

runScript :: Script -> IOManager -> IO (Either Error ())
runScript script iom = do
  result <- go
  performGC
  threadDelay $ 150 * 1_000
  return result
  where
    cleanup :: Env -> ActionM () IO () -> IO (Either Error (), Env, ())
    cleanup s a = runActionMEnv s a iom
    execScript = do
      setProtocolParameters QueryLocalNode
      forM_ script action
    go = runActionM execScript iom >>= \case
      (Right a  , s ,  ()) -> do
        _ <- cleanup s shutDownLogging
        return $ Right a
      (Left err , s  , ()) -> do
        _ <- cleanup s (traceError (show err) >> shutDownLogging)
        return $ Left err

shutDownLogging :: (Monoid w, MonadIO m) => ActionM w m ()
shutDownLogging = do
  traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ threadDelay $ 350 * 1_000
