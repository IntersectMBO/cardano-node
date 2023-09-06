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
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (parseScriptFileAeson)
import           Cardano.Benchmarking.Script.Core (setProtocolParameters, traceTxGeneratorVersion)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Types

type Script = [Action]

runScript :: Script -> IOManager -> IO (Either Error ())
runScript script iom = runActionM execScript iom >>= \case
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
  execScript = do
    setProtocolParameters QueryLocalNode
    case break (\case StartProtocol {} -> True ; _ -> False) script of
      (beforeTracerInit, startProtoTracerInit : afterTracerInit) -> do
        sequence_ $ map action beforeTracerInit
                  ++ [action startProtoTracerInit, traceTxGeneratorVersion]
                  ++ map action afterTracerInit
      _ -> throwE $ UserError "runScript: StartProtocol missing"

shutDownLogging :: ActionM ()
shutDownLogging = do
  traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ threadDelay (200 * 1_000)
