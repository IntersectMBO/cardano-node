{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Benchmarking.Script
  ( Script
  , runScript
  , parseScriptFileAeson
  )
where

import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (parseScriptFileAeson)
import           Cardano.Benchmarking.Script.Core (setProtocolParameters)
import qualified Cardano.Benchmarking.Script.Env as Env (ActionM, Env (..), Error (TxGenError),
                   getEnvThreads, runActionMEnv, traceError)
import           Cardano.Benchmarking.Script.Types
import qualified Cardano.TxGenerator.Types as Types (TxGenError (..))

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TVar as STM (readTVar)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM as STM (atomically)
import           Control.Monad.Trans.Except as Except (throwE)
import qualified Data.List as List (unwords)
import           System.Mem (performGC)

type Script = [Action]

runScript :: Env.Env -> Script -> EnvConsts -> IO (Either Env.Error (), AsyncBenchmarkControl)
runScript env script constants@EnvConsts { .. } = do
  result <- go
  performGC
  threadDelay $ 150 * 1_000
  return result
  where
    go :: IO (Either Env.Error (), AsyncBenchmarkControl)
    go = Env.runActionMEnv env execScript constants >>= \case
      (Right abc, env', ()) -> do
        cleanup env' shutDownLogging
        pure (Right (), abc)
      (Left err,  env', ()) -> do
        cleanup env' (Env.traceError (show err) >> shutDownLogging)
        abcMaybe <- STM.atomically $ STM.readTVar envThreads
        case abcMaybe of
          Just abc -> pure (Left err, abc)
          Nothing  -> error $ List.unwords
                                [ "Cardano.Benchmarking.Script.runScript:"
                                , "AsyncBenchmarkControl uninitialized" ]
      where
        cleanup :: Env.Env -> Env.ActionM () -> IO ()
        cleanup env' acts = void $ Env.runActionMEnv env' acts constants
        execScript :: Env.ActionM AsyncBenchmarkControl
        execScript = do
          setProtocolParameters QueryLocalNode
          forM_ script action
          abcMaybe <- Env.getEnvThreads
          case abcMaybe of
            Nothing  -> throwE $ Env.TxGenError $ Types.TxGenError $
              List.unwords
                  [ "Cardano.Benchmarking.Script.runScript:"
                  , "AsyncBenchmarkControl absent from map in execScript" ]
            Just abc -> pure abc

shutDownLogging :: Env.ActionM ()
shutDownLogging = do
  Env.traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ threadDelay $ 350 * 1_000
