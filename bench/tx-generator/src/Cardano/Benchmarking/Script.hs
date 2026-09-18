{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import qualified Cardano.Benchmarking.Script.Env as Env (ActionM, Env (..), Error,
                   getEnvThreads, runActionMEnv, traceDebug, traceError)
import           Cardano.Benchmarking.Script.Types

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TVar as STM (readTVar)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM as STM (atomically)
import           System.Mem (performGC)

type Script = [Action]

-- | Run a benchmarking script. The second component of the result carries
-- the 'AsyncBenchmarkControl' of the submission threads when the script
-- started any: submit modes other than 'Benchmark' (e.g. 'LocalSocket',
-- 'Ogmios') never create one, in which case it is 'Nothing'.
runScript :: Env.Env -> Script -> EnvConsts -> IO (Either Env.Error (), Maybe AsyncBenchmarkControl)
runScript env script constants@EnvConsts { .. } = do
  -- Plain 'putStrLn', not a trace: this runs before the script's own
  -- 'StartProtocol' action (if it even has one), so BenchTracers are
  -- not yet constructed.
  putStrLn $ "This workload script will generate " ++
    ( let count = countScriptTxns script in
      if
        | count == 0        -> "no transactions"
        | count == maxBound -> "an infinite stream of transactions"
        | count == (-1)     -> "an unknown number of transactions (stream is non-deterministic)"
        | otherwise         -> "a finite stream yielding " ++ show count ++ " transactions"
    )
  result <- go
  performGC
  threadDelay $ 150 * 1_000
  return result
  where
    go :: IO (Either Env.Error (), Maybe AsyncBenchmarkControl)
    go = Env.runActionMEnv env execScript constants >>= \case
      (Right abcMaybe, env', ()) -> do
        cleanup env' shutDownLogging
        pure (Right (), abcMaybe)
      (Left err,  env', ()) -> do
        cleanup env' (Env.traceError (show err) >> shutDownLogging)
        abcMaybe <- STM.atomically $ STM.readTVar envThreads
        pure (Left err, abcMaybe)
      where
        cleanup :: Env.Env -> Env.ActionM () -> IO ()
        cleanup env' acts = void $ Env.runActionMEnv env' acts constants
        execScript :: Env.ActionM (Maybe AsyncBenchmarkControl)
        execScript = do
          setProtocolParameters QueryLocalNode
          forM_ script action
          Env.getEnvThreads

shutDownLogging :: Env.ActionM ()
shutDownLogging = do
  Env.traceDebug "QRT Last Message. LoggingLayer shutting down ..."
  liftIO $ threadDelay $ 350 * 1_000

-- Results:
--   (-1):     somwhere in [0..Infinity] due to use of non-deterministic generator combinator
--   maxBound: infinite txn stream
--   n:        finite stream of n txns
countScriptTxns :: Script -> Int
countScriptTxns = sum . map actionTxCount
  where
    actionTxCount = \case
      Submit _ _ _ gen -> genTxCount gen
      _                -> 0
    genTxCount = \case
      SecureGenesis{} -> 1
      Split{}         -> 1
      SplitN{}        -> 1
      NtoM{}          -> 1
      Cycle g         -> if genTxCount g < 0 then (-1) else maxBound
      Take i g        -> min i (genTxCount g)
      Sequence gs     -> guardedSum $ map genTxCount gs
      RoundRobin gs   -> guardedSum $ map genTxCount gs
      OneOf{}         -> (-1)   -- is non-deterministic
      where
        guardedSum is
          | any (< 0) is            = (-1)
          | any (== maxBound) is    = maxBound
          | otherwise               = sum is
