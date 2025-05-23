{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Cardano.Benchmarking.Script.Env
Description : State type for 'ActionM' monad stack and its accessors.

The 'Env' type is the ADT for the state component of the 'ActionM'
monad stack. Its actual definition isn't exported in part because of a
transition from an earlier very generic and polymorphic definition.
In a number of respects, this module covers more of the 'ActionM'
like 'runActionM' and 'liftTxGenError', but the only significant
structure is 'Env' for state. The accessors could likely be removed
in favour of just using the record syntax to trim a few lines of
code at the cost of exposing the structure's internals. Some of the
naming related to the fact that "Cardano.Benchmarking.Script.Action"
ran into circular dependency issues during the above transition.
 -}
module Cardano.Benchmarking.Script.Env (
        ActionM
        , Env (..)
        , Error (..)
        , emptyEnv
        , newEnvConsts
        , runActionMEnv
        , liftTxGenError
        , liftIOSafe
        , askIOManager
        , askNixSvcOpts
        , askEnvThreads
        , traceDebug
        , traceError
        , traceBenchTxSubmit
        , getBenchTracers
        , setBenchTracers
        , getEnvGenesis
        , setEnvGenesis
        , getEnvKeys
        , setEnvKeys
        , getEnvNetworkId
        , setEnvNetworkId
        , getEnvProtocol
        , setEnvProtocol
        , getProtoParamMode
        , setProtoParamMode
        , getEnvSocketPath
        , setEnvSocketPath
        , getEnvThreads
        , setEnvThreads
        , getEnvWallets
        , setEnvWallets
        , getEnvSummary
        , setEnvSummary
) where

import           Cardano.Api (File (..), SocketPath)

import           Cardano.Benchmarking.GeneratorTx
import qualified Cardano.Benchmarking.LogTypes as Tracer
import           Cardano.Benchmarking.OuroborosImports (NetworkId, PaymentKey, ShelleyGenesis,
                   SigningKey)
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Wallet
import           Cardano.Logging
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Cardano.TxGenerator.PlutusContext (PlutusBudgetSummary)
import           Cardano.TxGenerator.Setup.NixService as Nix (NixServiceOptions)
import           Cardano.TxGenerator.Types (TxGenError (..))
import           Ouroboros.Network.NodeToClient (IOManager)

import           Prelude

import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM (TVar, atomically, newTVar, readTVar, writeTVar)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict (RWST)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified System.IO as IO (hPutStrLn, stderr)


-- | The 'Env' type represents the state maintained while executing
-- a series of actions. The 'Maybe' types are largely to represent
-- as-of-yet unset values.
data Env = Env { -- | 'Cardano.Api.ProtocolParameters' is ultimately
                 -- wrapped by 'ProtocolParameterMode' which itself is
                 -- a sort of custom 'Maybe'.
                 protoParams :: Maybe ProtocolParameterMode
               , envGenesis :: Maybe ShelleyGenesis
               , envProtocol :: Maybe SomeConsensusProtocol
               , envNetworkId :: Maybe NetworkId
               , envSocketPath :: Maybe FilePath
               , envKeys :: Map String (SigningKey PaymentKey)
               , envWallets :: Map String WalletRef
               , envSummary :: Maybe PlutusBudgetSummary
               }
-- | `Env` uses `Maybe` to represent values that might be uninitialized.
-- This being empty means `Nothing` is used across the board, along with
-- all of the `Map.Map` structures being `Map.empty`.
emptyEnv :: Env
emptyEnv = Env { protoParams = Nothing
               , envGenesis = Nothing
               , envKeys = Map.empty
               , envProtocol = Nothing
               , envNetworkId = Nothing
               , envSocketPath = Nothing
               , envWallets = Map.empty
               , envSummary = Nothing
               }

newEnvConsts :: IOManager -> Maybe Nix.NixServiceOptions -> STM Tracer.EnvConsts
newEnvConsts envIOManager envNixSvcOpts = do
  envThreads <- STM.newTVar Nothing
  benchTracers <- STM.newTVar Nothing
  pure Tracer.EnvConsts { .. }

-- | This abbreviates an `ExceptT` and `RWST` with particular types
-- used as parameters.
type ActionM a = ExceptT Error (RWST Tracer.EnvConsts () Env IO) a

-- | This runs an `ActionM` starting with the `Env` being passed.
runActionMEnv :: Env -> ActionM ret -> Tracer.EnvConsts -> IO (Either Error ret, Env, ())
runActionMEnv env action envConsts = RWS.runRWST (runExceptT action) envConsts env

-- | 'Error' adds two cases to 'Cardano.TxGenerator.Types.TxGenError'
-- which in turn wraps 'Cardano.Api.Error' implicit contexts to a
-- couple of its constructors. These represent errors that might arise
-- in the execution of a transaction with some distinctions as to the
-- layers where the errors could arise. At this highest level, invalid
-- users and wallets are potentially encountered. Plutus, protocol, API
-- and some arbitrary errors are potentially encountered at the next
-- layer. The layers correspond to "Cardano.Benchmarking.Script.Core"
-- for the outermost and "Cardano.Benchmarking.Set.Plutus" for the
-- middle, and the innermost to "Cardano.Api.Error".
data Error where
  TxGenError  :: !TxGenError -> Error
  UserError   :: !String     -> Error
  WalletError :: !String     -> Error

deriving instance Show Error

-- | This throws a `TxGenError` in the `ActionM` monad.
liftTxGenError :: TxGenError -> ActionM a
liftTxGenError = throwE . Cardano.Benchmarking.Script.Env.TxGenError

-- | The safety comes from the invocation of `throwE`
-- instead of just using the constructor for `ExceptT`
-- to convert the `Either` to an `ExceptT` monadic value.
liftIOSafe :: IO (Either TxGenError a) -> ActionM a
liftIOSafe a = liftIO a >>= either liftTxGenError pure

-- | Accessor for the `IOManager` reader monad aspect of the `RWST`.
askIOManager :: ActionM IOManager
askIOManager = lift $ RWS.asks Tracer.envIOManager

-- | Accessor for the `NixServiceOptions` reader monad aspect of the `RWST`.
askNixSvcOpts :: ActionM (Maybe Nix.NixServiceOptions)
askNixSvcOpts = lift $ RWS.asks Tracer.envNixSvcOpts

askEnvThreads :: ActionM (STM.TVar (Maybe AsyncBenchmarkControl))
askEnvThreads = lift $ RWS.asks Tracer.envThreads

-- | Helper to modify `Env` record fields.
modifyEnv :: (Env -> Env) -> ActionM ()
modifyEnv = lift . RWS.modify

-- | Write accessor for `protoParams`.
setProtoParamMode :: ProtocolParameterMode -> ActionM ()
setProtoParamMode val = modifyEnv (\e -> e { protoParams = Just val })

-- | Write accessor for `benchTracers`.
setBenchTracers :: Tracer.BenchTracers -> ActionM ()
setBenchTracers val = do
  btTVar <- lift $ RWS.asks Tracer.benchTracers
  liftIO $ STM.atomically do STM.writeTVar btTVar $ Just val

-- | Write accessor for `envGenesis`.
setEnvGenesis :: ShelleyGenesis -> ActionM ()
setEnvGenesis val = modifyEnv (\e -> e { envGenesis = Just val })

-- | Write accessor for `envKeys`.
setEnvKeys :: String -> SigningKey PaymentKey -> ActionM ()
setEnvKeys key val = modifyEnv (\e -> e { envKeys = Map.insert key val (envKeys e) })

-- | Write accessor for `envProtocol`.
setEnvProtocol :: SomeConsensusProtocol -> ActionM ()
setEnvProtocol val = modifyEnv (\e -> e { envProtocol = Just val })

-- | Write accessor for `envNetworkId`.
setEnvNetworkId :: NetworkId -> ActionM ()
setEnvNetworkId val = modifyEnv (\e -> e { envNetworkId = Just val })

-- | Write accessor for `envSocketPath`.
setEnvSocketPath :: FilePath -> ActionM ()
setEnvSocketPath val = modifyEnv (\e -> e { envSocketPath = Just val })

-- | Write accessor for `envThreads`.
setEnvThreads :: AsyncBenchmarkControl -> ActionM ()
setEnvThreads abc = do
  abcTVar <- lift $ RWS.asks Tracer.envThreads
  liftIO do STM.atomically $ abcTVar `STM.writeTVar` Just abc

-- | Write accessor for `envWallets`.
setEnvWallets :: String -> WalletRef -> ActionM ()
setEnvWallets key val = modifyEnv (\e -> e { envWallets = Map.insert key val (envWallets e) })

-- | Write accessor for `envSummary`.
setEnvSummary :: PlutusBudgetSummary -> ActionM ()
setEnvSummary val = modifyEnv (\e -> e { envSummary = Just val })

-- | Read accessor helper for `Maybe` record fields of `Env`.
getEnvVal :: (Env -> Maybe t) -> String -> ActionM t
getEnvVal acc s = do
  lift (RWS.gets acc) >>= \case
    Just x -> return x
    Nothing -> throwE . UserError $ "Unset " ++ s

-- | Read accessor helper for `Map.Map` record fields of `Env`.
getEnvMap :: (Env -> Map String t) -> String -> ActionM t
getEnvMap acc key = do
  m <- lift $ RWS.gets acc
  case Map.lookup key m of
    Just x -> return x
    Nothing -> throwE . UserError $ "Lookup of " ++ key ++ " failed"

-- | Read accessor for `protoParams`.
getProtoParamMode :: ActionM ProtocolParameterMode
getProtoParamMode = getEnvVal protoParams "ProtocolParameterMode"

-- | Read accessor for `benchTracers`.
-- It would be burdensome on callers to have to have to case analyze
-- this result. EnvConsts :: (Type -> Type) -> Type would make sense,
-- using the pattern of data HKT f = HKT { f1 :: f t1, f2 :: f t2, ..}
-- Then EnvConsts Maybe can be converted to EnvConsts Identity once
-- initialization is complete so the main phase doesn't need to do this.
getBenchTracers :: ActionM Tracer.BenchTracers
getBenchTracers = do
  btTVar <- lift $ RWS.asks Tracer.benchTracers
  mTracer <- liftIO $ STM.atomically do STM.readTVar btTVar
  case mTracer of
    Just tracer -> pure tracer
    Nothing -> do
      -- If this occurs, it may be worthwhile to output it in more ways
      -- because the tracer isn't actually initialized.
      let errMsg = "Env.getBenchTracers: attempted to set tracer before\
                   \  STM.TVar init"
      traceError errMsg
      liftIO $ do
        putStrLn errMsg
        IO.hPutStrLn IO.stderr errMsg
      pure $ error errMsg

-- | Read accessor for `envGenesis`.
getEnvGenesis :: ActionM ShelleyGenesis
getEnvGenesis = getEnvVal envGenesis "Genesis"

-- | Read accessor for `envKeys`.
getEnvKeys :: String -> ActionM (SigningKey PaymentKey)
getEnvKeys = getEnvMap envKeys

-- | Read accessor for `envNetworkId`.
getEnvNetworkId :: ActionM NetworkId
getEnvNetworkId = getEnvVal envNetworkId "Genesis"

-- | Read accessor for `envProtocol`.
getEnvProtocol :: ActionM SomeConsensusProtocol
getEnvProtocol = getEnvVal envProtocol "Protocol"

-- | Read accessor for `envSocketPath`.
getEnvSocketPath :: ActionM SocketPath
getEnvSocketPath = File <$> getEnvVal envSocketPath "SocketPath"

-- | Read accessor for `envThreads`.
getEnvThreads :: ActionM (Maybe AsyncBenchmarkControl)
getEnvThreads = do
  abcTVar <- lift $ RWS.asks Tracer.envThreads
  liftIO do STM.atomically $ STM.readTVar abcTVar

-- | Read accessor for `envWallets`.
getEnvWallets :: String -> ActionM WalletRef
getEnvWallets = getEnvMap envWallets

-- | Read accessor for `envSummary`.
getEnvSummary :: ActionM (Maybe PlutusBudgetSummary)
getEnvSummary = lift (RWS.gets envSummary)

-- | Helper to make submissions to the `Tracer.BenchTracers`.
traceBenchTxSubmit :: (forall txId. x -> Tracer.TraceBenchTxSubmit txId) -> x -> ActionM ()
traceBenchTxSubmit tag msg = do
  tracers  <- getBenchTracers
  liftIO $ traceWith (Tracer.btTxSubmit_ tracers) $ tag msg

-- | Submit an error message to the `Tracer.BenchTracers`.
traceError :: String -> ActionM ()
traceError = traceBenchTxSubmit (Tracer.TraceBenchTxSubError . Text.pack)

-- | Submit a debug message to the `Tracer.BenchTracers`.
traceDebug :: String -> ActionM ()
traceDebug = traceBenchTxSubmit Tracer.TraceBenchTxSubDebug
