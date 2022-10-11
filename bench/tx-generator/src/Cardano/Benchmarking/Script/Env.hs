{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Env (
        ActionM
        , Error(..)
        , runActionM
        , runActionMEnv
        , liftTxGenError
        , askIOManager
        , traceDebug
        , traceError
        , traceBenchTxSubmit
        , getBenchTracers
        , setBenchTracers
        , getEnvGenesis
        , setEnvGenesis
        , getProtoParamMode
        , setProtoParamMode
        , get
        , set
) where

import           Prelude
import           Data.Functor.Identity
import qualified Data.Text as Text
import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict (RWST)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import           "contra-tracer" Control.Tracer (traceWith)

import qualified Cardano.Benchmarking.LogTypes as Tracer
import           Ouroboros.Network.NodeToClient (IOManager)
import           Cardano.Benchmarking.OuroborosImports(ShelleyGenesis, StandardShelley)
import           Cardano.Benchmarking.Script.Store

import           Cardano.TxGenerator.Types (TxGenError(..))

data Env = Env { dmap :: DMap Store Identity
               , protoParams :: Maybe ProtocolParameterMode
               , benchTracers :: Maybe Tracer.BenchTracers
               , envGenesis :: Maybe (ShelleyGenesis StandardShelley)
               }

emptyEnv :: Env
emptyEnv = Env { dmap = DMap.empty
               , protoParams = Nothing
               , benchTracers = Nothing
               , envGenesis = Nothing
               }

type ActionM a = ExceptT Error (RWST IOManager () Env IO) a

runActionM :: ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionM = runActionMEnv emptyEnv

runActionMEnv :: Env -> ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionMEnv env action iom = RWS.runRWST (runExceptT action) iom env

data Error where
  LookupError :: !(Store v)  -> Error
  TxGenError  :: !TxGenError -> Error
  UserError   :: !String     -> Error
  WalletError :: !String     -> Error

deriving instance Show Error

liftTxGenError :: TxGenError -> ActionM a
liftTxGenError = throwE . Cardano.Benchmarking.Script.Env.TxGenError


askIOManager :: ActionM IOManager
askIOManager = lift RWS.ask

set :: Store v -> v -> ActionM ()
set key val = lift $ RWS.modify $ (\e -> e { dmap = DMap.insert key (pure val) (dmap e)})

modifyEnv :: (Env -> Env) -> ActionM ()
modifyEnv = lift . RWS.modify

setProtoParamMode :: ProtocolParameterMode -> ActionM ()
setProtoParamMode val = modifyEnv $ (\e -> e { protoParams = pure val })

setBenchTracers :: Tracer.BenchTracers -> ActionM ()
setBenchTracers val = modifyEnv $ (\e -> e { benchTracers = pure val })

setEnvGenesis :: ShelleyGenesis StandardShelley -> ActionM ()
setEnvGenesis val = modifyEnv $ (\e -> e { envGenesis = pure val })

get :: Store v -> ActionM v
get key = do
  lift (RWS.gets $ (\e -> DMap.lookup key $ dmap e)) >>= \case
    Just (Identity v) -> return v
    Nothing -> throwE $ LookupError key

getEnvVal :: (Env -> Maybe t) -> String -> ActionM t
getEnvVal acc s = do
  lift (RWS.gets acc) >>= \case
    Just x -> return x
    Nothing -> throwE . UserError $ "Unset " ++ s

getProtoParamMode :: ActionM ProtocolParameterMode
getProtoParamMode = getEnvVal protoParams "ProtocolParameterMode"

getBenchTracers :: ActionM Tracer.BenchTracers
getBenchTracers = getEnvVal benchTracers "BenchTracers"

getEnvGenesis :: ActionM (ShelleyGenesis StandardShelley)
getEnvGenesis = getEnvVal envGenesis "Genesis"

traceBenchTxSubmit :: (forall txId. x -> Tracer.TraceBenchTxSubmit txId) -> x -> ActionM ()
traceBenchTxSubmit tag msg = do
  tracers  <- getBenchTracers
  liftIO $ traceWith (Tracer.btTxSubmit_ tracers) $ tag msg

traceError :: String -> ActionM ()
traceError = traceBenchTxSubmit (Tracer.TraceBenchTxSubError . Text.pack)

traceDebug :: String -> ActionM ()
traceDebug = traceBenchTxSubmit Tracer.TraceBenchTxSubDebug
