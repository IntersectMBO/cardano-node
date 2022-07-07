{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Env
where

import           Prelude
import           Data.Functor.Identity
import qualified Data.Text as Text
import           Data.Dependent.Sum (DSum(..))
import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict (RWST)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import           Control.Tracer (traceWith)

import qualified Cardano.Node.Types (ConfigError)
import qualified Cardano.Benchmarking.Tracer as Tracer
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Benchmarking.GeneratorTx.Error (TxGenError)
import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.Script.Store

type Env = DMap Store Identity

emptyEnv :: Env
emptyEnv = DMap.empty

type ActionM a = ExceptT Error (RWST IOManager () Env IO) a

runActionM :: ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionM = runActionMEnv emptyEnv

runActionMEnv :: Env -> ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionMEnv env action iom = RWS.runRWST (runExceptT action) iom env

type SetKeyVal = DSum Setters.Tag Identity

data Error where
  LookupError :: !(Store v)  -> Error
  TxGenError  :: !TxGenError -> Error
  NodeConfigError :: Cardano.Node.Types.ConfigError -> Error
  ProtocolInstantiationError :: !String -> Error
  MkNodeConfigError :: !String -> Error
  ApiError    :: !String     -> Error
  UserError   :: !String     -> Error
  WalletError :: !String     -> Error
  MetadataError :: !String   -> Error

deriving instance Show Error

liftTxGenError :: TxGenError -> ActionM a
liftTxGenError = throwE . TxGenError

askIOManager :: ActionM IOManager
askIOManager = lift RWS.ask

set :: Store v -> v -> ActionM ()
set key val = lift $ RWS.modify $ DMap.insert key (pure val)

unSet :: Store v -> ActionM ()
unSet key = lift $ RWS.modify $ DMap.delete key

setName :: Name v -> v -> ActionM ()
setName = set . Named

get :: Store v -> ActionM v
get key = do
  lift (RWS.gets $ DMap.lookup key) >>= \case
    Just (Identity v) -> return v
    Nothing -> throwE $ LookupError key

getName :: Name v -> ActionM v
getName = get . Named

getUser :: Tag v -> ActionM v
getUser = get . User

consumeName :: Name v -> ActionM v
consumeName n = do
  v <- getName n
  unSet $ Named n
  return v

traceBenchTxSubmit :: (forall txId. x -> Tracer.TraceBenchTxSubmit txId) -> x -> ActionM ()
traceBenchTxSubmit tag msg = do
  tracers  <- get BenchTracers
  liftIO $ traceWith (Tracer.btTxSubmit_ tracers) $ tag msg

traceError :: String -> ActionM ()
traceError = traceBenchTxSubmit (Tracer.TraceBenchTxSubError . Text.pack)

traceDebug :: String -> ActionM ()
traceDebug = traceBenchTxSubmit Tracer.TraceBenchTxSubDebug
