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
        , Env
        , Error(..)
        , HoldsIOM(..)
        , PureActionData(..)
        , runActionM
        , runActionMEnv
        , liftTxGenError
        , liftIOSafe
        , askIOManager
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

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict (RWST)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Prelude

import           Cardano.Api (File (..), SocketPath)

import           Cardano.Logging

import           Cardano.Benchmarking.GeneratorTx
import qualified Cardano.Benchmarking.LogTypes as Tracer
import           Cardano.Benchmarking.OuroborosImports (NetworkId, PaymentKey, ShelleyGenesis,
                   SigningKey)
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Wallet
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.TxGenerator.PlutusContext (PlutusBudgetSummary)
import           Cardano.TxGenerator.Types (TxGenError (..))


-- | The 'Env' type represents the state maintained while executing
-- a series of actions. The 'Maybe' types are largely to represent
-- as-of-yet unset values.
data Env = Env { -- | 'Cardano.Api.ProtocolParameters' is ultimately
                 -- wrapped by 'ProtocolParameterMode' which itself is
                 -- a sort of custom 'Maybe'.
                 protoParams :: Maybe ProtocolParameterMode
               , benchTracers :: Maybe Tracer.BenchTracers
               , envGenesis :: Maybe (ShelleyGenesis StandardCrypto)
               , envProtocol :: Maybe SomeConsensusProtocol
               , envNetworkId :: Maybe NetworkId
               , envSocketPath :: Maybe FilePath
               , envKeys :: Map String (SigningKey PaymentKey)
               , envThreads :: Map String AsyncBenchmarkControl
               , envWallets :: Map String WalletRef
               , envSummary :: Maybe PlutusBudgetSummary
               }

-- | `Env` uses `Maybe` to represent values that might be uninitialized.
-- This being empty means `Nothing` is used across the board, along with
-- all of the `Map.Map` structures being `Map.empty`.
emptyEnv :: Env
emptyEnv = Env { protoParams = Nothing
               , benchTracers = Nothing
               , envGenesis = Nothing
               , envKeys = Map.empty
               , envProtocol = Nothing
               , envNetworkId = Nothing
               , envSocketPath = Nothing
               , envThreads = Map.empty
               , envWallets = Map.empty
               , envSummary = Nothing
               }

-- | 'PureActionData' is the 'Control.Monad.Reader' monad parameter to
-- RWST' so that the wallet setup impure by dint of
-- 'Control.Concurrent.MVar.newMVar' and whatever other impure setup
-- needs to be done can be done entirely in advance.
data PureActionData
  = PureActionData {
    padIOManager :: IOManager
  , padInitWallet :: Map String WalletRef
  }

class HoldsIOM t where
  getIOM :: t -> IOManager

instance HoldsIOM IOManager where
  getIOM = id

instance HoldsIOM PureActionData where
  getIOM = padIOManager

-- | This abbreviates an `ExceptT` and `RWST` with particular types
-- used as parameters.
type ActionM r w m a = ExceptT Error (RWST r w Env m) a

-- | This runs an `ActionM` starting with an empty `Env`.
runActionM :: ActionM r w m ret -> r -> m (Either Error ret, Env, w)
runActionM = runActionMEnv emptyEnv

-- | This runs an `ActionM` starting with the `Env` being passed.
runActionMEnv :: Env -> ActionM r w m ret -> r -> m (Either Error ret, Env, w)
runActionMEnv env action iom = RWS.runRWST (runExceptT action) iom env

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
liftTxGenError :: (Monoid w, Monad m) => TxGenError -> ActionM r w m a
liftTxGenError = throwE . Cardano.Benchmarking.Script.Env.TxGenError

-- | The safety comes from the invocation of `throwE`
-- instead of just using the constructor for `ExceptT`
-- to convert the `Either` to an `ExceptT` monadic value.
liftIOSafe :: (Monoid w, MonadIO m) => IO (Either TxGenError a) -> ActionM r w m a
liftIOSafe a = liftIO a >>= either liftTxGenError pure

-- | Accessor for the `IOManager` reader monad aspect of the `RWST`.
askIOManager :: (HoldsIOM r, Monoid w, Monad m) => ActionM r w m IOManager
askIOManager = getIOM <$> lift RWS.ask

-- | Helper to modify `Env` record fields.
modifyEnv :: (Monoid w, Monad m) => (Env -> Env) -> ActionM r w m ()
modifyEnv = lift . RWS.modify

-- | Write accessor for `protoParams`.
setProtoParamMode :: (Monoid w, Monad m) => ProtocolParameterMode -> ActionM r w m ()
setProtoParamMode val = modifyEnv (\e -> e { protoParams = Just val })

-- | Write accessor for `benchTracers`.
setBenchTracers :: (Monoid w, Monad m) => Tracer.BenchTracers -> ActionM r w m ()
setBenchTracers val = modifyEnv (\e -> e { benchTracers = Just val })

-- | Write accessor for `envGenesis`.
setEnvGenesis :: (Monoid w, Monad m) => ShelleyGenesis StandardCrypto -> ActionM r w m ()
setEnvGenesis val = modifyEnv (\e -> e { envGenesis = Just val })

-- | Write accessor for `envKeys`.
setEnvKeys :: (Monoid w, Monad m) => String -> SigningKey PaymentKey -> ActionM r w m ()
setEnvKeys key val = modifyEnv (\e -> e { envKeys = Map.insert key val (envKeys e) })

-- | Write accessor for `envProtocol`.
setEnvProtocol :: (Monoid w, Monad m) => SomeConsensusProtocol -> ActionM r w m ()
setEnvProtocol val = modifyEnv (\e -> e { envProtocol = Just val })

-- | Write accessor for `envNetworkId`.
setEnvNetworkId :: (Monoid w, Monad m) => NetworkId -> ActionM r w m ()
setEnvNetworkId val = modifyEnv (\e -> e { envNetworkId = Just val })

-- | Write accessor for `envSocketPath`.
setEnvSocketPath :: (Monoid w, Monad m) => FilePath -> ActionM r w m ()
setEnvSocketPath val = modifyEnv (\e -> e { envSocketPath = Just val })

-- | Write accessor for `envThreads`.
setEnvThreads :: (Monoid w, Monad m) => String -> AsyncBenchmarkControl -> ActionM r w m ()
setEnvThreads key val = modifyEnv (\e -> e { envThreads = Map.insert key val (envThreads e) })

-- | Write accessor for `envWallets`.
setEnvWallets :: (Monoid w, Monad m) => String -> WalletRef -> ActionM r w m ()
setEnvWallets key val = modifyEnv (\e -> e { envWallets = Map.insert key val (envWallets e) })

-- | Write accessor for `envSummary`.
setEnvSummary :: (Monoid w, Monad m) => PlutusBudgetSummary -> ActionM r w m ()
setEnvSummary val = modifyEnv (\e -> e { envSummary = Just val })

-- | Read accessor helper for `Maybe` record fields of `Env`.
getEnvVal :: (Monoid w, Monad m) => (Env -> Maybe t) -> String -> ActionM r w m t
getEnvVal acc s = do
  lift (RWS.gets acc) >>= \case
    Just x -> return x
    Nothing -> throwE . UserError $ "Unset " ++ s

-- | Read accessor helper for `Map.Map` record fields of `Env`.
getEnvMap :: (Monoid w, Monad m) => (Env -> Map String t) -> String -> ActionM r w m t
getEnvMap acc key = do
  m <- lift $ RWS.gets acc
  case Map.lookup key m of
    Just x -> return x
    Nothing -> throwE . UserError $ "Lookup of " ++ key ++ " failed"

-- | Read accessor for `protoParams`.
getProtoParamMode :: (Monoid w, Monad m) => ActionM r w m ProtocolParameterMode
getProtoParamMode = getEnvVal protoParams "ProtocolParameterMode"

-- | Read accessor for `benchTracers`.
getBenchTracers :: (Monoid w, Monad m) => ActionM r w m Tracer.BenchTracers
getBenchTracers = getEnvVal benchTracers "BenchTracers"

-- | Read accessor for `envGenesis`.
getEnvGenesis :: (Monoid w, Monad m) => ActionM r w m (ShelleyGenesis StandardCrypto)
getEnvGenesis = getEnvVal envGenesis "Genesis"

-- | Read accessor for `envKeys`.
getEnvKeys :: (Monoid w, Monad m) => String -> ActionM r w m (SigningKey PaymentKey)
getEnvKeys = getEnvMap envKeys

-- | Read accessor for `envNetworkId`.
getEnvNetworkId :: (Monoid w, Monad m) => ActionM r w m NetworkId
getEnvNetworkId = getEnvVal envNetworkId "Genesis"

-- | Read accessor for `envProtocol`.
getEnvProtocol :: (Monoid w, Monad m) => ActionM r w m SomeConsensusProtocol
getEnvProtocol = getEnvVal envProtocol "Protocol"

-- | Read accessor for `envSocketPath`.
getEnvSocketPath :: (Monoid w, Monad m) => ActionM r w m SocketPath
getEnvSocketPath = File <$> getEnvVal envSocketPath "SocketPath"

-- | Read accessor for `envThreads`.
getEnvThreads :: (Monoid w, Monad m) => String -> ActionM r w m AsyncBenchmarkControl
getEnvThreads = getEnvMap envThreads

-- | Read accessor for `envWallets`.
getEnvWallets :: (Monoid w, Monad m) => String -> ActionM r w m WalletRef
getEnvWallets = getEnvMap envWallets

-- | Read accessor for `envSummary`.
getEnvSummary :: (Monoid w, Monad m) => ActionM r w m (Maybe PlutusBudgetSummary)
getEnvSummary = lift (RWS.gets envSummary)

-- | Helper to make submissions to the `Tracer.BenchTracers`.
traceBenchTxSubmit :: (Monoid w, MonadIO m) => (forall txId. x -> Tracer.TraceBenchTxSubmit txId) -> x -> ActionM r w m ()
traceBenchTxSubmit tag msg = do
  tracers  <- getBenchTracers
  liftIO $ traceWith (Tracer.btTxSubmit_ tracers) $ tag msg

-- | Submit an error message to the `Tracer.BenchTracers`.
traceError :: (Monoid w, MonadIO m) => String -> ActionM r w m ()
traceError = traceBenchTxSubmit (Tracer.TraceBenchTxSubError . Text.pack)

-- | Submit a debug message to the `Tracer.BenchTracers`.
traceDebug :: (Monoid w, MonadIO m) => String -> ActionM r w m ()
traceDebug = traceBenchTxSubmit Tracer.TraceBenchTxSubDebug
