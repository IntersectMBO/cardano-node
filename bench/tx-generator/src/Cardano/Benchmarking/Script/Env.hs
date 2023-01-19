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
import           "contra-tracer" Control.Tracer (traceWith)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Prelude

import           Cardano.Benchmarking.GeneratorTx
import qualified Cardano.Benchmarking.LogTypes as Tracer
import           Cardano.Benchmarking.OuroborosImports (NetworkId, PaymentKey, ShelleyGenesis,
                   SigningKey, StandardShelley)
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Wallet
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.TxGenerator.PlutusContext (PlutusBudgetSummary)
import           Cardano.TxGenerator.Types (TxGenError (..))


data Env = Env { protoParams :: Maybe ProtocolParameterMode
               , benchTracers :: Maybe Tracer.BenchTracers
               , envGenesis :: Maybe (ShelleyGenesis StandardShelley)
               , envProtocol :: Maybe SomeConsensusProtocol
               , envNetworkId :: Maybe NetworkId
               , envSocketPath :: Maybe FilePath
               , envKeys :: Map String (SigningKey PaymentKey)
               , envThreads :: Map String AsyncBenchmarkControl
               , envWallets :: Map String WalletRef
               , envSummary :: Maybe PlutusBudgetSummary
               }

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

type ActionM a = ExceptT Error (RWST IOManager () Env IO) a

runActionM :: ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionM = runActionMEnv emptyEnv

runActionMEnv :: Env -> ActionM ret -> IOManager -> IO (Either Error ret, Env, ())
runActionMEnv env action iom = RWS.runRWST (runExceptT action) iom env

data Error where
  TxGenError  :: !TxGenError -> Error
  UserError   :: !String     -> Error
  WalletError :: !String     -> Error

deriving instance Show Error

liftTxGenError :: TxGenError -> ActionM a
liftTxGenError = throwE . Cardano.Benchmarking.Script.Env.TxGenError

liftIOSafe :: IO (Either TxGenError a) -> ActionM a
liftIOSafe a = liftIO a >>= either liftTxGenError pure

askIOManager :: ActionM IOManager
askIOManager = lift RWS.ask

modifyEnv :: (Env -> Env) -> ActionM ()
modifyEnv = lift . RWS.modify

setProtoParamMode :: ProtocolParameterMode -> ActionM ()
setProtoParamMode val = modifyEnv (\e -> e { protoParams = pure val })

setBenchTracers :: Tracer.BenchTracers -> ActionM ()
setBenchTracers val = modifyEnv (\e -> e { benchTracers = pure val })

setEnvGenesis :: ShelleyGenesis StandardShelley -> ActionM ()
setEnvGenesis val = modifyEnv (\e -> e { envGenesis = pure val })

setEnvKeys :: String -> SigningKey PaymentKey -> ActionM ()
setEnvKeys key val = modifyEnv (\e -> e { envKeys = Map.insert key val (envKeys e) })

setEnvProtocol :: SomeConsensusProtocol -> ActionM ()
setEnvProtocol val = modifyEnv (\e -> e { envProtocol = pure val })

setEnvNetworkId :: NetworkId -> ActionM ()
setEnvNetworkId val = modifyEnv (\e -> e { envNetworkId = pure val })

setEnvSocketPath :: FilePath -> ActionM ()
setEnvSocketPath val = modifyEnv (\e -> e { envSocketPath = pure val })

setEnvThreads :: String -> AsyncBenchmarkControl -> ActionM ()
setEnvThreads key val = modifyEnv (\e -> e { envThreads = Map.insert key val (envThreads e) })

setEnvWallets :: String -> WalletRef -> ActionM ()
setEnvWallets key val = modifyEnv (\e -> e { envWallets = Map.insert key val (envWallets e) })

setEnvSummary :: PlutusBudgetSummary -> ActionM ()
setEnvSummary val = modifyEnv (\e -> e { envSummary = pure val })

getEnvVal :: (Env -> Maybe t) -> String -> ActionM t
getEnvVal acc s = do
  lift (RWS.gets acc) >>= \case
    Just x -> return x
    Nothing -> throwE . UserError $ "Unset " ++ s

getEnvMap :: (Env -> Map String t) -> String -> ActionM t
getEnvMap acc key = do
  m <- lift $ RWS.gets acc
  case Map.lookup key m of
    Just x -> return x
    Nothing -> throwE . UserError $ "Lookup of " ++ key ++ " failed"

getProtoParamMode :: ActionM ProtocolParameterMode
getProtoParamMode = getEnvVal protoParams "ProtocolParameterMode"

getBenchTracers :: ActionM Tracer.BenchTracers
getBenchTracers = getEnvVal benchTracers "BenchTracers"

getEnvGenesis :: ActionM (ShelleyGenesis StandardShelley)
getEnvGenesis = getEnvVal envGenesis "Genesis"

getEnvKeys :: String -> ActionM (SigningKey PaymentKey)
getEnvKeys = getEnvMap envKeys

getEnvNetworkId :: ActionM NetworkId
getEnvNetworkId = getEnvVal envNetworkId "Genesis"

getEnvProtocol :: ActionM SomeConsensusProtocol
getEnvProtocol = getEnvVal envProtocol "Protocol"

getEnvSocketPath :: ActionM FilePath
getEnvSocketPath = getEnvVal envSocketPath "SocketPath"

getEnvThreads :: String -> ActionM AsyncBenchmarkControl
getEnvThreads = getEnvMap envThreads

getEnvWallets :: String -> ActionM WalletRef
getEnvWallets = getEnvMap envWallets

getEnvSummary :: ActionM (Maybe PlutusBudgetSummary)
getEnvSummary = lift (RWS.gets envSummary)

traceBenchTxSubmit :: (forall txId. x -> Tracer.TraceBenchTxSubmit txId) -> x -> ActionM ()
traceBenchTxSubmit tag msg = do
  tracers  <- getBenchTracers
  liftIO $ traceWith (Tracer.btTxSubmit_ tracers) $ tag msg

traceError :: String -> ActionM ()
traceError = traceBenchTxSubmit (Tracer.TraceBenchTxSubError . Text.pack)

traceDebug :: String -> ActionM ()
traceDebug = traceBenchTxSubmit Tracer.TraceBenchTxSubDebug
