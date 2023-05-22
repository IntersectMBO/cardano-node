{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.Script.Types (
          Action(..)
        , Generator(Cycle, NtoM, OneOf, RoundRobin, SecureGenesis,
                Sequence, Split, SplitN, Take)
        , PayMode(PayToAddr, PayToScript)
        , ProtocolParameterMode(..)
        , ProtocolParametersSource(QueryLocalNode, UseLocalProtocolFile)
        , ScriptBudget(AutoScript, StaticScriptBudget)
        , ScriptSpec(..)
        , SubmitMode(Benchmark, DiscardTX, DumpToFile, LocalSocket,
                NodeToNode)
        , TargetNodes
        , TxList(..)
) where

import           GHC.Generics
import           Prelude

import           Data.Function (on)
import           Data.List.NonEmpty
import           Data.Text (Text)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)

import           Cardano.TxGenerator.Types


-- FIXME: temporary workaround instance until Action ADT is refactored
instance Eq (SigningKey PaymentKey) where
  (==) = (==) `on` serialiseToTextEnvelope Nothing

-- | 'Action' represents the individual actions to be executed by the
-- tx-generator. It gets translated to
-- 'Cardano.Benchmarking.Script.ActionM' using 'Env' as the
-- state, 'IOManager' as the reader, and 'IO' as the monad, and further
-- wrapped in an 'ExceptT' with an 'Error' as the exception.
data Action where
  -- | 'SetNetworkId' only entails changing a state variable in an 'Env'.
  SetNetworkId       :: !NetworkId -> Action
  -- | 'SetSocketPath' likewise only entails a state variable change.
  SetSocketPath      :: !FilePath -> Action
  -- | 'InitWallet' just uses the name in a state variable and creates a
  -- fresh 'MVar' with an empty 'FundQueue' in it.
  InitWallet         :: !String -> Action
  -- | 'StartProtocol' sets state variables for protocol and genesis,
  -- but via 'mkNodeConfig' and 'mkConsensusProtocol' from the
  -- "Cardano.Node" part of the module hierarchy beneath the
  -- @cardano-node@ directory in the @cardano-node@ repo.
  StartProtocol      :: !FilePath -> !(Maybe FilePath) -> Action
  -- | 'Delay' translates to 'threadDelay' via 'delay' in
  -- "Cardano.Benchmarking.Script.Core".
  Delay              :: !Double -> Action
  -- | 'ReadSigningKey' translates to a 'readFileTextEnvelopeAnyOf' from
  -- "Cardano.Api.SerialiseTextEnvelope" on the signing key file and then
  -- drops it into a state variable via 'setEnvKeys'.
  ReadSigningKey     :: !String -> !(SigningKeyFile In) -> Action
  -- | 'DefineSigningKey' is just a 'Map.insert' on the state variable.
  DefineSigningKey   :: !String -> !(SigningKey PaymentKey) -> Action
  -- | 'AddFund' is mostly a wrapper around 'walletRefInsertFund' from
  -- "Cardano.Benchmarking.Wallet" which in turn is just 'modifyMVar'
  -- around insertion using "Cardano.TxGenerator.FundQueue" ops.
  AddFund            :: !AnyCardanoEra -> !String -> !TxIn -> !Lovelace -> !String -> Action
  -- | 'WaitBenchmark' signifies a 'waitCatch' on the
  -- 'AsyncBenchmarkControl' associated with the ID and also folds
  -- tracers into the completion. 
  WaitBenchmark      :: !String -> Action
  -- | 'Submit' mostly wraps 'benchmarkTxStream' from
  -- "Cardano.Benchmarking.Script.Core" which in turn wraps
  -- 'walletBenchmark' from "Cardano.Benchmarking.GeneratorTx" which
  -- in turn wraps 'txSubmissionClient' from
  -- "Cardano.Benchmarking.GeneratorTx.SubmissionClient", and
  -- functions local to that like 'requestTxs'.
  Submit             :: !AnyCardanoEra -> !SubmitMode -> !TxGenTxParams -> !Generator -> Action
  -- | 'CancelBenchmark' wraps a callback from the 
  -- 'AsyncBenchmarkControl' type, which is a shutdown action.
  CancelBenchmark    :: !String -> Action
  -- | 'Reserved' just emits an error and is a placeholder that helps
  -- with testing and quick fixes.
  Reserved           :: [String] -> Action
  -- 'WaitForEra' loops doing delays/sleeps until the current era matches.
  WaitForEra         :: !AnyCardanoEra -> Action
  -- | 'SetProtocolParameters' has one option to read from a file and
  -- another to pass directly and just sets a state variable.
  SetProtocolParameters :: ProtocolParametersSource -> Action
  -- | 'LogMsg' logs its message calling 'traceDebug' i.e. via the tracer.
  LogMsg             :: !Text -> Action
  deriving (Show, Eq)
deriving instance Generic Action

data Generator where
  SecureGenesis :: !String -> !String -> !String -> Generator -- 0 to N
  Split :: !String -> !PayMode -> !PayMode -> [ Lovelace ] -> Generator
  SplitN :: !String -> !PayMode -> !Int -> Generator            -- 1 to N
  NtoM  :: !String -> !PayMode -> !NumberOfInputsPerTx -> !NumberOfOutputsPerTx
        -> !(Maybe Int) -> Maybe String -> Generator
  Sequence :: [Generator] -> Generator
  Cycle :: !Generator -> Generator
  Take :: !Int -> !Generator -> Generator
  RoundRobin :: [Generator] -> Generator
  OneOf :: [(Generator, Double)] -> Generator
  deriving (Show, Eq)
deriving instance Generic Generator

data ProtocolParametersSource where
  QueryLocalNode :: ProtocolParametersSource
  UseLocalProtocolFile :: !FilePath -> ProtocolParametersSource
  deriving (Show, Eq)
deriving instance Generic ProtocolParametersSource

type TargetNodes = NonEmpty NodeIPv4Address

data SubmitMode where
  LocalSocket :: SubmitMode
  Benchmark   :: !TargetNodes -> !String -> !TPSRate -> !NumberOfTxs -> SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  NodeToNode  :: NonEmpty NodeIPv4Address -> SubmitMode --deprecated
  deriving (Show, Eq)
deriving instance Generic SubmitMode

data PayMode where
  PayToAddr :: !String -> !String -> PayMode
  PayToScript :: !ScriptSpec -> !String -> PayMode
  deriving (Show, Eq)
deriving instance Generic PayMode

data ScriptBudget where
  StaticScriptBudget :: !FilePath -> !FilePath -> !ExecutionUnits -> !Bool -> ScriptBudget
  AutoScript :: !FilePath -> !Int -> ScriptBudget
  deriving (Show, Eq)
deriving instance Generic ScriptBudget

data ScriptSpec = ScriptSpec
  {
    scriptSpecFile :: !(Either String FilePath)
  , scriptSpecBudget :: !ScriptBudget
  , scriptSpecPlutusType :: !TxGenPlutusType
  }
  deriving (Show, Eq)
deriving instance Generic ScriptSpec

newtype TxList era = TxList [Tx era]

data ProtocolParameterMode where
  ProtocolParameterQuery :: ProtocolParameterMode
  ProtocolParameterLocal :: ProtocolParameters -> ProtocolParameterMode
