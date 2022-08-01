{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Types
where

import           Prelude
import           GHC.Generics

import           Data.List.NonEmpty

import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)
import           Cardano.Api (AnyCardanoEra, ExecutionUnits, Lovelace, ScriptData, ScriptRedeemer, TextEnvelope, TxIn)


import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Types (TPSRate, NodeIPv4Address)

data Action where
  Set                :: !SetKeyVal -> Action
--  Declare            :: SetKeyVal   -> Action --declare (once): error if key was set before
  InitWallet         :: !WalletName -> Action
  StartProtocol      :: !FilePath -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  DefineSigningKey   :: !KeyName -> !TextEnvelope -> Action
  AddFund            :: !AnyCardanoEra -> !WalletName -> !TxIn -> !Lovelace -> !KeyName -> Action
  ImportGenesisFund  :: !AnyCardanoEra -> !WalletName -> !SubmitMode -> !KeyName -> !KeyName -> Action
  CreateChange       :: !AnyCardanoEra -> !WalletName -> !WalletName -> !SubmitMode -> !PayMode -> !Lovelace -> !Int -> Action
  RunBenchmark       :: !AnyCardanoEra -> !WalletName -> !SubmitMode -> !ThreadName -> !RunBenchmarkAux -> Maybe WalletName -> !TPSRate -> Action
  WaitBenchmark      :: !ThreadName -> Action
  CancelBenchmark    :: !ThreadName -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyCardanoEra -> Action
  SetProtocolParameters :: ProtocolParametersSource -> Action
  deriving (Show, Eq)
deriving instance Generic Action

data ProtocolParametersSource where
  QueryLocalNode :: ProtocolParametersSource
  UseLocalProtocolFile :: !FilePath -> ProtocolParametersSource
  deriving (Show, Eq)
deriving instance Generic ProtocolParametersSource

data SubmitMode where
  LocalSocket :: SubmitMode
  NodeToNode  :: NonEmpty NodeIPv4Address -> SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  deriving (Show, Eq)
deriving instance Generic SubmitMode

data PayMode where
  PayToAddr :: !KeyName -> PayMode
  PayToCollateral :: !KeyName  -> PayMode
  PayToScript :: !ScriptSpec -> PayMode
  deriving (Show, Eq)
deriving instance Generic PayMode

data ScriptBudget where
  StaticScriptBudget :: !ScriptData -> !ScriptRedeemer -> !ExecutionUnits -> ScriptBudget
  CheckScriptBudget  :: !ScriptData -> !ScriptRedeemer -> !ExecutionUnits -> ScriptBudget
  AutoScript :: ScriptBudget --todo: add fraction of total available budget to use (==2 with 2 inputs !)
  deriving (Show, Eq)
deriving instance Generic ScriptBudget

data ScriptSpec = ScriptSpec
  {
    scriptSpecFile :: !FilePath
  , scriptSpecBudget :: !ScriptBudget
  }
  deriving (Show, Eq)
deriving instance Generic ScriptSpec

data RunBenchmarkAux = RunBenchmarkAux {
    auxTxCount :: Int
  , auxFee :: Lovelace
  , auxOutputsPerTx :: Int
  , auxInputsPerTx :: Int
  , auxInputs :: Int
  , auxOutputs ::Int
  , auxMinValuePerUTxO :: Lovelace
  }
  deriving (Show, Eq)
deriving instance Generic RunBenchmarkAux
