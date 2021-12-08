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

import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)
import           Cardano.Api (AnyCardanoEra, ExecutionUnits, Lovelace, ScriptData, ScriptRedeemer, TextEnvelope, TxIn)

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Types (TPSRate, NumberOfTxs)

data Action where
  Set                :: !SetKeyVal -> Action
--  Declare            :: SetKeyVal   -> Action --declare (once): error if key was set before
  StartProtocol      :: !FilePath -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  DefineSigningKey   :: !KeyName -> !TextEnvelope -> Action
  AddFund            :: !TxIn -> !Lovelace -> !KeyName -> Action
  SecureGenesisFund  :: !FundName -> !KeyName -> !KeyName -> Action
  SplitFund          :: [FundName] -> !KeyName -> !FundName -> Action
  SplitFundToList    :: !FundListName -> !KeyName -> !FundName -> Action
  PrepareTxList      :: !TxListName -> !KeyName -> !FundListName -> Action
  AsyncBenchmark     :: !ThreadName -> !TxListName -> !TPSRate -> Action
  ImportGenesisFund  :: !SubmitMode -> !KeyName -> !KeyName -> Action
  CreateChange       :: !SubmitMode -> !PayMode -> !Lovelace -> !Int -> Action
  RunBenchmark       :: !SubmitMode -> !SpendMode -> !ThreadName -> !NumberOfTxs -> !TPSRate -> Action
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
  NodeToNode  :: SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  deriving (Show, Eq)
deriving instance Generic SubmitMode

data PayMode where
  PayToAddr :: !KeyName -> PayMode
  PayToCollateral :: !KeyName  -> PayMode
  PayToScript :: !FilePath -> !ScriptData -> PayMode
  deriving (Show, Eq)
deriving instance Generic PayMode

data SpendMode where
  SpendOutput :: SpendMode
  SpendScript :: !FilePath -> ScriptBudget -> !ScriptData -> !ScriptRedeemer -> SpendMode
  SpendAutoScript :: !FilePath -> SpendMode
  deriving (Show, Eq)
deriving instance Generic SpendMode

data ScriptBudget where
  StaticScriptBudget :: !ExecutionUnits -> ScriptBudget
  PreExecuteScript   :: ScriptBudget
  CheckScriptBudget  :: !ExecutionUnits -> ScriptBudget
  deriving (Show, Eq)
deriving instance Generic ScriptBudget
