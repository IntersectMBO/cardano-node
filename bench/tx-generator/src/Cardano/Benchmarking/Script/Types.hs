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

module Cardano.Benchmarking.Script.Types
where

import           GHC.Generics
import           Prelude

import           Data.Function (on)
import           Data.List.NonEmpty
import           Data.Text (Text)

import           Cardano.Api (AnyCardanoEra, ExecutionUnits, Lovelace, PaymentKey, ScriptData,
                   ScriptRedeemer, SigningKey, TxIn, serialiseToTextEnvelope)
import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)

import           Cardano.TxGenerator.Types

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store

-- FIXME: temporary workaround instance until Action ADT is refactored
instance Eq (SigningKey PaymentKey) where
  (==) = (==) `on` serialiseToTextEnvelope Nothing

data Action where
  Set                :: !SetKeyVal -> Action
--  Declare            :: SetKeyVal   -> Action --declare (once): error if key was set before
  InitWallet         :: !WalletName -> Action
  StartProtocol      :: !FilePath -> !(Maybe FilePath) -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  DefineSigningKey   :: !KeyName -> !(SigningKey PaymentKey) -> Action
  AddFund            :: !AnyCardanoEra -> !WalletName -> !TxIn -> !Lovelace -> !KeyName -> Action
  WaitBenchmark      :: !ThreadName -> Action
  Submit             :: !AnyCardanoEra -> !SubmitMode -> !TxGenTxParams -> !Generator -> Action
  CancelBenchmark    :: !ThreadName -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyCardanoEra -> Action
  SetProtocolParameters :: ProtocolParametersSource -> Action
  LogMsg             :: !Text -> Action
  deriving (Show, Eq)
deriving instance Generic Action

data Generator where
  SecureGenesis :: !WalletName -> !KeyName -> !KeyName -> Generator -- 0 to N
  Split :: !WalletName -> !PayMode -> !PayMode -> [ Lovelace ] -> Generator
  SplitN :: !WalletName -> !PayMode -> !Int -> Generator            -- 1 to N
  NtoM  :: !WalletName -> !PayMode -> !NumberOfInputsPerTx -> !NumberOfOutputsPerTx
        -> !(Maybe Int) -> Maybe WalletName -> Generator
  Sequence :: [Generator] -> Generator
  Cycle :: !Generator -> Generator
  Take :: !Int -> !Generator -> Generator
  RoundRobin :: [Generator] -> Generator
  OneOf :: [(Generator, Double)] -> Generator
--  AddLogMessages :: Text -> Text -> Generator -> Generator
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
  Benchmark   :: !TargetNodes -> !ThreadName -> !TPSRate -> !NumberOfTxs -> SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  NodeToNode  :: NonEmpty NodeIPv4Address -> SubmitMode --deprecated
  deriving (Show, Eq)
deriving instance Generic SubmitMode

data PayMode where
  PayToAddr :: !KeyName -> !WalletName -> PayMode
  PayToScript :: !ScriptSpec -> !WalletName -> PayMode
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
