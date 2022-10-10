{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Types (
        Generator(Cycle, NtoM, OneOf, RoundRobin, SecureGenesis,
                Sequence, Split, SplitN, Take)
        , PayMode(PayToAddr, PayToScript)
        , ProtocolParametersSource(QueryLocalNode, UseLocalProtocolFile)
        , ScriptBudget(AutoScript, CheckScriptBudget, StaticScriptBudget)
        , ScriptSpec(ScriptSpec, scriptSpecFile, scriptSpecBudget)
        , SubmitMode(Benchmark, DiscardTX, DumpToFile, LocalSocket,
                NodeToNode)
        , TargetNodes
) where

import           GHC.Generics
import           Prelude

import           Data.List.NonEmpty

import           Cardano.Api (ExecutionUnits, Lovelace,
                   ScriptData, ScriptRedeemer)
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)

import           Cardano.TxGenerator.Types

import           Cardano.Benchmarking.Script.Store

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
