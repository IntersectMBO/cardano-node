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

data Action where
  SetNetworkId       :: !NetworkId -> Action
  SetSocketPath      :: !FilePath -> Action
  InitWallet         :: !String -> Action
  StartProtocol      :: !FilePath -> !(Maybe FilePath) -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !String -> !(SigningKeyFile 'In) -> Action
  DefineSigningKey   :: !String -> !(SigningKey PaymentKey) -> Action
  AddFund            :: !AnyCardanoEra -> !String -> !TxIn -> !Lovelace -> !String -> Action
  WaitBenchmark      :: !String -> Action
  Submit             :: !AnyCardanoEra -> !SubmitMode -> !TxGenTxParams -> !Generator -> Action
  CancelBenchmark    :: !String -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyCardanoEra -> Action
  SetProtocolParameters :: ProtocolParametersSource -> Action
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
