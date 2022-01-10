{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Store
where

import           Prelude

import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH (deriveGShow)

import           Cardano.Api as Cardano (Tx)
import           Cardano.Api.Shelley as Cardano (ProtocolParameters)
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)

import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.OuroborosImports as Cardano
                    ( LoggingLayer, ShelleyGenesis, StandardShelley
                    , SigningKey, PaymentKey)

import           Cardano.Benchmarking.GeneratorTx as Core (AsyncBenchmarkControl)
import qualified Cardano.Benchmarking.GeneratorTx.Tx as Core (Fund)
import           Cardano.Benchmarking.Tracer as Core (BenchTracers)
import           Cardano.Benchmarking.Wallet as Wallet

type Fund = (Core.Fund, SigningKey PaymentKey)

data Store v where
  User         :: Setters.Tag x -> Store x
  GlobalWallet :: Store WalletRef
  LoggingLayer :: Store LoggingLayer
  Protocol     :: Store SomeConsensusProtocol
  BenchTracers :: Store Core.BenchTracers
  Genesis      :: Store (ShelleyGenesis StandardShelley)
  Named        :: Name x -> Store x
  ProtocolParameterMode :: Store ProtocolParameterMode

data Name x where
  KeyName      :: !String -> Name (SigningKey PaymentKey)
  ThreadName   :: !String -> Name AsyncBenchmarkControl

type KeyName      = Name (SigningKey PaymentKey)
type ThreadName   = Name AsyncBenchmarkControl

newtype TxList era = TxList [Tx era]

data ProtocolParameterMode where
  ProtocolParameterQuery :: ProtocolParameterMode
  ProtocolParameterLocal :: ProtocolParameters -> ProtocolParameterMode

-- Remember when debugging at 4:00AM :
-- TH-Haskell is imperative: It breaks up Main into smaller binding groups!
-- This means declarations below a splice are not visible above.
-- The order of splices & declarations matters.

deriveGEq ''Name
deriveGCompare ''Name
deriveGShow ''Name
deriveArgDict ''Name
deriving instance Show (Name x)
deriving instance Eq (Name x)

deriveGEq ''Store
deriveGCompare ''Store
deriveGShow ''Store
deriveArgDict ''Store

deriving instance Show (Store v)
deriving instance Eq (Store x)
