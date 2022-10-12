{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Store
where

import           Prelude

import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH (deriveGShow)

import           Cardano.Api as Cardano (Tx)
import           Cardano.Api.Shelley as Cardano (ProtocolParameters)

import           Cardano.Benchmarking.OuroborosImports as Cardano (PaymentKey,
                   SigningKey)

import           Cardano.Benchmarking.GeneratorTx as Core (AsyncBenchmarkControl)
import           Cardano.Benchmarking.Wallet as Wallet

data Store v where
  KeyName      :: !String -> Store (SigningKey PaymentKey)
  ThreadName   :: !String -> Store AsyncBenchmarkControl
  WalletName   :: !String -> Store WalletRef

type KeyName      = Store (SigningKey PaymentKey)
type ThreadName   = Store AsyncBenchmarkControl
type WalletName   = Store WalletRef

newtype TxList era = TxList [Tx era]

data ProtocolParameterMode where
  ProtocolParameterQuery :: ProtocolParameterMode
  ProtocolParameterLocal :: ProtocolParameters -> ProtocolParameterMode

-- Remember when debugging at 4:00AM :
-- TH-Haskell is imperative: It breaks up Main into smaller binding groups!
-- This means declarations below a splice are not visible above.
-- The order of splices & declarations matters.

deriveGEq ''Store
deriveGCompare ''Store
deriveGShow ''Store
deriveArgDict ''Store

deriving instance Show (Store v)
deriving instance Eq (Store x)
