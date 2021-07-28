{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Benchmarking.Script.Setters
where

import           Prelude
import           GHC.Generics
import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.Dependent.Sum (DSum(..) , (==>) )
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH (deriveGShow)
import           Data.List.NonEmpty

import           Cardano.Api (Lovelace, SlotNo, AnyCardanoEra(..))

import           Cardano.Benchmarking.Types

-- Some boiler plate; ToDo may generate this.
data Tag v where
  TNumberOfInputsPerTx  :: Tag NumberOfInputsPerTx
  TNumberOfOutputsPerTx :: Tag NumberOfOutputsPerTx
  TNumberOfTxs          :: Tag NumberOfTxs
  TFee                  :: Tag Lovelace
  TMinValuePerUTxO      :: Tag Lovelace
  TTTL                  :: Tag SlotNo
  TTxAdditionalSize     :: Tag TxAdditionalSize
  TLocalSocket          :: Tag String
  TEra                  :: Tag AnyCardanoEra
  TTargets              :: Tag (NonEmpty NodeIPv4Address)

deriveGEq ''Tag
deriveGCompare ''Tag
deriveGShow ''Tag
deriveArgDict ''Tag

deriving instance Show (Tag v)
deriving instance Eq (Tag v)

data Sum where
  SNumberOfInputsPerTx  :: !NumberOfInputsPerTx  -> Sum
  SNumberOfOutputsPerTx :: !NumberOfOutputsPerTx -> Sum
  SNumberOfTxs          :: !NumberOfTxs          -> Sum
  SFee                  :: !Lovelace             -> Sum
  SMinValuePerUTxO      :: !Lovelace             -> Sum
  STTL                  :: !SlotNo               -> Sum
  STxAdditionalSize     :: !TxAdditionalSize     -> Sum
  SLocalSocket          :: !String               -> Sum
  SEra                  :: !AnyCardanoEra        -> Sum
  STargets              :: !(NonEmpty NodeIPv4Address) -> Sum
  deriving (Eq, Show, Generic)

taggedToSum :: Applicative f => DSum Tag f -> f Sum
taggedToSum x = case x of
  (TNumberOfInputsPerTx  :=> v) -> SNumberOfInputsPerTx  <$> v
  (TNumberOfOutputsPerTx :=> v) -> SNumberOfOutputsPerTx <$> v
  (TNumberOfTxs          :=> v) -> SNumberOfTxs          <$> v
  (TFee                  :=> v) -> SFee                  <$> v
  (TMinValuePerUTxO      :=> v) -> SMinValuePerUTxO      <$> v
  (TTTL                  :=> v) -> STTL                  <$> v
  (TTxAdditionalSize     :=> v) -> STxAdditionalSize     <$> v
  (TLocalSocket          :=> v) -> SLocalSocket          <$> v
  (TEra                  :=> v) -> SEra                  <$> v
  (TTargets              :=> v) -> STargets              <$> v

sumToTaggged :: Applicative f => Sum -> DSum Tag f
sumToTaggged x = case x of
  SNumberOfInputsPerTx  v -> TNumberOfInputsPerTx  ==> v
  SNumberOfOutputsPerTx v -> TNumberOfOutputsPerTx ==> v
  SNumberOfTxs          v -> TNumberOfTxs          ==> v
  SFee                  v -> TFee                  ==> v
  SMinValuePerUTxO      v -> TMinValuePerUTxO      ==> v
  STTL                  v -> TTTL                  ==> v
  STxAdditionalSize     v -> TTxAdditionalSize     ==> v
  SLocalSocket          v -> TLocalSocket          ==> v
  SEra                  v -> TEra                  ==> v
  STargets              v -> TTargets              ==> v
