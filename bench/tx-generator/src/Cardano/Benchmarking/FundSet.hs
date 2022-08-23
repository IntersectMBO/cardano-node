{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Benchmarking.FundSet
       ( module Cardano.Benchmarking.FundSet
       , module Cardano.TxGenerator.Fund
       )
      where

import           Prelude

import           Cardano.Api as Api

import           Cardano.Benchmarking.Fifo as Fifo
import           Cardano.TxGenerator.Fund


type FundSet = Fifo Fund

type FundSource m = m (Either String [Fund])
type FundToStore m = Fund -> m ()
type FundToStoreList m = [Fund] -> m ()


emptyFundSet :: FundSet
emptyFundSet = Fifo.emptyFifo

insertFund :: FundSet -> Fund -> FundSet
insertFund = Fifo.insert

liftAnyEra :: ( forall era. IsCardanoEra era => f1 era -> f2 era ) -> InAnyCardanoEra f1 -> InAnyCardanoEra f2
liftAnyEra f x = case x of
  InAnyCardanoEra ByronEra a   ->   InAnyCardanoEra ByronEra $ f a
  InAnyCardanoEra ShelleyEra a ->   InAnyCardanoEra ShelleyEra $ f a
  InAnyCardanoEra AllegraEra a ->   InAnyCardanoEra AllegraEra $ f a
  InAnyCardanoEra MaryEra a    ->   InAnyCardanoEra MaryEra $ f a
  InAnyCardanoEra AlonzoEra a  ->   InAnyCardanoEra AlonzoEra $ f a
  InAnyCardanoEra BabbageEra a  ->  InAnyCardanoEra BabbageEra $ f a

-- Todo: check sufficient funds and minimumValuePerUtxo
inputsToOutputsWithFee :: Lovelace -> Int -> [Lovelace] -> [Lovelace]
inputsToOutputsWithFee fee count inputs = map (quantityToLovelace . Quantity) outputs
  where
    (Quantity totalAvailable) = lovelaceToQuantity $ sum inputs - fee
    (out, rest) = divMod totalAvailable (fromIntegral count)
    outputs = (out + rest) : replicate (count-1) out
