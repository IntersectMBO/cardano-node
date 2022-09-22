{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Benchmarking.FundSet
where
import           Prelude

import           Cardano.Api as Api

import           Cardano.Benchmarking.Fifo as Fifo

-- Outputs that are available for spending.
-- When building a new TX they provide the TxIn parts.

data FundInEra era = FundInEra {
    _fundTxIn :: !TxIn
  , _fundWitness :: Witness WitCtxTxIn era
  , _fundVal  :: !(TxOutValue era)
  , _fundSigningKey :: !(Maybe (SigningKey PaymentKey))
  } deriving (Show)

newtype Fund = Fund {unFund :: InAnyCardanoEra FundInEra}

type FundSet = Fifo Fund

type FundSource m = m (Either String [Fund])
type FundToStore m = Fund -> m ()
type FundToStoreList m = [Fund] -> m ()

getFundTxIn :: Fund -> TxIn
getFundTxIn (Fund (InAnyCardanoEra _ a)) = _fundTxIn a

getFundKey :: Fund -> Maybe (SigningKey PaymentKey)
getFundKey (Fund (InAnyCardanoEra _ a)) = _fundSigningKey a

getFundLovelace :: Fund -> Lovelace
getFundLovelace (Fund (InAnyCardanoEra _ a)) = case _fundVal a of
  TxOutAdaOnly _era l -> l
  TxOutValue _era v -> selectLovelace v

-- This effectively rules out era-transitions for transactions !
-- This is not what we want !!
getFundWitness :: forall era. IsShelleyBasedEra era => Fund -> Witness WitCtxTxIn era
getFundWitness fund = case (cardanoEra @era, fund) of
  (ByronEra   , Fund (InAnyCardanoEra ByronEra   a)) -> _fundWitness a
  (ShelleyEra , Fund (InAnyCardanoEra ShelleyEra a)) -> _fundWitness a
  (AllegraEra , Fund (InAnyCardanoEra AllegraEra a)) -> _fundWitness a
  (MaryEra    , Fund (InAnyCardanoEra MaryEra    a)) -> _fundWitness a
  (AlonzoEra  , Fund (InAnyCardanoEra AlonzoEra  a)) -> _fundWitness a
  (BabbageEra , Fund (InAnyCardanoEra BabbageEra a)) -> _fundWitness a
-- This effectively rules out era-transitions for transactions !
-- This is not what we want !!
-- It should be possible to cast KeyWitnesses from one era to an other !
  (_ , _) -> error "getFundWitness: era mismatch"

instance Show Fund where
  show (Fund (InAnyCardanoEra _ f)) = show f

-- TxIn/fundTxOut is the primary key.
-- There must be no two entries for the same TxIn !.

instance Eq Fund where
  (==) a b = getFundTxIn a == getFundTxIn b

instance Ord Fund where
  compare a b = compare (getFundTxIn a) (getFundTxIn b)

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
