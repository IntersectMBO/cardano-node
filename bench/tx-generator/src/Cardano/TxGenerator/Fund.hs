{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.TxGenerator.Fund
    ( Fund(..)
    , FundInEra(..)
    , getFundTxIn
    , getFundKey
    , getFundLovelace
    , getFundWitness
    )
    where

import           Data.Function (on)

import           Cardano.Api as Api


-- | Outputs that are available for spending.
-- When building a new transaction, they provide the TxIn parts.
data FundInEra era = FundInEra {
    _fundTxIn       :: !TxIn
  , _fundWitness    :: Witness WitCtxTxIn era
  , _fundVal        :: !(TxOutValue era)
  , _fundSigningKey :: !(Maybe (SigningKey PaymentKey))
  }
  deriving (Show)

newtype Fund = Fund {unFund :: InAnyCardanoEra FundInEra}

instance Eq Fund where
    (==) = (==) `on` getFundTxIn

instance Ord Fund where
    compare = compare `on` getFundTxIn

instance Show Fund where
  show (Fund (InAnyCardanoEra _ f)) = show f


getFundTxIn :: Fund -> TxIn
getFundTxIn (Fund (InAnyCardanoEra _ a)) = _fundTxIn a

getFundKey :: Fund -> Maybe (SigningKey PaymentKey)
getFundKey (Fund (InAnyCardanoEra _ a)) = _fundSigningKey a

getFundLovelace :: Fund -> Lovelace
getFundLovelace (Fund (InAnyCardanoEra _ a)) = case _fundVal a of
  TxOutAdaOnly _era l -> l
  TxOutValue _era v -> selectLovelace v

-- TODO: facilitate casting KeyWitnesses between eras -- Note [Era transitions]
getFundWitness :: forall era. IsShelleyBasedEra era => Fund -> Witness WitCtxTxIn era
getFundWitness fund = case (cardanoEra @era, fund) of
  (ByronEra   , Fund (InAnyCardanoEra ByronEra   a)) -> _fundWitness a
  (ShelleyEra , Fund (InAnyCardanoEra ShelleyEra a)) -> _fundWitness a
  (AllegraEra , Fund (InAnyCardanoEra AllegraEra a)) -> _fundWitness a
  (MaryEra    , Fund (InAnyCardanoEra MaryEra    a)) -> _fundWitness a
  (AlonzoEra  , Fund (InAnyCardanoEra AlonzoEra  a)) -> _fundWitness a
  (BabbageEra , Fund (InAnyCardanoEra BabbageEra a)) -> _fundWitness a
  _                                                  -> error "getFundWitness: era mismatch"

{-
Note [Era transitions]
~~~~~~~~~~~~~~~~~~~~~~
getFundWitness *must* be a partial function since it makes no sense to spend funds in an earlier era that doesn't
support a specific feature). Currently, it's implemented here simply by enforcing via the type system a fund witness
only exists in the same era as the fund.
However, that is too constrained: this approach rules out era transitions for transactions, which is 1) a valid case
and 2) a potential use case for benchmarking.
-}
