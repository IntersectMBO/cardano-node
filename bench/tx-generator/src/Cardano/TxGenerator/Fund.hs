{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Cardano.TxGenerator.Fund
Description : A type for funds to build transactions with.
-}
module Cardano.TxGenerator.Fund
    (
    -- * Types
    -- $Types
      Fund(..)
    , FundInEra(..)
    -- * Accessors
    -- $Accessors
    , getFundTxIn
    , getFundKey
    , getFundCoin
    , getFundWitness
    )
    where

import           Cardano.Api as Api
import           Cardano.Api.Experimental (AnyWitness (..), IsEra, obtainCommonConstraints, useEra)

import qualified Cardano.Ledger.Coin as L

import           Data.Function (on)


-- $Types
--
-- Outputs that are available for spending.
-- When building a new transaction, they provide the `TxIn` parts.

-- | `FundInEra` has field naming conventions suggesting anticipated
-- use of lenses.
data FundInEra era = FundInEra {
    _fundTxIn       :: !TxIn
  , _fundWitness    :: AnyWitness era
  , _fundVal        :: !(TxOutValue era)
  , _fundSigningKey :: !(Maybe (SigningKey PaymentKey))
  }
  deriving (Show)

-- | `InAnyCardanoEra` helps form heterogenous collections wrt. eras.
newtype Fund = Fund {unFund :: InAnyCardanoEra FundInEra}

instance Eq Fund where
    (==) = (==) `on` getFundTxIn

instance Ord Fund where
    compare = compare `on` getFundTxIn

instance Show Fund where
  show (Fund (InAnyCardanoEra _ f)) = show f


-- $Accessors
--
-- Accessors for the various fields of `FundInEra` starting from a
-- `Fund`.

-- | The `TxIn` is only a `TxId` and `TxIx` pair, and so era-independent.
getFundTxIn :: Fund -> TxIn
getFundTxIn (Fund (InAnyCardanoEra _ a)) = _fundTxIn a

-- | Signing keys are optional as far as funds go.
getFundKey :: Fund -> Maybe (SigningKey PaymentKey)
getFundKey (Fund (InAnyCardanoEra _ a)) = _fundSigningKey a

getFundCoin :: Fund -> L.Coin
getFundCoin (Fund (InAnyCardanoEra _ a)) = case _fundVal a of
  TxOutValueByron l -> l
  TxOutValueShelleyBased era v -> selectLovelace $ Api.fromLedgerValue era v

-- TODO: facilitate casting KeyWitnesses between eras -- Note [Era transitions]
-- | The `Fund` alternative is checked against `cardanoEra`, but
-- `getFundWitness` otherwise wraps `_fundWitness`.
getFundWitness :: forall era. IsEra era => Fund -> AnyWitness era
getFundWitness fund = obtainCommonConstraints (useEra @era) $ case (cardanoEra @era, fund) of
  (ConwayEra  , Fund (InAnyCardanoEra ConwayEra  a)) -> _fundWitness a
  (DijkstraEra, Fund (InAnyCardanoEra DijkstraEra a)) -> _fundWitness a
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
