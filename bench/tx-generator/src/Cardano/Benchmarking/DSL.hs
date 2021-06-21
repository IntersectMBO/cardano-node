{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Benchmarking.DSL
where

import           Prelude (error)
import           Cardano.Prelude

import           Cardano.Api
import           Cardano.Benchmarking.Tracer
import           Cardano.Benchmarking.Types (SubmissionErrorPolicy, NodeIPv4Address)
import           Cardano.Benchmarking.GeneratorTx
import           Cardano.Benchmarking.GeneratorTx.Tx

type ScriptM a = ExceptT TxGenError IO a
type BenchmarkScript a = (BenchTracers, MonoDSLs) ->  ScriptM a

type MonoDSLs = (DSL ShelleyEra, DSL AllegraEra, DSL MaryEra)

getDSL :: MonoDSLs -> CardanoEra era -> DSL era
getDSL _         ByronEra = error "ByronEra not supported"
getDSL (x, _, _) ShelleyEra = x
getDSL (_, x, _) AllegraEra = x
getDSL (_, _, x) MaryEra    = x
getDSL _         AlonzoEra = error "AlonzoEra not supported" -- use json mode

type Fee = Lovelace
type TTL = SlotNo

type SecureGenesisFund era =
     Fee
  -> TTL
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> ScriptM Fund

type SplitFunds era =
     Lovelace
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Fund
  -> ScriptM [Fund]

-- txGenerator is basically pure except for logging.
type TxGenerator era =
     Lovelace
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> TxAdditionalSize
  -> AddressInEra era
  -> SigningKey PaymentKey
  -> Int
  -> [Fund]
  -> ScriptM [Tx era]

type RunBenchmark era =
     NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> [Tx era]
  -> ScriptM ()

type KeyAddress era = SigningKey PaymentKey -> AddressInEra era

data DSL era = DSL {
    keyAddress :: !(KeyAddress era)
  , secureGenesisFund :: !(SecureGenesisFund era)
  , splitFunds :: !(SplitFunds era)
  , txGenerator :: !(TxGenerator era)
  , runBenchmark :: !(RunBenchmark era)
  }

coolDown :: InitCooldown -> ScriptM ()
coolDown (InitCooldown t) = liftIO $ threadDelay $ 1000 * 1000 * t
