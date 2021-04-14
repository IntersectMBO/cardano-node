{- HLINT ignore "Use record patterns" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.CliArgsScript
  (
    GeneratorCmd
  , parseGeneratorCmd
  , runPlainOldCliScript
  , runEraTransitionTest
  ) where

import           Prelude (error)
import           Data.Text (unpack)
import qualified Data.List.NonEmpty as NE

import           Control.Tracer (traceWith)

import           Cardano.Api
import           Cardano.Prelude hiding (option)

import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Benchmarking.GeneratorTx.Benchmark
import           Cardano.Benchmarking.GeneratorTx (readSigningKey)
import           Cardano.Benchmarking.DSL
import           Cardano.Benchmarking.Tracer
import           Cardano.Benchmarking.GeneratorTx.Error (TxGenError(..))
import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition (CliError(..), runBenchmarkScriptWith)

runPlainOldCliScript :: IOManager -> GeneratorCmd -> IO (Either CliError ())
runPlainOldCliScript
  iocp
  (GenerateCmd
     logConfigFile
     socketFile
     benchmarkEra
     cliPartialBenchmark
     fundOptions
  )
  = runExceptT $ runBenchmarkScriptWith iocp logConfigFile socketFile
      $ plainOldCliScript cliPartialBenchmark benchmarkEra fundOptions

runEraTransitionTest :: IOManager -> GeneratorCmd -> IO (Either CliError ())
runEraTransitionTest
  iocp
  (GenerateCmd
     logConfigFile
     socketFile
     _benchmarkEra
     cliPartialBenchmark
     fundOptions
  )
  = runExceptT $ runBenchmarkScriptWith iocp logConfigFile socketFile
      $ eraTransitionTest cliPartialBenchmark fundOptions

plainOldCliScript :: PartialBenchmark -> AnyCardanoEra -> GeneratorFunds -> BenchmarkScript ()
plainOldCliScript _ _ (FundsUtxo _ _ _) _ = error "plainOldCliScript FundsUtxo not supported"
plainOldCliScript _ _ (FundsSplitUtxo _ _) _ = error "plainOldCliScript FundsSplitUtxo not supported"
plainOldCliScript cliPartialBenchmark benchmarkEra (FundsGenesis keyFile) (tracers, dslSet) = do
  case benchmarkEra of
      AnyCardanoEra AlonzoEra  -> error "AlonzoEra not supported"
      AnyCardanoEra ByronEra   -> error "ByronEra not supported"
      AnyCardanoEra ShelleyEra -> do
        myTracer "POScript :: ShelleyEra"
        genericScript $ getDSL dslSet ShelleyEra
      AnyCardanoEra AllegraEra -> do
        myTracer "POScript  :: AllegraEra"
        genericScript $ getDSL dslSet AllegraEra
      AnyCardanoEra MaryEra    -> do
        myTracer "POScript  :: MaryEra"
        genericScript $ getDSL dslSet MaryEra
 where
  myTracer msg = liftIO $ traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubDebug msg
  genericScript :: forall era. DSL era -> ExceptT TxGenError IO ()
  genericScript (DSL{..}) = do
    b <- case mkBenchmark (defaultBenchmark <> cliPartialBenchmark) of
       Left e -> error $ "Incomplete benchmark spec (is defaultBenchmark complete?):  " <> unpack e
       Right b -> return b
    let
      fees = bTxFee b
      coolDownDelay = bInitCooldown b

    key <- readSigningKey keyFile
    let
      globalOutAddr = keyAddress key    -- A globalOutAddr is used for all TXs in the generator.

    myTracer "POScript: securing funds"
    firstUTxO <- secureGenesisFund fees (bInitialTTL b) key globalOutAddr

    myTracer $ "******* Tx generator: waiting for first UTxO" ++ show coolDownDelay ++ "s *******"
    coolDown coolDownDelay
    funds <- splitFunds fees (bTxCount b) (bTxFanIn b) key globalOutAddr firstUTxO
    myTracer $ "******* Tx generator: waiting for funds split" ++ show coolDownDelay ++ "s *******"
    coolDown coolDownDelay

    myTracer "POScript: pre-computing transactions"
    finalTransactions <- txGenerator (bTxFee b) (bTxCount b) (bTxFanIn b) (bTxFanOut b) (bTxExtraPayload b)
      globalOutAddr key (fromIntegral $ NE.length $ bTargets b) funds

    myTracer "POScript: sending transactions"
    runBenchmark (bTargets b) (bTps b) (bErrorPolicy b) finalTransactions

eraTransitionTest :: PartialBenchmark -> GeneratorFunds -> BenchmarkScript ()
eraTransitionTest _ (FundsUtxo _ _ _) _ = error "eraTransitionTest FundsUtxo not supported"
eraTransitionTest _ (FundsSplitUtxo _ _) _ = error "eraTransitionTest FundsSplitUtxo not supported"
eraTransitionTest cliPartialBenchmark (FundsGenesis keyFile) (tracers, dslSet) = do
  b <- case mkBenchmark (defaultBenchmark <> cliPartialBenchmark) of
     Left e -> error $ "Incomplete benchmark spec (is defaultBenchmark complete?):  " <> unpack e
     Right b -> return b
  let
    fees = bTxFee b
    coolDownDelay = bInitCooldown b

  key <- readSigningKey keyFile
  let
    addr_shelley :: AddressInEra ShelleyEra
    addr_shelley = keyAddress key

    addr_mary :: AddressInEra MaryEra
    addr_mary = keyAddress_mary key

  myTracer "POScript: securing funds"

  firstUTxO <- secureGenesisFund fees (bInitialTTL b) key addr_shelley

  myTracer $ "******* Tx generator: waiting for first UTxO" ++ show coolDownDelay ++ "s *******"
  coolDown coolDownDelay
  [fund1,fund2] <- splitFunds fees 2 (bTxFanIn b) key addr_shelley firstUTxO
  funds_shelley <- splitFunds fees (bTxCount b) (bTxFanIn b) key addr_shelley fund1
  funds_mary   <- splitFunds fees (bTxCount b) (bTxFanIn b) key addr_shelley fund2

  myTracer $ "******* Tx generator: waiting for funds split" ++ show coolDownDelay ++ "s *******"
  coolDown coolDownDelay

  myTracer "POScript: pre-computing transactions Shelley"
  tx1 <- txGenerator (bTxFee b) (bTxCount b) (bTxFanIn b) (bTxFanOut b) (bTxExtraPayload b)
             addr_shelley key (fromIntegral $ NE.length $ bTargets b) funds_shelley
  myTracer "POScript: sending transactions Shelley"
  runBenchmark (bTargets b) (bTps b) (bErrorPolicy b) tx1

  myTracer "POScript: pre-computing transactions Mary"
  (tx2 :: [Tx MaryEra]) <- txGenerator_mary  (bTxFee b) (bTxCount b) (bTxFanIn b) (bTxFanOut b) (bTxExtraPayload b)
                              addr_mary key (fromIntegral $ NE.length $ bTargets b) funds_mary
  myTracer "POScript: sending transactions Mary"
  runBenchmark_mary (bTargets b) (bTps b) (bErrorPolicy b) tx2
 where
  DSL {..} = getDSL dslSet ShelleyEra
  DSL {
    runBenchmark = runBenchmark_mary
   , txGenerator  = txGenerator_mary
   , keyAddress   = keyAddress_mary
   } =getDSL dslSet MaryEra

  myTracer msg = liftIO $ traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubDebug msg
