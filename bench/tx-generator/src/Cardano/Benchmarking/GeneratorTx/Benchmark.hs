{- HLINT ignore "Move brackets to avoid $" -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Benchmark
  ( Benchmark(..)
  , GeneratorCmd(..)
  , GeneratorFunds(..)
  , PartialBenchmark(..)
  , defaultBenchmark
  , mkBenchmark
  , parseGeneratorCmd
  ) where

import           Cardano.Prelude hiding (TypeError)
import           Prelude (String)

import qualified Data.List.NonEmpty as NE
import           Data.Monoid.Generic
import           Options.Applicative as Opt

-- Node API imports
import           Cardano.Api
import           Cardano.CLI.Types (SigningKeyFile (..))

-- Node imports
import           Cardano.Node.NodeAddress
import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
import           Cardano.Benchmarking.Types

parseNumberOfTxs :: String -> String -> Parser NumberOfTxs
parseNumberOfTxs opt desc = NumberOfTxs <$> parseIntegral opt desc

parseNumberOfInputsPerTx :: String -> String -> Parser NumberOfInputsPerTx
parseNumberOfInputsPerTx opt desc = NumberOfInputsPerTx <$> parseIntegral opt desc

parseNumberOfOutputsPerTx :: String -> String -> Parser NumberOfOutputsPerTx
parseNumberOfOutputsPerTx opt desc = NumberOfOutputsPerTx <$> parseIntegral opt desc

parseTPSRate :: String -> String -> Parser TPSRate
parseTPSRate opt desc = TPSRate <$> parseDouble opt desc

parseInitCooldown :: String -> String -> Parser InitCooldown
parseInitCooldown opt desc = InitCooldown <$> parseIntegral opt desc

parseTxAdditionalSize :: String -> String -> Parser TxAdditionalSize
parseTxAdditionalSize opt desc = TxAdditionalSize <$> parseIntegral opt desc

-- | Specification for a benchmark run.
data Benchmark
  = Benchmark
      { bTargets        :: !(NonEmpty NodeIPv4Address)
      , bInitCooldown   :: !InitCooldown
      , bInitialTTL     :: !SlotNo
      , bTxCount        :: !NumberOfTxs
      , bTps            :: !TPSRate
      , bTxFanIn        :: !NumberOfInputsPerTx
      , bTxFanOut       :: !NumberOfOutputsPerTx
      , bTxFee          :: !Lovelace
      , bTxExtraPayload :: !TxAdditionalSize
      , bErrorPolicy    :: !SubmissionErrorPolicy
      }
  deriving stock (Generic, Show)
-- Warning:  make sure to maintain correspondence between the two data structures.
data PartialBenchmark
  = PartialBenchmark
      { pbTargets        :: !(Last (NonEmpty NodeIPv4Address))
      , pbInitCooldown   :: !(Last InitCooldown)
      , pbInitialTTL     :: !(Last SlotNo)
      , pbTxCount        :: !(Last NumberOfTxs)
      , pbTps            :: !(Last TPSRate)
      , pbTxFanIn        :: !(Last NumberOfInputsPerTx)
      , pbTxFanOut       :: !(Last NumberOfOutputsPerTx)
      , pbTxFee          :: !(Last Lovelace)
      , pbTxExtraPayload :: !(Last TxAdditionalSize)
      , pbErrorPolicy    :: !(Last SubmissionErrorPolicy)
      }
  deriving stock (Generic, Show)
  deriving Semigroup via GenericSemigroup PartialBenchmark
  deriving Monoid via GenericMonoid PartialBenchmark

parsePartialBenchmark :: Opt.Parser PartialBenchmark
parsePartialBenchmark =
  PartialBenchmark
    <$> lastly (NE.fromList <$> some (
            parseTargetNodeAddress
              "target-node"
              "IP address and port of the node transactions will be sent to."
          )
        )
    <*> (lastly $ parseInitCooldown
          "init-cooldown"
          "Delay between init and main submission phases.")
    <*> (lastly $ parseInitialTTL
          "initial-ttl"
          "Slot denoting TTL of the initial transactions.")
    <*> (lastly $ parseNumberOfTxs
         "num-of-txs"
         "Number of transactions generator will create.")
    <*> (lastly $ parseTPSRate
          "tps"
          "TPS (transaction per second) rate.")
    <*> (lastly $ parseNumberOfInputsPerTx
          "inputs-per-tx"
          "Number of inputs in each of transactions.")
    <*> (lastly $ parseNumberOfOutputsPerTx
          "outputs-per-tx"
          "Number of outputs in each of transactions.")
    <*> (lastly $ parseFeePerTx
          "tx-fee"
          "Fee per transaction, in Lovelaces.")
    <*> (lastly $ parseTxAdditionalSize
          "add-tx-size"
          "Additional size of transaction, in bytes.")
    <*> (lastly $ parseFlag'
          LogErrors FailOnError
          "fail-on-submission-errors"
          "Fail on submission thread errors, instead of logging them.")

defaultBenchmark :: PartialBenchmark
defaultBenchmark =
  PartialBenchmark
  { pbTargets        = mempty
  , pbInitCooldown   = pure 20
  , pbInitialTTL     = pure (SlotNo 100000000)
  , pbTxCount        = pure 1000
  , pbTps            = pure 10
  , pbTxFanIn        = pure 1
  , pbTxFanOut       = pure 1
  , pbTxFee          = pure 1000
  , pbTxExtraPayload = pure 100
  , pbErrorPolicy    = pure LogErrors
  }

-- This is called at the last stage of the Partial Options Monoid approach.
-- https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
mkBenchmark :: PartialBenchmark -> Either Text Benchmark
mkBenchmark PartialBenchmark{..} = do
  bTargets        <- mkComplete "bTargets       " pbTargets
  bInitCooldown   <- mkComplete "bInitCooldown  " pbInitCooldown
  bInitialTTL     <- mkComplete "bInitialTTL    " pbInitialTTL
  bTxCount        <- mkComplete "bTxCount       " pbTxCount
  bTps            <- mkComplete "bTps           " pbTps
  bTxFanIn        <- mkComplete "bTxFanIn       " pbTxFanIn
  bTxFanOut       <- mkComplete "bTxFanOut      " pbTxFanOut
  bTxFee          <- mkComplete "bTxFee         " pbTxFee
  bTxExtraPayload <- mkComplete "bTxExtraPayload" pbTxExtraPayload
  bErrorPolicy    <- mkComplete "bErrorPolicy"    pbErrorPolicy
  pure Benchmark{..}
 where
   -- | Return an error if the @Last@ option is incomplete.
   mkComplete :: Text -> Last a -> Either Text a
   mkComplete err (Last x) = maybe (Left err) Right x


data GeneratorCmd =
  GenerateCmd FilePath
              SocketPath
              AnyCardanoEra
              PartialBenchmark
              GeneratorFunds

defaultEra :: AnyCardanoEra
defaultEra = AnyCardanoEra ShelleyEra

parseGeneratorCmd :: Opt.Parser GeneratorCmd
parseGeneratorCmd =
  GenerateCmd
    <$> parseConfigFile
          "config"
          "Configuration file for the cardano-node"
    <*> parseSocketPath
          "socket-path"
          "Path to a cardano-node socket"
   <*> ( fromMaybe defaultEra <$>
         (
             eraFlag "shelley" ShelleyEra
         <|> eraFlag "mary"    MaryEra
         <|> eraFlag "allegra" AllegraEra
         )
       )
    <*> parsePartialBenchmark
    <*> parseGeneratorFunds
 where
   eraFlag name tag = flag Nothing (Just $ AnyCardanoEra tag)
                         (long name <> help ("Initialise Cardano in " ++ name ++" submode."))

data GeneratorFunds
  = FundsGenesis   SigningKeyFile
  | FundsUtxo      SigningKeyFile TxIn (TxOut CtxTx ShelleyEra)
  | FundsSplitUtxo SigningKeyFile FilePath
  deriving stock Show

parseGeneratorFunds :: Opt.Parser GeneratorFunds
parseGeneratorFunds =
  (FundsGenesis
    <$> parseSigningKeysFile
        "genesis-funds-key"
        "Genesis UTxO funds signing key.")
  <|>
  (FundsUtxo
    <$> parseSigningKeysFile
        "utxo-funds-key"
        "UTxO funds signing key."
    <*> pTxIn
    <*> pTxOut)
  <|>
  (FundsSplitUtxo
    <$> parseSigningKeysFile
        "split-utxo-funds-key"
        "UTxO funds signing key."
    <*> parseFilePath
        "split-utxo"
        "UTxO funds file.")
