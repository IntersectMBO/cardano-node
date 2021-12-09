module Cardano.Benchmarking.Script.Action
where

import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..))

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Core
import           Cardano.Benchmarking.Script.Types

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> (Identity val)) -> set (User key) val
  SetProtocolParameters p -> setProtocolParameters p
  StartProtocol filePath -> startProtocol filePath
  ReadSigningKey name filePath -> readSigningKey name filePath
  DefineSigningKey name descr -> defineSigningKey name descr
  AddFund txIn lovelace keyName -> addFund txIn lovelace keyName
  SecureGenesisFund fundName fundKey genesisKey -> secureGenesisFund fundName fundKey genesisKey
  SplitFund newFunds newKey sourceFund -> splitFund  newFunds newKey sourceFund
  SplitFundToList fundList destKey sourceFund -> splitFundToList fundList destKey sourceFund
  Delay t -> delay t
  PrepareTxList name key fund -> prepareTxList name key fund
  AsyncBenchmark thread txs tps -> asyncBenchmark thread txs tps
  ImportGenesisFund submitMode genesisKey fundKey -> importGenesisFund submitMode genesisKey fundKey
  CreateChange payMode submitMode value count -> createChange payMode submitMode value count
  RunBenchmark submitMode spendMode thread count tps -> runBenchmark submitMode spendMode thread count tps
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  Reserved options -> reserved options
