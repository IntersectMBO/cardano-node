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
  InitWallet name -> initWallet name
  SetProtocolParameters p -> setProtocolParameters p
  StartProtocol filePath -> startProtocol filePath
  ReadSigningKey name filePath -> readSigningKey name filePath
  DefineSigningKey name descr -> defineSigningKey name descr
  AddFund wallet txIn lovelace keyName -> addFund wallet txIn lovelace keyName
  Delay t -> delay t
  ImportGenesisFund wallet submitMode genesisKey fundKey -> importGenesisFund wallet submitMode genesisKey fundKey
  CreateChange sourceWallet dstWallet payMode submitMode value count -> createChange sourceWallet dstWallet payMode submitMode value count
  RunBenchmark sourceWallet submitMode spendMode thread count tps -> runBenchmark sourceWallet submitMode spendMode thread count tps
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  Reserved options -> reserved options
