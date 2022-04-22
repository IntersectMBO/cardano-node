module Cardano.Benchmarking.Script.Action
where

import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..))

import           Cardano.Benchmarking.Script.Core
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.NodeConfig (startProtocol)
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Types

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> (Identity val)) -> set (User key) val
  InitWallet name -> initWallet name
  SetProtocolParameters p -> setProtocolParameters p
  StartProtocol filePath -> startProtocol filePath
  ReadSigningKey name filePath -> readSigningKey name filePath
  DefineSigningKey name descr -> defineSigningKey name descr
  AddFund era wallet txIn lovelace keyName -> addFund era wallet txIn lovelace keyName
  Delay t -> delay t
  ImportGenesisFund era wallet submitMode genesisKey fundKey -> importGenesisFund era wallet submitMode genesisKey fundKey
  CreateChange era sourceWallet dstWallet payMode submitMode value count -> createChange era sourceWallet dstWallet payMode submitMode value count
  RunBenchmark era sourceWallet submitMode spendMode thread auxArgs tps -> runBenchmark era sourceWallet submitMode spendMode thread auxArgs tps
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  Reserved options -> reserved options
