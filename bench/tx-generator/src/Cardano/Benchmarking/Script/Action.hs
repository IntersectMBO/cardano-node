module Cardano.Benchmarking.Script.Action
where

import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..))
import qualified Data.Text as Text (unpack)

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
  StartProtocol configFile cardanoTracerSocket -> startProtocol configFile cardanoTracerSocket
  ReadSigningKey name filePath -> readSigningKey name filePath
  DefineSigningKey name descr -> defineSigningKey name descr
  AddFund era wallet txIn lovelace keyName -> addFund era wallet txIn lovelace keyName
  Delay t -> delay t
  Submit era submitMode txParams generator -> submitAction era submitMode generator txParams
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  LogMsg txt -> traceDebug $ Text.unpack txt
  Reserved options -> reserved options
