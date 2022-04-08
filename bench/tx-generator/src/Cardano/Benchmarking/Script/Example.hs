module Cardano.Benchmarking.Script.Example
where

import           Prelude
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Dependent.Sum ((==>) )

import           Cardano.Api (AnyCardanoEra(..), CardanoEra(..), Quantity(..), ScriptData(..), SlotNo(..), quantityToLovelace )
import           Cardano.Api.Shelley (ExecutionUnits(..))
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Types

runTestScript :: IO (Either Error (), Env, ())
runTestScript = withIOManager $ runActionM (forM_ testScript action)

printJSON :: IO ()
printJSON = BSL.putStrLn $ prettyPrint testScript

txConfig :: [Action]
txConfig = map Set [
    TNumberOfInputsPerTx  ==> NumberOfInputsPerTx 1
  , TNumberOfOutputsPerTx ==> NumberOfOutputsPerTx 1
  , TNumberOfTxs          ==> NumberOfTxs 500
  , TTxAdditionalSize     ==> TxAdditionalSize 0
  , TFee                  ==> quantityToLovelace (Quantity 0)
  , TTTL                  ==> SlotNo 1000000
  ]

-- This is only for testing the Aeson instances.
-- This script is not functional in any way.
testScript :: [Action]
testScript =
  txConfig
  ++
  [
    StartProtocol "configuration/configuration-generator.yaml"
  , Set $ TEra ==> AnyCardanoEra MaryEra
  , Set $ TLocalSocket ==> "logs/sockets/1"
  , ReadSigningKey passPartout "configuration/genesis-shelley/utxo-keys/utxo1.skey"
  , Delay 10
  , Delay 10
  , WaitForEra $ AnyCardanoEra ByronEra
  , CancelBenchmark threadName
  , ImportGenesisFund globalWalletName DiscardTX passPartout passPartout
  , CreateChange globalWalletName globalWalletName  LocalSocket (PayToAddr passPartout) (quantityToLovelace 10000) 1000
  , RunBenchmark globalWalletName (DumpToFile "/tmp/tx-list.txt") SpendOutput (ThreadName "walletThread") (NumberOfTxs 1000) (TPSRate 10)
  , RunBenchmark globalWalletName (DumpToFile "/tmp/tx-list.txt") scriptDef (ThreadName "walletThread") (NumberOfTxs 1000) (TPSRate 10)
  , Reserved []
  ]
 where
  globalWalletName = WalletName "test_wallet"
  scriptDef = SpendScript "filePath" (StaticScriptBudget $ ExecutionUnits 70000000 70000000) (ScriptDataNumber 3) (ScriptDataNumber 6)
  passPartout = KeyName "pass-partout"

  threadName = ThreadName "thread1"
