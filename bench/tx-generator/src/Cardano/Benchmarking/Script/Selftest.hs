{-# LANGUAGE LambdaCase #-}
module Cardano.Benchmarking.Script.Selftest
where

import           Prelude

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Dependent.Sum ((==>))
import           Data.String

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)

import           Cardano.Api
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (prettyPrint)
import           Cardano.Benchmarking.Script.Env as Script
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Tracer (initDefaultTracers)
import           Cardano.Benchmarking.Types

import           Paths_tx_generator

runSelftest :: IOManager -> Maybe FilePath -> IO (Either Script.Error ())
runSelftest iom outFile = do
  protocolFile <-  getDataFileName "data/protocol-parameters.json"
  let
    submitMode = maybe DiscardTX DumpToFile outFile
    fullScript = do
        liftIO initDefaultTracers >>= set BenchTracers
        forM_ (testScript protocolFile submitMode) action
  runActionM fullScript iom >>= \case
    (Right a  , _ ,  ()) -> return $ Right a
    (Left err , _  , ()) -> return $ Left err

printJSON :: IO ()
printJSON = BSL.putStrLn $ prettyPrint $ testScript "/dev/zero" DiscardTX

testScript :: FilePath -> SubmitMode -> [Action]
testScript protocolFile submitMode =
  [ SetProtocolParameters (UseLocalProtocolFile protocolFile)
  , Set (TTxAdditionalSize ==>  39)
  , Set (TFee ==>  Lovelace 212345)
  , Set (TTTL ==> SlotNo 1000000)
  , Set (TNetworkId ==> Testnet (NetworkMagic {unNetworkMagic = 42}))
  , InitWallet wallet
  , DefineSigningKey key
    (TextEnvelope { teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
                  , teDescription = fromString "Genesis Initial UTxO Signing Key"
                  , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h\153\171\SI/m\186\242D\228\NAK\182(&\162"})
  , AddFund era wallet
    (TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0))
    (Lovelace 90000000000000) key
  , createChange 2200000000000 10
  , createChange 70000000000 300
  , createChange 2300000000 9000
  , RunBenchmark era wallet
    submitMode
    SpendOutput
    (ThreadName "walletBasedBenchmark") extraArgs (TPSRate 10.0)
  ]
  where
    era = AnyCardanoEra AllegraEra
    wallet = WalletName "test-wallet"
    key = KeyName "pass-partout"
    addr = PayToAddr key
    createChange val count
      = CreateChange era wallet wallet submitMode addr (Lovelace val) count
    extraArgs = RunBenchmarkAux {
        auxTxCount = 4000
      , auxFee = 1000000
      , auxOutputsPerTx = 2
      , auxInputsPerTx = 2
      , auxInputs = 8000
      , auxOutputs = 8000
      , auxMinValuePerUTxO = 10500000
      }
