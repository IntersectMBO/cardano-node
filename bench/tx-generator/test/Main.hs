{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}

module Main (main) where

import           Prelude
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Heredoc
import           Options.Applicative

import           Cardano.Benchmarking.Command (commandParser)
import           Cardano.Benchmarking.CliArgsScript (parseGeneratorCmd)
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata
import           Hedgehog ((===))

import           Test.Tasty.Hedgehog (testProperty)

import           Hedgehog qualified as H

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =  testGroup "cardano-tx-generator"
  [
    cliArgs
  , sizedMetadata
  , mockServer
  ]

mockServer = testGroup "direct/pure client-server connect"
  [ testCase "tx-send == tx-received" $ assertBool "tx-send == tx-received" True -- TODO !
  ]

sizedMetadata = testGroup "properties of the CBOR encoding relevant for generating sized metadat"
  [ testCase "Shelley metadata map costs" $ assertBool "metadata map costs" prop_mapCostsShelley
  , testCase "Shelley metadata ByteString costs" $ assertBool "metadata ByteString costs" prop_bsCostsShelley
  , testCase "Allegra metadata map costs" $ assertBool "metadata map costs" prop_mapCostsAllegra
  , testCase "Allegra metadata ByteString costs" $ assertBool "metadata ByteString costs" prop_bsCostsAllegra
  , testCase "Mary metadata map costs"    $ assertBool "metadata map costs" prop_mapCostsMary
  , testCase "Marymetadata ByteString costs"    $ assertBool "metadata ByteString costs" prop_bsCostsMary
  , testCase "Test mkMetadata" $ assertBool "" True --WIP
  ]


cliArgs = testGroup "cli arguments"
  [
     -- Also update readme and documentation when the help-messages changes.
    testProperty "check help message against pinned version"
      $ H.withTests 1 $ H.property $ H.test $ filter (/= ' ') helpMessage === filter (/= ' ') pinnedHelpMessage

     -- examples for calling the tx-generator found in the shell scripts.
  , testCmdLine [here|cliArguments --config /work/cli-tests/benchmarks/shelley3pools/configuration/configuration-generator.yaml --socket-path /work/cli-tests/benchmarks/shelley3pools/logs/sockets/1 --num-of-txs 1000 --add-tx-size 0 --inputs-per-tx 1 --outputs-per-tx 1 --tx-fee 1000000 --tps 10 --init-cooldown 5 --target-node ("127.0.0.1",3000) --target-node ("127.0.0.1",3001) --target-node ("127.0.0.1",3002) --genesis-funds-key configuration/genesis-shelley/utxo-keys/utxo1.skey|]
  , testCmdLine [here|eraTransition --config /work/cli-tests/benchmarks/shelley3pools/configuration/configuration-generator.yaml --socket-path /work/cli-tests/benchmarks/shelley3pools/logs/sockets/1 --num-of-txs 1000 --add-tx-size 0 --inputs-per-tx 1 --outputs-per-tx 1 --tx-fee 1000000 --tps 10 --init-cooldown 5 --target-node ("127.0.0.1",3000) --target-node ("127.0.0.1",3001) --target-node ("127.0.0.1",3002) --genesis-funds-key configuration/genesis-shelley/utxo-keys/utxo1.skey|]

  ]
  where
    testCmdLine :: String -> TestTree
    testCmdLine l = testCase "check that example cmd line parses" $ assertBool l $ isJust
                        $ getParseResult $ execParserPure defaultPrefs (info commandParser fullDesc)
                           $ words l
pinnedHelpMessage = [here|ParserFailure(Usage: <program> --config FILEPATH
                   --socket-path FILEPATH
                   [--shelley | --mary | --allegra]
                   [(--target-node (HOST,PORT))]
                   [--init-cooldown INT]
                   [--initial-ttl INT]
                   [--num-of-txs INT]
                   [--tps DOUBLE]
                   [--inputs-per-tx INT]
                   [--outputs-per-tx INT]
                   [--tx-fee INT]
                   [--add-tx-size INT]
                   [--fail-on-submission-errors]
                   ( --genesis-funds-key FILEPATH
                   | --utxo-funds-key FILEPATH --tx-in TX-IN --tx-out TX-OUT
                   | --split-utxo-funds-key FILEPATH --split-utxo FILEPATH
                   )

Available options:
  --config FILEPATH        Configuration file for the cardano-node
  --socket-path FILEPATH   Path to a cardano-node socket
  --shelley                Initialise Cardano in shelley submode.
  --mary                   Initialise Cardano in mary submode.
  --allegra                Initialise Cardano in allegra submode.
  --target-node (HOST,PORT)
                           IP address and port of the node transactions will be
                           sent to.
  --init-cooldown INT      Delay between init and main submission phases.
  --initial-ttl INT        Slot denoting TTL of the initial transactions.
  --num-of-txs INT         Number of transactions generator will create.
  --tps DOUBLE             TPS (transaction per second) rate.
  --inputs-per-tx INT      Number of inputs in each of transactions.
  --outputs-per-tx INT     Number of outputs in each of transactions.
  --tx-fee INT             Fee per transaction, in Lovelaces.
  --add-tx-size INT        Additional size of transaction, in bytes.
  --fail-on-submission-errors
                           Fail on submission thread errors, instead of logging
                           them.
  --genesis-funds-key FILEPATH
                           Genesis UTxO funds signing key.
  --utxo-funds-key FILEPATH
                           UTxO funds signing key.
  --tx-in TX-IN            The input transaction as TxId#TxIx where TxId is the
                           transaction hash and TxIx is the index.
  --tx-out TX-OUT          The transaction output as Address+Lovelace where
                           Address is the Bech32-encoded address followed by the
                           amount in Lovelace.
  --split-utxo-funds-key FILEPATH
                           UTxO funds signing key.
  --split-utxo FILEPATH    UTxO funds file.,ExitSuccess,80)|]

helpMessage = show $ parserFailure defaultPrefs (info parseGeneratorCmd fullDesc ) (ShowHelpText Nothing) []
