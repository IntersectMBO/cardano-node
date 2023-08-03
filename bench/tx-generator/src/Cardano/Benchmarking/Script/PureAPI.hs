{-# LANGUAGE ExplicitNamespaces #-}
{-|
Module:      Cardano.Benchmarking.Script.PureAPI
Description: Pure API for transaction generation

The design of this is a wee bit unclear as of yet, but I'll
work out some way to arrange this.
-}
module Cardano.Benchmarking.Script.PureAPI
where

import Cardano.Api (type CardanoEra (..), AnyCardanoEra (..))
import Cardano.Benchmarking.Compiler (CompileError (..), compileToScript, runCompiler)
import Cardano.Benchmarking.Script.Types (Action (..))
import Cardano.CLI.Types.Legacy (File (..))
import Cardano.Node.Configuration.NodeAddress (NodeAddress'(..), NodeHostIPv4Address (..))
import Cardano.TxGenerator.Setup.NixService (NixServiceOptions (..))
-- import Cardano.TxGenerator.Types (NumberOfInputsPerTx, NumberOfOutputsPerTx)

import Data.List.NonEmpty (NonEmpty (..))

-- Cardano.Benchmarking.Command
-- Cardano.Benchmarking.OuroborosImports
-- Cardano.Benchmarking.Script.Types
-- Cardano.Benchmarking.Types
-- Cardano.Crypto.Wallet
-- Cardano.Crypto.Wallet.Encrypted

thing :: Either CompileError [Action]
thing = flip runCompiler compileToScript
      $ NixServiceOptions { _nix_tx_count = 1024, _nix_tps = 128, _nix_inputs_per_tx = 8, _nix_outputs_per_tx = 8, _nix_tx_fee = 0, _nix_era = AnyCardanoEra BabbageEra, _nix_nodeConfigFile = Just "./cardano-testnet/test/cardano-testnet-golden/files/golden/shelley_node_default_config.json", _nix_cardanoTracerSocket = Nothing, _nix_localNodeSocketPath = "", _nix_plutus = Nothing, _nix_debugMode = False, _nix_init_cooldown = 0.0, _nix_targetNodes = NodeAddress { naHostAddress = NodeHostIPv4Address "192.0.2.1", naPort = 6666 } :| [], _nix_sigKey = File "", _nix_add_tx_size = 4, _nix_min_utxo_value = 1 }
