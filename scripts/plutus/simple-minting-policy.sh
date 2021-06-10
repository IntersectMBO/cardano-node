#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

# This is an example plutus minting script derived from the validator in
# Cardano.CLI.Plutus.SimpleMintingScript. Essentially we demand that the
# script owner is allowed to mint "MillarCoin" and you can only mint
# a single "MillarCoin" per tx.

# Step 1 - Send ADA to token script owner

export CARDANO_NODE_SOCKET_PATH=example/node-bft1/node.sock

utxovkey=example/shelley/utxo-keys/utxo1.vkey
utxoskey=example/shelley/utxo-keys/utxo1.skey
utxoaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $utxovkey)
utxo=$(cardano-cli query utxo --address $utxoaddr --cardano-mode --testnet-magic 42 --out-file utxo.json)
txin=$(jq -r 'keys[]' utxo.json)

scriptownervkey=scripts/plutus/examples/mintingscript/minting-owner.vkey
scriptowneraddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $utxovkey)

#cardano-cli transaction build-raw \
#  --alonzo-era \
#  --fee 0 \
#  --tx-in $txin \
#  --tx-out $scriptowneraddr+500000000 \
#  --tx-out $scriptowneraddr+500000000 \
#  --out-file fund-script-owner.body

cardano-cli transaction sign \
  --tx-body-file fund-script-owner.body \
  --testnet-magic 42 \
  --signing-key-file $utxoskey \
  --out-file fund-script-owner.tx

# SUBMIT
#cardano-cli transaction submit --tx-file fund-script-owner.tx --testnet-magic 42

#echo "Pausing for 5 seconds..."
#sleep 5

scriptownerutxo=$(cardano-cli query utxo --address $scriptowneraddr --testnet-magic 42 --out-file utxo.json)
scriptownertxin=$(jq -r 'keys[0]' utxo.json)
scriptownerCollateral=$(jq -r 'keys[1]' utxo.json)

# Step 2: Mint a single MillarCoin

# We need the script policy ID
policyid=$(cardano-cli transaction policyid --script-file scripts/plutus/examples/mintingscript/mintingscript.plutus)

cardano-cli query protocol-parameters --testnet-magic 42 --out-file example/pparams.json

cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in $scriptownertxin \
  --tx-in-collateral $scriptownerCollateral \
  --tx-in-script-file scripts/plutus/always-succeeds-txin.plutus \
  --datum-value 42 \
  --tx-out "$utxoaddr+500000000 + 5 $policyid.MillarCoin" \
  --mint "5 $policyid.MillarCoin" \
  --redeemer-value 42 \
  --minting-script-file  scripts/plutus/examples/mintingscript/mintingscript.plutus \
  --protocol-params-file example/pparams.json\
  --execution-units "(0,0)" \
  --out-file plutusmint.body

cardano-cli transaction sign \
  --tx-body-file plutusmint.body \
  --testnet-magic 42 \
  --signing-key-file example/shelley/utxo-keys/utxo1.skey \
  --out-file plutusmint.tx
