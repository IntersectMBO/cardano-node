#!/usr/bin/env bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# In order to use this script you must set up a local Alonzo cluster and have a
# stake pool set up that you can delegate to.

#if [ "$1" == "guessinggame" ]; then
# # NB: This plutus script uses a "typed" redeemer and "typed" datum.
# plutusscriptinuse=scripts/plutus/scripts/typed-guessing-game-redeemer-42-datum-42.plutus
# # This datum hash is the hash of the typed 42
# scriptdatumhash="e68306b4087110b0191f5b70638b9c6fc1c3eb335275e40d110779d71aa86083"
# plutusrequiredspace=700000000
# plutusrequiredtime=700000000
# #50000000000
# datumfilepath=scripts/plutus/data/typed-42.datum
# redeemerfilepath=scripts/plutus/data/typed-42.redeemer
# echo "Guessing game Plutus script in use. The datum and redeemer must be equal to 42."
# echo "Script at: $plutusscriptinuse"
#
#elif [ "$1" == "" ]; then
# plutusscriptinuse=scripts/plutus/scripts/untyped-always-succeeds-txin.plutus
# # This datum hash is the hash of the untyped 42
# scriptdatumhash="9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
# plutusrequiredspace=70000000
# plutusrequiredtime=70000000
# datumfilepath=scripts/plutus/data/42.datum
# redeemerfilepath=scripts/plutus/data/42.redeemer
# echo "Always succeeds Plutus script in use. Any datum and redeemer combination will succeed."
# echo "Script at: $plutusscriptinuse"
#fi

plutusscriptinuse=scripts/plutus/scripts/staking-cred-lock.plutus

# Step 1: Create stake registration certificate with a script credential

cardano-cli stake-address registration-certificate \
  --stake-script-file "$plutusscriptinuse" \
  --out-file example/work/plutus-lock.reg.cert

# Step 2: Send ADA to address that has a stake registration certificate already constucted

seedutxovkey=example/shelley/utxo-keys/utxo1.vkey
seedutxoskey=example/shelley/utxo-keys/utxo1.skey
utxoaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $seedutxovkey)
echo "$utxoaddr"
cardano-cli query utxo --address $utxoaddr --cardano-mode --testnet-magic 42 --out-file example/work/seedutxo.json
seedtxin=$(jq -r 'keys[]' example/work/seedutxo.json)
lovelaceatseedtxin=$(jq -r ".[\"$seedtxin\"].value.lovelace" example/work/seedutxo.json)
echo "$lovelaceatseedtxin"
lovelaceatseedtxindiv2=$(expr $lovelaceatseedtxin / 2)
targetaddr=$(< example/addresses/user1.addr)

cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in "$seedtxin" \
  --tx-out "$targetaddr+$lovelaceatseedtxindiv2" \
  --tx-out "$targetaddr+$lovelaceatseedtxindiv2" \
  --out-file example/work/seed.body

cardano-cli transaction sign \
  --tx-body-file example/work/seed.body \
  --testnet-magic 42 \
  --signing-key-file $seedutxoskey\
  --out-file example/work/seed.tx

# SUBMIT
cardano-cli transaction submit --tx-file example/work/seed.tx --testnet-magic 42
echo "Pausing for 5 seconds..."
sleep 5

# Step 3
# Submit stake address registration certificate

cardano-cli query utxo --address $utxoaddr --cardano-mode --testnet-magic 42 --out-file example/work/seedutxo.json
registrationtxin=$(jq -r 'keys[0]' example/work/seedutxo.json)
lovelaceatregistrationtxin=$(jq -r ".[\"$registrationtxin\"].value.lovelace" example/work/seedutxo.json)

cardano-cli transaction build-raw \
  --alonzo-era \
  --fee 0 \
  --tx-in "$registrationtxin" \
  --tx-out "$targetaddr+$lovelaceatregistrationtxin" \
  --certificate-file "example/work/plutus-lock.reg.cert" \
  --certificate-script-file "$plutusscriptinuse" \
  --certificate-redeemer-value "$plutusscriptinuse"
  --out-file example/work/seed.body

cardano-cli transaction sign \
  --tx-body-file example/work/seed.body \
  --testnet-magic 42 \
  --signing-key-file $seedutxoskey\
  --out-file example/work/seed.tx
