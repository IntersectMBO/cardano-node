#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail


# This script sets up a cluster that starts out in Byron, and can transition to Mary.
#
# The script generates all the files needed for the setup, and prints commands
# to be run manually (to start the nodes, post transactions, etc.).
#
# There are three ways of triggering the transition to Shelley:
# 1. Trigger transition at protocol version 2.0.0 (as on mainnet)
#    The system starts at 0.0.0, and we can only increase it by 1 in the major
#    version, so this does require to
#    a) post an update proposal and votes to transition to 1.0.0
#    b) wait for the protocol to change (end of the epoch, or end of the last
#      epoch if it's posted near the end of the epoch)
#    c) change configuration.yaml to have 'LastKnownBlockVersion-Major: 2',
#      and restart the nodes
#    d) post an update proposal and votes to transition to 2.0.0
#    This is what will happen on the mainnet, so it's vital to test this, but
#    it does contain some manual steps.
# 2. Trigger transition at protocol version 2.0.0
#    For testing purposes, we can also modify the system to do the transition to
#    Shelley at protocol version 1.0.0, by uncommenting the line containing
#    'TestShelleyHardForkAtVersion' below. Then, we just need to execute step a)
#    above in order to trigger the transition.
#    This is still close to the procedure on the mainnet, and requires less
#    manual steps.
# 3. Schedule transition in the configuration
#    To do this, uncomment the line containing 'TestShelleyHardForkAtEpoch'
#    below. It's good for a quick test, and does not rely on posting update
#    proposals to the chain.
#    This is quite convenient, but it does not test that we can do the
#    transition by posting update proposals to the network. For even more convenience
#    if you want to start a node in Shelley, Allegra or Mary from epoch 0, supply the script
#    with a shelley, allegra or mary string argument. E.g mkfiles.sh mary.

ROOT=example

BFT_NODES="node-bft1 node-bft2"
BFT_NODES_N="1 2"
NUM_BFT_NODES=2

POOL_NODES="node-pool1"

ALL_NODES="${BFT_NODES} ${POOL_NODES}"

INIT_SUPPLY=10020000000
FUNDS_PER_GENESIS_ADDRESS=$((${INIT_SUPPLY} / ${NUM_BFT_NODES}))
FUNDS_PER_BYRON_ADDRESS=$((${FUNDS_PER_GENESIS_ADDRESS} - 1000000))
# We need to allow for a fee to transfer the funds out of the genesis.
# We don't care too much, 1 ada is more than enough.

NETWORK_MAGIC=42
SECURITY_PARAM=10

UNAME=$(uname -s) DATE=
case $UNAME in
  Darwin )      DATE="gdate";;
  Linux )       DATE="date";;
  MINGW64_NT* ) UNAME="Windows_NT"
                DATE="date";;
esac

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac

sprocket() {
  if [ "$UNAME" == "Windows_NT" ]; then
    # Named pipes names on Windows must have the structure: "\\.\pipe\PipeName"
    # See https://docs.microsoft.com/en-us/windows/win32/ipc/pipe-names
    echo -n '\\.\pipe\'
    echo "$1" | sed 's|/|\\|g'
  else
    echo "$1"
  fi
}

START_TIME="$(${DATE} -d "now + 30 seconds" +%s)"

if ! mkdir "${ROOT}"; then
  echo "The ${ROOT} directory already exists, please move or remove it"
  exit
fi

# copy and tweak the configuration
cp configuration/defaults/byron-mainnet/configuration.yaml ${ROOT}/
$SED -i "${ROOT}/configuration.yaml" \
    -e 's/Protocol: RealPBFT/Protocol: Cardano/' \
    -e '/Protocol/ aPBftSignatureThreshold: 0.6' \
    -e 's/minSeverity: Info/minSeverity: Debug/' \
    -e 's|GenesisFile: genesis.json|ByronGenesisFile: byron/genesis.json|' \
    -e '/ByronGenesisFile/ aShelleyGenesisFile: shelley/genesis.json' \
    -e '/ByronGenesisFile/ aAlonzoGenesisFile: shelley/genesis.alonzo.json' \
    -e '/ByronGenesisFile/ aConwayGenesisFile: shelley/genesis.conway.json' \
    -e 's/RequiresNoMagic/RequiresMagic/' \
    -e 's/LastKnownBlockVersion-Major: 0/LastKnownBlockVersion-Major: 1/' \
    -e 's/LastKnownBlockVersion-Minor: 2/LastKnownBlockVersion-Minor: 0/'
# Options for making it easier to trigger the transition to Shelley
# If neither of those are used, we have to
# - post an update proposal + votes to go to protocol version 1
# - after that's activated, change the configuration to have
#   'LastKnownBlockVersion-Major: 2', and restart the nodes
# - post another proposal + vote to go to protocol version 2

#uncomment this for an automatic transition after the first epoch
# echo "TestShelleyHardForkAtEpoch: 1" >> ${ROOT}/configuration.yaml
#uncomment this to trigger the hardfork with protocol version 1
#echo "TestShelleyHardForkAtVersion: 1"  >> ${ROOT}/configuration.yaml

# Create the node directories
for NODE in ${ALL_NODES}; do
  mkdir -p "${ROOT}/${NODE}/byron" "${ROOT}/${NODE}/shelley"
done

# Make topology files
#TODO generalise this over the N BFT nodes and pool nodes
cat > "${ROOT}/node-bft1/topology.json" <<EOF
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3002,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3003,
       "valency": 1
     }
   ]
 }
EOF
echo 3001 > "${ROOT}/node-bft1/port"

cat > "${ROOT}/node-bft2/topology.json" <<EOF
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3001,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3003,
       "valency": 1
     }
   ]
 }
EOF
echo 3002 > "${ROOT}/node-bft2/port"

cat > "${ROOT}/node-pool1/topology.json" <<EOF
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3001,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3002,
       "valency": 1
     }
   ]
 }
EOF
echo 3003 > "${ROOT}/node-pool1/port"


cat > "${ROOT}/byron.genesis.spec.json" <<EOF
{
  "heavyDelThd":     "300000000000",
  "maxBlockSize":    "2000000",
  "maxTxSize":       "4096",
  "maxHeaderSize":   "2000000",
  "maxProposalSize": "700",
  "mpcThd": "20000000000000",
  "scriptVersion": 0,
  "slotDuration": "1000",
  "softforkRule": {
    "initThd": "900000000000000",
    "minThd": "600000000000000",
    "thdDecrement": "50000000000000"
  },
  "txFeePolicy": {
    "multiplier": "43946000000",
    "summand": "155381000000000"
  },
  "unlockStakeEpoch": "18446744073709551615",
  "updateImplicit": "10000",
  "updateProposalThd": "100000000000000",
  "updateVoteThd": "1000000000000"
}
EOF

cardano-cli byron genesis genesis \
  --protocol-magic ${NETWORK_MAGIC} \
  --start-time "${START_TIME}" \
  --k ${SECURITY_PARAM} \
  --n-poor-addresses 0 \
  --n-delegate-addresses ${NUM_BFT_NODES} \
  --total-balance ${INIT_SUPPLY} \
  --delegate-share 1 \
  --avvm-entry-count 0 \
  --avvm-entry-balance 0 \
  --protocol-parameters-file "${ROOT}/byron.genesis.spec.json" \
  --genesis-output-dir "${ROOT}/byron"
mv "${ROOT}/byron.genesis.spec.json" "${ROOT}/byron/genesis.spec.json"

pwd

# Symlink the BFT operator keys from the genesis delegates, for uniformity
for N in ${BFT_NODES_N}; do
  ln -s "../../byron/delegate-keys.00$((${N} - 1)).key"     "${ROOT}/node-bft${N}/byron/delegate.key"
  ln -s "../../byron/delegation-cert.00$((${N} - 1)).json"  "${ROOT}/node-bft${N}/byron/delegate.cert"
done

# Create keys, addresses and transactions to withdraw the initial UTxO into
# regular addresses.
for N in ${BFT_NODES_N}; do

  cardano-cli byron key keygen \
    --secret "${ROOT}/byron/payment-keys.00$((${N} - 1)).key"

  cardano-cli byron key signing-key-address \
    --testnet-magic ${NETWORK_MAGIC} \
    --secret "${ROOT}/byron/payment-keys.00$((${N} - 1)).key" > "${ROOT}/byron/address-00$((${N} - 1))"

  cardano-cli byron key signing-key-address \
    --testnet-magic ${NETWORK_MAGIC} \
    --secret "${ROOT}/byron/genesis-keys.00$((${N} - 1)).key" > "${ROOT}/byron/genesis-address-00$((${N} - 1))"

  cardano-cli byron transaction issue-genesis-utxo-expenditure \
    --genesis-json "${ROOT}/byron/genesis.json" \
    --testnet-magic ${NETWORK_MAGIC} \
    --tx "${ROOT}/tx$((${N} - 1)).tx" \
    --wallet-key "${ROOT}/byron/delegate-keys.00$((${N} - 1)).key" \
    --rich-addr-from "$(head -n 1 "${ROOT}/byron/genesis-address-00$((${N} - 1))")" \
    --txout "(\"$(head -n 1 "${ROOT}/byron/address-00$((${N} - 1))")\", $FUNDS_PER_BYRON_ADDRESS)"
done

# Update Proposal and votes
cardano-cli byron governance create-update-proposal \
            --filepath "${ROOT}/update-proposal" \
            --testnet-magic "${NETWORK_MAGIC}" \
            --signing-key "${ROOT}/byron/delegate-keys.000.key" \
            --protocol-version-major 1 \
            --protocol-version-minor 0 \
            --protocol-version-alt 0 \
            --application-name "cardano-sl" \
            --software-version-num 1 \
            --system-tag "linux" \
            --installer-hash 0

for N in ${BFT_NODES_N}; do
    cardano-cli byron governance create-proposal-vote \
                --proposal-filepath "${ROOT}/update-proposal" \
                --testnet-magic ${NETWORK_MAGIC} \
                --signing-key "${ROOT}/byron/delegate-keys.00$((${N} - 1)).key" \
                --vote-yes \
                --output-filepath "${ROOT}/update-vote.00$((${N} - 1))"
done

cardano-cli byron governance create-update-proposal \
            --filepath "${ROOT}/update-proposal-1" \
            --testnet-magic ${NETWORK_MAGIC} \
            --signing-key "${ROOT}/byron/delegate-keys.000.key" \
            --protocol-version-major 2 \
            --protocol-version-minor 0 \
            --protocol-version-alt 0 \
            --application-name "cardano-sl" \
            --software-version-num 1 \
            --system-tag "linux" \
            --installer-hash 0

for N in ${BFT_NODES_N}; do
    cardano-cli byron governance create-proposal-vote \
                --proposal-filepath "${ROOT}/update-proposal-1" \
                --testnet-magic ${NETWORK_MAGIC} \
                --signing-key "${ROOT}/byron/delegate-keys.00$((${N} - 1)).key" \
                --vote-yes \
                --output-filepath "${ROOT}/update-vote-1.00$((${N} - 1))"
done

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 "${ROOT}/byron"/*
echo "====================================================================="


# Set up our template
mkdir "${ROOT}/shelley"

# Copy the QA testnet alonzo and conway genesis files which is equivalent to the mainnet

cp configuration/cardano/shelley_qa-alonzo-genesis.json "${ROOT}/shelley/genesis.alonzo.spec.json"
cp configuration/cardano/shelley_qa-conway-genesis.json "${ROOT}/shelley/genesis.conway.spec.json"

cardano-cli genesis create --testnet-magic ${NETWORK_MAGIC} --genesis-dir "${ROOT}/shelley"


# Now generate for real:

cardano-cli genesis create \
    --testnet-magic ${NETWORK_MAGIC} \
    --genesis-dir "${ROOT}/shelley/" \
    --gen-genesis-keys ${NUM_BFT_NODES} \
    --gen-utxo-keys 1

echo "What is in shelley"
echo "$(ls ${ROOT}/shelley)"

cp "${ROOT}/shelley/genesis.json" "${ROOT}/shelley/copy-genesis.json"

# We're going to use really quick epochs (300 seconds), by using short slots 0.2s
# and K=10, but we'll keep long KES periods so we don't have to bother
# cycling KES keys


jq -M '. +
    { slotLength:0.1
    , securityParam:10
    , activeSlotsCoeff:0.1
    , securityParam:10
    , epochLength:500
    , maxLovelaceSupply:1000000000000
    , updateQuorum:2
    }' \
  "${ROOT}/shelley/copy-genesis.json" > "${ROOT}/shelley/copy2-genesis.json"
jq --raw-output '
      .protocolParams.protocolVersion.major = 2
    | .protocolParams.minFeeA = 44
    | .protocolParams.minFeeB = 155381
    | .protocolParams.minUTxOValue = 1000000
    | .protocolParams.decentralisationParam = 0.7
    | .protocolParams.rho = 0.1
    | .protocolParams.tau = 0.1' \
  "${ROOT}/shelley/copy2-genesis.json" > "${ROOT}/shelley/genesis.json"

rm "${ROOT}/shelley/copy2-genesis.json"
rm "${ROOT}/shelley/copy-genesis.json"

cardano-cli stake-address key-gen \
  --verification-key-file "${ROOT}/shelley/utxo-keys/utxo-stake.vkey" \
  --signing-key-file "${ROOT}/shelley/utxo-keys/utxo-stake.skey"

cardano-cli address key-gen \
  --verification-key-file "${ROOT}/shelley/utxo-keys/utxo2.vkey" \
  --signing-key-file "${ROOT}/shelley/utxo-keys/utxo2.skey"

cardano-cli stake-address key-gen \
  --verification-key-file "${ROOT}/shelley/utxo-keys/utxo2-stake.vkey" \
  --signing-key-file "${ROOT}/shelley/utxo-keys/utxo2-stake.skey"

# Create a symlink to all the payment keys in utxo-keys directory
mkdir -p "${ROOT}/utxo-keys"

for x in $(find "${ROOT}/shelley/utxo-keys" -type f); do
  if cat "$x" | jq -e 'select(
      false
      or .type == "GenesisUTxOVerificationKey_ed25519"
      or .type == "GenesisUTxOSigningKey_ed25519"
      or .type == "PaymentSigningKeyShelley_ed25519"
      or .type == "PaymentVerificationKeyShelley_ed25519"
      )' > /dev/null; then
    ln -sf "../$x" "${ROOT}/utxo-keys/$(basename "$x")"
  fi
done

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 "${ROOT}/shelley"/*
echo "====================================================================="

echo "Generated shelley/genesis.json:"
echo
cat "${ROOT}/shelley/genesis.json"
echo
echo "====================================================================="

# Make the pool operator cold keys
# This was done already for the BFT nodes as part of the genesis creation

for NODE in ${POOL_NODES}; do
  cardano-cli node key-gen \
      --cold-verification-key-file                 "${ROOT}/${NODE}/shelley/operator.vkey" \
      --cold-signing-key-file                      "${ROOT}/${NODE}/shelley/operator.skey" \
      --operational-certificate-issue-counter-file "${ROOT}/${NODE}/shelley/operator.counter"

  cardano-cli node key-gen-VRF \
      --verification-key-file "${ROOT}/${NODE}/shelley/vrf.vkey" \
      --signing-key-file      "${ROOT}/${NODE}/shelley/vrf.skey"
done

# Symlink the BFT operator keys from the genesis delegates, for uniformity

for N in ${BFT_NODES_N}; do
  ln -s ../../shelley/delegate-keys/delegate${N}.skey "${ROOT}/node-bft${N}/shelley/operator.skey"
  ln -s ../../shelley/delegate-keys/delegate${N}.vkey "${ROOT}/node-bft${N}/shelley/operator.vkey"
  ln -s ../../shelley/delegate-keys/delegate${N}.counter "${ROOT}/node-bft${N}/shelley/operator.counter"
  ln -s ../../shelley/delegate-keys/delegate${N}.vrf.vkey "${ROOT}/node-bft${N}/shelley/vrf.vkey"
  ln -s ../../shelley/delegate-keys/delegate${N}.vrf.skey "${ROOT}/node-bft${N}/shelley/vrf.skey"
done


# Make hot keys and for all nodes

for NODE in ${ALL_NODES}; do
  cardano-cli node key-gen-KES \
      --verification-key-file "${ROOT}/${NODE}/shelley/kes.vkey" \
      --signing-key-file      "${ROOT}/${NODE}/shelley/kes.skey"

  cardano-cli node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file                  "${ROOT}/${NODE}/shelley/kes.vkey" \
      --cold-signing-key-file                      "${ROOT}/${NODE}/shelley/operator.skey" \
      --operational-certificate-issue-counter-file "${ROOT}/${NODE}/shelley/operator.counter" \
      --out-file                                   "${ROOT}/${NODE}/shelley/node.cert"
done

echo "Generated node operator keys (cold, hot) and operational certs:"
echo
for NODE in ${ALL_NODES}; do
  ls -1 "${ROOT}/${NODE}"
done
echo "====================================================================="


# Make some payment and stake addresses
# user1..n:       will own all the funds in the system, we'll set this up from
#                 initial utxo the
# pool-owner1..n: will be the owner of the pools and we'll use their reward
#                 account for pool rewards

USER_ADDRS="user1"
POOL_ADDRS="pool-owner1"

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"

mkdir "${ROOT}/addresses"

for ADDR in ${ADDRS}; do

  # Payment address keys
  cardano-cli address key-gen \
      --verification-key-file "${ROOT}/addresses/${ADDR}.vkey" \
      --signing-key-file      "${ROOT}/addresses/${ADDR}.skey"

  # Stake address keys
  cardano-cli stake-address key-gen \
      --verification-key-file "${ROOT}/addresses/${ADDR}-stake.vkey" \
      --signing-key-file      "${ROOT}/addresses/${ADDR}-stake.skey"

  # Payment addresses
  cardano-cli address build \
      --payment-verification-key-file "${ROOT}/addresses/${ADDR}.vkey" \
      --stake-verification-key-file "${ROOT}/addresses/${ADDR}-stake.vkey" \
      --testnet-magic ${NETWORK_MAGIC} \
      --out-file "${ROOT}/addresses/${ADDR}.addr"

  # Stake addresses
  cardano-cli stake-address build \
      --stake-verification-key-file "${ROOT}/addresses/${ADDR}-stake.vkey" \
      --testnet-magic ${NETWORK_MAGIC} \
      --out-file "${ROOT}/addresses/${ADDR}-stake.addr"

  # Stake addresses registration certs
  cardano-cli stake-address registration-certificate \
      --stake-verification-key-file "${ROOT}/addresses/${ADDR}-stake.vkey" \
      --out-file "${ROOT}/addresses/${ADDR}-stake.reg.cert"

done

# user N will delegate to pool N
USER_POOL_N="1"

for N in ${USER_POOL_N}; do

  # Stake address delegation certs
  cardano-cli stake-address delegation-certificate \
      --stake-verification-key-file "${ROOT}/addresses/user${N}-stake.vkey" \
      --cold-verification-key-file  "${ROOT}/node-pool${N}/shelley/operator.vkey" \
      --out-file "${ROOT}/addresses/user${N}-stake.deleg.cert"

  ln -s "../addresses/pool-owner${N}-stake.vkey" "${ROOT}/node-pool${N}/owner.vkey"
  ln -s "../addresses/pool-owner${N}-stake.skey" "${ROOT}/node-pool${N}/owner.skey"

done

echo "Generated payment address keys, stake address keys,"
echo "stake address registration certs, and stake address delegation certs"
echo
ls -1 "${ROOT}/addresses/"
echo "====================================================================="


# Next is to make the stake pool registration cert

for NODE in ${POOL_NODES}; do

  cardano-cli stake-pool registration-certificate \
    --testnet-magic ${NETWORK_MAGIC} \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             "${ROOT}/${NODE}/shelley/operator.vkey" \
    --vrf-verification-key-file              "${ROOT}/${NODE}/shelley/vrf.vkey" \
    --reward-account-verification-key-file   "${ROOT}/${NODE}/owner.vkey" \
    --pool-owner-stake-verification-key-file "${ROOT}/${NODE}/owner.vkey" \
    --out-file                               "${ROOT}/${NODE}/registration.cert"
done

echo "Generated stake pool registration certs:"
ls -1 "${ROOT}/node-"*"/registration.cert"
echo "====================================================================="

echo "So you can now do various things:"
echo " * Start the nodes"
echo " * Initiate successive protocol updates"
echo " * Query the node's ledger state"
echo
echo "To start the nodes, in separate terminals use the following scripts:"
echo

mkdir -p "${ROOT}/run"

for NODE in ${BFT_NODES}; do
  (
    echo "#!/usr/bin/env bash"
    echo ""
    echo "cardano-node run \\"
    echo "  --config                          ${ROOT}/configuration.yaml \\"
    echo "  --topology                        ${ROOT}/${NODE}/topology.json \\"
    echo "  --database-path                   ${ROOT}/${NODE}/db \\"
    echo "  --socket-path                     '$(sprocket "${ROOT}/${NODE}/node.sock")' \\"
    echo "  --shelley-kes-key                 ${ROOT}/${NODE}/shelley/kes.skey \\"
    echo "  --shelley-vrf-key                 ${ROOT}/${NODE}/shelley/vrf.skey \\"
    echo "  --shelley-operational-certificate ${ROOT}/${NODE}/shelley/node.cert \\"
    echo "  --port                            $(cat "${ROOT}/${NODE}/port") \\"
    echo "  --delegation-certificate          ${ROOT}/${NODE}/byron/delegate.cert \\"
    echo "  --signing-key                     ${ROOT}/${NODE}/byron/delegate.key \\"
    echo "  | tee -a \"${ROOT}/${NODE}/node.log\""
  ) > "${ROOT}/run/${NODE}.sh"

  chmod a+x "${ROOT}/run/${NODE}.sh"

  echo "${ROOT}/run/${NODE}.sh"
done

for NODE in ${POOL_NODES}; do
  (
    echo "#!/usr/bin/env bash"
    echo ""
    echo "cardano-node run \\"
    echo "  --config                          ${ROOT}/configuration.yaml \\"
    echo "  --topology                        ${ROOT}/${NODE}/topology.json \\"
    echo "  --database-path                   ${ROOT}/${NODE}/db \\"
    echo "  --socket-path                     '$(sprocket "${ROOT}/${NODE}/node.sock")' \\"
    echo "  --shelley-kes-key                 ${ROOT}/${NODE}/shelley/kes.skey \\"
    echo "  --shelley-vrf-key                 ${ROOT}/${NODE}/shelley/vrf.skey \\"
    echo "  --shelley-operational-certificate ${ROOT}/${NODE}/shelley/node.cert \\"
    echo "  --port                            $(cat "${ROOT}/${NODE}/port") \\"
    echo "  | tee -a \"${ROOT}/${NODE}/node.log\""
  ) > "${ROOT}/run/${NODE}.sh"

  chmod a+x "${ROOT}/run/${NODE}.sh"

  echo "$ROOT/run/${NODE}.sh"
done

echo "#!/usr/bin/env bash" > "${ROOT}/run/all.sh"
echo "" >> "${ROOT}/run/all.sh"

chmod a+x "${ROOT}/run/all.sh"

for NODE in ${BFT_NODES}; do
  echo "$ROOT/run/${NODE}.sh &" >> "${ROOT}/run/all.sh"
done

for NODE in ${POOL_NODES}; do
  echo "$ROOT/run/${NODE}.sh &" >> "${ROOT}/run/all.sh"
done

echo "" >> "${ROOT}/run/all.sh"
echo "wait" >> "${ROOT}/run/all.sh"

chmod a+x "${ROOT}/run/all.sh"

echo
echo "Alternatively, you can run all the nodes in one go:"
echo
echo "$ROOT/run/all.sh"

echo
echo "In order to do the protocol updates, proceed as follows:"
echo
echo "  0. invoke ./scripts/cardano/mkfiles.sh"
echo "  1. wait for the nodes to start producing blocks"
echo "  2. invoke ./scripts/cardano/update-1.sh <N>"
echo "     if you are early enough in the epoch N = current epoch"
echo "     if not N = current epoch + 1. This applies for all update proposals"
echo "     wait for the next epoch for the update to take effect"
echo
echo "  3. invoke ./scripts/cardano/update-2.sh"
echo "  4. restart the nodes"
echo "     wait for the next epoch for the update to take effect"
echo "     you should be in the Shelley era if the update was successful"
echo
echo "  5. invoke ./scripts/cardano/update-3.sh <N>"
echo "     Here, <N> the current epoch (2 if you're quick)."
echo "     If you provide the wrong epoch, you will see an error"
echo "     that will tell you the current epoch, and can run"
echo "     the script again."
echo "  6. restart the nodes"
echo "     wait for the next epoch for the update to take effect"
echo "     you should be in the Allegra era if the update was successful"
echo "  7. invoke ./scripts/cardano/update-4.sh <N>"
echo "  8. restart the nodes"
echo "     wait for the next epoch for the update to take effect"
echo "     you should be in the Mary era if the update was successful"
echo "  9. invoke ./scripts/cardano/update-5.sh <N>"
echo "     wait for the next epoch for the update to take effect"
echo "     you should be in the Alonzo era if the update was successful"
echo
echo "You can observe the status of the updates by grepping the logs, via"
echo
echo "  grep LedgerUpdate ${ROOT}/node-pool1/node.log"
echo
echo "When in Shelley (after 3, and before 4), you should be able "
echo "to look at the protocol parameters, or the ledger state, "
echo "using commands like"
echo
echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
echo "  cardano-cli query protocol-parameters \\"
echo "  --cardano-mode --testnet-magic 42"
echo
echo "This will fail outside of the Shelley era. In particular, "
echo "after step 3, you will get an error message that tells you "
echo "that you are in the Allegra era. You must then use the --allegra-era flag:"
echo
echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
echo "  cardano-cli query protocol-parameters \\"
echo "  --cardano-mode --allegra-era --testnet-magic 42"
echo
echo "Similarly, use --mary-era in the Mary era."

# For an automatic transition at epoch 0, specifying mary, allegra or shelley
# will start the node in the appropriate era.
echo ""

# These are needed for cardano-submit-api
echo "EnableLogMetrics: False" >> "${ROOT}/configuration.yaml"
echo "EnableLogging: True" >> "${ROOT}/configuration.yaml"

if [ "$1" = "babbage" ]; then
  echo "TestShelleyHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "TestAllegraHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "TestMaryHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "TestAlonzoHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "TestBabbageHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "ExperimentalHardForksEnabled: True" >> "${ROOT}/configuration.yaml"
  echo "ExperimentalProtocolsEnabled: True" >> "${ROOT}/configuration.yaml"

  $SED -i "${ROOT}/configuration.yaml" \
      -e 's/LastKnownBlockVersion-Major: 1/LastKnownBlockVersion-Major: 7/'

  # Copy the cost model
  echo "Nodes will start in Alonzo era from epoch 0"

elif [ "$1" = "alonzo" ]; then
  echo "TestShelleyHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "TestAllegraHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "TestMaryHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "TestAlonzoHardForkAtEpoch: 0" >> "${ROOT}/configuration.yaml"
  echo "ExperimentalHardForksEnabled: True" >> "${ROOT}/configuration.yaml"
  echo "ExperimentalProtocolsEnabled: True" >> "${ROOT}/configuration.yaml"

  $SED -i "${ROOT}/configuration.yaml" \
      -e 's/LastKnownBlockVersion-Major: 1/LastKnownBlockVersion-Major: 5/'

  # Copy the cost model
  echo "Nodes will start in Alonzo era from epoch 0"

elif [ "$1" = "mary" ]; then
  echo "TestShelleyHardForkAtEpoch: 0"  >> "${ROOT}/configuration.yaml"
  echo "TestAllegraHardForkAtEpoch: 0"  >> "${ROOT}/configuration.yaml"
  echo "TestMaryHardForkAtEpoch: 0"  >> "${ROOT}/configuration.yaml"
  $SED -i "${ROOT}/configuration.yaml" \
      -e 's/LastKnownBlockVersion-Major: 1/LastKnownBlockVersion-Major: 4/'
  echo "Nodes will start in Mary era from epoch 0"

elif [ "$1" = "allegra" ]; then
  echo "TestShelleyHardForkAtEpoch: 0"  >> "${ROOT}/configuration.yaml"
  echo "TestAllegraHardForkAtEpoch: 0"  >> "${ROOT}/configuration.yaml"
  $SED -i "${ROOT}/configuration.yaml" \
      -e 's/LastKnownBlockVersion-Major: 1/LastKnownBlockVersion-Major: 3/'
  echo "Nodes will start in Allegra era from epoch 0"

elif [ "$1" = "shelley" ]; then
  echo "TestShelleyHardForkAtEpoch: 0"  >> "${ROOT}/configuration.yaml"
  $SED -i "${ROOT}/configuration.yaml" \
      -e 's/LastKnownBlockVersion-Major: 1/LastKnownBlockVersion-Major: 2/'
  echo "Nodes will start in Shelley era from epoch 0"

else
  echo "Default yaml configuration applied."
fi

(cd "$ROOT"; ln -s "node-bft1/node.sock" "main.sock")
