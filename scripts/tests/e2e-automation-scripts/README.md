### 0 A Conventions

- 4 spaces indentations

- common library that scripts should source in

```bash
# source common lib

. $CWD/e2e-automation-scripts/common.sh
```
##### extract commonly used functionalities to common.sh

- Use exit statuses and check them when calling scripts and functions when needed

- Variable naming examples:

```bash
# local variables lower case with underscore
testnet_magic=42
touch $pool_counter_filepath

# environment variables and constants - capital case
PI=3.1415
export CARDANO_NODE_SOCKET_PATH='example/node-pool1/node.sock'
```

- Function naming convention:

```bash
get_slots_per_kes_period () {
    # use local variables inside function by default
    local slots_per_kes_period=$(cat $genesis_filepath | grep KESPeriod |grep -o '[0-9]\+')

    if (( slots_per_kes_period < 1 )); then
        error_msg "Number of slots per KES period is less than 1: $slots_per_kes_period"
        exit 1
    fi

    # Important - echo is used to return value to function caller - don't use info_msg by mistake here !
    echo $slots_per_kes_period
    exit 0
}
```

- tests
 - For arithemtic tests use round (()) brackets and variable names without $:

```bash
# arithemtic tests:

if (( slots_per_kes_period < 1 )); then
    ...
fi
```
- other tests

```bash
if [ -f $filepath ]; then
  ...
fi

if [[ -z $stake_distribution_stats || $stake_distribution_stats == *"----"* ]]; then
    warn_msg "There is no info about stake distibution. You might need to wait for a little bit longer."
    warn_msg "Exiting ..."
    exit 2
fi
```

-  Log information about what scripts does at each stage, inform about errors / warnings by checking exit status and using following functions from common.sh

```bash
error_msg
info_msg
warn_msg
success_msg

if (( actual_balance != expected_balance )); then
    error_msg "Incorrect amount of funds on address. Is: $actual_balance. Should be: $expected_balance"
    exit 1
fi
```

### 0 B Assumptions

- scripts are *currently* based on assumption that that initial pool is started by `mkfiles.sh` script from `cardano-node/scripts/shelley-from-scratch` dir.
This will change in near future.

- List of `e2e automation` [tickets](https://github.com/input-output-hk/cardano-node/issues?page=2&q=is%3Aissue+is%3Aopen+label%3A%22e2e+automation%22)

- TODO: Add other usefull info

### 1 To run scripts first clone node repo:

```bash
git clone git@github.com:input-output-hk/cardano-node.git
```
### 2 Build:

```bash
cd cardano-node
cabal new-build all
```

OR build and install to specified location:

```bash
cabal install cardano-node cardano-cli --installdir="$HOME/.local/bin"
```

### 3 Copy binaries to your path (run inside cardano-node dir)

Use this if in step 2 you used:

```bash
cabal new-build all
```


```bash
cp $(find . -name cardano-node -executable -type f) ~/.local/bin
cp $(find . -name cardano-cli -executable -type f) ~/.local/bin
```

### 4 Copy `e2e-automation-scripts` from this repo to `cardano-node`


### 5 Run script that creates blockchain from scratch

```bash
cardano-node$ ./scripts/shelley-from-scratch/mkfiles.sh
```

It will produce output ending with:

```bash
So you can now do various things:
 * Start the nodes
 * Submit the initial 'do it all' transaction
 * Query the node's ledger state

To start the nodes, in separate terminals use:

cardano-node run \
  --config                          example/configuration.yaml \
  --topology                        example/node-bft1/topology.json \
  --database-path                   example/node-bft1/db \
  --socket-path                     example/node-bft1/node.sock \
  --shelley-kes-key                 example/node-bft1/kes.skey \
  --shelley-vrf-key                 example/node-bft1/vrf.skey \
  --shelley-operational-certificate example/node-bft1/node.cert \
  --port                            3001
cardano-node run \
  --config                          example/configuration.yaml \
  --topology                        example/node-bft2/topology.json \
  --database-path                   example/node-bft2/db \
  --socket-path                     example/node-bft2/node.sock \
  --shelley-kes-key                 example/node-bft2/kes.skey \
  --shelley-vrf-key                 example/node-bft2/vrf.skey \
  --shelley-operational-certificate example/node-bft2/node.cert \
  --port                            3002
cardano-node run \
  --config                          example/configuration.yaml \
  --topology                        example/node-pool1/topology.json \
  --database-path                   example/node-pool1/db \
  --socket-path                     example/node-pool1/node.sock \
  --shelley-kes-key                 example/node-pool1/kes.skey \
  --shelley-vrf-key                 example/node-pool1/vrf.skey \
  --shelley-operational-certificate example/node-pool1/node.cert \
  --port                            3003

To submit the transaction

CARDANO_NODE_SOCKET_PATH=example/node-bft1/node.sock \
  cardano-cli shelley transaction submit \
    --tx-file example/tx1.tx \
    --testnet-magic 42

Then wait until epoch #2 (counting from 0) starting at slot 3000
and query the stake distribution, and see if the pool node creates blocks

CARDANO_NODE_SOCKET_PATH=example/node-bft1/node.sock \
  cardano-cli shelley query stake-distribution --testnet-magic 42

~/Projects/Node_1_13/cardano-node
```

So as the output suggests - open 3 terminals and start nodes then submit a transaction

### 6 Run some test scripts:

```
cardano-node$ ./e2e-automation-scripts/check-stake-distribution.sh
cardano-node$ ./e2e-automation-scripts/create-payment-and-stake-addresses-with-registartion.sh
```
