# Create Cardano

In [Shelley genesis](doc/reference/shelley-genesis.md) we discussed how to manually create a Shelley blockchain and how to create a testnet semi-automatically with `cardano-cli genesis create`. That does not cover what to do when we need a testnet that starts in Byron era and is updated to the latest era. This requires a fair amount of manual work: converting genesis and delegate keys from Byron to Shelley, creating genesis files, handling file hashing and manually updating the configuration file, etc.

Creating a testnet that starts in Byron and can easily transition to Shelley and later eras is easy with `cardano-cli genesis create-cardano` command. Keep in mind that on a production network, we need to use the manual method described in [Shelley genesis](./shelley-genesis.md).

`create-cardano` automatically generates the Byron, Shelley and Alonzo genesis files, together with the needed genesis keys, delegate keys, utxo keys; it also handles all the hashing and generates the configuration file for the node.  

`create-cardano` requires us to provide template files for node configuration file, Byron genesis, Shelley genesis and Alonzo genesis. These template files contain the parameters we want for our testnet for each era and the configuration file for the nodes. You can find template files in the [cardano-world repository](https://github.com/input-output-hk/cardano-world/tree/master/nix/cardano/environments/testnet-template) and edit them to your needs.

The help for `create-cardano` shows the parameters that it takes:

```bash
$ cardano-cli genesis create-cardano

--genesis-dir DIR        The genesis directory containing the genesis template
                         and required genesis/delegation/spending keys.
--gen-genesis-keys INT   The number of genesis keys to make [default is 3].
--gen-utxo-keys INT      The number of UTxO keys to make [default is 0].
--start-time UTC-TIME    The genesis start time in YYYY-MM-DDThh:mm:ssZ
                         format. If unspecified, will be the current time +30
                         seconds.
--supply LOVELACE        The initial coin supply in Lovelace which will be
                         evenly distributed across initial, non-delegating
                         stake holders.
--security-param INT     Security parameter for genesis file [default is 108].
--slot-length INT        slot length (ms) parameter for genesis file [default
                         is 1000].
--slot-coefficient RATIONAL
                         Slot Coefficient for genesis file [default is .05].
--mainnet                Use the mainnet magic id.
--testnet-magic NATURAL  Specify a testnet magic id.
--byron-template FILEPATH
                         JSON file with genesis defaults for each byron.
--shelley-template FILEPATH
                         JSON file with genesis defaults for each shelley.
--alonzo-template FILEPATH
                         JSON file with genesis defaults for each alonzo.
--node-config-template FILEPATH
                         the node config template

```

A few things to consider:

* The _maximum supply_ is hardcoded to 45 billion ADA (like in mainnet). The amount in `--supply` is distributed evenly across initial UTXO keys. The difference between 45 billion and `--supply` will be available on the _Reserves_ when updating to the Shelley era.

* `--slot-length`, `--security-param` and `--slot-coefficient` together determine the epoch length on the resulting network. Byron epochs last _10k_ slots, and Shelley epochs last _10k/f_ slots. Where _k_ is the security parameter and _f_ is the slot coefficient.

### Example

For a network with 3 genesis keys, 3 delegate nodes, 2 non-delegated UTXO keys with 5 billion each and 10 minutes epochs, run:

```bash
$ cardano-cli genesis create-cardano \
--genesis-dir cluster \
--gen-genesis-keys 3 \
--gen-utxo-keys 2 \
--supply 10000000000000000 \
--security-param 300 \
--slot-length 100 \
--slot-coefficient 5/100 \
--testnet-magic 42 \
--byron-template byron.json \     
--shelley-template shelley.json \
--alonzo-template alonzo.json \
--node-config-template config.json
```

This creates the following:

* The "cluster" directory
* Byron, Shelley and Alonzo genesis files
* Node configuration file
* 3 Byron era genesis keys
* 3 Shelley era genesis keys (converted from Byron keys)
* 3 Delegate Byron keys
* 3 Delegation certificates
* 3 Operational certificates and operational certificate counters
* 3 Cold, KES and VRF keys
* 2 Byron era non delegated UTXO keys
* 2 Shelley era UTXO keys (Converted from Byron keys)  

```bash
$ tree cluster/
cluster/
├── alonzo-genesis.json
├── byron-genesis.json
├── delegate-keys
│   ├── byron.000.cert.json
│   ├── byron.000.key
│   ├── byron.001.cert.json
│   ├── byron.001.key
│   ├── byron.002.cert.json
│   ├── byron.002.key
│   ├── shelley.000.counter.json
│   ├── shelley.000.kes.skey
│   ├── shelley.000.kes.vkey
│   ├── shelley.000.opcert.json
│   ├── shelley.000.skey
│   ├── shelley.000.vkey
│   ├── shelley.000.vrf.skey
│   ├── shelley.000.vrf.vkey
│   ├── shelley.001.counter.json
│   ├── shelley.001.kes.skey
│   ├── shelley.001.kes.vkey
│   ├── shelley.001.opcert.json
│   ├── shelley.001.skey
│   ├── shelley.001.vkey
│   ├── shelley.001.vrf.skey
│   ├── shelley.001.vrf.vkey
│   ├── shelley.002.counter.json
│   ├── shelley.002.kes.skey
│   ├── shelley.002.kes.vkey
│   ├── shelley.002.opcert.json
│   ├── shelley.002.skey
│   ├── shelley.002.vkey
│   ├── shelley.002.vrf.skey
│   └── shelley.002.vrf.vkey
├── genesis-keys
│   ├── byron.000.key
│   ├── byron.001.key
│   ├── byron.002.key
│   ├── shelley.000.skey
│   ├── shelley.000.vkey
│   ├── shelley.001.skey
│   ├── shelley.001.vkey
│   ├── shelley.002.skey
│   └── shelley.002.vkey
├── node-config.json
├── shelley-genesis.json
└── utxo-keys
   ├── byron.000.key
   ├── byron.001.key
   ├── shelley.000.skey
   ├── shelley.000.vkey
   ├── shelley.001.skey
   └── shelley.001.vkey

4 directories, 53 files
```

Starting the cluster requires topology files for each of the nodes. For example:

```JSON
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3001,
       "valency": 1
     },
     {
       "addr": "127.0.0.1",
       "port": 3002,
       "valency": 1
     }
   ]
 }
```
Note: For details about topology files please refer to [Understanding configuration files](../getting-started/understanding-config-files.md)


Then we run the nodes with block production keys, for example:

```bash
$ cardano-node run \
--config node-config.json \
--topology topology.json \
--database-path db \
--socket-path node.socket \
--port 3000 \
--delegation-certificate delegate-keys/byron.000.cert.json \
--signing-key delegate-keys/byron.000.key
```  

Updating the testnet to later eras can be done via update proposals, please refer to [Cardano governance](./cardano-governance.md) to learn how to do it.
