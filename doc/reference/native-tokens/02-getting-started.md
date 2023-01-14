Before you start, you should:

- have the [latest version of the node](https://github.com/input-output-hk/cardano-node/releases)
- configure the node to communicate with the [testnet environment](https://book.world.dev.cardano.org/environments.html)
- set up a relay node and [run CLI](https://github.com/input-output-hk/cardano-node#using-cardano-cli)

### Understanding values

Cardano-node v.1.32.1 and later of the CLI no longer supports ASCII token names, only hex-encoded format is supported. The result of running  

```
$ echo -n "assetName" | xxd -ps
```

Lovelace values can be specified in two ways:

- `${quantity} lovelace` (where quantity is a signed integer)
- `${quantity}` (where quantity is a signed integer)
- `${assetName}` (where assetName is hex-encoded 61737365744e616d65)

Values for other assets can be specified as:

- `${quantity} ${policyId}.${assetName}`
- `${quantity} ${policyId}` 

Where `quantity` is a signed integer and `policyId` is a hex-encoded policy ID [a script hash]), and `assetName` is a hex-encoded assetName.

#### Syntax of multi-asset values

The `cardano-cli` can specify multi-asset values in transaction outputs and when minting or burning tokens. The syntax for these values has been designed to be backwards-compatible with the previous ada-only syntax (`address+lovelace`):

- ada values are defined as integer (INT) lovelace, e.g. `42 lovelace`
- multi-asset values can be defined as:
  - `INT policyid.assetName`, e.g. `42 $MYPOLICY.61737365744e616d65`
  - `INT policyid`, e.g. `42 $MYPOLICY` (No assetName specified)
  - `policyid.assetName`, e.g `$MYPOLICY.61737365744e616d65` (This will mint only one of `assetName`)
- Multiple assets can be combined in the same multi-asset value using the `+` operator, e.g:

`100 lovelace + 42 $MYPOLICY.666f6f + -2 $MYPOLICY.626172 + 10 lovelace`

**Negating individual values**

Any individual value can be negated using the `-` prefix operator. For example:

- `-42 $MYPOLICY`
- `-72191 $MYPOLICY.666f6f`
- `-100`
- `-920 lovelace`

**Combining individual values**

Values can be combined using the binary operator `+`. For example:

- `42 lovelace + -1 (this would result in a Value of 41 lovelace)`
- `20 $MYPOLICY + 12 $MYPOLICY.666f6f + -2 $MYPOLICY.626172`
- `201 4$MYPOLICY.666f6f + 12 + -1 + 9 lovelace + 10 $MYPOLICY`

### Creating a transaction

The native tokens syntax can be used in the following contexts:

- `cardano-cli transaction build-raw --tx-out="..."`
- `cardano-cli transaction build-raw --mint="..."`

The CLI command `cardano-cli transaction build-raw` creates the transaction body. The `--tx-out` option specifies the transaction output in the usual way *(This is expressed as address+lovelace, where address is a Bech32-encoded address, and lovelace is the amount in lovelace)*, and the `--mint` option specifies the value to be minted or burnt.

#### Transaction outputs (TxOuts)

The syntax for TxOut values has been extended to include multi-asset tokens. These values can be specified in two different ways:

- `$address $value`
- `${address}+${value}`

(where *address* is a Cardano address and *value* is a value). The second form is provided for backwards compatibility with earlier versions of the node.

To receive tokens, you just need to specify any address. It is not necessary to use special addresses to hold multi-asset tokens.

To inspect the values in an address, you need to view a UTXO value using:

```
cardano-cli query utxo --testnet-magic 1
```

This will show the content of any token bundles that you possess. You can choose to see a specific address using the `--address` `$ADDRESS` option:

```
cardano-cli query utxo --address "$ADDRESS" --testnet-magic 1
```

### Token minting policies

Token minting policies are written using multi-signature scripts. This allows the asset controller to express conditions such as the need for specific token issuers to agree to mint new tokens, or to forbid minting tokens after a certain slot (if [token locking](https://docs.cardano.org/learn/about-hard-forks/#tokenlocking:shelleyprotocolupgrade) is also used).

Here’s an example of a very simple minting policy, which grants the right to mint tokens to a single key:

```
{
  "keyHash": "fe38d7...599",
  "type": "sig"
}
```

This minting policy requires any transaction that mints tokens to be witnessed by the key with the hash `fe38d7...599`. More involved examples can be found in the [multi-signature simple scripts documentation](https://github.com/input-output-hk/cardano-node/blob/c6b574229f76627a058a7e559599d2fc3f40575d/doc/reference/simple-scripts.md).

### Example: minting a new native token

#### Overview

This section describes how to manually mint a new native token ('melcoin') using cardano-cli, and send a transaction of this newly minted token to a new address. 
 
#### Pre-requisites 

1. Download the latest version of cardano-node from the releases page (https://github.com/input-output-hk/cardano-node/releases) and config files for the public testnet from the Cardano World (https://book.world.dev.cardano.org/environments.html)

2. Run the cardano-node:

```bash
./cardano-node run --topology ./lpconfig/testnet-topology.json --database-path ./state-lp --port 3001
--config ./lpconfig/testnet-config.json --socket-path ~/cardano-lp.socket

export CARDANO_NODE_SOCKET_PATH=~/cardano-lp.socket
```
3. Generate a verification key and a signing key:

```bash
cardano-cli address key-gen \
    --verification-key-file pay.vkey \
    --signing-key-file pay.skey
```

The code should output something similar to this:

```bash
$ cat pay.skey 
{
    "type": "PaymentSigningKeyShelley_ed25519",
    "description": "Payment Signing Key",
    "cborHex": "5820aed07e0b1ddd946da278ffb1f671cc5b24c8453e6b47c24b0a6b15d818444fe8"
}
$ cat pay.vkey 
{
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": "582031752dd50ffe7ed90ba136ea775dacd5113ff67d13001a25aac953f719aa1f92"
}
```

4. Generate the payment address:

```bash
./cardano-cli address build \
--payment-verification-key-file pay.vkey \
--out-file pay.addr \
--testnet-magic 1
```

This code produces the following payment address:

```bash
$ cat pay.addr 
addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz
```

5. Check the balance of the payment address:

```bash
./cardano-cli query utxo --address addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz --testnet-magic 1
```

The response should show no funds:

```bash
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

6. Fund the address: 

`curl -X POST -s 'https://faucet.preprod.world.dev.cardano.org/send-money/YOURADDRESS?api_key=YOURAPIKEY'`

and check again:

```bash
./cardano-cli query utxo --address addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz --testnet-magic 1

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b1ddb0347fed2aecc7f00caabaaf2634f8e2d17541f6237bbed78e2092e1c414     0        1000000000 lovelace
```

7. Export the protocol parameters to a file for later use:

```bash
cardano-cli  query protocol-parameters \
--testnet-magic 1 \
--out-file protocol.json
```

#### Start the minting process

1. Create a policy:

```bash

mkdir policy

cardano-cli address key-gen \
    --verification-key-file policy/policy.vkey \
    --signing-key-file policy/policy.skey


touch policy/policy.script && echo "" > policy/policy.script 


echo "{" >> policy/policy.script 
echo "  \"keyHash\": \"$(./cardano-cli address key-hash --payment-verification-key-file policy/policy.vkey)\"," >> policy/policy.script 
echo "  \"type\": \"sig\"" >> policy/policy.script 
echo "}" >> policy/policy.script 

cat ./policy/policy.script 
{
  "keyHash": "5805823e303fb28231a736a3eb4420261bb42019dc3605dd83cccd04",
  "type": "sig"
}
```

2. Mint the new asset:

```bash 

$ ./cardano-cli transaction policyid --script-file ./policy/policy.script 
328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b
```

#### Build the raw transaction

1. Use this code to build the raw transaction:

```
$ echo -n "melcoin" | xxd -ps 
6d656c636f696e

./cardano-cli transaction build-raw \
             --fee 0 \
             --tx-in b1ddb0347fed2aecc7f00caabaaf2634f8e2d17541f6237bbed78e2092e1c414#0 \
             --tx-out addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz+1000000000+"1000000000 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
             --mint="1000000000 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
             --out-file matx.raw
```

#### Calculate the minimum fee

Use this code to calculate the minimum fee required for the transaction:

```
./cardano-cli transaction calculate-min-fee \
--tx-body-file matx.raw \
--tx-in-count 1 \
--tx-out-count 1 \
--witness-count 2 \
--testnet-magic 1 \
--protocol-params-file protocol.json

180109 Lovelace

```

#### Build the transaction again

The transaction will now include the fee:

```

./cardano-cli transaction build-raw \
--fee 180109 \
--tx-in b1ddb0347fed2aecc7f00caabaaf2634f8e2d17541f6237bbed78e2092e1c414#0 \
--tx-out addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz+999819891+"1000000000 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
--mint="1000000000 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
--out-file matx.raw
```

#### Sign the transaction:

```
./cardano-cli transaction sign \
--signing-key-file pay.skey \
--signing-key-file policy/policy.skey \
--script-file policy/policy.script \
--testnet-magic 1 \
--tx-body-file matx.raw \
 --out-file matx.signed
```

#### Submit the transaction:

```bash
./cardano-cli transaction submit --tx-file  matx.signed --testnet-magic 1
```
No response, which is the expected result. Check the Utxo for 

addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz
```bash
./cardano-cli query utxo --address addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz --testnet-magic 1

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fd0790f3984348f65ee22f35480b873b4eb9862065514f3e3a9c0f04d0a6ad63     0        999821915 lovelace + 1000000000 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e
```

#### Send the new native asset to another address

1. Generate a recipient address:

First, we need to generate an address to send the newly minted asset to.

```bash
mkdir recipient
```

2. Generate the key pair:

```
cardano-cli address key-gen \
--verification-key-file recipient/recipientpay.vkey \
--signing-key-file recipient/recipientpay.skey
```    

3. Derive the payment address:

```
./cardano-cli address build \
--payment-verification-key-file recipient/recipientpay.vkey \
--out-file recipient/recipientpay.addr \
--testnet-magic 1

$ cat recipient/recipientpay.addr 
addr_test1vp8s8zu6mr73nvlsjf935k0a38n8xvp3fptkyz2vl8pserqkcx5yz
```

4. Send 1 melcoin to the recipient address:

```bash
./cardano-cli transaction build-raw \
--fee 0 \
--tx-in fd0790f3984348f65ee22f35480b873b4eb9862065514f3e3a9c0f04d0a6ad63#0 \
--tx-out addr_test1vp8s8zu6mr73nvlsjf935k0a38n8xvp3fptkyz2vl8pserqkcx5yz+10000000+"1 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
--tx-out addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz+999821915+"999000000 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
--out-file rec_matx.raw
```

5. Calculate the minimum fee.

Use this code to calculate the minimum fee for the transaction:

```
./cardano-cli transaction calculate-min-fee \
--tx-body-file rec_matx.raw \
--tx-in-count 1 \
--tx-out-count 2 \
--witness-count 1 \
--testnet-magic 1 \
--protocol-params-file protocol.json

178393 Lovelace

./cardano-cli transaction build-raw \
--fee 178393 \
--tx-in fd0790f3984348f65ee22f35480b873b4eb9862065514f3e3a9c0f04d0a6ad63#0 \
--tx-out addr_test1vp8s8zu6mr73nvlsjf935k0a38n8xvp3fptkyz2vl8pserqkcx5yz+10000000+"1 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
--tx-out addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz+989643522+"999999999 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e" \
--out-file rec_matx.raw
```

#### Sign the transaction

Sign the transaction using the keys generated earlier:
```
./cardano-cli transaction sign \
--signing-key-file pay.skey \
--testnet-magic 1 \
--tx-body-file rec_matx.raw \
--out-file rec_matx.signed
```

#### Submit the transaction

Submit the transaction to the chain:

```
./cardano-cli transaction submit --tx-file  rec_matx.signed --testnet-magic 1

```

Note that we must send more than 1000000 Lovelace in the transaction. This minimum value is specified in the config file:

```bash
$ cat lpconfig/launchpad-shelley-genesis.json | grep minUTxOValue
        "minUTxOValue": 1000000,
```

#### Check the UTXO for address addr_test1vp8s8zu6mr73nvlsjf935k0a38n8xvp3fptkyz2vl8pserqkcx5yz:

```bash
./cardano-cli query utxo --address addr_test1vp8s8zu6mr73nvlsjf935k0a38n8xvp3fptkyz2vl8pserqkcx5yz --testnet-magic 1

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f90b8457a2cf6a1aba9c0001ae2c7084f653083c6108826115a0a64e862333a3     0        10000000 lovelace + 1 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e
```
The recipient address we created now has 10000000 Lovelace and 1 melcoin.


#### Check the UTXO for address addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz:

```bash

./cardano-cli query utxo --address addr_test1vqvlku0ytscqg32rpv660uu4sgxlje25s5xrpz7zjqsva3c8pfckz --testnet-magic 1
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f90b8457a2cf6a1aba9c0001ae2c7084f653083c6108826115a0a64e862333a3     1        989643522 lovelace + 999999999 328a60495759e0d8e244eca5b85b2467d142c8a755d6cd0592dff47b.6d656c636f696e
```
The sender address now has 989643522 Lovelace and 999999999 melcoin.

### Submitting a transaction

Before submitting the transaction to the network, it needs to be signed. We need witnesses from two keys - one to spend the input `$UTXO`, and one to satisfy the minting policy script:

```
SPENDING_KEY=...
MINTING_KEY=...
TX_BODY_FILE=...
TX_FILE=...

cardano-cli transaction sign \
--signing-key-file "$SPENDING_KEY" \
--signing-key-file "$MINTING_KEY" \
--script-file "$SCRIPT" \
--testnet-magic 1 \
--tx-body-file  "$TX_BODY_FILE" \
--out-file  	"$TX_FILE"
```

Here, `$SPENDING_KEY` is the key that allows spending from `$UTXO`, and `$MINTING_KEY` is the key that hashes to the value specified in the `$SCRIPT`.

To submit a transaction to the network, use the following command:

```
cardano-cli transaction submit --tx-file  "$TX_FILE" --testnet-magic 1
```

The newly minted tokens will appear in the UTXO, and can be checked by:

```
cardano-cli query utxo --testnet-magic 1
```

The corresponding output shows the different types of asset that are embedded in the UTXO:

```
 TxHash         TxIx    	Amount


-----------------------------------------------------------------


377eab...ad7 	0    	500000000 lovelace + 5 1cc8a9...a25.couttscoin
377eab...ad7 	1    	500000000 lovelace
```

Once tokens are minted, they can be communicated using ordinary transactions, without using the `--mint` field. Note that in order to be valid, a transaction has to be *balanced*, and you should also have a minimum value of lovelace in every transaction output.

### Transferring tokens

Tokens can be sent just like ada by any token holder. There is a caveat: every transaction output *must* contain some ada. This is because there is a minimum value of ada that is needed per transaction output. This value is given by a protocol parameter. In particular, it is not possible to send *only* multi-asset tokens in a transaction, as some ada always needs to be included in each output.

For example, some `couttscoin` tokens could be sent using the following commands:

```
TXID=$(cardano-cli transaction txid --tx-body-file "$TX_BODY_FILE")
TX_BODY_FILE_1=...
TX_FILE_1=...
 
cardano-cli transaction build-raw \
--fee 0 \
--tx-in "$TXID"#0 \
--tx-out="$ADDR+$LOVELACE+5 $POLICYID.636f75747473636f696e" \
--out-file "$TX_BODY_FILE_1"
 
cardano-cli transaction sign \
--signing-key-file "$SPENDING_KEY" \
--testnet-magic 1097911063 \
--tx-body-file  "$TX_BODY_FILE_1" \
--out-file  	"$TX_FILE_1"
 
cardano-cli transaction submit --tx-file "$TX_FILE_1" --testnet-magic 1
```

### Buying and spending tokens

Token holders “buy” tokens from a token issuer. This will usually involve sending some ada to a specific address that has been set up by the token issuer and informing the token issuer about the address where the tokens should be sent. The token issuer will then set up a transaction that will transfer a multi-asset token to the specified address.

Tokens that have been issued to a token holder can be “spent” by returning them to a token issuer (i.e. by redeeming the tokens). This is done using a normal transaction. The token issuer will then provide the token holder with the agreed object in return (which may be an item of value, a service, a different kind of token, some ada, etc).

```
cardano-cli transaction build-raw ... --out-file txbody
 
cardano-cli transaction sign ... --tx-body-file txbody --out-file tx

cardano-cli transaction submit ... --tx-file tx 
```

### Destroying (burning) tokens

Tokens can be destroyed by a token issuer according to the token policy by supplying a negative value in the `--mint` field. That allows acquiring tokens in the UTXO entry in the input of a transaction, without adding them to one of the outputs, effectively destroying them. For example, tokens created in the previous section can be destroyed as follows:

```
TXID1=$(cardano-cli transaction txid --tx-body-file "$TX_BODY_FILE_1")
TX_BODY_FILE_2=...
TX_FILE_2=...
 
cardano-cli transaction build-raw \
--fee 0 \
--tx-in "$TXID1"#0 \
--tx-out="$ADDR+$LOVELACE" \
--mint="-5 $POLICYID.636f75747473636f696e" \
--out-file "$TX_BODY_FILE_2"
 
cardano-cli transaction sign \
--signing-key-file "$SPENDING_KEY" \
--signing-key-file "$MINTING_KEY" \
--script-file "$SCRIPT" \
--testnet-magic 1 \
--tx-body-file  "$TX_BODY_FILE_2" \
--out-file  	"TX_FILE_2"
 
cardano-cli transaction submit --tx-file  "$TX_FILE_2" --testnet-magic 1097911063
```

> Note: Destroying tokens requires both the payment credential for using the UTXO entry with the tokens, *and* a credential for the minting policy script.

