# Register a Stake Pool with Metadata

Make sure you have access to:

| File | Content |
| :--- | :--- |
| `payment.vkey` | payment verification key |
| `payment.skey` | payment signing key |
| `stake.vkey` | staking verification key |
| `stake.skey` | staking signing key |
| `stake.addr` | registered stake address |
| `payment.addr` | funded address linked to `stake` |
| `cold.vkey` | cold verification key |
| `cold.skey` | cold signing key |
| `cold.counter` | issue counter |
| `node.cert` | operational certificate |
| `kes.vkey` | KES verification key |
| `kes.skey` | KES signing key |
| `vrf.vkey` | VRF verification key |
| `vrf.skey` | VRF signing key |

Registering your stake pool requires:

* Create JSON file with your metadata and store it in the node and in a url you maintain
* Get the hash of your JSON file
* Generate the stake pool registration certificate
* Create a delegation certificate pledge
* Submit the certificates to the blockchain

**WARNING:** Generating the **stake pool registration certificate** and the **delegation certificate** requires the **cold keys**. So, when doing this on mainnet you may want to generate these certificates in your local machine taking the proper security measures to avoid exposing your cold keys to the internet.

#### Create a JSON file with your pool's metadata

      {
      "name": "TestPool",
      "description": "The pool that tests all the pools",
      "ticker": "TEST",
      "homepage": "https://teststakepool.com"
      }


Store the file in a url you control. For example [https://teststakepool.com/poolMetadata.json](https://git.io/JJWdJ) You can use a GIST in github, make sure that the URL is less than 65 characters long.

#### Get the hash of your metadata JSON file:

This validates that the JSON fits the required schema, if it does, you will get the hash of your file.

    cardano-cli stake-pool metadata-hash --pool-metadata-file pool_Metadata.json

    >6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af


#### Generate Stake pool registration certificate

    cardano-cli stake-pool registration-certificate \
    --cold-verification-key-file cold.vkey \
    --vrf-verification-key-file vrf.vkey \
    --pool-pledge <AMOUNT TO PLEDGE IN LOVELACE> \
    --pool-cost <POOL COST PER EPOCH IN LOVELACE> \
    --pool-margin <POOL COST PER EPOCH IN PERCENTAGE> \
    --pool-reward-account-verification-key-file stake.vkey \
    --pool-owner-stake-verification-key-file stake.vkey \
    --mainnet \
    --pool-relay-ipv4 <RELAY NODE PUBLIC IP> \
    --pool-relay-port <RELAY NODE PORT> \
    --metadata-url https://git.io/JJWdJ \
    --metadata-hash <POOL METADATA HASH> \
    --out-file pool-registration.cert


| Parameter | Explanation |
| :--- | :--- |
| cold-verification-key-file | verification _cold_ key |
| vrf-verification-key-file | verification _VRS_ key |
| pool-pledge | pledge lovelace |
| pool-cost | operational costs per epoch lovelace |
| pool-margin | operator margin |
| pool-reward-account-verification-key-file | verification staking key for the rewards |
| pool-owner-staking-verification-key-file | verification staking keys for the pool owners |
| pool-relay-ipv4 | relay node ip address |
| pool-relay-port | port |
| metadata-url | url of your json file |
| metadata-hash | the hash of pools json metadata file |
| out-file | output file to write the certificate to |

**You can use a different key for the rewards, and can provide more than one owner key if there were multiple owners who share the pledge.**

The **pool-registration.cert** file should look like this:


    type: CertificateShelley
    description: Stake Pool Registration Certificate
    cborHex:
    18b58a03582062d632e7ee8a83769bc108e3e42a674d8cb242d7375fc2d97db9b4dd6eded6fd5820
    48aa7b2c8deb8f6d2318e3bf3df885e22d5d63788153e7f4040c33ecae15d3e61b0000005d21dba0
    001b000000012a05f200d81e820001820058203a4e813b6340dc790f772b3d433ce1c371d5c5f5de
    46f1a68bdf8113f50e779d8158203a4e813b6340dc790f772b3d433ce1c371d5c5f5de46f1a68bdf
    8113f50e779d80f6

#### Generate delegation certificate pledge

To honor your pledge, create a _delegation certificate_:

    cardano-cli stake-address delegation-certificate \
    --stake-verification-key-file stake.vkey \
    --cold-verification-key-file cold.vkey \
    --out-file delegation.cert

This creates a delegation certificate which delegates funds from all stake addresses associated with key `stake.vkey` to the pool belonging to cold key `cold.vkey`. If there are many staking keys as pool owners in the first step, we need delegation certificates for all of them.

#### Submit the pool certificate and delegation certificate to the blockchain

To submit the `pool registration certificate` and the `delegation certificates` to the blockchain by including them in one or more transactions. We can use one transaction for multiple certificates, the certificates will be applied in order.

#### Draft the transaction

    cardano-cli transaction build-raw \
    --tx-in <TxHash>#<TxIx> \
    --tx-out $(cat payment.addr)+0 \
    --invalid-hereafter 0 \
    --fee 0 \
    --out-file tx.draft \
    --certificate-file pool-registration.cert \
    --certificate-file delegation.cert

#### Calculate the fees

    cardano-cli transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 3 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

For example:

    > 184685

Registering a stake pool requires a deposit. This amount is specified in `protocol.json`. For example, for Shelley Mainnet we have:

"poolDeposit": 500000000

#### Calculate the change for --tx-out
All amounts in Lovelace

    expr <UTxO BALANCE> - <poolDeposit> - <TRANSACTION FEE>

#### Build the transaction:

    cardano-cli transaction build-raw \
    --tx-in <TxHash>#<TxIx> \
    --tx-out $(cat payment.addr)+<CHANGE IN LOVELACE> \
    --invalid-hereafter <TTL> \
    --fee <TRANSACTION FEE> \
    --out-file tx.raw \
    --certificate-file pool-registration.cert \
    --certificate-file delegation.cert

#### Sign the transaction:

    cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file stake.skey \
    --signing-key-file cold.skey \
    --mainnet \
    --out-file tx.signed

#### Submit the transaction:

    cardano-cli transaction submit \
    --tx-file tx.signed \
    --mainnet


#### Verify that your stake pool registration was successful.

Get Pool ID

    cardano-cli stake-pool id --cold-verification-key-file cold.vkey

Check for the presence of your poolID in the network ledger state, with:

    cardano-cli query ledger-state --mainnet --allegra-era | grep publicKey | grep <poolId>
