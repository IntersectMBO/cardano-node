---
description: >-
  cardano-node v.8.0.0 introduced a new subset of commands to conduct polls
  among stake pool operators. A poll is official when it is signed by a genesis
  delegate key.
---

# Polls

## Creating a poll

```
cardano-cli governance create-poll \
--question "Question to ask" \
--answer "Option A" \
--answer "Option B" \
--answer "Option C" \
--nonce 20231501 \
--out-file poll.cbor > poll.json
```

`--nonce`  is an optional but highly recommended option. It takes a UINT (used as a unique identifier) allowing the same question to be asked multiple times at different time.

`--out-file` creates a serialized version of the poll that is suitable for distribution among stake pool operators for answering the poll.

`>` redirects the output to a file to save the metadata in JSON. This file will be included in a transaction.

```
Poll created successfully.
Please submit a transaction using the resulting metadata.

Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' from the build or build-raw commands.
Hint (2): You can redirect the standard output of this command to a JSON file to capture metadata.

Note: A serialized version of the poll suitable for sharing with participants has been generated at 'poll.cbor'.

```

Let's take a look at both files:

```
cat poll.cbor

{
    "type": "GovernancePoll",
    "description": "An on-chain poll for SPOs: Question to ask",
    "cborHex": "a1185ea300816f5175657374696f6e20746f2061736b018381684f7074696f6e204181684f7074696f6e204281684f7074696f6e2043615f1a0134b54d"
}
```
```
cat poll.json

{
    "94": {
        "map": [
            {
                "k": {
                    "int": 0
                },
                "v": {
                    "list": [
                        {
                            "string": "Question to ask"
                        }
                    ]
                }
            },
            {
                "k": {
                    "int": 1
                },
                "v": {
                    "list": [
                        {
                            "list": [
                                {
                                    "string": "Option A"
                                }
                            ]
                        },
                        {
                            "list": [
                                {
                                    "string": "Option B"
                                }
                            ]
                        },
                        {
                            "list": [
                                {
                                    "string": "Option C"
                                }
                            ]
                        }
                    ]
                }
            },
            {
                "k": {
                    "string": "_"
                },
                "v": {
                    "int": 20231501
                }
            }
        ]
    }
}
```


Participants (SPOs) will use the `poll.cbor` file to create and submit their responses.

The _delegate-key-holder_ who proposes the poll will use `poll.json` to publish the poll in a transaction.

To build such a transaction, run:

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 42 \
--tx-in <TxID#TxIx> \
--change-address $(cat payment.addr) \
--metadata-json-file poll.json \
--json-metadata-detailed-schema \
--required-signer-hash $(cat delegate.hash) \
--out-file question.tx
```

**Note**: When building the transaction, we can use `--required-signer-hash` or `--required-signer`. In our example we used `--required-signer-hash` because, in a real-world scenario, the delegate signing keys are stored in cold storage, and the build command requires access to a live node.. 

To get the hash of a delegate key, run:

```
cardano-cli genesis key-hash --verification-key-file delegate.vkey
0f455663bd57b2145bcea12302664a842bd4b8e69a1e05bb9f8e45ed
```

Sign the transaction with the delegate signing key and with a payment signing key to pay for the transaction fees:

```
cardano-cli transaction sign \
--tx-body-file question.tx \
--signing-key-file delegate.skey \
--signing-key-file payment.skey \
--testnet-magic 42 \
--out-file question.tx.signed
```

When inspecting the transaction (question.tx.signed), run:

```
cardano-cli transaction view --tx-file question.tx.signed
```

You should see something like this:

```
auxiliary scripts: null
certificates: null
collateral inputs: []
era: Babbage
fee: 174257 Lovelace
inputs:
- c8f62577433c2f306dce34481af3848513ad13f3f00c5d61dd1ed1d189605453#0
metadata:
  '94':
  - - 0
    - - Question to ask
  - - 1
    - - - Option A
      - - Option B
      - - Option C
  - - _
    - 20231501
mint: null
outputs:
- address: addr_test1vphng0rf3cp262ajfs8pmhcsjdygqkpwg04n4cn97mlxfdsy2cnls
  address era: Shelley
  amount:
    lovelace: 299999825743
  datum: null
  network: Testnet
  payment credential key hash: 6f343c698e02ad2bb24c0e1ddf10934880582e43eb3ae265f6fe64b6
  reference script: null
  stake reference: null
reference inputs: []
required signers (payment key hashes needed for scripts):
- 7af230bbf76f77034a67cff0f6e56b795bbef4115d187ad698602d15
return collateral: null
total collateral: null
update proposal: null
validity range:
  lower bound: null
  upper bound: null
withdrawals: null
witnesses:
- key: VKey (VerKeyEd25519DSIGN "41b7dc7d9a5b5a7029771ea002c7f3e916cb49f32e7396b451e4417057515d47")
  signature: SignedDSIGN (SigEd25519DSIGN "1112e1e5965bc66f2701e57be5cb49785d9c4c87ed91c1bd392832b69dbed5775565ccec5830120ba64b6067dfda192d4cdac7bf5aa4e9b8f14b502c2429cd04")
- key: VKey (VerKeyEd25519DSIGN "4127bc46ea0d36bea6ff6b2ad8f6f533158851a3550318b7d445b6b6d26bdcff")
  signature: SignedDSIGN (SigEd25519DSIGN "02e465c0f9e16f0d32a073cc8d7938f1fbc792150d2f4e1b4f8f98e8eb8aa0a9381e04136b39c8bacfb9d16e403952d7ac2a41a4904805691ab02ce372fa4109")
```
Note that **required signers** include the hash of the delegate key; and **witnesses** include the delegate and payment keys' data. 

Finally, submit the transaction as usual:

```
cardano-cli transaction submit \
--testnet-magic 42 \
--tx-file question.tx.signed
```

You can use _**DB Sync**_ to check how the transaction was registered online:

```
cexplorer=# SELECT * FROM tx_metadata WHERE key = 94;
```

```
(END)
 id | key |                                            json                                            |                                                            bytes                                                             | tx_id
----+-----+--------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------+-------
  1 |  94 | {"0": ["Question to ask"], "1": [["Option A"], ["Option B"], ["Option C"]], "_": 20231501} | \xa1185ea300816f5175657374696f6e20746f2061736b018381684f7074696f6e204181684f7074696f6e204281684f7074696f6e2043615f1a0134b54d |    11
(1 row)
```

```
cexplorer=# SELECT * FROM extra_key_witness;
```

```
 id |                            hash                            | tx_id
----+------------------------------------------------------------+-------
  1 | \x7af230bbf76f77034a67cff0f6e56b795bbef4115d187ad698602d15 |    11
(1 row)
```

Of course, the hash matches the hash of the delegate key. This ensures that when SPOs encounter a poll signed with any of the delegate keys (verified by the delegate key hashes), they know that it is an official poll.

## Answering the poll

Use `answer-poll` to create a response, you can use the `--answer` option to promptly record your response by specifying its index, or you can omit it to access the interactive method.

Note that in either case, the output is being redirected to `poll-answer.json`.

The proponents of the poll will have distributed the `poll.cbor` file from above. As an SPO, you will need it to answer the poll. It will look like this:

```
cat poll.cbor

{
    "type": "GovernancePoll",
    "description": "An on-chain poll for SPOs: Question to ask",
    "cborHex": "a1185ea300816f5175657374696f6e20746f2061736b018381684f7074696f6e204181684f7074696f6e204281684f7074696f6e2043615f1a0134b54d"
}
```
### Responding interactively

As before, we will use **>** to redirect the output to a file, let's name it `poll-answer.json`:

```
cardano-cli governance answer-poll \
--poll-file poll.cbor > poll-answer.json
Question to ask
[0] Option A
[1] Option B
[2] Option C

Please indicate an answer (by index): 0 
```

Once you select your response, in our case **Option A**  which corresponds to **index 0**, you get:

```
Poll answer created successfully.
Please submit a transaction using the resulting metadata.
To be valid, the transaction must also be signed using a valid key
identifying your stake pool (eg, your cold key).


Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' from the build or build-raw commands.
Hint (2): You can redirect the standard output of this command to a JSON file to capture metadata.
```

### Using --answer

We have the option to bypass the interactive screen by providing our response directly using the `--answer <Index>` flag. For instance, if we vote for **Option A**, which corresponds to index [0], we can use the following command: 

```
cardano-cli governance answer-poll \
--poll-file poll.cbor \
--answer 0 > poll-answer.json
```
which immediately outputs:

```
Question to ask
→ Option A

Poll answer created successfully.
Please submit a transaction using the resulting metadata.
To be valid, the transaction must also be signed using a valid key
identifying your stake pool (e.g. your cold key).


Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' from the build or build-raw commands.
Hint (2): You can redirect the standard output of this command to a JSON file to capture metadata.
```

### Submitting the response in a transaction

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 42 \
--tx-in <TxId#TxIx> \
--change-address $(cat payment.addr) \
--metadata-json-file poll-answer.json \
--json-metadata-detailed-schema \
--required-signer-hash <pool hex id> \
--out-file answer.tx
```

```
cardano-cli transaction sign \
--tx-body-file answer.tx \
--signing-key-file cold.skey \
--signing-key-file payment.skey \
--testnet-magic 42 \
--out-file answer.tx.signed
```

When we inspect the signed transaction, we can see that **required signers** contain our cold key hash (the pool ID), and **witnesses** contain both the cold and payment key data:

```
cardano-cli transaction view --tx-file answer.tx.signed
```

```
auxiliary scripts: null
certificates: null
collateral inputs: []
era: Babbage
fee: 173377 Lovelace
inputs:
- 325a405f86807f1a2a68b40e54833ca7261850beadf049d3f2efc9628bd266a2#0
metadata:
  '94':
  - - 2
    - '";''\250\168\168\134b\221\245p\200\233\201u4.\\\SYN\129nW\193\254\&6.\b;\202\151(\191u"'
  - - 3
    - 0
mint: null
outputs:
- address: addr_test1vphng0rf3cp262ajfs8pmhcsjdygqkpwg04n4cn97mlxfdsy2cnls
  address era: Shelley
  amount:
    lovelace: 299999652366
  datum: null
  network: Testnet
  payment credential key hash: 6f343c698e02ad2bb24c0e1ddf10934880582e43eb3ae265f6fe64b6
  reference script: null
  stake reference: null
reference inputs: []
required signers (payment key hashes needed for scripts):
- d7277451057c5583e46214f10bd8478bcaa284bd9e14a8808c18d82b
return collateral: null
total collateral: null
update proposal: null
validity range:
  lower bound: null
  upper bound: null
withdrawals: null
witnesses:
- key: VKey (VerKeyEd25519DSIGN "41b7dc7d9a5b5a7029771ea002c7f3e916cb49f32e7396b451e4417057515d47")
  signature: SignedDSIGN (SigEd25519DSIGN "19309c132f5310cb858bbea8c0e42a72ca26a34cdb59394acbb1868d9de4bb9077cfcfdeff1e9a7256e451a0024e986000f7aa32a0422fde28ee79ca0d417709")
- key: VKey (VerKeyEd25519DSIGN "8cc324e93993814a1b4bc55fe8eb5d259e080895098cc6489017b51ce6ffa1b1")
  signature: SignedDSIGN (SigEd25519DSIGN "8e42893bc96141bf47e2a7050470e357450ee267c11a663fa5470d9843d2ea001bef4f62856fed88e6cef04d3023c92d1034c65a6af833eab63c5d7b87c7c403")
```

```
cardano-cli transaction submit \
--testnet-magic 42 \
--tx-file answer.tx.signed
```

We can use _**DB Sync**_ again to track responses:

```
SELECT * FROM tx_metadata WHERE key = 94;
```

```
 id | key |                                            json                                            |                                                            bytes                                                             | tx_id
----+-----+--------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------+-------
  1 |  94 | {"0": ["Question to ask"], "1": [["Option A"], ["Option B"], ["Option C"]], "_": 20231501} | \xa1185ea300816f5175657374696f6e20746f2061736b018381684f7074696f6e204181684f7074696f6e204281684f7074696f6e2043615f1a0134b54d |    11
  2 |  94 | {"2": "0x3b27faa8a88662ddf570c8e9c975342e5c16816e57c1fe362e083bca9728bf75", "3": 0}        | \xa1185ea20258203b27faa8a88662ddf570c8e9c975342e5c16816e57c1fe362e083bca9728bf750300                                         |    12
(2 rows)
```

```
SELECT * FROM extra_key_witness;
```

```
 id |                            hash                            | tx_id
----+------------------------------------------------------------+-------
  1 | \x7af230bbf76f77034a67cff0f6e56b795bbef4115d187ad698602d15 |    11
  2 | \xd7277451057c5583e46214f10bd8478bcaa284bd9e14a8808c18d82b |    12
(2 rows)
```

### Verifying answers <a href="#verifying-answers" id="verifying-answers"></a>

Finally, it is possible to verify the answers observed on-chain by using the `governance verify-poll` command. The term 'verify' in this context is two-folds:

* It checks that an answer is valid within the context of a given survey
* It returns the list of signatories key hashes found in the transaction;\
  in the case of a valid submission, one key hash will correspond to a known\
  stake pool ID.

```
cardano-cli governance verify-poll \
--poll-file poll.cbor \
--tx-file answer.tx.signed
Found valid poll answer with 1 signatories
[
    "d7277451057c5583e46214f10bd8478bcaa284bd9e14a8808c18d82b"
]
```
