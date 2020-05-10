# Making a Shelley blockchain from scratch

**Last validated: 2020/05/10**

## Preliminaries

This assumes that you already have built and installed the executables from
this repository. So `cardano-cli` and `cardano-node` should be on your `$PATH`.

We also assume a Linux system, though it should work fine on OSX too.

```
$ cardano-cli version
cardano-cli 1.11.0 - linux-x86_64 - ghc-8.6
```

Everything we'll be doing uses the `shelley` sub-command
```
$ cardano-cli shelley
Usage: cardano-cli shelley COMMAND
  Shelley specific commands

Available options:
  -h,--help                Show this help text

Available commands:
  address                  Shelley address commands
  stake-address            Shelley stake address commands
  transaction              Shelley transaction commands
  node                     Shelley node operaton commands
  stake-pool               Shelley stake pool commands
  query                    Shelley node query commands
  block                    Shelley block commands
  system                   Shelley system commands
  devops                   Shelley devops commands
  genesis                  Shelley genesis block commands
```

We'll put all files under an `example` directory.

## Making a genesis file manually

To start a new blockchain we of course need a genesis file. A Shelley genesis
file is JSON file.

There is a manual method and a semi-automagic method, but we'll start by
explaining the manual method since that makes it easier to explain what the
things are and what they are for. So read this section even if you want to use
the automagic method.

So when doing it for real, we have to use the manual method since the different
steps are run by different people in different locations.

A real chain should use several genesis keys, and they should be created
separately by the members of the federation bootstrapping the system. They
should be created offline, kept offline and only the verification keys shared.

For a demo we're going to put all the keys together in once place

To start with we will set up our template directory:
```
$ cardano-cli shelley genesis create-genesis --genesis-dir example/
```
This gives us
```
$ ls example/*
example/genesis.json  example/genesis.spec.json

example/delegate-keys:

example/genesis-keys:

example/utxo-keys:
```
Note that it created both a `genesis.spec.json` and a `genesis.json`. This
command can be re-run at any time and it will re-generate the `genesis.json`
based on the `genesis.spec.json` (which we can edit by hand) and any keys
placed in the three sub-directories.

Our next steps will be to create the various keys, adjust the
`genesis.spec.json` to our liking and re-generate the `genesis.json`.

### Genesis keys

Shelley supports decentralised block production but not yet decentralised
governance, so we still have genesis keys with special governance powers.

So the first step will be to make the genesis keys.
```
$ cardano-cli shelley genesis key-gen-genesis
Usage: cardano-cli shelley genesis key-gen-genesis --verification-key-file FILEPATH
                                                   --signing-key-file FILEPATH
  Create a Shelley genesis key pair

Available options:
  --verification-key-file FILEPATH
                           Output filepath of the verification key.
  --signing-key-file FILEPATH
                           Output filepath of the signing key.
```
So lets make one example genesis key pair
```
$ cardano-cli shelley genesis key-gen-genesis \
    --verification-key-file example/genesis-keys/genesis1.vkey \
    --signing-key-file example/genesis-keys/genesis1.skey
```

### Semi-readable file formats

You can look at these files, they are semi-readable
```
$ cat example/genesis-keys/genesis1.vkey
type: Genesis verification key
title: Genesis key
cbor-hex:
 582066361ff2e1b5fdf25bc14bc48424b64fa62cee7692ed7e86216eafd3bdb28cbd
```
The "type" must not be edited. This is used as a sanity check. The "title"
field is free-form and you can use it for whatever purpose you like, such as
identifying different keys. Don't edit the binary data of course, but you
can inspect it using any CBOR tool, e.g. [cbor.me] or the `cardano-cli`
itself:
```
cardano-cli shelley text-view decode-cbor 
Usage: cardano-cli shelley text-view decode-cbor --file FILENAME
  Print a TextView file as decoded CBOR.

Available options:
  --file FILENAME          Input file.
```
Like so
```
$ cardano-cli shelley text-view decode-cbor \
    --file example/genesis-keys/genesis1.vkey

58 20 46 4e f4 95 59 f4 e3 6f b7 02 1f cb 12 71 
c5 ba 84 f3 66 22 0a 15 0e 66 bb a8 71 87 2f 27 
7c ed  # bytes(32)
```
So we can see this is just a 32 byte string. Not surprising, since this is of
course just an ed25519 verification key.

[cbor.me]: http://cbor.me/

### Genesis delegate keys

When we start a Shelley blockchain it will not be in decentralised block
production mode. Initially all blocks will be created by designated genesis
delegate nodes in the BFT overlay schedule. These genesis delegate nodes are
similar to stake pool nodes (but take part in the BFT overlay and don't get
rewards). The genesis file contains a special mapping from genesis keys to
genesis delegate keys.

So we need to make genesis delegate keys, as many as you made genesis keys
(just one in our example).
```
$ cardano-cli shelley genesis key-gen-delegate
Usage: cardano-cli shelley genesis key-gen-delegate --verification-key-file FILEPATH
                                                    --signing-key-file FILEPATH
                                                    --operational-certificate-issue-counter FILE
  Create a Shelley genesis delegate key pair

Available options:
  --verification-key-file FILEPATH
                           Output filepath of the verification key.
  --signing-key-file FILEPATH
                           Output filepath of the signing key.
  --operational-certificate-issue-counter FILE
                           The file with the issue counter for the operational
                           certificate.
```
Much the same as for genesis keys, but there is an additional output, the
operational certificate issue counter. We will talk about this later.

Lets make a genesis delegate key pair
```
$ cardano-cli shelley genesis key-gen-delegate \
    --verification-key-file example/delegate-keys/delegate1.vkey \
    --signing-key-file example/delegate-keys/delegate1.skey \
    --operational-certificate-issue-counter example/delegate-keys/delegate1.counter
```
Let's see what's in that counter file
```
$ cat example/delegate-keys/delegate1.counter
type: Node operational certificate issue counter
title: Next certificate issue number: 0
cbor-hex:
 00
```
Yes, we count from zero. We will talk about what this counter is for later.


### Initial UTxO

We need to start the system with some money or it will be very boring. The
genesis file can list number of initial addresses and values, but we need
keys for those addresses and later to sign transactions to spend the initial
UTxO values.

So we need to make genesis initial UTxO keys.
```
$ cardano-cli shelley genesis key-gen-utxo
Usage: cardano-cli shelley genesis key-gen-utxo --verification-key-file FILEPATH
                                                --signing-key-file FILEPATH
  Create a Shelley genesis UTxO key pair

Available options:
  --verification-key-file FILEPATH
                           Output filepath of the verification key.
  --signing-key-file FILEPATH
                           Output filepath of the signing key.
```

We can make as many as is useful. Lets make one.
```
$ cardano-cli shelley genesis key-gen-utxo \
    --verification-key-file example/utxo-keys/utxo1.vkey \
    --signing-key-file example/utxo-keys/utxo1.skey
```

### The genesis file itself

When we set up our template using the `create-genesis` command, it generated an
example genesis template for us in `example/genesis.spec.json`:
```
{
    "activeSlotsCoeff": 5.0e-2,
    "protocolParams": {
        "poolDecayRate": 0,
        "poolDeposit": 0,
        "protocolVersion": {
            "minor": 0,
            "major": 0
        },
        "decentralisationParam": 1,
        "maxTxSize": 16384,
        "minFeeA": 0,
        "maxBlockBodySize": 65536,
        "keyMinRefund": 0,
        "minFeeB": 0,
        "eMax": 0,
        "extraEntropy": {
            "tag": "NeutralNonce"
        },
        "maxBlockHeaderSize": 1000,
        "keyDeposit": 0,
        "keyDecayRate": 0,
        "nOpt": 100,
        "rho": 0,
        "poolMinRefund": 0,
        "tau": 0,
        "a0": 0
    },
    "protocolMagicId": 42,
    "startTime": "1970-01-01T00:00:00Z",
    "genDelegs": {},
    "updateQuorum": 5,
    "maxMajorPV": 1,
    "initialFunds": {},
    "maxLovelaceSupply": 0,
    "networkMagic": 42,
    "epochLength": 432000,
    "staking": null,
    "slotsPerKESPeriod": 129600,
    "slotLength": 1,
    "maxKESEvolutions": 60,
    "securityParam": 2160
}
```
TODO: the generated file puts the fields in an unhelpful order.

We will mostly use these defaults for this demo. The meaning of all the ones
we do not edit here will be covered elsewhere.

When we regenerate the genesis file it will fill in the:

 * `genDelegs`,
 * `initialFunds`,
 * `maxLovelaceSupply` and the
 * `startTime`.

Lets regenerate the genesis file
```
$ cardano-cli shelley genesis create-genesis --genesis-dir example/
```
and then look at it and understand what the command has done
```
$ cat example/genesis.json 
{
    "activeSlotsCoeff": 5.0e-2,
    "protocolParams": {
        "poolDecayRate": 0,
        "poolDeposit": 0,
        "protocolVersion": {
            "minor": 0,
            "major": 0
        },
        "decentralisationParam": 1,
        "maxTxSize": 16384,
        "minFeeA": 0,
        "maxBlockBodySize": 65536,
        "keyMinRefund": 0,
        "minFeeB": 0,
        "eMax": 0,
        "extraEntropy": {
            "tag": "NeutralNonce"
        },
        "maxBlockHeaderSize": 1000,
        "keyDeposit": 0,
        "keyDecayRate": 0,
        "nOpt": 100,
        "rho": 0,
        "poolMinRefund": 0,
        "tau": 0,
        "a0": 0
    },
    "protocolMagicId": 42,
    "startTime": "2020-05-10T16:28:12.17999965Z",
    "genDelegs": {
        "a4d927a8e50c7a51e0f7d41a75057073cd2fc49bfc87a44891a8a9f80800cd8a":
          "b2ee836b2b92fd3dd5e4228c943d8854e673c0510983956ce4f4ea7afcd9f761"
    },
    "updateQuorum": 5,
    "maxMajorPV": 1,
    "initialFunds": {
        "820658207c3c942eaa39dd0aec74415312cf80dfc92b34fa1d0e1c2fd2499ee105219305": 0
    },
    "maxLovelaceSupply": 0,
    "networkMagic": 42,
    "epochLength": 432000,
    "staking": null,
    "slotsPerKESPeriod": 129600,
    "slotLength": 1,
    "maxKESEvolutions": 60,
    "securityParam": 2160
}
```

The `GenDelegs` is the mapping from genesis keys to genesis delegates. The
representation in the JSON file is between key hashes.

So to understand where it got the key hashes from we can use a command to
get the key hash:
```
$ cardano-cli shelley genesis key-hash
Usage: cardano-cli shelley genesis key-hash --verification-key-file FILEPATH
  Print the identifier (hash) of a public key

Available options:
  --verification-key-file FILEPATH
                           Input filepath of the verification key.
```
Let's do that for our genesis key and genesis delegate key
```
$ cardano-cli shelley genesis key-hash \
    --verification-key-file example/genesis-keys/genesis1.vkey
  a4d927a8e50c7a51e0f7d41a75057073cd2fc49bfc87a44891a8a9f80800cd8a

$ cardano-cli shelley genesis key-hash \
    --verification-key-file example/delegate-keys/delegate1.vkey
  b2ee836b2b92fd3dd5e4228c943d8854e673c0510983956ce4f4ea7afcd9f761
```
So now we can see where the hashes from the `genDelegs` came from
```
  "genDelegs": {
    "a4d927a8e50c7a51e0f7d41a75057073cd2fc49bfc87a44891a8a9f80800cd8a":
      "b2ee836b2b92fd3dd5e4228c943d8854e673c0510983956ce4f4ea7afcd9f761"
  },
```
Next it's a similar deal with the `initialFunds`. This is a mapping from the
initial _addresses_ to the initial values at those address. So we need a
command to get the address corresponding to an initial UTxO verification key:
```
$ cardano-cli shelley genesis initial-addr
Usage: cardano-cli shelley genesis initial-addr --verification-key-file FILEPATH
  Get the address for an initial UTxO based on the verification key

Available options:
  --verification-key-file FILEPATH
                           Input filepath of the verification key.
```
So lets do that for the UTxO key
```
$ cardano-cli shelley genesis initial-addr \
    --verification-key-file example/utxo-keys/utxo1.vkey
  820658207c3c942eaa39dd0aec74415312cf80dfc92b34fa1d0e1c2fd2499ee105219305
```
And if we compare this with the `initialFunds` from the generated file we see
```
  "initialFunds": {
    "820658207c3c942eaa39dd0aec74415312cf80dfc92b34fa1d0e1c2fd2499ee105219305":
      0
  }
```
This means we'll start with 0 lovelace in a special genesis UTxO at that
address.

Ok, so zero lovelace is not that useful. We can however edit the final genesis
or we specify the initial supply when we re-generate the genesis file and it
will be split equally between all the utxo keys.
```
$ cardano-cli shelley genesis create-genesis \
    --genesis-dir example/
    --supply 1000000
```
Yes [ONE MILLION LOVELACE].

If we look again at the generated genesis file now we'll see
```
  "initialFunds": {
    "820658207c3c942eaa39dd0aec74415312cf80dfc92b34fa1d0e1c2fd2499ee105219305":
      1000000
  }
```
Or if we had more initial UTxO keys we would see them all listed and the amount
split between them.

You will also see that the `maxLovelaceSupply` is set to the same supply. If
you edit this manually note that it has to be at least as big as the sum total
from our `initialFunds`, but it can be bigger to allow for monetary expansion
later for stake rewards.

Finally there is the `startTime`, which is the agreed time of slot zero.
By default the `create-genesis` command filled this in to be 30s into the
future, but you can also specify this manually with `--start-time UTC_TIME`
or edit it manually afterwards. It needs to be set to a time in the near future
or near past. It cannot be too far in the past otherwise the system would start
having missed a very large number of slots.


## Making a genesis file semi-automagically

If you jumped straight in here, skipping the manual method, do go back and
review that section covers the concepts about what these keys are all for.

Also remember: **when doing it for real you cannot use the automagic method.**
It is not secure to because it makes all the keys in one place.
When doing it for real the people involved have to follow the manual method
where keys are generated separately on secure offline machines.

But for demos it is fine
```
$ cardano-cli shelley genesis
Usage: cardano-cli shelley genesis COMMAND
  Shelley genesis block commands

Available commands:
  key-gen-genesis          Create a Shelley genesis key pair
  key-gen-delegate         Create a Shelley genesis delegate key pair
  key-gen-utxo             Create a Shelley genesis UTxO key pair
  key-hash                 Print the identifier (hash) of a public key
  get-ver-key              Derive the verification key from a signing key
  initial-txin             Get the TxIn for an initial UTxO based on the
                           verification key
  create-genesis           Create a Shelley genesis file from a genesis template
                           and genesis/delegation/spending keys.
```

The automagic method uses the last one, `create-genesis`, and all the others
are for the manual method.
```
$ cardano-cli shelley genesis create-genesis
Usage: cardano-cli shelley genesis create-genesis --genesis-dir DIR
                                                  [--gen-genesis-keys INT]
                                                  [--gen-utxo-keys INT]
                                                  [--start-time UTC_TIME]
                                                  --supply LOVELACE
  Create a Shelley genesis file from a genesis template and
  genesis/delegation/spending keys.

Available options:
  --genesis-dir DIR        The genesis directory containing the genesis template
                           and required genesis/delegation/spending keys.
  --gen-genesis-keys INT   The number of genesis keys to make [default is 0].
  --gen-utxo-keys INT      The number of UTxO keys to make [default is 0].
  --start-time UTC_TIME    The genesis start time in YYYY-MM-DDThh:mm:ssZ
                           format. If unspecified, will be the current time +30
                           seconds.
  --supply LOVELACE        The initial coin supply in Lovelace which will be
                           evenly distributed across initial stake holders.
```
This command will generate a genesis file. It can also generate all the keys,
or it can pick up keys you created manually.

It follows this file layout convention:

 * `${genesisdir}/genesis.json`
 * `${genesisdir}/genesis.spec.json`
 * `${genesisdir}/genesis-keys/genesis${N}.{vkey,skey}`
 * `${genesisdir}/delegate-keys/delegate${N}.{vkey,skey}`
 * `${genesisdir}/delegate-keys/delegate-opcert${N}.counter`
 * `${genesisdir}/utxo-keys/utxo${N}.{vkey,skey}`

By default it will not create any keys for you, and will pick up any that you
have created manually following the file layout convention.

You can set up the directory layout with a default genesis spec file
```
$ cardano-cli shelley genesis create-genesis --genesis-dir example/
```

The `create-genesis` command can also create all the necessary keys for you.
The optional `--gen-genesis-keys` and `--gen-utxo-keys` flags can be used to
specify the number of keys of each kind to generate.

We still need a genesis spec to start from. Here's the default genesis spec
file `example/genesis.spec.json`
```
{
    "activeSlotsCoeff": 5.0e-2,
    "protocolParams": {
        "poolDecayRate": 0,
        "poolDeposit": 0,
        "protocolVersion": {
            "minor": 0,
            "major": 0
        },
        "decentralisationParam": 1,
        "maxTxSize": 16384,
        "minFeeA": 0,
        "maxBlockBodySize": 65536,
        "keyMinRefund": 0,
        "minFeeB": 0,
        "eMax": 0,
        "extraEntropy": {
            "tag": "NeutralNonce"
        },
        "maxBlockHeaderSize": 1000,
        "keyDeposit": 0,
        "keyDecayRate": 0,
        "nOpt": 100,
        "rho": 0,
        "poolMinRefund": 0,
        "tau": 0,
        "a0": 0
    },
    "protocolMagicId": 42,
    "startTime": "1970-01-01T00:00:00Z",
    "genDelegs": {},
    "updateQuorum": 5,
    "maxMajorPV": 1,
    "initialFunds": {},
    "maxLovelaceSupply": 0,
    "networkMagic": 42,
    "epochLength": 432000,
    "staking": null,
    "slotsPerKESPeriod": 129600,
    "slotLength": 1,
    "maxKESEvolutions": 60,
    "securityParam": 2160
}
```
The `create-genesis` will read the `genesis.spec.json` and produce the
`genesis.json` by filling in the:

 * `genDelegs`,
 * `initialFunds`,
 * `maxLovelaceSupply` and the
 * `startTime`.

Everything else we have to fill in manually, either in the template or
afterwards.

So lets try it:
```
$ cardano-cli shelley genesis create-genesis \
    --genesis-dir example/ \
    --supply 1000000000 \
    --gen-genesis-keys 3 \
    --gen-utxo-keys 2
```
We're going for more zeros on our money supply this time, after all
[why make trillions when we could make billions?]

Lets have a look at the result
```
$ cat example/genesis.json
{
    "decentralisationParam": 1,
    "activeSlotsCoeff": 5.0e-2,
    "protocolMagicId": 42,
    "startTime": "2020-05-07T01:46:02.884394538Z",
    "genDelegs": {
        "40a5d0f1db7ec1bcf92758f1909677576b4edf7164c94602bee0f7848495c615":
          "6e8f60c82449be3d6c17415d784881476d81ed99d88108526e5a32c8d087bdc8",
        "ed58a13ab0dd88df401c6a8db4bb0fe166c6f80b20f80905d31d0226883e4fcb":
          "6e8f60c82449be3d6c17415d784881476d81ed99d88108526e5a32c8d087bdc8",
        "60262fd1a9c700730c93a8cc855d840f8a9795d956e8ee1f6980657efa172fbb":
          "6e8f60c82449be3d6c17415d784881476d81ed99d88108526e5a32c8d087bdc8"
    },
    "updateQuorum": 5,
    "maxHeaderSize": 1400,
    "maxMajorPV": 1000,
    "maxBodySize": 16384,
    "maxLovelaceSupply": 1000000,
    "initialFunds": {
        "820658209b9d64a0bb979d2067757403262353f383809e43f48fe3d391b5ce6591e11278": 500000000,
        "82065820918195bc60cb5b91f36cc8ce97ce7d657f0e38a12eacaf135a6be9501210df6a": 500000000
    },
    "networkMagic": 42,
    "epochLength": 21600,
    "slotLength": 1,
    "slotsPerKESPeriod": 86400,
    "maxKESEvolutions": 90,
    "securityParam": 2160,
    "protocolParams": {
           "a0": 0,
           "decentralisationParam": 0.99,
           "eMax": 0,
           "extraEntropy": {
               "tag": "NeutralNonce"
           },
           "keyDecayRate": 0,
           "keyDeposit": 0,
           "keyMinRefund": 0,
           "maxBlockBodySize": 2097152,
           "maxBlockHeaderSize": 8192,
           "maxTxSize": 2048,
           "minFeeA": 0,
           "minFeeB": 0,
           "nOpt": 100,
           "poolDecayRate": 0,
           "poolDeposit": 0,
           "poolMinRefund": 0,
           "protocolVersion": {
               "major": 0,
               "minor": 0
           },
           "rho": 0,
           "tau": 0
       }
}
```
And the files it made
```
$ ls example/*/
example/delegate-keys/:
delegate1.counter  delegate2.vkey            delegate-opcert2.counter
delegate1.skey     delegate3.skey            delegate-opcert3.counter
delegate1.vkey     delegate3.vkey
delegate2.skey     delegate-opcert1.counter

example/genesis-keys/:
genesis1.skey  genesis2.skey  genesis3.skey
genesis1.vkey  genesis2.vkey  genesis3.vkey

example/utxo-keys/:
utxo1.skey  utxo1.vkey  utxo2.skey  utxo2.vkey
```

You'll notice that the automagic method has divided the total supply amongst
the initial UTxO keys, but you can still edit this file manually to adjust that
if you want.

[ONE MILLION LOVELACE]: https://www.youtube.com/watch?v=l91ISfcuzDw
[why make trillions when we could make billions?]: https://www.youtube.com/watch?v=xyyqoHCkw9I

## Node operational keys

Armed just with the genesis file we could now start a node, however it would
raise difficult philosophical questions about the nature of a blockchain with
no blocks. To avoid such questions, let's aim to have some nodes that can create
blocks. Of course this means everyone's favourite: more keys.

Shelley uses a "hot key / cold key" scheme for block producing nodes:

 * the cold key is intended to be kept securely offline (hence "cold"),
 * while the hot key is kept on the node itself and used to sign block headers.

Rather than cold and hot, we typically refer to the operator's offline key
(cold) and their operational key (hot).

The basic idea of such a scheme is that if the operational key is compromised
then a new one can be issued and the old one invalidated. This involves
establishing the link between the operator's offline key and their operational
key. This is done by means of a certificate. The certificate identifies the
current operational key, and is signed by the offline key. The certificate also
contains an issue counter number so that all other nodes can see when a new
certificate is being used and old certificates should be considered invalid.

The act of "issuing" a new certificate simply means the act of signing a new
certificate using the offline key.

To make things even more fun, Shelley uses *two* operational keys:

 * A KES key, using magic crypto;
 * A VRF key, using even more magic crypto.

They are both used in block headers. The KES key is used to prove that the
node is who it says it is, just like a normal signature. The VRF key is used
to prove that the node has the right to create a block in this slot.

The use of a VRF key is special to Ouroboros Praos. In a "normal" proof-of-stake
blockchain (like Ouroboros Classic or BFT) one simply knows who has the right to
make the block in each slot, because we *know* what the slot leader schedule is:
that is the slot leader schedule is public. So in that case you only have to
prove you are who you say you are, and everyone can check that the slot leader
schedule says if you're the slot leader or not. Ouroboros Praos has a *private*
slot leader schedule. This means that nobody knows in advance who is going to
be the slot leader, but once someone is, they can prove to everyone else that
they are. And that is what the VRF key is for: proving that.

KES stands for **K**ey **E**volving **S**ignature. It is like a normal
signature scheme, but with the "forward-security" property. The signing key is
"evolved" after a number of slots (e.g. the number of slots equivalent to 24
hours) to give a new signing key, and the old key is forgotten. It means that
if someone breaks into a server running a node, while they can steal the
current signing key, they should not be able to recover the signing keys from
earlier periods. This means that an attacker cannot sign blocks for this node
for the past, only for the current and future. This helps to prevent the
creation of large false alternative histories of the blockchain.

As is normal security practice, hot or operational keys should be cycled after
a while, and new operational keys issued. There is also a technical limitation
that KES signing keys can only be evolved a finite number of times. The larger
that choice of the maximum number of evolutions, the larger the signatures
become. The signatures are included in block headers, and for good performance
we want to keep block headers small. In Shelley, the KES keys will need to
be reissued before 90 days, but they can always be reissued earlier.

So in order to run a Shelley node we will need to:

- generate an operator's offline key;
- generate a KES operational key;
- generate a VRF operational key; and
- issue an operational certificate

The latter three are needed by the node itself, and issuing the operational
certificate needs the operator's offline key.

Now if you followed the previous section on constructing a genesis file, then
you have already generated operator offline keys: the genesis delegates are
exactly that. The other operator offline keys are stake pool keys. For most
purposes the genesis delegate and stake pool operator offline keys are the same:
both get used to issue operational certs.

We can create stake pool operator keys using:
```
$ cardano-cli shelley node key-gen
Usage: cardano-cli shelley node key-gen --verification-key-file FILEPATH
                                        --signing-key-file FILEPATH
                                        --operational-certificate-issue-counter FILE
  Create a key pair for a node operator's offline key and a new certificate
  issue counter

Available options:
  --verification-key-file FILEPATH
                           Output filepath of the verification key.
  --signing-key-file FILEPATH
                           Output filepath of the signing key.
  --operational-certificate-issue-counter FILE
                           The file with the issue counter for the operational
                           certificate.
```
For now we will ignore stake pools however since we need to get the system
bootstrapped with the BFT overlay.

### KES Keys

There's a command to generate new KES keys
```
cardano-cli shelley node key-gen-KES
Usage: cardano-cli shelley node key-gen-KES --verification-key-file FILEPATH
                                            --signing-key-file FILEPATH
  Create a key pair for a node KES operational key

Available options:
  --verification-key-file FILEPATH
                           Output filepath of the verification key.
  --signing-key-file FILEPATH
                           Output filepath of the signing key.
```

So lets go ahead and create a KES key for our first node
```
$ mkdir example/node1
$ cardano-cli shelley node key-gen-KES \
    --verification-key-file example/node1/kes.vkey \
    --signing-key-file example/node1/kes.skey
```
If you look at these files, you'll see that KES signing keys are quite chunky,
especially compared to our normal ed25519 keys.

### VRF Keys

And there's a command to generate new VRF keys
```
cardano-cli shelley node key-gen-VRF
Usage: cardano-cli shelley node key-gen-VRF --verification-key-file FILEPATH
                                            --signing-key-file FILEPATH
  Create a key pair for a node VRF operational key

Available options:
  --verification-key-file FILEPATH
                           Output filepath of the verification key.
  --signing-key-file FILEPATH
                           Output filepath of the signing key.
```

So lets do that too
```
$ cardano-cli shelley node key-gen-VRF \
    --verification-key-file example/node1/vrf.vkey \
    --signing-key-file example/node1/vrf.skey
```

## Issuing node operational certificates

Now we get to the stage of wanting to issue an operational certificate.

When doing this for real, the operator's offline key should of course be
offline, so we would issue the certificate on the offline machine with
the offline key, and copy the resulting certificate to the operational machine.

```
$ cardano-cli shelley node issue-op-cert
Usage: cardano-cli shelley node issue-op-cert --hot-kes-verification-key-file FILEPATH
                                              --cold-signing-key-file FILEPATH
                                              --operational-certificate-issue-counter FILE
                                              --kes-period NATURAL
                                              --out-file FILE
  Issue a node operational certificate

Available options:
  --hot-kes-verification-key-file FILEPATH
                           Filepath of the hot KES verification key.
  --cold-signing-key-file FILEPATH
                           Filepath of the cold signing key.
  --operational-certificate-issue-counter FILE
                           The file with the issue counter for the operational
                           certificate.
  --kes-period NATURAL     The start of the KES key validity period.
  --out-file FILE          The output file.

```
There's a few things here to understand.

As discussed above, a certificate identifies an operational KES key that we
will be using to sign block headers, so we need is verification key. It is
signed by the operator's offline key so we need that signing key.

As mentioned, certificates have an issue counter number that is used to
inform other nodes that older certificates are now invalid. This certificate
issue counter must be kept with the operator's offline key. You'll notice they
got created when we created the genesis delegate keys, or if you create a new
stake pool key.

Finally we have a confusing flag for the KES period. Each operational
certificate specifies when the certificate is valid from. This is like a date
but is specified in terms of KES periods, which is some number of slots long.
Frankly, this needs improving in the CLI tools and/or documentation. For now
we are creating a system from scratch so we can start with period 0.

So lets go ahead and issue ourselves an operational certificate. We will sign
using use the genesis delegate key we created earlier.
```
$ cardano-cli shelley node issue-op-cert \
    --hot-kes-verification-key-file example/node1/kes.vkey \
    --cold-signing-key-file example/delegate-keys/delegate1.skey \
    --operational-certificate-issue-counter example/delegate-keys/delegate1.counter \
    --kes-period 0 \
    --out-file example/node1/cert
```

## Starting a node

Now that we have generated our genesis.json, operational certificate, VRF signing key and KES signing key, we can run a node via:
--TODO: What about creation of topology?
--TODO: Probably need to add a default config.yaml for a shelley node
--TODO: I needed to manuall update my time
```
$ cardano-node run \
    --topology configuration/defaults/shelley/topology.json \
    --shelley-kes-key example/KES/signkey \
    --shelley-operational-certificate example/opcert \
    --shelley-vrf-key example/VRF/signkey \
    --config configuration/defaults/shelley/config.yaml \
    --database-path example/db \
    --port 3001 \
    --socket-path example/socket/shelley-node
```

## Submitting a transaction

Once you have a node up and running, you can submit transactions to it. You will need to:

  1. Query the genesis UTxO
  2. Create an address
  3. Build a transaction
  4. Sign the transaction
  5. Submit the transaction

#

1. Querying the genesis UTxO. This will display the addresses and their funds that you defined in your genesis.json. It will allow you to get the transaction ids and their indexes to build the transaction.

-- TODO: I had to create the socket file manually at this point and restart the node

```
cabal exec cardano-cli -- shelley query filtered-utxo \
                        --address ${initialfundaddress} \
                        --config configuration/defaults/shelley/config.yaml \
                        --socket-path example/socket/shelley-node \
                        --out-file /dev/null
```
You will get something like the following:

```
Filtered UTxO: UTxO (fromList [(TxIn (TxId {_TxId = 97681ef59a9d76c6db9247e7897d9e35c6f1c9d244ac233c0f30d02e2906549f}) 0,TxOut (Addr (KeyHashObj (KeyHash 97681ef59a9d76c6db9247e7897d9e35c6f1c9d244ac233c0f30d02e2906549f)) StakeRefNull) (Coin 1000000))])
```


2. You can send your ADA to another initial fund address or you can generate a new address to send ADA to.

- Generate a keypair for the new output address

```
mkdir example/new-address
cabal run cardano-cli:cardano-cli -- shelley address key-gen \
                                   --verification-key-file example/new-address/verkey \
                                   --signing-key-file example/new-address/signkey
```

- Build the address

```
cabal run cardano-cli:cardano-cli -- shelley address build \
                                   --verification-key-file example/new-address/verkey
```

3. Build the transaction.
- Be sure to add the correct index to the end of the txid using '#'.
- Be sure to add the amount you like to spend at the end of the output using '+'.
- Make sure the inputs = outputs + fee.

```
 cabal run cardano-cli:cardano-cli -- shelley transaction build-raw \
                                    --tx-in 97681ef59a9d76c6db9247e7897d9e35c6f1c9d244ac233c0f30d02e2906549f#0 \
                                    --tx-out 82065820debd78e08f0aa77ba6e30047f45e8335d908735f7dd73c808f56c27dc42a82a2+900000 \
                                    --ttl 1000 \
                                    --fee 100000 \
                                    --tx-body-file example/shelley-tx-body
```

4. Signing the transaction.

- You must sign the transaction with the signing key the tx input you are spending belongs to.
-- TODO: Would be nice to easily determine which keys correspond to which initial fund addresses if
-- users want to send to other initial funding addresses
```
cabal run cardano-cli:cardano-cli -- shelley transaction sign \
                                   --tx-body-file example/shelley-tx-body \
                                   --signing-key-file example/utxo-keys/utxo1.skey \
                                   --tx-file example/shelley-tx-file
```

5. Submitting the transaction

Now you can submit the transaction to the node! You can check the logs and if you've done everything right you should see...

```
cabal run cardano-cli:cardano-cli -- shelley transaction submit \
                                   --config configuration/defaults/shelley/config.yaml \
                                   --socket-path example/socket/shelley-node \
                                   --tx-filepath example/shelley-tx-file
```
