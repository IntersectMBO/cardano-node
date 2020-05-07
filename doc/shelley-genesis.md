# Making a Shelley blockchain from scratch

**Last validated: 2020/05/07**

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
```
$ mkdir example
```

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
```
$ mkdir example/{genesis-keys,delegate-keys,utxo-keys}
```

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
cardano-cli shelley genesis key-gen-genesis \
  --verification-key-file example/genesis-keys/genesis1.vkey \
  --signing-key-file example/genesis-keys/genesis1.skey
```

### Semi-readable file formats

You can look at these files, they are semi-readable
```
$ cat example/genesis1.vkey 
type: Genesis verification key
title: Genesis key
cbor-hex:
 582066361ff2e1b5fdf25bc14bc48424b64fa62cee7692ed7e86216eafd3bdb28cbd
```
The "type" must not be edited. This is used as a sanity check. The "title"
field is free-form and you can use it for whatever purpose you like, such as
identifying different keys. Don't edit the binary data of course, but you
can inspect it using any CBOR tool, e.g. http://cbor.me/
```
58 20                                   # bytes(32)
   66361FF2E1B5FDF25BC14BC48424B64FA62CEE7692ED7E86216EAFD3BDB28CBD
    # "f6\x1F\xF2\xE1\xB5\xFD\xF2[\xC1K\xC4\x84$\xB6O\xA6,\xEEv\x92\xED~\x86!n\xAF\xD3\xBD\xB2\x8C\xBD"
```
So we can see this is just a 32 byte string. Not surprising, since this is of
course just an ed25519 verification key.

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
$ cat example/delegate1.counter
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

Here is a starting template. We will mostly use these defaults for this demo.
The meaning of all the ones we do not edit here will be covered elsewhere.
```
{
   "StartTime":"1970-01-01T00:00:00Z",
   "NetworkMagic":42,
   "ProtocolMagicId":42,

   "SlotLength":1,
   "ActiveSlotsCoeff": 5.0e-2,
   "DecentralisationParam":1,
   "SecurityParam":2160,
   "EpochLength":21600,
   "SlotsPerKESPeriod":86400,
   "MaxKESEvolutions":90,
   "UpdateQuorum":5,

   "MaxMajorPV":1000,
   "MaxBodySize":16384,
   "MaxHeaderSize":1400,

   "GenDelegs":{},
   "InitialFunds":{},
   "MaxLovelaceSupply":0
}
```
We will fill in the:

 * `GenDelegs`,
 * `InitialFunds`,
 * `MaxLovelaceSupply` and the
 * `StartTime`.

The `GenDelegs` is the mapping from genesis keys to genesis delegates. The
representation in the JSON file is between key hashes. So we need a command to
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
So these are the hashes we'll copy into the `GenDelegs`
```
  "GenDelegs": {
    "a4d927a8e50c7a51e0f7d41a75057073cd2fc49bfc87a44891a8a9f80800cd8a":
      "b2ee836b2b92fd3dd5e4228c943d8854e673c0510983956ce4f4ea7afcd9f761"
  },
```
Next it's a similar deal with the `InitialFunds`. This is a mapping from the
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
We can copy that into the `InitialFunds`
```
  "InitialFunds": {
    "820658207c3c942eaa39dd0aec74415312cf80dfc92b34fa1d0e1c2fd2499ee105219305":
      1000000,
  }
```
This means we'll start with 1000000 lovelace in a special genesis UTxO at that
address.

We need to set the `MaxLovelaceSupply` to be at least as big as the sum total
from our `InitialFunds`, but it can be bigger to allow for monetary expansion
used later for stake rewards.

Finally the `StartTime`, which is the agreed time of slot zero, needs to be
set to a time in the near future or near past. It cannot be too far in the past
otherwise the system would start having missed a very large number of slots.

Here's our final example
```
{
   "StartTime":"2020-05-07T00:00:00Z",
   "NetworkMagic":42,
   "ProtocolMagicId":42,

   "SlotLength":1,
   "ActiveSlotsCoeff": 5.0e-2,
   "DecentralisationParam":1,
   "SecurityParam":2160,
   "EpochLength":21600,
   "SlotsPerKESPeriod":86400,
   "MaxKESEvolutions":90,
   "UpdateQuorum":5,

   "MaxMajorPV":1000,
   "MaxBodySize":16384,
   "MaxHeaderSize":1400,

    "GenDelegs": {
      "a4d927a8e50c7a51e0f7d41a75057073cd2fc49bfc87a44891a8a9f80800cd8a":
        "b2ee836b2b92fd3dd5e4228c943d8854e673c0510983956ce4f4ea7afcd9f761"
    },

    "InitialFunds": {
      "820658207c3c942eaa39dd0aec74415312cf80dfc92b34fa1d0e1c2fd2499ee105219305":
        1000000,
    }

   "MaxLovelaceSupply":1000000
}
```

Now in truth we could have automated that last copy and pasting step. Provided
that we follow exactly the file layout convention above, we can use the
`create-genesis` command to do the final genesis file construction, even if
we make all the keys manually.


## Making a genesis file semi-automagically

If you jumped straight in here, skipping the manual method, do go back and
review that section covers the concepts about what these keys are all for.

Also remember: **when doing it for real you cannot use the automagic method.**
It is not secure to  because it makes all the keys in one place.
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

So if you were following the previous manual section, the final step could be
automated like so
```
$ cardano-cli shelley genesis create-genesis \
  --genesis-dir example/ \
  --supply 1000000
```

Alternatively the `create-genesis` command can also create all the necessary
keys for you. The optional `--gen-genesis-keys` and `--gen-utxo-keys` flags
can be used to specify the number of keys of each kind to generate.

We still need a genesis template to start from. No there is no command (yet) to
make the template. That would be nice. Here's one I made earlier.
```
$ cat example/genesis.spec.json
{
   "StartTime":"1970-01-01T00:00:00Z",
   "NetworkMagic":42,
   "ProtocolMagicId":42,

   "SlotLength":1,
   "ActiveSlotsCoeff": 5.0e-2,
   "DecentralisationParam":1,
   "SecurityParam":2160,
   "EpochLength":21600,
   "SlotsPerKESPeriod":86400,
   "MaxKESEvolutions":90,
   "UpdateQuorum":5,

   "MaxMajorPV":1000,
   "MaxBodySize":16384,
   "MaxHeaderSize":1400,

   "GenDelegs":{},
   "InitialFunds":{},
   "MaxLovelaceSupply":0
}
```
The `create-genesis` will fill in the:
 * `GenDelegs`,
 * `InitialFunds`,
 * `MaxLovelaceSupply` and the
 * `StartTime`.
Everything else we have to fill in manually, either in the template or
afterwards.

So lets try it:
```
$ cardano-cli shelley genesis create-genesis \
  --genesis-dir example/ \
  --supply 1000000 \
  --gen-genesis-keys 3 \
  --gen-utxo-keys 2
```
Yes [ONE MILLION LOVELACE](https://www.youtube.com/watch?v=l91ISfcuzDw).

Lets have a look at the result
```
$ cat example/genesis.json 
{
    "DecentralisationParam": 1,
    "ActiveSlotsCoeff": 5.0e-2,
    "ProtocolMagicId": 42,
    "StartTime": "2020-05-07T01:46:02.884394538Z",
    "GenDelegs": {
        "40a5d0f1db7ec1bcf92758f1909677576b4edf7164c94602bee0f7848495c615":
          "6e8f60c82449be3d6c17415d784881476d81ed99d88108526e5a32c8d087bdc8",
        "ed58a13ab0dd88df401c6a8db4bb0fe166c6f80b20f80905d31d0226883e4fcb":
          "6e8f60c82449be3d6c17415d784881476d81ed99d88108526e5a32c8d087bdc8",
        "60262fd1a9c700730c93a8cc855d840f8a9795d956e8ee1f6980657efa172fbb":
          "6e8f60c82449be3d6c17415d784881476d81ed99d88108526e5a32c8d087bdc8"
    },
    "UpdateQuorum": 5,
    "MaxHeaderSize": 1400,
    "MaxMajorPV": 1000,
    "MaxBodySize": 16384,
    "MaxLovelaceSupply": 1000000,
    "InitialFunds": {
        "820658209b9d64a0bb979d2067757403262353f383809e43f48fe3d391b5ce6591e11278": 500000,
        "82065820918195bc60cb5b91f36cc8ce97ce7d657f0e38a12eacaf135a6be9501210df6a": 500000
    },
    "NetworkMagic": 42,
    "EpochLength": 21600,
    "SlotLength": 1,
    "SlotsPerKESPeriod": 86400,
    "MaxKESEvolutions": 90,
    "SecurityParam": 2160
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


## Creating node operational keys

TODO

## Issuing node operational certificates

TODO

## Starting a node

TODO

