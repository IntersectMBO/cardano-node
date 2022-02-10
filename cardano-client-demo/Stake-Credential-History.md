# Stake Credential History Tool

The stake credential history tool will produce a linear history of events for a given stake credential.
In particular, it logs registrations, de-registrations, delegations, instantaneous rewards,
reward withdrawals, mentions inside pool parameter registrations, and also per-epoch active stake and rewards.
Additionally, it records the transitions to new eras, and can also be configured to display regular
checkpoint messages in order to view progress (the first block seen after n-many slots).

## Building

```
cabal build stake-credential-history
```

## Running

In order to run the stake credential history tool, you must have a running cardano-node.
You must provide the tool with three things:
* The cardano node configuration file. For example, the mainnet configuration is located in the cardano-node repo at `cardano-node/configuration/cardano/mainnet-config.json`.
* The socket for the running node. For example, if a mainnet node was built with `nix build .#mainnet/node`, the socket will be located at `cardano-node/state-node-mainnet/node.socket`.
* Either the hex-encoded stake address (58 hex digits, corresponding to a 28 byte hash and a 1 byte address header) or the bech32-encoded stake address. For example, the hex-encoded stake credential `0123456789abcdef0123456789abcdef0123456789abcdef01234567` could be converted to a [bech32](https://github.com/input-output-hk/bech32) mainnet  address with:
    ```
    $ bech32 stake <<< e10123456789abcdef0123456789abcdef0123456789abcdef01234567
    stake1uyqjx3t83x4ummcpydzk0zdtehhszg69v7y6hn00qy352ecrxzgqe
    ```
    Note that `e1` was appended to mark the credential as a mainnet, key-locked stake address.

Putting it all together, using the example data above,
we can get the history of the stake address `stake1uyqjx3t83x4ummcpydzk0zdtehhszg69v7y6hn00qy352ecrxzgqe`
on mainnet with:
```
cabal run stake-credential-history -- --conf configuration/cardano/mainnet-config.json --socket state-node-mainnet/node.socket --stake-address-bech32 stake1uyqjx3t83x4ummcpydzk0zdtehhszg69v7y6hn00qy352ecrxzgqe
```

## Interpreting

### NEW-ERA

This event records the epoch number, slot number, and name of the first block in a new era.

### REGISTRATION

This event records then slot number when the given stake credential submits a registration certificate.

### DE-REGISTRATION

This event records then slot number when the given stake credential submits a de-registration certificate.

### DELEGATION

This event records the slot number and pool ID when the given stake credential submits a delegation certificate.

### POOL-REGISTRATION

This event records the slot number and pool ID when the given stake credential is a part of a pool registration certificate.
Additionally, it will display "Owner" if the credential is listed as an owner, and "Reward-Account" if it is listed as the reward account.

### MIR

This event records the slot number, pot (reserves or treasury), and Lovelace amount if the given stake credential in mentioned in a MIR certificate.

### WDRL

This event records the slot number and Lovelace amount if the given stake credential withdrawals rewards from the account.

### BALANCE

This event records the epoch number, slot number and current reward balance after the first block of each epoch
for the given stake credential, provided it is registered.

### REWARD-START

This event records the epoch number, slot number and active stake for the given stake credential when the reward calculation starts.

### REWARD-END

This event records the epoch number, slot number and reward amount for the given stake credential when the reward calculation ends.
NOTE: the rewards which are calculated during epoch `n` are often referred to as the "rewards earned" in epoch `n-1`.
This corresponds with the fact that these rewards are for the blocks produced during epoch `n-1`.
Note also that these rewards are added to the stake address on the boundary of epoch `n` and epoch `n+1`.
Rewards are broken down into member and leader rewards.
Member rewards can only appear once per reward event (since a stake credential can only be delegated to one pool),
but there can be multiple leader rewards. In each case, the pool ID is for each reward source is listed.

### CHECKPOINT

This event only occurs if the flag `--check-point-size N` is supplied, and is useful for determining the tool's progress.
It records the slot number of the first block processed after `N`-many slots since the last checkpoint event.

### ERROR

This event is not expected to occur, it signals a logic error in the tool.
