# Peer-to-Peer Block Production and Block Forging Configuration

For redundancy purposes, Stake Pool Operators (SPOs) often operate backup block production
nodes. However, with the introduction of peer-to-peer (P2P) nodes, the previous approach
of using firewall rules to prevent relays from connecting to the backup node, and thus
stop duplicate blocks from being propagated, will no longer be effective.

In the P2P environment, relays can repurpose inbound connections from the block producer,
leading to potential complications. To address this issue, we've introduced a way for a
block producing node to be started (and stopped) without immediately producing blocks. It
will only begin (or stop) block production when it receives a SIGHUP signal.

## Enabling and Disabling Block Forging

Block Forging can be toggled on and off using the SIGHUP signal. Sending such a signal
triggers the node to read the block forging credential files. Not that this will also
trigger the re-reading of the topology configuration file, so connections might be lost.

As these credential files are provided through CLI flags, they cannot be removed without
restarting the node. To disable block forging, you need to move, rename, or delete the
file at the specified path (for the credential flags), and then send the SIGHUP signal.
The code will then recognize that the specified files are missing and disable block
forging, recording the appropriate log messages.

## Starting as a Non-Producing Node

If you wish to start a block producing node (i.e., passing the credentials in the
respective flags) without it acting as a block producer immediately, you can use the
`--start-as-non-producing-node` flag. This will run the node with credentials as a
standard node. However, upon receiving the SIGHUP signal, it will read the credential
files and start block forging.

This configuration allows for safer management of block production in a P2P environment,
reducing the risk of duplicate blocks and improving overall network stability.

