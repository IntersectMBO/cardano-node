# Understanding your configuration files and how to use them

## The topology.json file

Tells your node to which nodes in the network it should talk to. A minimal version of this file looks like this:

```json
{
  "Producers": [
    {
      "addr": "x.x.x.x",
      "port": 3001,
      "valency": 1
    }
  ]
}
```
* This means that your node will contact node at IP `x.x.x.x` on `port 3001`.

* `valency` determines how many active (hot) outgoing connections to different resolved IP addresses your node should maintain when a DNS address is specified. The valency setting has no effect when setting an IP address, except when using `0` to disable the connection.

Your __block-producing__ node must __ONLY__ talk to your __relay nodes__, and the relay nodes should talk to other relay nodes in the network. Go to https://explorer.cardano.org/relays/topology.json to find out IP addresses and ports of peers.  The `topology.json` found at this link is updated once a week.

## The P2P topology.json file

The P2P topology file specifies how to obtain the _root peers_ (or _bootstrapping
peers_).

* The term _local roots_ refers to a group of peer nodes with which a node will aim to
    maintain a specific number of active, or "hot" connections. These hot connections are
    those that play an active role in the consensus algorithm. Conversely, "warm"
    connections refer to those not yet actively participating in the consensus algorithm.

    Local roots should comprise local relays or a local block producer node, and any other
    peers that the node needs to maintain a connection with. These connections are
    typically kept private.
* _public roots_: additional bootstrapping nodes.  They are either read from
  the configuration file directly, or from the chain.   The configured ones
  will be used to pass a recent snapshot of peers need before the node caches up
  with the recent enough chain to construct root peers by itself.

The node does not guarantee to have a connection with each public root,
unlike for local ones, but by being present in the set it gets a chance to have
an outbound connection towards that peer.

A minimal version of this file looks like this:

```json
{
  "localRoots": [
      { "accessPoints": [
            {
              "address": "x.x.x.x",
              "port": 3001
            }
          ],
        "advertise": false,
        "valency": 1
      }
  ],
  "publicRoots": [
    { "accessPoints": [
        {
          "address": "y.y.y.y",
          "port": 3002
        }
        ],
      "advertise": false
    }
  ],
  "useLedgerAfterSlot": 0
}
```

* The main difference between `LocalRoots` and `PublicRoots` is that with the former
    you can specify different groups with different valencies. That can be useful to
    inform your node of different targets within a group to achieve. `LocalRoots`
    is for peers which the node always should have as hot, such as their own block producer.
    `PublicRoots` represent a source of fallback peers, a source of peers to be used if peers
    from the ledger (`useLedgerAfterSlot`) is disabled or unavailable.

* This means that your node will contact node at IP `x.x.x.x` on `port 3001`, and resolve
    DNS domain `y.y.y.y` (assuming they are), and try to maintain a connection with at least `1` of the
    resolved IPs.

* `valency` (or `hotValency`) tells the node how many connections your node should try to
  pick from the given group. If a DNS address is given, valency governs to how many
  resolved ip addresses should we maintain active (hot) connection.

- `warmValency` is an optional field similar to `valency`/`hotValency` that tells the node
  how many peers the node should maintain as established (warm). As said, this field is
  optional and defaults to the value set in the `valency`/`hotValency` field. The
  `warmValency` value set should be greater than or equal to the one specified in
  `valency`/`hotValency` otherwise `valency`/`hotValency` will be truncated to this value.
  We recommend users to set `warmValency` value to `hotValency` + 1 in order to keep at
  least 1 backup peer to be promoted to hot in case something happens.

  Check [here](https://github.com/input-output-hk/ouroboros-network/issues/4565) for more
  context on this `WarmValency` addition.

* Local roots groups shall be non-overlapping.

* The advertise parameter instructs a node about the acceptability of sharing its address
  through Peer Sharing (which we'll explain in more detail in a subsequent section). In
  essence, if a node has activated Peer Sharing, it can receive requests from other nodes
  seeking peers. However, it will only disclose those peers for which it has both local
  and remote permissions.

  Local permission corresponds to the value of the advertise parameter. On the other
  hand, 'remote permission' is tied to the `PeerSharing` value associated with the
  remote address, which is ascertained after the initial handshake between nodes.

* Local roots should not be greater than the `TargetNumberOfKnownPeers`.
  If they are they will get clamped to the limit.

Your __block-producing__ node must __ONLY__ talk to your __relay nodes__, and the relay node should talk to other relay nodes in the network.

You have the option to notify the node of any changes to the topology configuration file
by sending a SIGHUP signal to the `cardano-node` process. This can be done, for example,
with the command `pkill -HUP cardano-node`. Upon receiving the signal, the `cardano-node`
will re-read the configuration file and restart all DNS resolutions.

Please be aware that this procedure is specific to the topology configuration file, not
the node configuration file. Additionally, the SIGHUP signal will prompt the system to
re-read the block forging credentials file paths and attempt to fetch them to initiate
block forging. If this process fails, block forging will be disabled. To re-enable block
forging, ensure that the necessary files are present.

One can disable ledger peers by setting the `useLedgerAfterSlot` to a negative
value.

If you are synchronizing the network for the first time, please connect to IOG
relays `relays-new.cardano-mainnet.iohkdev.io` by adding it to your local root
peers and set `useLedgerAfterSlot` to `0` (to disable ledger peers).  You can
use different relays as long as you trust them to provide you the honest chain.
When the node is synced, you can remove it from the local root peers, you
should also manually check if other stake pool relays are on the same chain as
you are.  Once you enabled ledger peers by setting `useLedgerAfterSlot` the
node will connect to relays registered on the chain, and churn through them by
randomly picking new peers (weighted by stake distribution) and forgetting 20%
least performing ones.

## The genesis.json file

The genesis file is generated with the `cardano-cli` by reading a `genesis.spec.json` file, which is out of scope for this document.
But it is important because it is used to set:

* `genDelegs`, a mapping from genesis keys to genesis delegates.
* `initialFunds`, a mapping from the initial addresses to the initial values at those address.
* `maxLovelaceSupply`, the total amount of lovelaces in the blockchain.
* `systemStart`, the time of slot zero.

The `genesis.json` file looks like the one below.
```json
{
  "activeSlotsCoeff": 0.05,
  "protocolParams": {
    "protocolVersion": {
      "minor": 0,
      "major": 2
    },
    "decentralisationParam": 1,
    "eMax": 18,
    "extraEntropy": {
      "tag": "NeutralNonce"
    },
    "maxTxSize": 16384,
    "maxBlockBodySize": 65536,
    "maxBlockHeaderSize": 1100,
    "minFeeA": 44,
    "minFeeB": 155381,
    "minUTxOValue": 1000000,
    "poolDeposit": 500000000,
    "minPoolCost": 340000000,
    "keyDeposit": 2000000,
    "nOpt": 150,
    "rho": 0.003,
    "tau": 0.20,
    "a0": 0.3
  },
  "genDelegs": {
    "ad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950c": {
      "delegate": "d9e5c76ad5ee778960804094a389f0b546b5c2b140a62f8ec43ea54d",
      "vrf": "64fa87e8b29a5b7bfbd6795677e3e878c505bc4a3649485d366b50abadec92d7"
    },
    "b9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497": {
      "delegate": "855d6fc1e54274e331e34478eeac8d060b0b90c1f9e8a2b01167c048",
      "vrf": "66d5167a1f426bd1adcc8bbf4b88c280d38c148d135cb41e3f5a39f948ad7fcc"
    },
    "60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1": {
      "delegate": "7f72a1826ae3b279782ab2bc582d0d2958de65bd86b2c4f82d8ba956",
      "vrf": "c0546d9aa5740afd569d3c2d9c412595cd60822bb6d9a4e8ce6c43d12bd0f674"
    },
    "f7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757": {
      "delegate": "69ae12f9e45c0c9122356c8e624b1fbbed6c22a2e3b4358cf0cb5011",
      "vrf": "6394a632af51a32768a6f12dac3485d9c0712d0b54e3f389f355385762a478f2"
    },
    "162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82": {
      "delegate": "4485708022839a7b9b8b639a939c85ec0ed6999b5b6dc651b03c43f6",
      "vrf": "aba81e764b71006c515986bf7b37a72fbb5554f78e6775f08e384dbd572a4b32"
    },
    "2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6": {
      "delegate": "6535db26347283990a252313a7903a45e3526ec25ddba381c071b25b",
      "vrf": "fcaca997b8105bd860876348fc2c6e68b13607f9bbd23515cd2193b555d267af"
    },
    "268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819b": {
      "delegate": "1d4f2e1fda43070d71bb22a5522f86943c7c18aeb4fa47a362c27e23",
      "vrf": "63ef48bc5355f3e7973100c371d6a095251c80ceb40559f4750aa7014a6fb6db"
    }
  },
  "updateQuorum": 5,
  "networkId": "Mainnet",
  "initialFunds": {},
  "maxLovelaceSupply": 45000000000000000,
  "networkMagic": 764824073,
  "epochLength": 432000,
  "systemStart": "2017-09-23T21:44:51Z",
  "slotsPerKESPeriod": 129600,
  "slotLength": 1,
  "maxKESEvolutions": 62,
  "securityParam": 2160
}
```
Here is a brief description of each parameter. You can learn more in the [spec](https://github.com/input-output-hk/cardano-ledger/tree/master/eras/shelley/impl).


| PARAMETER | MEANING |
|----------| --------- |
| activeSlotsCoeff | The proportion of slots in which blocks should be issued. |
| poolDeposit | The amount of a pool registration deposit |
| protocolVersion| Accepted protocol versions |
| decentralisationParam | Percentage of blocks produced by federated nodes |
| maxTxSize | Maximum transaction size |
| minPoolCost | Stake pools cannot register/re-register their stake cost below this value |
| minFeeA | The linear factor for the minimum fee calculation |
| maxBlockBodySize | Maximum block body size |
| minFeeB | The constant factor for the minimum fee calculation |
| eMax | Epoch bound on pool retirement |
| extraEntropy | Well, extra entropy =) |
| maxBlockHeaderSize | Maximum block header size |
| keyDeposit | The amount of a key registration deposit |
| nOpt | Desired number of pools |
| rho | Monetary expansion |
| tau | Treasury expansion |
| a0 | Pool's pledge influence |
| networkMagic | To identify the testnets |
| systemStart | Time of slot 0 |
| genDelegs | Mapping from genesis keys to genesis delegate |
| updateQuorum | Determines the quorum needed for votes on the protocol parameter updates |
| initialFunds | Mapping address to values |
| maxLovelaceSupply | The total number of lovelace in the system, used in the reward calculation. |
| networkMagic | To identify the testnet |
| epochLength | Number of slots in an epoch. |
| staking | Initial delegation |
| slotsPerKESPeriod | Number of slots in an KES period |
| slotLength | in seconds |
| maxKESEvolutions | The maximum number of time a KES key can be evolved before a pool operator must create a new operational certificate |
| securityParam | Security parameter k |


## The config.json file

The default `config.json` file that we downloaded is shown below.

This file has __4__ sections that allow you to have full control on what your node does and how the information is presented.

__NOTE Due to how the config.json file is generated, fields on the real file are shown in a different (less coherent) order. Here we present them in a more structured way__

### Basic Node Configuration.

First section relates the basic node configuration parameters. Make sure you have to `TPraos`as the protocol, the correct path to the `mainnet-shelley-genesis.json` file, `RequiresMagic`for its use in a testnet.

	  "Protocol": "TPraos",
	  "GenesisFile": "mainnet-shelley-genesis.json",
	  "RequiresNetworkMagic": "RequiresMagic",

### Update parameters

This protocol version number gets used by block producing nodes as part of the system for agreeing on and synchronising protocol updates. You just need to be aware of the latest version supported by the network. You don't need to change anything here.

	  "LastKnownBlockVersion-Alt": 0,
	  "LastKnownBlockVersion-Major": 2,
	  "LastKnownBlockVersion-Minor": 0,


### Tracing

`Tracers` tell your node what information you are interested in when logging. Like switches that you can turn ON or OFF according the type and quantity of information that you are interesetd in. This provides fairly coarse grained control, but it is relatively efficient at filtering out unwanted trace output.

`TurnOnLogging`: Enables or disables logging overall.

`TurnOnLogMetrics`: Enable the collection of various OS metrics such as memory and CPU use. These metrics can be directed to the logs or monitoring backends.

`setupBackends`, `defaultBackends`, `hasEKG`and `hasPrometheus`: The system supports a number of backends for logging and monitoring. This settings list the backends available to use in the configuration. The logging backend is called `Katip`.
Also enable the EKG backend if you want to use the EKG or Prometheus monitoring interfaces.

`setupScribes` and `defaultScribes`: For the Katip logging backend we must set up outputs (called scribes) The available types of scribe are:

* FileSK: for files
* StdoutSK/StderrSK: for stdout/stderr
* JournalSK: for systemd's journal system
* DevNullSK
* The scribe output format can be ScText or ScJson.

`rotation` The default file rotation settings for katip scribes, unless overridden in the setupScribes above for specific scribes.

```json
"TurnOnLogging": true,
"TurnOnLogMetrics": true,
"TracingVerbosity": "NormalVerbosity",
"minSeverity": "Debug",
"TraceBlockFetchClient": false,
"TraceBlockFetchDecisions": false,
"TraceBlockFetchProtocol": false,
"TraceBlockFetchProtocolSerialised": false,
"TraceBlockFetchServer": false,
"TraceBlockchainTime": false,
"TraceChainDb": true,
"TraceChainSyncBlockServer": false,
"TraceChainSyncClient": false,
"TraceChainSyncHeaderServer": false,
"TraceChainSyncProtocol": false,
"TraceDNSResolver": true,
"TraceDNSSubscription": true,
"TraceErrorPolicy": true,
"TraceForge": true,
"TraceHandshake": false,
"TraceIpSubscription": true,
"TraceLocalChainSyncProtocol": false,
"TraceLocalErrorPolicy": true,
"TraceLocalHandshake": false,
"TraceLocalTxSubmissionProtocol": false,
"TraceLocalTxSubmissionServer": false,
"TraceMempool": true,
"TraceMux": false,
"TraceTxInbound": false,
"TraceTxOutbound": false,
"TraceTxSubmissionProtocol": false,
"setupBackends": [
  "KatipBK"
],
"defaultBackends": [
  "KatipBK"
],
"hasEKG": 12788,
"hasPrometheus": [
  "127.0.0.1",
  12798
],
"setupScribes": [
  {
    "scFormat": "ScText",
    "scKind": "StdoutSK",
    "scName": "stdout",
    "scRotation": null
  }
],
"defaultScribes": [
  [
    "StdoutSK",
    "stdout"
  ]
],
"rotation": {
  "rpKeepFilesNum": 10,
  "rpLogLimitBytes": 5000000,
  "rpMaxAgeHours": 24
  },
```

### Fine grained logging control

It is also possible to have more fine grained control over filtering of trace output, and to match and route trace output to particular backends. This is less efficient than the coarse trace filters above but provides much more precise control. `options`:

`mapBackends`This routes metrics matching specific names to particular backends. This overrides the defaultBackends listed above. And note that it is an **override** and not an extension so anything matched here will not go to the default backend, only to the explicitly listed backends.

`mapSubtrace` This section is more expressive, we are working on its documentation.

```json
	  "options": {
	    "mapBackends": {
	      "cardano.node.metrics": [
	        "EKGViewBK"
	      ]
	    },
	    "mapSubtrace": {
	      "cardano.node.metrics": {
	        "subtrace": "Neutral"
	      }
	    }
	  }
	}
```

### Peer-to-Peer Parameters & Tracers

To run a node in P2P mode set `EnableP2P` to `true` (_the default is `False`_) in the
configuration file.  You will also need to specify the topology in a new format which is
described above.

There are a few new tracers and configuration options which you can set (listed below by
component):

#### Outbound Governor

The outbound governor is responsible for satisfying targets of root peers, known (_cold_,
_warm_ and _hot_), established (_warm_ & _hot_) and active peers (synonym for _hot_ peers)
and local root peers.  The primary way to configure them is by setting the following
options:

* `TargetNumberOfRootPeers` (_default value: `100`_) - a minimal number of root peers
  (unlike other targets this one is one sided, e.g. a node might have more root peers
* `TargetNumberOfKnownPeers` (_default value: `100`_) - a target of known peers (must be
  larger or equal to `TargetNumberOfRootPeers`)
* `TargetNumberOfEstablishedPeers` (_default value: `50`_) - a target of all established
  peers (including local roots, ledger peers)
* `TargetNumberOfActivePeers` (_default value: `20`_) - a target for _hot_ peers which
  engage in the consensus protocol

Let us note two more targets.  In the topology file you may include local root peers.
This is a list of groups of peers, each group comes with its own valency.  The outbound
governor will maintain a connection with every local root peer, and will enforce that at
least the specified number of them (the valency) are _hot_.  Thus the
`TargetNumberOfKnownPeers` , `TargetNumberOfEstablishedPeers` and
`TargetNumberOfActivePeers` must be large enough to accommodate local root peers.

The following traces can be enabled:

* `TracePeerSelection` (_by default on_) - tracks selection of upstream peers done by the
  _outbound-governor_.  **Warm peers** are ones with which we have an open connection but
  don't engage in consensus protocol, **hot peers** are peers which engage in consensus
  protocol (via `chain-sync`, `block-fetch` and `tx-submission` mini-protocols), **cold
  peers** are ones which we know about but the node doesn't have an established
  connection.  Note that the notions of _hot_, _warm_ and _cold_ are only related to usage
  of initiator sides of mini-protocols in a connection (which can be either inbound or
  outbound).
* `TracePeerSelectionCounters` (_by default on_) - traces how many cold / warm / hot  /
  local root peers the node has, it's also available via ekg.
* `TracePeerStateActions` (_by default on_) - includes traces from a component which
  executes peer promotion / demotions between cold / warm & hot states.
* `TracePublicRootPeers` (_by default off_) - traces information about root / ledger peers
  (e.g. ip addresses or dns names of ledger peers, dns resolution)
* `DebugPeerSelectionInitiator` and `DebugPeerSelectionInitiatorResponder` (_by default
  off_) - a debug tracers which log the information about current state of the _outbound
  governor_.

At this point [haddock
documentation](https://input-output-hk.github.io/ouroboros-network/ouroboros-network/Ouroboros-Network-PeerSelection-Governor.html)
of the outbound governor is available.

#### Peer Sharing

Peer Sharing is a novel feature that provides an additional method for the Outbound
Governor to reach its targets for known peers. With Peer Sharing, the node can request
peer information from other nodes with which it has an established connection.

**IMPORTANT:** _Peer Sharing_ is an experimental feature that is turned off by default.
Please be aware that until the availability of genesis & eclipse evasion, this feature may
leave a node vulnerable to eclipse attacks.

The main method for configuring Peer Sharing involves setting the following option:

- `PeerSharing` (default value: `NoPeerSharing`) - This option can take 3 possible values:
    * `NoPeerSharing`: Peer Sharing is disabled, which means the node won't request peer
      information from any other node, and will not respond to such requests from others
      (the mini-protocol won't even start);
    * `PeerSharingPrivate`: Peer Sharing is enabled, meaning the node will query other
      nodes for peers. However, during the handshake process, it will inform other nodes
      not to share its address.
    * `PeerSharingPublic`: Peer Sharing is enabled and the node will notify other nodes
      that it is permissible to share its address.

The `PeerSharing` flag interacts with `PeerAdvertise` (`advertise` flag in the topology
file) values as follows:

`AdvertisePeer` (`advertise: true`) is local to the configuration of a specific node. A
node might be willing to share those peers it has set as `PeerAdvertise`. Conversely,
`PeerSharing` is about whether the peer (itself) is willing to participate in
`PeerSharing` or allows others to share its address.

`PeerSharing` takes precedence over `AdvertisePeer`. Consider the following example:

A Block Producer (BP) has the `NoPeerSharing` flag value (which means it won't participate
in Peer Sharing or run the mini-protocol). A Relay node has the BP set as a local peer
configured as `AdvertisePeer` (likely a misconfiguration). When the handshake between the
BP and the Relay occurs, the Relay will see that the BP doesn't want to participate in
Peer Sharing. As a result, it won't engage in peer sharing with it or share its details
with others.

The `combinePeerInformation` function determines the sharing interaction semantics between
the two flags. Please take a look to better understand how the two values combine.

```haskell
-- Combine a 'PeerSharing' value and a 'PeerAdvertise' value into a
-- resulting 'PeerSharing' that can be used to decide if we should
-- share or not the given Peer. According to the following rules:
--
-- - If no PeerSharing value is known then there's nothing we can assess
-- - If a peer is not participating in Peer Sharing ignore all other information
-- - If a peer said it wasn't okay to share its address, respect that no matter what.
-- - If a peer was privately configured with DoNotAdvertisePeer respect that no matter
-- what.
--
combinePeerInformation :: PeerSharing -> PeerAdvertise -> PeerSharing
combinePeerInformation NoPeerSharing      _                  = NoPeerSharing
combinePeerInformation PeerSharingPrivate _                  = PeerSharingPrivate
combinePeerInformation PeerSharingPublic  DoNotAdvertisePeer = PeerSharingPrivate
combinePeerInformation _                         _           = PeerSharingPublic
```

#### Inbound Governor

The inbound governor is maintaining responder side of all mini-protocols.  Unlike the
outbound governor it is a purely responsive component which reacts to actions of remote
peer (its outbound governor).

* `TraceInboundGovernor` (_by default on_) - traces information about inbound connection,
  e.g. we track if the remote side is using  our node as _warm_ or _hot peer_, traces when
  we restart a responder.
* `TraceInboundGovernorCounters` (_by default on_) - traces number of peers which use the
  node as `cold`, `warm` or `hot` (which we call `remote cold`, `remote warm` or `remote
  hot`).  Note that we only know if a peer is in the remote cold state if we connected to
  that peer and it's not using the connection.   This information is also available via
  ekg.
* `TraceInboundGovernorTransitions` (_by default on_) - a debug tracer which traces
  transitions between remote cold, remote warm and remote hot states.

The inbound governor is documented in [The Shelley Networking
Protocol](https://input-output-hk.github.io/ouroboros-network/pdfs/network-spec) (section
4.5).

#### Connection Manager

Connection manager tracks the state of all tcp connections, and enforces various timeouts,
e.g. when the connection is not used by either of the sides.  The following traces are
available:

* `TraceConnectionManager` (_by default on_) - traces information about new inbound or
  outbound connection, connection errors.
* `TraceConnectionManagerCounters` (_by default on_) - traces the number of inbound,
  outbound, duplex (connections which negotiated P2P mode and can use a connection in full
  duplex mode), full duplex (connections which run mini-protocols in both directions, e.g.
  at least _warm_ and _remote warm_ at the same time), unidirectional connections
  (connections with non p2p nodes, or p2p nodes which configured themselves as initiator
  only nodes).
* `TraceConnectionManagerTransitions` (_by default on_) - a low level traces which traces
  connection state changes in the connection manager state machine.

The connection manager is documented in [The Shelley Networking
Protocol](https://input-output-hk.github.io/ouroboros-network/pdfs/network-spec) (section
4).

#### Ledger Peers

Ledger peers are the relays registered on the chain.  Currently we use square of the stake
distribution to randomly pick new ledger peers.   You can enable `TraceLedgerPeers` (_by
default off_) to log actions taken by this component.

#### Server

The accept loop.  You can enable `TraceServer` to log its actions or errors it encounters
(_by default it is off_, however we suggest to turn it on) .

**Please note that this version contains no breaking changes**
