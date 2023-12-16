.. raw:: html

  <p align="center">
    <a href="https://github.com/intersectmbo/cardano-node/releases"><img src="https://img.shields.io/github/release-pre/intersectmbo/cardano-node.svg?style=for-the-badge" /></a>
  </p>

  <table align="center">
    <tr><td colspan="2" align="center">GitHub Actions</td></tr>
    <tr>
      <td>
        <a href="https://github.com/intersectmbo/cardano-node/actions/workflows/haskell.yml"><img alt="GitHub Workflow Status (master)" src="https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-node/haskell.yml?branch=master" /></a>
        <a href="https://github.com/intersectmbo/cardano-node/actions/workflows/haskell.yml"><img alt="GitHub Workflow Status (branch)" src="https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-node/haskell.yml?branch=nightly" /></a>
      </td>
    </tr>
  </table>

.. contents:: Contents

*************************
Overview of the ``cardano-node`` repository
*************************

Integration of the `ledger <https://github.com/input-output-hk/cardano-ledger-specs>`_, `consensus <https://github.com/input-output-hk/ouroboros-consensus>`_,
`networking <https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network>`_ and
`node shell <https://github.com/input-output-hk/cardano-shell>`_ repositories.

`Logging <https://github.com/input-output-hk/iohk-monitoring-framework>`_ is provided as a
`feature <https://github.com/input-output-hk/cardano-shell/blob/master/app/Cardano/Shell/Features/Logging.hs>`_ by the node shell to the other packages.

- The cardano-node is the top level for the node and
  aggregates the other components from other packages: consensus, ledger and
  networking, with configuration, CLI, logging and monitoring.

- The node no longer incorporates wallet or explorer functionality. The wallet
  backend and explorer backend are separate components that run in separate
  external processes that communicate with the node via local IPC.

Network Configuration, Genesis and Topology Files
=================================================

The latest supported networks can be found at `<https://book.world.dev.cardano.org/environments.html>`_

****
Obtaining ``cardano-node``
****

Building from source
====

Documentation for building the node can be found `here <https://docs.cardano.org/getting-started/installing-the-cardano-node>`_.

Executables
===========

You can download the hydra binaries of ``cardano-node`` and ``cardano-cli`` from the `release notes <https://github.com/intersectmbo/cardano-node/releases>`_


Running the node on Windows
----

The download includes cardano-node.exe and a .dll. To run the node with cardano-node run you need to reference a few files and directories as arguments. These can be copied from the cardano-node repo into the executables directory. The command to run the node on mainnet looks like this:

.. code-block:: console

    cardano-node.exe run --topology ./configuration/cardano/mainnet-topology.json --database-path ./state --port 3001 --config ./configuration/cardano/mainnet-config.yaml  --socket-path \\.\pipe\cardano-node

Docker image
============

You can pull the docker image with the latest version of cardano-node from `here <https://hub.docker.com/r/inputoutput/cardano-node>`_.

.. code-block:: console

    docker pull inputoutput/cardano-node

****
Using ``cardano-node``
****

Command line summary: ``cardano-node``
====
This refers to the client that is used for running a node.

The general synopsis is as follows:

.. code-block:: console

   Usage: cardano-node run [--topology FILEPATH] [--database-path FILEPATH]
                           [--socket-path FILEPATH]
                           [--byron-delegation-certificate FILEPATH]
                           [--byron-signing-key FILEPATH]
                           [--shelley-kes-key FILEPATH]
                           [--shelley-vrf-key FILEPATH]
                           [--shelley-operational-certificate FILEPATH]
                           [--start-as-non-producing-node]
                           [--host-addr IPV4-ADDRESS]
                           [--host-ipv6-addr IPV6-ADDRESS]
                           [--port PORT]
                           [--config NODE-CONFIGURATION] [--validate-db]
     Run the node.

* ``--topology`` - Filepath to a topology file describing which peers the node should connect to.

* ``--database-path`` - Path to the blockchain database.

* ``--byron-delegation-certificate`` - Optional path to the Byron delegation certificate. The delegation certificate allows the delegator (the issuer of said certificate) to give his/her own block signing rights to somebody else (the delegatee). The delegatee can then sign blocks on behalf of the delegator.

* ``--byron-signing-key`` - Optional path to the Byron signing key.

* ``--shelley-signing-key`` - Optional path to the Shelley signing key.

* ``--shelley-kes-key`` - Optional path to the Shelley KES signing key.

* ``--shelley-vrf-key`` - Optional path to the Shelley VRF signing key.

* ``--shelley-operational-certificate`` - Optional path to the Shelley operational certificate.

* ``--start-as-non-producing-node`` -  Optional flag to disable block production on node
  start. If credentials flags are passed the node will start block producing, however with
  this flag the node will only start block producing on SIGHUP (see `here <https://github.com/input-output-hk/cardano-node-wiki/wiki/dynamic-block-forging>`_ for more details)

* ``--socket-path`` - Path to the socket file.

* ``--host-addr`` - Optionally specify your node's IPv4 address.

* ``--host-ipv6-addr`` - Optionally specify your node's IPv6 address.

* ``--port`` - Specify which port to assign to the node.

* ``--config`` - Specify the filepath to the config ``.yaml`` file. This file is responsible for all the other node's required settings. See examples in ``configuration`` (e.g. `config-0.yaml <configuration/defaults/simpleview/config-0.yaml>`_).

* ``--validate-db`` - Flag to revalidate all on-disk database files

Configuration
====

The ``--config`` flag points to a ``.yaml`` (or a structurally equivalent ``.json``) file that is responsible to configuring the logging & other important settings for the node. E.g. see the Byron mainnet configuration in this
`configuration.yaml <https://github.com/intersectmbo/cardano-node/blob/master/configuration/defaults/byron-mainnet/configuration.yaml>`_.

Some of the more important settings are as follows:

* ``Protocol: RealPBFT`` -- Protocol the node will execute

* ``RequiresNetworkMagic``: RequiresNoMagic -- Used to distinguish between mainnet (``RequiresNoMagic``) and testnets (``RequiresMagic``)

Scripts
=======

Please see ``scripts/README.md`` for information on the various scripts.

****
Using ``cardano-cli``
****

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.
Usage documentation can be found at ``https://github.com/input-output-hk/cardano-cli/tree/master/cardano-cli/README.md``.

The general synopsis is as follows:

.. code-block:: console

   Usage: cardano-cli (Era based commands | Byron specific commands | Miscellaneous commands)

> NOTE: the exact invocation command depends on the environment.  If you have only built ``cardano-cli``, without installing it, then you have to prepend ``cabal run -- ``
before ``cardano-cli``.  We henceforth assume that the necessary environment-specific adjustment has been made, so we only mention ``cardano-cli``.

Command line options: ``cardano-cli``
====

The subcommands are subdivided in groups, and their full list can be seen in the output of ``cardano-cli --help``.

All subcommands have help available.  For example:

.. code-block:: console

   cabal run -- cardano-cli -- byron key migrate-delegate-key-from --help

   cardano-cli -- byron key migrate-delegate-key-from
   Usage: cardano-cli byron key migrate-delegate-key-from --from FILEPATH
                                                          --to FILEPATH
     Migrate a delegate key from an older version.


   Available options:
     --byron-legacy-formats   Byron/cardano-sl formats and compatibility
     --byron-formats          Byron era formats and compatibility
     --from FILEPATH          Signing key file to migrate.
     --to FILEPATH            Non-existent file to write the signing key to.
     -h,--help                Show this help text


Genesis generation
====

The Byron genesis generation operations will create a directory that contains:

* ``genesis.json``:
  The genesis JSON file itself.

* ``avvm-seed.*.seed``:
  Ada Voucher Vending Machine seeds (secret). Affected by ``--avvm-entry-count`` and ``--avvm-entry-balance``.

* ``delegate-keys.*.key``:
  Delegate private keys. Affected by: ``--n-delegate-addresses``.

* ``delegation-cert.*.json``:
  Delegation certificates. Affected by: ``--n-delegate-addresses``.

* ``genesis-keys.*.key``:
  Genesis stake private keys. Affected by: ``--n-delegate-addresses``, ``--total-balance``.

* ``poor-keys.*.key``:
  Non-delegate private keys with genesis UTxO. Affected by: ``--n-poor-addresses``, ``--total-balance``.

More details on the Byron Genesis ``JSON`` file can be found in ``https://github.com/input-output-hk/cardano-node-wiki/wiki/byron-genesis``

 Byron genesis delegation and related concepts are described in detail in:

  `<https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_

The canned ``scripts/benchmarking/genesis.sh`` example provides a nice set of defaults and
illustrates available options.

Key operations
==============

Note that key operations do not support password-protected keys.

Signing key generation & verification key extraction
----------------------------------------------------

Signing keys can be generated using the ``keygen`` subcommand.

Extracting a verification key out of the signing key is performed by the ``to-verification`` subcommand.

Delegate key migration
----------------------

In order to continue using a delegate key from the Byron Legacy era in the new implementation,
it needs to be migrated over, which is done by the ``migrate-delegate-key-from`` subcommand:

.. code-block:: console

  $ cabal v2-run -- cardano-cli byron key migrate-delegate-key-from
          --from key0.sk --to key0Converted.sk

Signing key queries
-------------------

One can gather information about a signing key's properties through the ``signing-key-public``
and ``signing-key-address`` subcommands (the latter requires the network magic):

.. code-block:: console

   $ cabal v2-run -- cardano-cli byron key signing-key-public --byron-formats --secret key0.sk

   public key hash: a2b1af0df8ca764876a45608fae36cf04400ed9f413de2e37d92ce04
   public key: sc4pa1pAriXO7IzMpByKo4cG90HCFD465Iad284uDYz06dHCqBwMHRukReQ90+TA/vQpj4L1YNaLHI7DS0Z2Vg==

   $ cabal v2-run -- cardano-cli signing-key-address --byron-formats --secret key0.pbft --testnet-magic 42

   2cWKMJemoBakxhXgZSsMteLP9TUvz7owHyEYbUDwKRLsw2UGDrG93gPqmpv1D9ohWNddx
   VerKey address with root e5a3807d99a1807c3f161a1558bcbc45de8392e049682df01809c488, attributes: AddrAttributes { derivation path: {} }

Transactions
============

Creation
--------

Transactions can be created via the  ``issue-genesis-utxo-expenditure`` & ``issue-utxo-expenditure`` commands.

The easiest way to create a transaction is via the ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh`` script as follows:

``./scripts/benchmarking/issue-genesis-utxo-expenditure.sh transaction_file``

NB: This by default creates a transaction based on ``configuration/defaults/liveview/config-0.yaml``

If you do not have a ``genesis_file`` you can run ``scripts/benchmarking/genesis.sh`` which will create an example ``genesis_file`` for you.
The script ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh`` has defaults for all the requirements of the ``issue-genesis-utxo-expenditure`` command.

Submission
----------

The ``submit-tx`` subcommand provides the option of submitting a pre-signed
transaction, in its raw wire format (see GenTx for Byron transactions).

The canned ``scripts/benchmarking/submit-tx.sh`` script will submit the supplied transaction to a testnet
launched by ``scripts/benchmarking/shelley-testnet-liveview.sh`` script.

Issuing UTxO expenditure (genesis and regular)
----------------------------------------------

To make a transaction spending UTxO, you can either use the:

  - ``issue-genesis-utxo-expenditure``, for genesis UTxO
  - ``issue-utxo-expenditure``, for normal UTxO

subcommands directly, or, again use canned scripts that will make transactions tailored
for the aforementioned testnet cluster:

  - ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh``.
  - ``scripts/benchmarking/issue-utxo-expenditure.sh``.

The script requires the target file name to write the transaction to, input TxId
(for normal UTxO), and optionally allows specifying the source txin output index,
source and target signing keys and lovelace value to send.

The target address defaults to the 1-st richman key (``configuration/delegate-keys.001.key``)
of the testnet, and lovelace amount is almost the entirety of its funds.

Local node queries
==================

You can query the tip of your local node via the ``get-tip`` command as follows

1. Open ``tmux``
2. Run ``cabal build cardano-node``
3. Run ``./scripts/lite/shelley-testnet.sh example``
4. Run ``export CARDANO_NODE_SOCKET_PATH=/cardano-node/example/socket/node-1-socket
4. ``cabal exec cardano-cli -- get-tip --testnet-magic 42``

You will see output from stdout in this format:

.. code-block:: console

   Current tip:
   Block hash: 4ab21a10e1b25e39
   Slot: 6
   Block number: 5

Update proposals
================

Update proposal creation
------------------------

A Byron update proposal can be created as follows:

.. code-block:: console

   cardano-cli -- byron governance
                  create-update-proposal
                    (--mainnet | --testnet-magic NATURAL)
                    --signing-key FILEPATH
                    --protocol-version-major WORD16
                    --protocol-version-minor WORD16
                    --protocol-version-alt WORD8
                    --application-name STRING
                    --software-version-num WORD32
                    --system-tag STRING
                    --installer-hash HASH
                    --filepath FILEPATH
                  ..

The mandatory arguments are ``--mainnet | --testnet-magic``, ``signing-key``, ``protocol-version-major``, ``protocol-version-minor``, ``protocol-version-alt``, ``application-name``, ``software-version-num``, ``system-tag``, ``installer-hash`` and ``filepath``.

The remaining arguments are optional parameters you want to update in your update proposal.

You can also check your proposal's validity using the ``validate-cbor`` command. See: `Validate CBOR files`_.

See the `Byron specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_
for more details on update proposals.

Update proposal submission
--------------------------

You can submit your proposal using the ``submit-update-proposal`` command.

Example:

.. code-block:: console

   cardano-cli -- byron governance
               submit-update-proposal
               --config configuration/defaults/mainnet/configuration.yaml
               (--mainnet | --testnet-magic NATURAL)
               --filepath my-update-proposal

See the `Byron specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_
for more details on update proposals.

Update proposal voting
======================

You can create and submit byron update proposal votes with the ``create-proposal-vote`` & ``submit-proposal-vote`` commands. The following are two example commands:


Byron vote creation:

.. code-block:: console

   cabal exec cardano-cli -- byron governance create-proposal-vote
                          (--mainnet | --testnet-magic NATURAL)
                          --signing-key configuration/defaults/liveview/genesis/delegate-keys.000.key
                          --proposal-filepath ProtocolUpdateProposalFile
                          --vote-yes
                          --output-filepath UpdateProposalVoteFile

Byron vote submission:

.. code-block:: console

   cabal exec cardano-cli -- byron governance submit-proposal-vote
                          (--mainnet | --testnet-magic NATURAL)
                          --filepath UpdateProposalVoteFile

Development
===========

GHCID
-----

run *ghcid* with: ``ghcid -c "cabal repl exe:cardano-node --reorder-goals"``

Note: When developing locally, for any package you are working on, in **cabal.project** set, *ghc-options* to ``-Wwarn`` and set the ``development`` flag, e.g.::

  package cardano-node
    ghc-options: -Wwarn
    flags: +development

Otherwise GHC might complain about unused packages.

****
Native Tokens
****

Native tokens is a new feature that enables the transacting of multi-assets on Cardano. Native tokens are now supported on mainnet and users can transact with ada, and an unlimited number of user-defined (custom) tokens natively. Note that users who do not need to create new assets (“token holders”) will be able to send and receive existing multi-asset tokens using a wallet such as Daedalus or Yoroi, and with no requirement to use any CLI commands.

To help you get started, see:

- `Cardano Forum discussion <https://forum.cardano.org/c/developers/cardano-tokens/150>`_

- `Ledger explanations: native tokens <https://cardano-ledger.readthedocs.io/en/latest/>`_. Covers explainers about assets, tokens, token bundles, minting policies, comparison to ERC20, and minimum ada value requirements.

- `A tutorial on how to get started with native tokens <https://github.com/input-output-hk/cardano-node-wiki/wiki/02-getting-started>`_. Explains how to create new currencies and assets, submit and send transactions containing multi-asset tokens, send and receive token bundles, manage your addresses and values.

- `Native tokens exercises <https://github.com/input-output-hk/cardano-node-wiki/wiki/03-exercises>`_

To start, please ensure that you are familiar with setting up and operating the `Cardano node <https://github.com/intersectmbo/cardano-node>`_. Alternatively, see instructions on how to `start your node <https://github.com/input-output-hk/cardano-node-wiki/wiki/2_start_your_nodes>`_ to submit the commands. You will not need to set up and start a full block producing node ('stake pool'), just a much simpler relay node. This node will need to connect to a Cardano network that is capable of processing native tokens (e.g., the native token pre-production environment (PPE), or the Cardano mainnet).

****
API Documentation
****

The API documentation is published `here <https://cardano-node.cardano.intersectmbo.org/>`_.

The documentation is built with each push, but is only published from ``master`` branch.  In order to
test if the documentation is working, build the documentation locally with ``cabal haddock-project --local --output=./haddocks`` and
open ``haddocks/index.html`` in the browser.

****
Using the ``cardano-node`` Haskell packages
****

If you want to use the ``cardano-node`` Haskell packages from another project, you should use `CHaP <https://github.com/input-output-hk/cardano-haskell-packages>`_ to get the packages defined in this repository.
Please note that you may need to use any ``source-repository-package`` stanzas defined in ``cabal.project``, although we will endeavour to keep these to an absolute minimum.

****
Style guide
****

The `style guide <https://github.com/input-output-hk/cardano-node-wiki/wiki/Style-guide>`_ for can be found
on the `cardano-node repository's wiki <https://github.com/input-output-hk/cardano-node-wiki/wiki>`_.

****
Troubleshooting ``cardano-node`` issues
****

For some troubleshooting help with building or running ``cardano-node``, the wiki has a
`troubleshooting page <https://github.com/input-output-hk/cardano-node-wiki/wiki/Troubleshooting>`_
that documents some common gotchas.
