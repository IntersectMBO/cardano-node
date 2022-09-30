.. raw:: html

  <p align="center">
    <a href="https://github.com/input-output-hk/cardano-node/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-node.svg?style=for-the-badge" /></a>
    <a href="https://buildkite.com/input-output-hk/cardano-node"><img src="https://img.shields.io/buildkite/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a/master?label=BUILD&style=for-the-badge"/></a>
  </p>

  <table align="center">
    <tr><td colspan="2" align="center">GitHub Actions</td></tr>
    <tr>
      <td>
        <a href="https://github.com/input-output-hk/cardano-node/actions/workflows/haskell.yml"><img alt="GitHub Workflow Status (master)" src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-node/Haskell%20CI/master" /></a>
        <a href="https://github.com/input-output-hk/cardano-node/actions/workflows/haskell.yml"><img alt="GitHub Workflow Status (branch)" src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-node/Haskell%20CI/nightly?label=nightly" /></a>
      </td>
    </tr>
  </table>

.. contents:: Contents

*************************
Overview of the ``cardano-node`` repository
*************************

Integration of the `ledger <https://github.com/input-output-hk/cardano-ledger-specs>`_, `consensus <https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus>`_,
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

You can download the hydra binaries of ``cardano-node`` and ``cardano-cli`` from the [release notes](https://github.com/input-output-hk/cardano-node/releases)


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

* ``--socket-path`` - Path to the socket file.

* ``--host-addr`` - Optionally specify your node's IPv4 address.

* ``--host-ipv6-addr`` - Optionally specify your node's IPv6 address.

* ``--port`` - Specify which port to assign to the node.

* ``--config`` - Specify the filepath to the config ``.yaml`` file. This file is responsible for all the other node's required settings. See examples in ``configuration`` (e.g. `config-0.yaml <configuration/defaults/simpleview/config-0.yaml>`_).

* ``--validate-db`` - Flag to revalidate all on-disk database files

Configuration
====

The ``--config`` flag points to a ``.yaml`` (or a structurally equivalent ``.json``) file that is responsible to configuring the logging & other important settings for the node. E.g. see the Byron mainnet configuration in this
`configuration.yaml <https://github.com/input-output-hk/cardano-node/blob/master/configuration/defaults/byron-mainnet/configuration.yaml>`_.

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
Usage documentation can be found at ``cardano-cli/README.md``.

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

More details on the Byron Genesis ``JSON`` file can be found in ``docs/reference/byron-genesis.md``

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

****
Development
****

Overview
====

The ``cardano-node`` development is primarily based on the Nix infrastructure (https://nixos.org/ ), which enables packaging, CI, development environments and deployments.

On how to set up Nix for ``cardano-node`` development, please see `Building Cardano Node with nix <https://github.com/input-output-hk/cardano-node/tree/master/doc/getting-started/building-the-node-using-nix.md>`_.

Workbench: a local cluster playground
====

You can quickly spin up a local cluster (on Linux and Darwin), based on any of a wide variety of configurations, and put it under a transaction generation workload -- using the ``workbench`` environment:

1. Optional: choose a workbench profile:
    - ``default`` stands for a light-state, 6-node cluster, under saturation workload, indefinite runtime
    - ``ci-test`` is the profile run in the node CI -- very light, just two nodes and short runtime
    - ``devops`` is an unloaded profile (no transaction generation) with short slots -- ``0.2`` sec.
    - ..and many more -- which can be either:
        - listed, by ``make ps``
        - observed at their point of definition: `nix/workbench/profiles/prof1-variants.jq <https://github.com/input-output-hk/cardano-node/tree/master/nix/workbench/profiles/prof1-variants.jq#L333-L526>`_
2. Optional: select mode of operation, by optionally providing a suffix:
    - default -- no suffix -- just enter the workbench shell, allowing you to run ``start-cluster`` at any time.  Binaries will be built locally, by ``cabal``.
    - ``autostay`` suffix -- enter the workbench shell, start the cluster, and stay in the shell afterwards.  Binaries will be built locally, by ``cabal``.
    - ``autonix`` suffix -- enter the workbench shell, start the cluster.  All binaries will be provided by the Nix CI.
    - ..there are other modes, as per `lib.mk <https://github.com/input-output-hk/cardano-node/tree/master/lib.mk>`_
3. Enter the workbench shell for the chosen profile & mode:
    ``make <PROFILE-NAME>`` or ``make <PROFILE-NAME>-<SUFFIX>`` (when there is a suffix).
4. Optional: start cluster:
    Depending on the chosen mode, your cluster might already start, or you are expected to start it yourself, using ``start-cluster``.

The workbench services are available only inside the workbench shell.

Using Cabal
----

By default, all binaries originating in the ``cardano-node`` repository are available to ``cabal build`` and ``cabal run``, unless the workbench was entered using one of the pure ``*nix`` modes.  Note that in all cases, the dependencies for the workbench are supplied through Nix and have been built/tested on CI.

**Dependency localisation** -or- *Cabal&Nix for painless cross-repository work*
----

The Cabal workflow described above only extends to the repository-local packages.  Therefore, ordinarily, to work on ``cardano-node`` dependencies in the context of the node itself, one needs to go through an expensive multi-step process -- with committing, pushing and re-pinning of the dependency changes.

The **dependency localisation** workflow allows us to pick a subset of leaf dependencies of the ``cardano-node`` repository, and declare them *local* -- so they can be iterated upon using the ``cabal build`` / ``cabal run`` of ``cardano-node`` itself.  This cuts development iteration time dramatically and enables effective cross-repo development of the full stack of Cardano packages.

Without further ado (**NOTE**: *the order of steps is important!*):

1. Ensure that your ``cardano-node`` checkout is clean, with no local modifications.  Also, ensure that you start outside the node's Nix shell.
2. Check out the repository with the dependencies, *beside* the ``cardano-node`` checkout.  You have to check out the git revision of the dependency used by your ``cardano-node`` checkout -- as listed in ``cardano-node/cabal.project``.
    - we'll assume the ``ouroboros-network`` repository
    - ..so a certain parent directory will include checkouts of both ``ouroboros-network`` and ``cardano-node``, at the same level
    - ..and the git revision checked out in ``ouroboros-network`` will match the corresponding ``source-repository-package`` clause in ``cardano-node/cabal.project``.
    - Extra point #1:  you can localise/check out several dependency repositories
    - Extra point #2:  for the dependencies that are not listed in ``cabal.project`` of the node -- how do you determine the version to check out?  You can ask the workbench shell:
         1. Temporarily enter the workbench shell
         2. Look for the package version in ``ghc-pkg list``
         3. Use that version to determine the git revision of the dependency's repository (using a tag or some special knowledge about the version management of said dependency).
3. Enter the workbench shell, as per instructions in previous sections -- or just a plain Nix shell.
4. Ensure you can build ``cardano-node`` with Cabal: ``cabal build exe:cardano-node``.  If you can't something else is wrong.
5. Determine the *leaf dependency set* you will have to work on.  The *leaf dependency set* is defined to include the target package you want to modify, and its reverse dependencies -- that is, packages that depend on it (inside the dependency repository).
    - let's assume, for example, that you want to modify ``ouroboros-consensus-shelley``
    - ``ouroboros-consensus-shelley`` is not a leaf dependency in itself, since ``ouroboros-consensus-cardano`` (of the same ``ouroboros-network`` repository) depends on it -- so the *leaf dependency set* will include both of them.
    - you might find out that you have to include a significant fraction of packages in ``ouroboros-network`` into this *leaf dependency set* -- do not despair.
    - if the *leaf dependency set* is hard to determine, you can use ``cabal-plan`` -- which is included in the workbench shell (which you, therefore, have to enter temporarily):
        .. code-block:: console

            [nix-shell:~/cardano-node]$ cabal-plan dot-png --revdep ouroboros-consensus-shelley

      This command will produce a HUGE ``deps.png`` file, which will contain the entire chart of the project dependencies.  The important part to look for will be the subset of packages highlighted in red -- those, which belong to the ``ouroboros-network`` repository.  This will be the full *leaf dependency set*.
6. Edit the ``cardano-node/cabal.project`` as follows:
    - for the *leaf dependency set*, in the very beginning of the ``cabal.project``, add their relative paths to the ``packages:`` section, e.g.:
        .. code-block:: console

            packages:
                cardano-api
                cardano-cli
                ...
                trace-resources
                trace-forward
                ../ouroboros-network/ouroboros-consensus-shelley
                ../ouroboros-network/ouroboros-consensus-cardano

7. The two packages have now become **local** -- when you try ``cabal build exe:cardano-node`` now, you'll see that Cabal starts to build these dependencies you just localised.  Hacking time!

Hoogle
----

The workbench shell provides ``hoogle``, with a local database for the full set of dependencies:

.. code-block:: console

    [nix-shell:~/cardano-node]$ hoogle search TxId
    Byron.Spec.Ledger.UTxO newtype TxId
    Byron.Spec.Ledger.UTxO TxId :: Hash -> TxId
    Cardano.Chain.UTxO type TxId = Hash Tx
    Cardano.Ledger.TxIn newtype TxId crypto
    Cardano.Ledger.TxIn TxId :: SafeHash crypto EraIndependentTxBody -> TxId crypto
    Cardano.Ledger.Shelley.API.Types newtype TxId crypto
    Cardano.Ledger.Shelley.API.Types TxId :: SafeHash crypto EraIndependentTxBody -> TxId crypto
    Cardano.Ledger.Shelley.Tx newtype TxId crypto
    Cardano.Ledger.Shelley.Tx TxId :: SafeHash crypto EraIndependentTxBody -> TxId crypto
    Ouroboros.Consensus.HardFork.Combinator data family TxId tx :: Type
    -- plus more results not shown, pass --count=20 to see more

Supplementary tooling
====

GHCID
----

run *ghcid* with: ``ghcid -c "cabal repl exe:cardano-node --reorder-goals"``

Haskell Language Server
----

When using Haskell Language Server with Visual Studio Code, you may find that
`HLINT annotations are ignored <https://github.com/haskell/haskell-language-server/issues/638>`_.

To work around this, you may run the script ``./scripts/reconfigure-hlint.sh`` to generate a ``.hlint.yaml``
file with HLINT ignore rules derived from the source code.

Testing
====

``cardano-node`` is essentially a container which implements several components such networking, consensus, and storage. These components have individual test coverage. The node goes through integration and release testing by Devops/QA while automated CLI tests are ongoing alongside development.

Developers on ``cardano-node`` can `launch their own testnets <doc/getting-started/launching-a-testnet.md>`_ or `run the chairman tests <doc/getting-started/running-chairman-tests.md>`_ locally.


Debugging
====

Pretty printing CBOR encoded files
----

It may be useful to print the on chain representations of blocks, delegation certificates, txs and update proposals. There are two commands that do this (for any cbor encoded file):

To pretty print as CBOR:
``cabal exec cardano-cli -- pretty-print-cbor --filepath CBOREncodedFile``

Validate CBOR files
----

You can validate Byron era blocks, delegation certificates, txs and update proposals with the ``validate-cbor`` command.

``cabal exec cardano-cli -- validate-cbor --byron-block 21600 --filepath CBOREncodedByronBlockFile``


****
Native Tokens
****

Native tokens is a new feature that enables the transacting of multi-assets on Cardano. Native tokens are now supported on mainnet and users can transact with ada, and an unlimited number of user-defined (custom) tokens natively.

To help you get started we have compiled a handy list of resources:

`Cardano Forum discussion <https://forum.cardano.org/c/developers/cardano-tokens/150>`_

`Documentation for native tokens <https://docs.cardano.org/native-tokens/learn>`_

You can also read more about `native tokens and how they compare to ada and ERC20 <https://github.com/input-output-hk/cardano-ledger-specs/blob/master/doc/explanations/features.rst>`_. Browse native tokens created on the Cardano blockchain and see their transactions in an interactive dashboard that allows filtering and searching: nativetokens.da.iogservices.io.

****
API Documentation
****

The API documentation is published `here <https://input-output-hk.github.io/cardano-node/>`_.

The documentation is built with each push, but is only published from ``master`` branch.  In order to
test if the documentation is working, build the documentation locally with ``./scripts/haddocs.sh`` and
open ``haddocks/index.html`` in the browser.
