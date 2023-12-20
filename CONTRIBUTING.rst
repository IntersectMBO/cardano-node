********************************************
Contributing to the ``cardano-node`` project
********************************************

The ``cardano-node`` development is primarily based on the Nix infrastructure (https://nixos.org/ ), which enables packaging, CI, development environments and deployments.

On how to set up Nix for ``cardano-node`` development, please see `Building Cardano Node with nix <https://github.com/input-output-hk/cardano-node-wiki/wiki/building-the-node-using-nix>`_.


Roles and Responsibilities
====

We maintain a [CODEOWNERS file][CODEOWNERS] which provides information who
should review a contributing PR.  Note that you might need to get approvals
from all code owners (even though GitHub doesn't give a way to enforce it).

[CODEOWNERS]: https://github.com/intersectmbo/cardano-node/blob/master//CODEOWNERS


Supplementary tooling
====

GHCID
----

run *ghcid* with: ``ghcid -c "cabal repl exe:cardano-node --reorder-goals"``

Testing
====

``cardano-node`` is essentially a container which implements several components such networking, consensus, and storage. These components have individual test coverage. The node goes through integration and release testing by Devops/QA while automated CLI tests are ongoing alongside development.

Developers on ``cardano-node`` can `launch their own testnets <https://github.com/input-output-hk/cardano-node-wiki/wiki/launching-a-testnet>`_ or `run the chairman tests <https://github.com/input-output-hk/cardano-node-wiki/wiki/running-chairman-tests>`_ locally.

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

Updating dependencies
====

... from Hackage
----

Updating package dependencies from Hackage should work like normal in a Haskell project. 
The most important thing to note is that we pin the ``index-state`` of the Hackage package index in ``cabal.project``. 
This means that cabal will always see Hackage “as if” it was that time, ensuring reproducibility. 
But it also means that if you need a package version that was released *after* that time, you need to bump the ``index-state`` (and to run ``cabal update`` locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you will also need to pull in the Nix equivalent of the newer ``index-state``. 
You can do this by running ``nix flake lock --update-input hackageNix``.

... from the Cardano package repository
----

Many Cardano packages are not on Hackage and are instead in the `Cardano package repository <https://github.com/input-output-hk/cardano-haskell-packages>`__, see the README for (lots) more information. 
Getting new packages from there works much like getting them from Hackage. 
The differences are that it has an independent ``index-state``, and that there is a different Nix command you need to run afterwards: ``nix flake lock --update-input CHaP``.

Using unreleased versions of dependencies
~~~~

Sometimes we need to use an unreleased version of one of our dependencies, either to fix an issue in a package that is not under our control, or to experiment with a pre-release version of one of our own packages.
You can use a ``source-repository-package`` stanza to pull in the unreleased version.
Try only to do this for a short time, as it does not play very well with tooling, and will interfere with the ability to release the node itself.

For packages that we do not control, we can end up in a situation where we have a fork that looks like it will be long-lived or permanent (e.g. the maintainer is unresponsive, or the change has been merged but not released).
In that case, release a patched version to the `Cardano package repository <https://github.com/input-output-hk/cardano-haskell-packages>`__, which allows us to remove the ``source-repository-package`` stanza.
See the README for instructions.

Releasing a version of the node
====

(There is much more to say here, this is just a small fragment)

... to the Cardano package repository
----

When releasing a new version of the node, it and the other packages in this repository should be released to the `Cardano package repository <https://github.com/input-output-hk/cardano-haskell-packages>`__.
See the README for instructions, including a script to automate most of the process. 
Please note that libraries need bounds on the version of their dependencies to avoid bitrot and be effectively reusable.

Workbench: a local cluster playground
====

You can quickly spin up a local cluster (on Linux and Darwin), based on any of a wide variety of configurations, and put it under a transaction generation workload -- using the ``workbench`` environment:

1. Optional: choose a workbench profile:
    - ``default`` stands for a light-state, 6-node cluster, under saturation workload, indefinite runtime
    - ``ci-test`` is the profile run in the node CI -- very light, just two nodes and short runtime
    - ``devops`` is an unloaded profile (no transaction generation) with short slots -- ``0.2`` sec.
    - ..and many more -- which can be either:
        - listed, by ``make ps``
        - observed at their point of definition: `nix/workbench/profiles/prof1-variants.jq <https://github.com/intersectmbo/cardano-node/tree/master/nix/workbench/profiles/prof1-variants.jq#L333-L526>`_
2. Optional: select mode of operation, by optionally providing a suffix:
    - default -- no suffix -- just enter the workbench shell, allowing you to run ``start-cluster`` at any time.  Binaries will be built locally, by ``cabal``.
    - ``autostay`` suffix -- enter the workbench shell, start the cluster, and stay in the shell afterwards.  Binaries will be built locally, by ``cabal``.
    - ``autonix`` suffix -- enter the workbench shell, start the cluster.  All binaries will be provided by the Nix CI.
    - ..there are other modes, as per `lib.mk <https://github.com/intersectmbo/cardano-node/tree/master/lib.mk>`_
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
    - ..and the git revision checked out in ``ouroboros-network`` will match the version of the ``ouroboros-network`` packages used currently 
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
    - for the *leaf dependency set* in the very beginning of the ``cabal.project``, add their relative paths to the ``packages:`` section, e.g.:
        .. code-block:: console

            packages:
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

