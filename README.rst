.. raw:: html

   <p align="center">
     <big><strong>Cardano Node</strong></big>
   </p>

.. raw:: html

   <p align="center">
     <a href="https://github.com/input-output-hk/cardano-node/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-node.svg?style=for-the-badge" /></a>
     <a href="https://buildkite.com/input-output-hk/cardano-node"><img src="https://img.shields.io/buildkite/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a/master?label=BUILD&style=for-the-badge"/></a>
   </p>

cardano-node
============

Integration of the `ledger`_, `consensus`_ and `networking`_
repositories. We use the `iohk-monitoring-framework`_ for logging.

-  The cardano-node is the top level for the node and aggregates the
   other components from other packages: consensus, ledger and
   networking, with configuration, CLI, logging and monitoring.

-  The node no longer incorporates wallet or explorer functionality. The
   wallet backend and explorer backend are separate components that run
   in separate external processes that communicate with the node via
   local IPC.

Network Configuration, Genesis and Topology Files
-------------------------------------------------

The latest supported networks can be found at
`https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html`_

How to build
------------

Cabal
-----

Dependencies:
^^^^^^^^^^^^^

You will need to install one or both of these libraries on your system:

-  libsodium
-  libsodium-dev

For example, on Ubuntu:

::

      sudo apt install libsodium-dev

For ease of use, also create a ``cabal.project.local`` at the toplevel
with:

::

   package cardano-crypto-praos
     flags: -external-libsodium-vrf

There are two choices for building ``cardano-crypto-praos``, selected
via a cabal flag ``external-libsodium-vrf``:

1. ``+external-libsodium-vrf`` Which requires a special fork of
   ``libsodium`` with the VRF code to be installed on the system
2. ``-external-libsodium-vrf`` Which uses a bundled version of the VRF C
   code and uses the vanilla system ``libsodium`` The second option is
   currently only suitable for development -- but not deployment --
   because it uses the libsodium C code in an unsupported configuration

Use `Cabal - Version 3.0`_ to build this project, including the CLI
interface to the node:

::

   $ cabal build cardano-node cardano-cli

Install the ``cardano-node`` and ``cardano-cli`` commands using
``cabal``. Not that you may need to specify the installation directory
using ``--installdir``.

::

   $ cabal install cardano-node cardano-cli

Windows Executable
------------------

Download
~~~~~~~~

You can download `here`_.

Instructions
~~~~~~~~~~~~

The download includes ``cardano-node.exe`` and a ``.dll`` file. To run
the

.. _ledger: https://github.com/input-output-hk/cardano-ledger
.. _consensus: https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus
.. _networking: https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network
.. _iohk-monitoring-framework: https://github.com/input-output-hk/iohk-monitoring-framework
.. _`https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html`: https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html
.. _Cabal - Version 3.0: https://www.haskell.org/cabal/
.. _here: https://hydra.iohk.io/job/Cardano/cardano-node/cardano-node-win64/latest-finished