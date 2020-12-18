# Installing the Cardano node

> Note: If you are only interested in holding and using ada, there is no need to separately install and run the `cardano-node` as it comes with the [Daedalus](https://daedaluswallet.io/) wallet.

This document outlines instructions on how to install the Cardano node in case you would like to run your own stake pool. 

There are three ways to install the cardano-node, which are outlined in the increasing order of complexity:

- use the pre-built binaries provided by IOHK;
- build (or install from cache) using the [Nix][nix] package manager;
- install the dependencies and tool chain manually and build from source.

## Pre-built binaries

TODO where are these found?

## Build using Nix

You can install the [Nix][nix] package manager on top of most Linux or Macintosh installations, and it will be provisioning all the build and runtime dependencies of the `cardano-node`.

First, follow instructions on how to install Nix on your system. You will require `sudo` root access.

```bash
curl -L https://nixos.org/nix/install | sh
```

Nix can be instructed to use the IOHK remote cache. This step is optional, however, it will significantly speed up the building process of the `cardano-node`. It can be set up as follows:

```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee /etc/nix/nix.conf
substituters = https://cache.nixos.org https://hydra.iohk.io
trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

> Note: if you are using NixOS, you should instead add the binary cache to your `/etc/nixos/configuration.nix`.

Once you have completed the Nix installation, you can build the node as follows:

```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
nix-build -A scripts.mainnet.node -o mainnet-node-local
./mainnet-node-local
```

This will build the master branch of the `cardano-node`. For building a specific release, see the relevant [https://github.com/input-output-hk/cardano-node/releases](release).

## Building the node manually

**Latest version:** [https://github.com/input-output-hk/cardano-node/releases](https://github.com/input-output-hk/cardano-node/releases)

#### Prerequisites

Set up your platform:

You will need:

* an x86 host \(AMD or Intel\), Virtual Machine or AWS instance with at least 2 cores, 4GB of RAM and at least 10GB of free disk space;
* a recent version of Linux.

#### Install dependencies

You will need the following packages and tools on your Linux system to download the source code and build it:

* the version control system `git`
* the `gcc` C-compiler
* C++ support for `gcc`
* developer libraries for the the arbitrary precision library `gmp`
* developer libraries for the compression library `zlib`
* developer libraries for `systemd`
* developer libraries for `ncurses`
* `ncurses` compatibility libraries
* the Haskell build tool `cabal`
* the GHC Haskell compiler

In Redhat, Fedora and Centos use:

    sudo yum update -y
    sudo yum install git gcc gcc-c++ tmux gmp-devel make tar wget zlib-devel libtool autoconf -y
    sudo yum install systemd-devel ncurses-devel ncurses-compat-libs -y

For Debian/Ubuntu use the following instead:

    sudo apt-get update -y
    sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

> Note: If you use a different version of Linux, you will need to use the package manager that is suitable for your platform (instead of `yum` or `apt-get`). The names of the packages you need to install might differ as well. 

#### Install GHC and Cabal

You will need to install [GHC 8.6.5][ghc865] and [Cabal-3.0][cabal30] versions. We recommend using the Haskell installer tool [ghcup][ghcup] for these needs. 

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
confirm 2x ENTER and type YES at the end to add ghcup to your PATH variable.
Then, restart your terminal session or execute:

```
source ~/.ghcup/env
```
to use the ghcup command for the next steps. 

You can now install and activate the required GHC version:
```
ghcup install ghc 8.6.5
ghcup set ghc 8.6.5
```

#### Install Libsodium

    git clone https://github.com/input-output-hk/libsodium
    cd libsodium
    git checkout 66f017f1
    ./autogen.sh
    ./configure
    make
    sudo make install

Add the following to your `.bashrc` file and source it:

    export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

#### Download the source code for the cardano-node

Run the following command in the home directory:

    git clone https://github.com/input-output-hk/cardano-node.git

Change the working directory to the downloaded source code folder:

    cd cardano-node

Check the latest version of the cardano-node:

    git fetch --all --tags
    git tag
    git checkout tags/<TAGGED VERSION>

#### Build and install the node

Build and install the node with `cabal`:

    cabal build all

Copy the executables files to the `.local/bin` directory. Replace the place holder <TAGGED VERSION> with your targeted version:

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-node-<TAGGED VERSION>/x/cardano-node/build/cardano-node/cardano-node ~/.local/bin/

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-cli-<TAGGED VERSION>/x/cardano-cli/build/cardano-cli/cardano-cli ~/.local/bin/

Check the installed version:

    cardano-cli --version

If you need to update to a newer version, repeat this process.

> Note: It might be necessary to delete the `db`-folder \(the database-folder\) before running an updated version of the node.


[ghcup]: https://www.haskell.org/ghcup/
[cabal30]: https://www.haskell.org/cabal/download.html
[ghc865]: https://www.haskell.org/ghc/blog/20190423-ghc-8.6.5-released.html
[nix]: https://nixos.org/nix/
