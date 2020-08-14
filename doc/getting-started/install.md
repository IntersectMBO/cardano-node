# Installing the node from source

#### Prerequisites

Set up your platform:

You will need:

* An x86 host \(AMD or Intel\), Virtual Machine or AWS instance with at least 2 cores, 4GB of RAM and at least 10GB of free disk space;
* A recent version of Linux.

#### Install dependencies

We need the following packages and tools on our Linux system to download the source code and build it:

* the version control system `git`,
* the `gcc` C-compiler,
* C++ support for `gcc`,
* developer libraries for the the arbitrary precision library `gmp`,
* developer libraries for the compression library `zlib`,
* developer libraries for `systemd`,
* developer libraries for `ncurses`,
* `ncurses` compatibility libraries,
* the Haskell build tool `cabal`,
* the GHC Haskell compiler.

In Redhat, Fedora and Centos

    sudo yum update -y
    sudo yum install git gcc gcc-c++ tmux gmp-devel make tar wget zlib-devel libtool autoconf -y
    sudo yum install systemd-devel ncurses-devel ncurses-compat-libs -y

For Debian/Ubuntu use the following instead:


    sudo apt-get update -y
    sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

If you are using a different flavor of Linux, you will need to use the package manager suitable for your platform instead of `yum` or `apt-get`, and the names of the packages you need to install might differ.

#### Download, unpack, install and update Cabal:

    wget https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    tar -xf cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    rm cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz cabal.sig
    mkdir -p ~/.local/bin
    mv cabal ~/.local/bin/

Verify that .local/bin is in your PATH

    echo $PATH

If `.local/bin` is not in the PATH, you need to add the following line to  your `.bashrc` file

    export PATH="~/.local/bin:$PATH"

and source the file

    source .bashrc

Update cabal

    cabal update

Confirm that you installed cabal version `3.2.0.0`.

    cabal --version

#### Download and install GHC:

    wget https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-deb9-linux.tar.xz
    tar -xf ghc-8.6.5-x86_64-deb9-linux.tar.xz
    rm ghc-8.6.5-x86_64-deb9-linux.tar.xz
    cd ghc-8.6.5
    ./configure
    sudo make install

Back in your home directory:

#### Install Libsodium

    git clone https://github.com/input-output-hk/libsodium
    cd libsodium
    git checkout 66f017f1
    ./autogen.sh
    ./configure
    make
    sudo make install

Add the following to your .bashrc file and source it.

    export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

#### Download the source code for cardano-node

Back in the home directory: 

    git clone https://github.com/input-output-hk/cardano-node.git

Change the working directory to the downloaded source code folder:

    cd cardano-node


Checkout the latest version of cardano-node

    git fetch --all --tags
    git tag
    git checkout tags/<TAGGED VERSION>

#### Build and install the node

Build and install the node with `cabal`,

    cabal build all

Copy the executables files to the `.local/bin` directory. Replace the place holder <TAGGED VERSION> with your targeted version:

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-node-<TAGGED VERSION>/x/cardano-node/build/cardano-node/cardano-node ~/.local/bin/

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-cli-<TAGGED VERSION>/x/cardano-cli/build/cardano-cli/cardano-cli ~/.local/bin/

Check the version installed:

    cardano-cli --version

If you need to update to a newer version repeat this process.


**Note:** It might be necessary to delete the `db`-folder \(the database-folder\) before running an updated version of the node.
