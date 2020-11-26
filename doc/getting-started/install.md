# Installing the node from source

**Latest version:** [https://github.com/input-output-hk/cardano-node/releases](https://github.com/input-output-hk/cardano-node/releases)

#### Prerequisites

Set up your platform:

You will need:

* An x86 host \(AMD or Intel\), Virtual Machine or AWS instance with at least 2 cores, 4GB of RAM and at least 10GB of free disk space;
* A recent version of Linux.

The node can be built and run on other operating systems, including Windows and MacOSX, but there are performance advantages that mean that
stake pool operators are recommended to use Linux.  If you are building on Windows, we recommend using WSL2 under Windows 10.  This provides a development and execution environment that is very similar to Ubuntu.

#### Install dependencies

We need the following packages and tools on our Linux system to download the source code and build it:

* the version control system `git`,
* the `gcc` C-compiler,
* C++ support for `gcc`,
* developer libraries for the arbitrary precision library `gmp`,
* developer libraries for the compression library `zlib`,
* developer libraries for `systemd`,
* developer libraries for `ncurses`,
* `ncurses` compatibility libraries,
* the Haskell build tool `cabal`,
* the GHC Haskell compiler.

In Redhat, Fedora and Centos

    sudo yum update -y
    sudo yum install git gcc gcc-c++ tmux gmp-devel make tar xz wget zlib-devel libtool autoconf -y
    sudo yum install systemd-devel ncurses-devel ncurses-compat-libs -y

For Debian/Ubuntu use the following instead:


    sudo apt-get update -y
    sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

If you are using a different flavor of Linux, you will need to use the package manager suitable for your platform instead of `yum` or `apt-get`, and the names of the packages you need to install might differ.  On MacOSX, use the Homebrew (`brew`) installer.

#### Download, unpack, install and update Cabal:

    wget https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    tar -xf cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    rm cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz cabal.sig
    mkdir -p ~/.local/bin
    mv cabal ~/.local/bin/

Verify that ~/.local/bin is in your PATH

    echo $PATH

If `~/.local/bin` is not in the PATH, you need to add the following line to  your `.bashrc` file

    export PATH="~/.local/bin:$PATH"

and source the file

    source .bashrc

Update cabal

    cabal update

Confirm that you installed cabal version `3.2.0.0`.

    cabal --version
       
**Note:** We no longer provide supported `stack` or `nix` installer packages. We recommend `cabal` instead.


#### Download and install GHC:

Create a working directory for your builds:

    mkdir -p ~/src
    cd ~/src
 
 Download and install the latest version of GHC:
 
    wget https://downloads.haskell.org/ghc/8.10.2/ghc-8.10.2-x86_64-deb9-linux.tar.xz
    tar -xf ghc-8.10.2-x86_64-deb9-linux.tar.xz
    rm ghc-8.10.2-x86_64-deb9-linux.tar.xz
    cd ghc-8.10.2
    ./configure
    sudo make install
    
This assumes GHC 8.10.2 on Linux (the most recent version at the time of writing).  If you are installing on MacOSX or Windows, download the compiler from `https://www.haskell.org/platform/mac.html` instead, and follow the installation instructions.

#### Install Libsodium

Create a working directory for your builds:

    mkdir -p ~/src
    cd ~/src
 
Download and install libsodium

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

Create a working directory for your builds:

    mkdir -p ~/src
    cd ~/src
    
Download the Cardano node sources:

    git clone https://github.com/input-output-hk/cardano-node.git

Change the working directory to the downloaded source code folder:

    cd cardano-node


Checkout the latest version of cardano-node (choose the tag with the highest version number: ``TAGGED-VERSION``):

    git fetch --all --recurse-submodules --tags
    git tag
    git checkout tags/<TAGGED VERSION>

#### Configure the build options

We explicitly use the GHC version that we installed earlier.  This avoids defaulting to a system version of GHC that might be older than the one you have installed.

    cabal configure --with-compiler=ghc-8.10.2

Update the local project file to use the VRF library that you installed earlier.
   
    echo "package cardano-crypto-praos" >>  cabal.project.local
    echo "  flags: -external-libsodium-vrf" >>  cabal.project.local


#### Build and install the node

Build the node and CLI with `cabal`:

    cabal build all

Install the newly built node and CLI commands:

    cabal install all --bindir ~/.local/bin

If this doesn't work, you can manually copy the executable files to the `~/.local/bin` directory. Replace the place holder `<TAGGED VERSION>` with your targeted version:

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.10.2/cardano-node-<TAGGED VERSION>/x/cardano-node/build/cardano-node/cardano-node ~/.local/bin/

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.10.2/cardano-cli-<TAGGED VERSION>/x/cardano-cli/build/cardano-cli/cardano-cli ~/.local/bin/

Check the version that has been installed:

    cardano-cli --version

Repeat the above process when you need to update to a new version.


**Note:** It might be necessary to delete the `db`-folder \(the database-folder\) before running an updated version of the node.
