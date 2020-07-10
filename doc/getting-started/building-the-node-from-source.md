# Building Cardano Node from Source

## Installing dependancies
By building and running a node directly from the source code, you can ensure that you get all the latest code updates.

The following instructions presume that you will be running your node on a Linux system and are using cabal. For more information, see the [supported platforms](shelley/about/supported-platforms/) page. You can run a node on any platform by [using a virtual machine](/shelley/get-started/installing-and-running-the-cardano-node/running-the-node-on-an-aws-instance/).

To build and run a node from source, you need the following packages and tools:

* the Haskell platform and Haskell build-tool cabal
* the version control system git
* the gcc C-compiler
* C++ support for gcc
* developer libraries for the the arbitrary precision library gmp
* developer libraries for the compression library zlib
* developer libraries for systemd and ncurses

You can install these dependencies as follows:

CentOS/RHEL based system
```shell
sudo yum update -y
sudo yum install git gcc gmp-devel -y
sudo yum install zlib-devel systemd-devel ncurses-devel -y
```

Debian/Ubuntu use the following instead:

```shell
sudo apt-get update -y
sudo apt-get install build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 -y
```

If you are using a different flavor of Linux, you will need to use the package manager suitable for your platform, and the names of the packages you need to install might differ.

download, unpack, install and update Cabal:

    wget https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    tar -xf cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    rm cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz cabal.sig
    mkdir -p ~/.local/bin
    mv cabal ~/.local/bin/

download and install GHC:

    wget https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-deb9-linux.tar.xz
    tar -xf ghc-8.6.5-x86_64-deb9-linux.tar.xz
    rm ghc-8.6.5-x86_64-deb9-linux.tar.xz
    cd ghc-8.6.5
    ./configure
    sudo make install

Add ~/.cabal/bin/ and ~/.local/bin/ to the PATH

Then run:

    cabal update

## How to build and run the node from source:

1. In the terminal, run the following git command to clone the Cardano node repository and download the source code:
   ```shell
   git clone https://github.com/input-output-hk/cardano-node.git
   ```
1. Download the latest source code from the [releases page](https://github.com/input-output-hk/cardano-node/releases) to this folder. After the download has finished, you can check the contents using the following command:
   ```shell
   ls cardano-node
   ```
1. Change your working directory to the folder in which the source code was downloaded using the following command:
   ```shell
   cd cardano-node
   ```
1. You should check out the latest release, for example tag 1.13.0 using the following command:
   ```shell
   git fetch --all --tags
   git checkout tags/1.13.0
   ```
5. Build the source code using Cabal by running the following command:
   ```shell
   cabal install cardano-node cardano-cli
   ```
Please note that building the node may take some time, possibly several hours.  

6. When the process finishes run the following to validate that you have the correct version: 
   ```shell
   cardano-cli --version
   > cardano-cli-1.13.0 ...
   ```
