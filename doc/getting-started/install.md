# Installing the node from source

The **latest** version of the node may be downloaded from the [cardano-node GitHub Releases](https://github.com/input-output-hk/cardano-node/releases) page.

#### Prerequisites

To set up your platform, you will need:

* An x86 host \(AMD or Intel\)
* A machine with at least **2 cores**
* A recent version of Linux
* **12GB** of RAM 
* At least **100GB** of free disk space to store the local copy of the blockchain. 

The space requirements increase with every new block so new more disk space will be needed after a few months of operation. 

**Note** The node can be built and run on other operating systems, including Windows and MacOSX, but we recommend that
stake pool operators use Linux to take advantage of the associated performance advantages. If you are building on Windows, we recommend using WSL2 under Windows 10 as this provides a development and execution environment that is very similar to Ubuntu.

#### Installation dependencies

To download the source code and build it, you need the following packages and tools on your Linux system:

* the version control system `git`,
* the `gcc` C-compiler,
* C++ support for `gcc`,
* developer libraries for the arbitrary precision library `gmp`,
* developer libraries for the compression library `zlib`,
* developer libraries for `systemd`,
* developer libraries for `ncurses`,
* `ncurses` compatibility libraries,
* the Haskell build tool `cabal`,
* the GHC Haskell compiler (version `8.10.7` or above).

In Redhat, Fedora, and Centos:

```bash
sudo yum update -y
sudo yum install git gcc gcc-c++ tmux gmp-devel make tar xz wget zlib-devel libtool autoconf -y
sudo yum install systemd-devel ncurses-devel ncurses-compat-libs -y
```

For Debian/Ubuntu, use the following instead:

```bash
sudo apt-get update -y
sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y
```

If you are using a different flavor of Linux, you will need to use the correct package manager for your platform instead of `yum` or `apt-get`, and the names of the packages you need to install might differ.  On MacOSX, use the Homebrew (`brew`) installer.

#### Installing the Haskell environment

The recommended way to install the Haskell tools is via [GHCup](https://www.haskell.org/ghcup/). Its installation script will guide you through the installation, and warn you about packages that you have to make sure are installed in the system (the ones described on the step above). Check [this page](https://www.haskell.org/ghcup/install/) for further explanation on the installation process.

Once GHCup is installed, open a new terminal (to get an updated environment) and run:

```bash
ghcup install ghc 8.10.7
ghcup install cabal 3.6.2.0
ghcup set ghc 8.10.7
ghcup set cabal 3.6.2.0
```

Alternatively, with `ghcup tui` you can pick the specific versions of the tools that you want to install, in particular you should have installed and set:
- `cabal >= 3.6.2.0`
- `GHC >= 8.10.7`

To check that you will use the GHCup tools (and not any other installation on the system), you can execute

```bash
which cabal
```

and it should return a path of this shape: `/home/<user>/.ghcup/bin/cabal`.

#### Installing Libsodium

Create a working directory for your builds:

```bash
mkdir -p ~/src
cd ~/src
```

Download and install libsodium:

```bash
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout 66f017f1
./autogen.sh
./configure
make
sudo make install
```

Add the following to your `~/.bashrc` file and source it (or re-open the terminal):

```bash
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

#### Installing Secp256k1

Create a working directory for your builds:

```bash
mkdir -p ~/src
cd ~/src
```

Download and install `libsecp256k1`:

```bash
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
sudo make install
```

#### Downloading the source code for cardano-node

Create a working directory for your builds:

```bash
mkdir -p ~/src
cd ~/src
```

Download the Cardano node sources:

```bash
git clone https://github.com/input-output-hk/cardano-node.git
```

Change the working directory to the downloaded source code folder:

```bash
cd cardano-node
```


Check out the latest version of cardano-node (choose the tag with the highest version number: ``TAGGED-VERSION``):

```bash
git fetch --all --recurse-submodules --tags
git tag
git checkout tags/<TAGGED VERSION>
```

#### Configuring the build options

We explicitly use the GHC version that we installed earlier.  This avoids defaulting to a system version of GHC that might be older than the one you have installed.

```bash
cabal configure --with-compiler=ghc-8.10.7
```

Note, that for a development build you can avoid installing the custom `libsodium` library and add the following lines to the local project file:

```bash
echo "package cardano-crypto-praos" >>  cabal.project.local
echo "  flags: -external-libsodium-vrf" >>  cabal.project.local
```

#### Building and installing the node

Build the node and CLI with `cabal`:

```bash
cabal build all
```

Install the newly built node and CLI commands to the `~/.local/bin` directory:

```bash
mkdir -p ~/.local/bin
cp -p "$(./scripts/bin-path.sh cardano-node)" ~/.local/bin/
cp -p "$(./scripts/bin-path.sh cardano-cli)" ~/.local/bin/
```

**Note:** `~/.local/bin` should be in the `$PATH`.

Note, we avoid using `cabal install` because that method prevents the installed binaries from reporting
the git revision with the `--version` switch.

Check the version that has been installed:

```bash
cardano-cli --version
```

Repeat the above process when you need to update to a new version.


**Note:** It might be necessary to delete the `db`-folder \(the database-folder\) before running an updated version of the node.
