[![Build Status](https://badge.buildkite.com/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a.svg)](https://buildkite.com/input-output-hk/cardano-node)

# cardano-node

Integration of the [ledger](https://github.com/input-output-hk/cardano-ledger),
[consensus](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus),
[networking](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network)
and [node shell](https://github.com/input-output-hk/cardano-shell)
repositories.
[Logging](https://github.com/input-output-hk/iohk-monitoring-framework) is
provided as
a [feature](https://github.com/input-output-hk/cardano-shell/blob/master/app/Cardano/Shell/Features/Logging.hs)
by the node shell to the other packages.

## Install from Source
The **Haskell Cabel** system is used to build `cardano-node`. While `cabal-install` may be used, this tutorial will use `stack` to build `cardano-node`.

Your target OS should be classified as [Tier 1 in the [The Glasgow Haskell Compiler platforms list](https://gitlab.haskell.org/ghc/ghc/wikis/platforms).

**Note:** Only x86_64 is supported according to [input-output-hk/cardano-base#31](https://github.com/input-output-hk/cardano-base/issues/31#issuecomment-512183547).

The following tools may need to be installed, especially when starting from a fresh minimal install of Linux:

* curl - Command line tool and library for transferring data with URLs.
* Git - Free and open source distributed version control system for managing source code.
* OpenSSL - A software library for applications that secure communications over computer networks.
* libtinfo - A developers library for `nCurses` text rendering.
* journald - Introduced with `systemd` to collect and manage logging information.
* zlib - A data compression software library.

### How to Install Prerequsites
For your specific distribution of Linix, use `sudo` to execute the commands with elevated priviledges from a terminal window:

##### Debian / Ubuntu:

```bash
# Download and update the package lists to get the latest versions of the applications and their dependencies. 
sudo apt-get update

# Install the necessary prerequsite files, programs, and libraries. 
sudo apt-get install -y curl git libssl-dev libsystemd-dev libtinfo-dev zlib1g-dev
```

##### CentOS / Fedora / RHEL:

```bash
# Install the necessary prerequsite files, programs, and libraries.
sudo yum install -y curl git ncurses-libs openssl-devel systemd-devel zlib-devel
```

The GHC linker may not detect the `libtinfo.so` library when building the `cardano-node` software. If you encounter this error you can return to this section and execute the commands below as a workaround. The commands manually link the installed library to the expected filename.

First, determine which version of the library is installed by executing the following command:
```bash
# Make note of the number at the end of the filename.
ls -la /usr/lib64/libtinfo.so* | grep "\->" -v
```
The filename returned above will end with a number. Replace the number in the example command below with the number returned above:

```bash
# This example uses the number 6. Use the version discovered above.
sudo ln -s /usr/lib64/libtinfo.so.6 /usr/lib64/libtinfo.so
```

##### openSUSE / SLES:

```bash
# Install the necessary prerequsite files, programs, and libraries.
sudo zypper install -y curl gcc git gmp-devel libopenssl-devel systemd-devel zlib-devel
```

### Install the `stack` Build Tool

Download and execute the stack installation script using `curl`. You will be prompted for the root password to proceed with the installation:

```bash
# Installing stack via curl requires you to enter the root password when prompted.
curl --proto '=https' --tlsv1.2 -sSf https://get.haskellstack.org/ | sh -s --
```

### Use Git to Download the `cardano-node` Source Code
Git is used to **clone** the **iohk/cardano-node** repository from GitHub into a local repository. From a terminal window, run the following commands:

```bash
# Clone the repository.
git clone https://github.com/input-output-hk/cardano-node/

# Change to the new cardano-node directory.
cd cardano-node

# Make sure you are using the latest version of the **master** branch.
git checkout
```

### Use `stack` to Build `cardano-node`

The following commands will use **stack** to build the `cardano-node` binaries.

```bash
stack build \
  --flag cardano-node:-with_wallet_client \
  --flag cardano-node:-with_chairman \
  cardano-node
stack install \
  --flag cardano-node:-with_wallet_client \
  --flag cardano-node:-with_chairman \
  cardano-node
```

If all goes well you should find the binaries in the `~/.local/bin` directory. You should see `cardano-node` and `cardano-cli` listed when executing the `ls -l ~/.local/bin` command.
