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

## Building cardano-node from source
**cardano-node** is written in **Haskell** programing language and uses **Cabal** build target definitions, which means both `cabal-install` and `stack` could be used to build `cardano-node`, however only `stack` is a supported method in this tutorial. In theory, building `cardano-node` on most platforms should be feasible, however only [Tier 1 platforms of The Glasgow Haskell Compiler](https://gitlab.haskell.org/ghc/ghc/wikis/platforms) are within scope of this tutorial.

### Building cardano-node on Linux with x86_64 architecture
In this tutorial we assume you're starting your build on frest, minimal install of your chosen Linux distribution, so firstly we will install basic required tools and libraries, we will need to install following:

* curl - command line tool and library for transferring data with URLs
* Git - Version Control System (SCM) also refered to as Source Code Management System (SCM)
* OpenSSL - software library and tool containing highly optimized version of basic cryptography primitives
* libtinfo - `nCurses` library for Text User Interface rendering
* journald - system service for collecting and storing log data, introduced with `systemd`
* zlib - software library used for data compression

#### Installing required tools on different Linux distributions
Open a terminal window and execute distribution specific commands under regular user with elevated priviledges using `sudo`.

**Debian** / **Ubuntu**

```bash
sudo apt-get update # Update metadata for backages provided by your Linux distribution
sudo apt-get install -y curl git libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev # Install required libraries and their header files required to build software
```
**RHEL** / **CentOS** / **Fedora**

```bash
sudo yum install -y curl git ncurses-libs openssl-devel systemd-devel zlib-devel # Install required libraries and their header files required to build software
```

**NOTE**: Additional work-around is needed for `vty-5.25.1` dependency, when on **RHEL** / **CentOS** / **Fedora** GHC linker is unable to detect the relevant object files for `libtinfo.so` library, apply following *hack* `sudo ln -s /usr/lib64/libtinfo.so.6 /usr/lib64/libtinfo.so`. The final number `6` may differ depending on the version of `ncurses-libs` linstalled.

**SLES** / **openSUSE**

```bash
sudo zypper install -y curl gcc git gmp-devel libopenssl-devel systemd-devel zlib-devel # Install required libraries and their header files required to build software
```

#### Installing `stack` software building toolkit on all Linux distributions
Execute `curl --proto '=https' --tlsv1.2 -sSf https://get.haskellstack.org/ | sh -s --` in the open terminal window and enter `root` password when prompted:

```bash
ladm_mark_stopka@mark-asus-zenbook-s:~> curl --proto '=https' --tlsv1.2 -sSf https://get.haskellstack.org/ | sh -s --
Detected Linux distribution: opensuse

This installer doesn't support your Linux distribution, trying generic
bindist...

  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   628  100   628    0     0   1764      0 --:--:-- --:--:-- --:--:--  1759
100 7404k  100 7404k    0     0  3150k      0  0:00:02  0:00:02 --:--:-- 5971k
Installing Stack to: /usr/local/bin/stack...

About to use 'sudo' to run the following command as root:
    install -c -o 0 -g 0 -m 0755 /tmp/tmp.h54iH2BT2W/stack /usr/local/bin
in order to copy 'stack' to the destination directory.

[sudo] password for root:
```

#### Make a copy of `cardano-node` source-code repository with Git
Use Git to make a `clone` of `cardano-node` GitHub repository and switch into the local copy, in open terminal window, run:
```bash
git clone https://github.com/input-output-hk/cardano-node/
cd cardano-node # Here you can also checkout specific cardano-node version using git checkout feature
```

This will build the latest version in Git `master` branch, you can also checkout and build a specific version or a feature to test using `git checkout`.

#### Initiate `cardano-node` build with `stack`
Use [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) to build required `cardano-node` binaries, there are 4 executable targets defined for `cardano-node`, however only `cardano-cli` and `cardano-node` are required to connect to the Cardano Blockchain network. In open terminal window, run:

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

Unless you've changed some other configuration, or the build failed for some reason ([open GitHub Issue](https://github.com/input-output-hk/cardano-node/issues/new)), your `cardano-node` and `cardano-cli` binaries are in `~/.local/bin` directory, which content you can list by issuing `ls -l ~/.local/bin` command.

**NOTE**: You can also change the installation path by supplyiong option `--local-bin-path=/usr/local/bin` to `stack install` command, but please note that any path non-writeable by regular user will require permission elevation.

### Building cardano-node on Linux on non-x86_64 architecture

Building `cardano-node` on non-x86_64 architecture is currently not supported as indicated in [input-output-hk/cardano-base#31](https://github.com/input-output-hk/cardano-base/issues/31#issuecomment-512183547)
