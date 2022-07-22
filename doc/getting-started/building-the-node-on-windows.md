# Building the development version of the Cardano node on Windows

This document explains how to build a __DEVELOPMENT__ version of the `cardano-node`. Note that this is *not* for building a __PRODUCTION__ version of the node.

See [install.md](./install.md) for a more thorough explanation of the dependencies and packages that have to be installed to run a node. Although the installation instructions in that document are aimed at building on Linux, large parts of the text are interesting to Windows users as well.

## Install the Haskell environment

The recommended way to install the Haskell tools is via [GHCup](https://www.haskell.org/ghcup/). Check [this page](https://www.haskell.org/ghcup/install/) for further explanation on the installation process.

Once GHCup is installed, open a new terminal (to get an updated environment) and run:

```bash
ghcup install ghc 8.10.7
ghcup install cabal 3.6.2.0
ghcup set ghc 8.10.7
ghcup set cabal 3.6.2.0
```

## Install MSYS2 and basic dependencies

### GHCUP and MSYS2
​
Note that the GHCUP comes with its own installation for MSYS2, and it can be used to build `cardano-node`. You can start the MSYS2 shell with
```cmd
<ghcup-path>\msys64\msys2_shell.cmd -defterm -here -no-start -mingw64
```
​
Make sure to enable `mingw64` subsystem. It is also possible to enable `mingw64` subsystem by
​
```bash
source shell mingw64
```
​
​
The MSYS2 shell is bundled with `pacman` package manager. `GHC` is bundled with `gcc` for `mingw`. Configure your environment to use this tool-chain.
​
```bash
pacman -Syu
pacman -S --needed base-devel automake git
export PATH=$PATH:/mingw64/bin:<ghcup-path>/bin:<ghcup-path>/ghc/<ghc-version>/mingw/bin
```
​
Alternatively, you can update the `mingw64` tool-chain, that adds number of packages including latest `gcc`. This may give rise to `crt` linker error. Please read the [section - global cabal configuration](#global-cabal-configuration) to mitigate the error.
​
```bash
pacman -S mingw-w64-x86_64-toolchain
```

### Installing MSYS independently

Go to [the MSYS2 website](https://www.msys2.org/) and follow its instructions to install MSYS2. It will also install `pacman`. Then, update and upgrade all `pacman` packages and install basic dependencies:
​
```
pacman -Syu
pacman -S --needed base-devel mingw-w64-x86_64-toolchain git
```

## Installing and configuring third party libraries
You can use `vcpkg`, or `pacman` or both to install third party libraries. Following are the dependencies for `cardano-node`.

| package   | `vcpkg`   | `pacman`                   |  Remarks
|-----------|-----------|----------------------------|--------------------------------------
| libsodium | libsodium | mingw-w64-x86_64-lmdb      |
| lmdb      | lmdb      | mingw-w64-x86_64-libsodium |
| secp256k1 | secp256k1 | __not available__          | `vcpkg` package is not complete.
| openssl   | openssl   | mingw-w64-x86_64-openssl   |
|-----------|-----------|----------------------------|--------------------------------------

### Using __vcpkg__
- Install `vcpkg` for MSYS2 with instruction given [here](https://vcpkg.io/en/docs/users/mingw.html)
- Set following variables for installing libraries
  ```bash
  export VCPKG_DEFAULT_TRIPLET=x64-mingw-static
  export VCPKG_DEFAULT_HOST_TRIPLET=x64-mingw-dynamic
  ```
- You can also use `x64-mingw-dynamic` for `VCPKG_DEFAULT_TRIPLET`, if you'd like to use shared libraries
- Install libraries
  ```bash
  vcpkg install libsodium
  vcpkg install openssl
  vcpkg install lmdb
  ```
- __cardano-crypto-class__ uses several features of `secp256k1` that are not compiled into `vcpkg` package. Hence it needs to be compiled separately.
- `vcpkg` generates `pkg-config` when it can. It generates configuration for `openssl`. To avoid mixing configurations with existing system, you can use following to install packages
  ```bash
  PATH="${PATH/:\/usr\/bin:\/bin:/:}:/ming64/bin" ./vcpkg.exe install <pkg-name>
  ```
  + Note that packages will be generated at `<vc-pkg>/installed/${VCPKG_DEFAULT_TRIPLET}/lib/pkgconfig`.

### Using __MSYS2 pacman__

You can search for packages [here](https://packages.msys2.org/)

- Install dependencies as below
  ```bash
  pacman -S mingw-w64-x86_64-libsodium
  pacman -S mingw-w64-x86_64-lmdb
  pacman -S mingw-w64-x86_64-openssl
  ```

### Install Secp256k1

You can use `secp256k1` either by downloading from hydra. Or by downloading from [github](https://github.com/bitcoin-core/secp256k1)

```
pacman -S unzip
curl https://hydra.iohk.io/job/Cardano/haskell-nix/windows-secp256k1/latest/download/1 -o secp256k1.zip
mkdir secp256k1
cd secp256k1/
unzip ../secp256k1.zip
cp -r * /mingw64
```

If `unzip` complains that `../secp256k1.zip` is not a zip file, unzip the
file manually in the explorer by choosing the option to unzip from the
right-click menu.

### Managing package configuration
Make sure that package configurations are correectly installed. If you are using chocolatey or other tool, it may be possible that more than one `pkg-config` tools are installed. You are encouraged to use `pkg-config` that __MSYS2__ has to minimize the error.  Also make sure that the pkg configuration are discoverable by checking that `pkg-config --list-all` lists all the installed packages.
​
Note that the pkg-config will only look in the paths pointed to by `pkg-config --variable pc_path pkg-config`. Also check if pkg-config correctly displays library path, by checking `pkg-config --variable libdir <pkg-name>`. In case, the path is incorrectly displayed, modify the file `pkg-config --path <pkg-name>` to correct the path.



## Global `cabal` configuration

Modify the following entries to your global `cabal` config file in your
`\path\to\cabal\` directory, such that they look like this:

```
-- extra-include-dirs:
extra-lib-dirs:  C:\ghcup\ghc\8.10.7\mingw\x86_64-w64-mingw32\lib
extra-prog-path: C:\ghcup\bin,
                 path\to\cabal\bin
```

Depending on the installation procedure, the path to your `\path\to\cabal\` directory could be `C:\Users\<username>\AppData\Roaming\cabal`).

> Note: The instructions in this section are a workaround for a
> [GHCup bug](https://github.com/msys2/MINGW-packages/issues/10837#issuecomment-1047105402).

## Downloading the source code for cardano-node

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

## Local `cabal` configuration

Add the following to the `cabal.project.local` file (if you don't already have it, create one) in the source code folder:

```
package HsOpenSSL
  flags: +use-pkg-config

package cardano-crypt-praos
  flags: -external-libsodium-vrf
```

## Building and installing the node

You can now build the node with:

```bash
cabal update
cabal build --builddir /c/dist exe:cardano-node
```

> Note: using `--builddir /c/dist` with a succinct directory protects you from exceeding the [maximal path size](https://docs.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation) on Windows (which is a limitation of the linker rather than `ghc` itself).

You can now verify whether the node runs:
```bash
cabal run --builddir /c/dist exe:cardano-node -- run --help
```

## Notes

### Speed up compilation

`reorder-goals` can affect the compilation time negatively. You can disable `reorder-goals` by setting it to `False` in `cabal.project.local`.
