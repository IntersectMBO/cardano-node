# Building Cardano Node using Nix

There are a number of ways the Cardano Node can be built and run, but the following document
details the Nix and Ubuntu/Debian methods. The Nix method is probably more robust and foolproof
than installing on Ubuntu/Debian.


### Building under Nix

The [Nix Package Manager][nix] can be installed on most Linux distributions by downloading and
running the installation script:
```
curl https://nixos.org/nix/install > install-nix.sh
./install-nix.sh
```
and following the directions.

To improve build speed, it is possible to set up a binary cache maintained by IOHK (**this is
optional**):
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee /etc/nix/nix.conf
substituters = https://cache.nixos.org https://hydra.iohk.io
trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

Once Nix is installed, log out and then log back in and then in a `nix-shell` session:
```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
nix-build -A scripts.mainnet.node -o mainnet-node-local
./mainnet-node-local
```

### Building under Debian/Ubuntu or CentOS
The required versions are [GHC 8.6.5][ghc865] and [Cabal-3.0][cabal30].
You best get them with the Haskell installer tool [ghcup][ghcup].

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
confirm 2x ENTER and type YES at the end to add ghcup to your PATH variable
Then restart your terminal session or execute 

```
source ~/.ghcup/env
```
to use the ghcup command for the next steps

Now install and activate the required GHC version
```
ghcup install 8.6.5
ghcup set 8.6.5
ghc --version
```
The code in the Haskell node also requires that the development packages for a couple of Linux
system libraries be installed:

The instructions for **Debian** and **Ubuntu** are identical.

```
sudo apt-get update
sudo apt-get -y install pkg-config libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev
```
If you're using **CentOS**, the corresponding packages will be:

```
sudo yum update
sudo yum -y install pkgconfig gmp-devel openssl-devel ncurses-libs systemd-devel zlib-devel
```

Finally the Cardano Node git repo can be cloned and the code built:
```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
cabal build all
```

Now you can copy the binaries 
```
cardano-node
cardano-cli
chairman
```
into your ~/.local/bin folder (when part of the PATH variable)
you can see the build location path from the last 3 output lines. 
for cardano-node 1.9.3 it is 
```
~/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-node-1.9.3/x/cardano-cli/build/cardano-cli/
```



 


[ghcup]: https://www.haskell.org/ghcup/
[cabal30]: https://www.haskell.org/cabal/download.html
[ghc865]: https://www.haskell.org/ghc/blog/20190423-ghc-8.6.5-released.html
[nix]: https://nixos.org/nix/
