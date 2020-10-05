# Cardanoノードを構築する

Cardanoノードを構築、実行する方法は複数ありますが、以下ではNixおよびUbuntu/Debianによる方法を紹介します。Nix方式はUbuntu/Debianにインストールするよりも堅固で確実でしょう。


### Nixで構築する

 [Nix Package Manager][nix]は、インストールスクリプトをダウンロードおよび実行することにより、ほとんどのLinuxディストリビューションにインストールすることができます
```
curl -L https://nixos.org/nix/install > install-nix.sh
./install-nix.sh
```
指示に従います

構築速度を上げるために、IOHKが管理するバイナリキャッシュを設定することもできます（**これはオプションです**）
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee /etc/nix/nix.conf
substituters = https://cache.nixos.org https://hydra.iohk.io
trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

Nixがインストールされたら、一旦ログアウトしてログインし直し、`nix-shell`セッションに入ります
```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
nix-build -A scripts.mainnet.node -o mainnet-node-local
./mainnet-node-local
```

### Debian/Ubuntu、CentOSで構築する
必要なバージョンは [GHC 8.6.5][ghc865]および [Cabal-3.0][cabal30]です。
Haskellインストーラーツール [ghcup][ghcup]から入手できます

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
[ENTER]を2回押して確認し、最後に「YES」と入力してPATH変数に追加し、ターミナルセッションを再起動するか 

```
source ~/.ghcup/env
```
を実行して次のステップでghcupコマンドを使用できるようにします

必要なGHCバージョンをインストールして有効化します
```
ghcup install ghc 8.6.5
ghcup set ghc 8.6.5
ghc --version
```
またはインタラクティブTUIを使用します
```
ghcup tui
```
Haskellノードのコードの要件には、複数のLinuxシステム用のライブラリ開発パッケージがインストールされていることも含まれます

**Debian**と**Ubuntu**の構築方法は同じです

```
sudo apt-get update
sudo apt-get -y install pkg-config libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev
```
**CentOS**を使用している場合は、対応するパッケージは以下となります

```
sudo yum update
sudo yum -y install pkgconfig gmp-devel openssl-devel ncurses-libs systemd-devel zlib-devel
```

最後にCardano Node、コードビルトのgitリポジトリをクローンできます
```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
cabal build all
```

次のバイナリを~/.local/binフォルダー（PATH変数の一部のとき）にコピーできるようになりました 
```
cardano-node
cardano-cli
chairman
```
最後の3アウトプットラインでビルドロケーションパスを見ることができます。cardano-node 1.9.3の場合は以下となります 
```
~/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-node-1.9.3/x/cardano-cli/build/cardano-cli/
```



 


[ghcup]: https://www.haskell.org/ghcup/
[cabal30]: https://www.haskell.org/cabal/download.html
[ghc865]: https://www.haskell.org/ghc/blog/20190423-ghc-8.6.5-released.html
[nix]: https://nixos.org/nix/
