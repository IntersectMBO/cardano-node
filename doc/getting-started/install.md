# ノードをソースからインストールする

**最新版：** [https://github.com/input-output-hk/cardano-node/releases](https://github.com/input-output-hk/cardano-node/releases)

#### 準備

プラットフォームのセットアップ：

要件：

* x86ホスト（AMDまたはIntel）、仮想マシンまたは2コア以上のAWSインスタンス、4GBのRAM、10GB以上のディスク空き容量
* 最新版Linux

#### ディペンデンシーをインストールする

We need the following packages and tools on our Linux system to download the source code and build it:

* バージョン管理システム`git`
* `gcc`Cコンパイラ
* `gcc`のC++サポート
* 任意精度ライブラリ`gmp`用開発者ライブラリ
* 圧縮ライブラリ`zlib`用開発者ライブラリ
* `systemd`用開発者ライブラリ
* `ncurses`用開発者ライブラリ
* `ncurses`対応ライブラリ
* Haskell構築ツール`cabal`
* GHC Haskellコンパイラ

Redhat、Fedora、Centosで

    sudo yum update -y
    sudo yum install git gcc gcc-c++ tmux gmp-devel make tar wget zlib-devel libtool autoconf -y
    sudo yum install systemd-devel ncurses-devel ncurses-compat-libs -y

Debian/Ubuntuでは、代わりに以下を使用します


    sudo apt-get update -y
    sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

Linuxで別のフレーバーを使用している場合は、`yum`または`apt-get`の代わりに、プラットフォームに適したパッケージマネージャーを使用する必要があります。

#### Cabalをダウンロード、解凍、インストール、更新する

    wget https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    tar -xf cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz
    rm cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz cabal.sig
    mkdir -p ~/.local/bin
    mv cabal ~/.local/bin/

PATHに.local/binがあることを確認します

    echo $PATH

`.local/bin`PATHがない場合、`.bashrc`ファイルに以下を追加します

    export PATH="~/.local/bin:$PATH"

このファイルをソースにします

    source .bashrc

cabalを更新します

    cabal update

Cabalバージョン`3.2.0.0`がインストールされたことを確認します

    cabal --version

#### GHCをダウンロード、インストールする

    wget https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-deb9-linux.tar.xz
    tar -xf ghc-8.6.5-x86_64-deb9-linux.tar.xz
    rm ghc-8.6.5-x86_64-deb9-linux.tar.xz
    cd ghc-8.6.5
    ./configure
    sudo make install

ホームディレクトリーへ戻ります

#### Libsodiumをダウンロードする

    git clone https://github.com/input-output-hk/libsodium
    cd libsodium
    git checkout 66f017f1
    ./autogen.sh
    ./configure
    make
    sudo make install

.bashrcファイルに以下を追加し、ソースにします

    export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

#### cardano-nodeのソースコードをダウンロードする

ホームディレクトリーへ戻ります 

    git clone https://github.com/input-output-hk/cardano-node.git

作業ディレクトリーをダウンロードしたソースコードフォルダーに変更します

    cd cardano-node


cardano-nodeの最新版をチェックアウトします

    git fetch --all --tags
    git tag
    git checkout tags/<TAGGED VERSION>

#### ノードを構築、インストールする

ノードを`cabal`で構築し、インストールします

    cabal build all

実行ファイルを`.local/bin`ディレクトリーへコピーし、プレースホルダーを対象バージョンに置き換えます

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-node-<TAGGED VERSION>/x/cardano-node/build/cardano-node/cardano-node ~/.local/bin/

    cp -p dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-cli-<TAGGED VERSION>/x/cardano-cli/build/cardano-cli/cardano-cli ~/.local/bin/

インストールしたバージョンを確認します

    cardano-cli --version

最新版に更新する必要場ある場合は、このプロセスを繰り返します


**注：** ノードの更新バージョンを実行する前に、`db`フォルダー（データベースフォルダー）を削除する必要がある場合があります
