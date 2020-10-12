# ステークプール鍵を生成する

ステークプールには稼働中の2つのノード、 __ブロック生成__ ノードと __リレー__ ノードが必要です。

まず、 __ブロック生成__ ノードをセットアップする必要があります。ノードはソースから構築することもできますが、自分のローカルマシンにシングルビルドを維持して、バイナリのみを __ブロック生成__ サーバーおよび __リレー__ サーバーにアップロードすることもできます。ただし、双方のバージョンが一貫していることを確認してください。



![network diagram](images/basic-network-with-relays-producers-passivenodes-walletnodes.png)

__ブロック生成__ ノードは __リレー__ ノードにのみ接続します。この __リレー__ ノードはネットワークの他のリレーに接続します。各ノードは独立したサーバーで稼働させる必要があります。

#### ブロック生成ノードファイアウォールの基本設定

* ログインはSSH鍵のみを用い、パスワードは使用しない
* デフォルトのポート22以外のポートでSSH接続をセットアップする
* IPアドレスのセットアップによりファイアウォールが自分のリレーノードからの接続のみ許可するように設定する

#### リレーノードファイアウォールの基本設定

 * ログインはSSH鍵のみを用い、パスワードは使用しない
 * デフォルトのポート22以外のポートでSSH接続をセットアップする
 * 絶対に必要なポートのみをオープンにする

#### ブロック生成ノードの鍵を作成する

**警告：**
このプロセスで、cardano-nodeとcardano-cliがインストールされた自分の __ローカルマシン__ を使用する場合は、 __コールドキー__ を安全な場所に保管し、ファイルをローカルマシンから削除するまで、絶対にオンラインにしないように注意してください。

__ブロック生成ノード__ または __プールノード__ には以下が必要です

* __コールド__ キーペア
* __VRF__ キーペア
* __KES__ キーペア
* __運営証明書__

ローカルマシンに鍵を保管するディレクトリーを作成します

    mkdir pool-keys
    cd pool-keys

#### __Cold__ キーと __Cold_counter__ を作成する

    cardano-cli shelley node key-gen \
    --cold-verification-key-file cold.vkey \
    --cold-signing-key-file cold.skey \
    --operational-certificate-issue-counter-file cold.counter

#### VRFキーペアを生成する

    cardano-cli shelley node key-gen-VRF \
    --verification-key-file vrf.vkey \
    --signing-key-file vrf.skey

#### KESキーペアを生成する

    cardano-cli shelley node key-gen-KES \
    --verification-key-file kes.vkey \
    --signing-key-file kes.skey

#### 運営証明書を生成する

KES期間内のスロット数を知る必要があります。これはジェネシスファイルから取得できます

    cat mainnet-shelley-genesis.json | grep KESPeriod
    > "slotsPerKESPeriod": 3600,

次に、ブロックチェーンの現在のチップが必要です

    cardano-cli shelley query tip --mainnet
    {
    "blockNo": 36929,
    "headerHash": "44c2a2be237ea485c15bf2a50c12b4d2aabe6d4233cb1b2131efc080615a17d0",
    "slotNo": 906528
    }

チップの `slotNo` 値を探します。この例では、現在スロット906528です。KES期間は120です

    expr 432571 / 3600
    > 251

証明書を生成します

    cardano-cli shelley node issue-op-cert \
    --kes-verification-key-file kes.vkey \
    --cold-signing-key-file cold.skey \
    --operational-certificate-issue-counter cold.counter \
    --kes-period 120 \
    --out-file node.cert

#### コールドキーを安全な保管場所に移し、ローカルマシンから削除する

コールドキーの保管場所として最適なのは __セキュアなUSB__ など、 __安全な外部デバイス__ であり、インターネットにアクセスするコンピューターではありません。

#### ファイルをサーバーにコピーする

VRFキー、KESキー、運営証明書を、 __ブロック生成__ サーバーにコピーします。例：

    scp -rv -P<SSH PORT> -i ~/.ssh/<SSH_PRIVATE_KEY> ~/pool-keys USER@<PUBLIC_IP>:~/

    > Transferred: sent 3220, received 6012 bytes, in 1.2 seconds
    Bytes per second: sent 2606.6, received 4866.8
    debug1: Exit status 0


サーバーにログインし、ファイルがあることを確認します

    ls pool-keys

    > kes.skey  kes.vkey  node.cert  vrf.skey  vrf.vkey  

ブロックチェーンにプールを登録する方法は後述します。
