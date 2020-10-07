# コマンドラインインターフェイス

前述のチュートリアルでは常に`cardano-node`コマンドを使用していましたが、
 [ここ](000_install.md)で説明しているようにソースからソフトウェアを構築する場合、実際にはコマンドラインインターフェイス
`cardano-cli`など、他の実行ファイルもインストールします。

このコマンドラインインターフェイスは、鍵生成、トランザクション構築、証明書作成、その他重要なタスクのためのツールセットを提供します。

これはサブコマンドの階層順に整理され、各レベルには、コマンドシンタックスやオプションのビルトインドキュメンテーションが付されています。

トップレベルのヘルプを得るには単にコマンドを引数なしで入力します

        cardano-cli

使用可能なサブコマンドが`shelley`であることが示されたら、次を入力します

        cardano-cli shelley

`node`を含む使用可能なサブ-サブコマンド候補が示されるので、階層の掘り下げを進めます

        cardano-cli shelley node

サブ-サブ-サブコマンド`key-gen`が見つかったら、以下を入力します

        cardano-cli shelley node key-gen

このコマンドに関連するパラメーターが示されるので、入力することで、例えばオフラインキーの鍵ペアや発行番号用ファイルを生成することができます。

        cardano-cli shelley node key-gen \
            --cold-verification-key-file cold.vkey \
            --cold-signing-key-file cold.skey \
            --operational-certificate-issue-counter-file cold.counter

![`cardano-cli` command hierarchy](images/cli.png)
