# 鍵変化型署名とKES期間

ブロック生成ノード用の運営証明書を作成するには、KESキーペアが必要です。

「KES」とは鍵変化型署名（ _**K**ey **E**volving **S**ignature_ ）の略称ですが、これは、一定期間後に鍵が新しい鍵に変化し、旧バージョンが破棄されるという役に立つものです。なぜなら、仮に攻撃者が鍵を侵害して署名鍵にアクセスできたとしても、これはそれ以降のブロックに署名することしかできず、以前のブロックにさかのぼって署名することはできないため、攻撃者が履歴を改ざんすることは不可能となるためです。

KESキーが変化する回数は限られており、その後は使用不能になります。したがって、その変化の限界数に達する前に、ノードオペレーターは新たなKESキーを生成する必要があります。その鍵ペアで新しいノード運営証明書を発行し、ノードをその新証明書で再起動します。

1期間の長さおよび鍵が変化する期間に関しては、ジェネシスファイルを参照します。ファイル名が`mainnet-shelley-genesis.json`の場合、以下を入力します

    cat mainnet-shelley-genesis.json | grep KES
    "slotsPerKESPeriod": 129600,
    "maxKESEvolutions": 62,


この例では、鍵は129600スロットの期間ごとに、新たな鍵が必要になるまで62回変化することができます。

ノード用の運営証明書を作成する前に、KES有効期間の開始、すなわち現在どのKES変化期間にいるのかを特定する必要があります。

ブロックチェーンの現在のチップを確認します

    cardano-cli shelley query tip --mainnet

    {
    "blockNo": 36914,
    "headerHash": "58df595137e71c0fa65edc99add11704b00e5f163475bd804e4bd59c126bfc9b",
    "slotNo": 8520857
    }

この例では、現在スロット8520857であり、ジェネシスファイルから1期間が129600スロット続くことがわかります。従って現在の期間を次のように計算します

    expr 8520857 / 129600
    > 65

これにより、ステークプールの運営証明書を生成することができます

    cardano-cli shelley node issue-op-cert \
    --kes-verification-key-file kes.vkey \
    --cold-signing-key-file cold.skey \
    --operational-certificate-issue-counter cold.counter \
    --kes-period 65 \
    --out-file node.cert
