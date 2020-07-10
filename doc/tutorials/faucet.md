# Using the faucet

UPDATED FOR TAG: 1.14.2

The faucet allows you to get some "test ada",
so that you can try out features that require funds.

In order to use the faucet, you first need a _payment address_,
which can optionally be associated with a _stake address_.
If you do not have such an address, find out how to create one [here](020_keys_and_addresses.md).

To use the faucet, simply enter

    curl -v -XPOST "https://faucet.shelley-testnet.dev.cardano.org/send-money/YOUR-ADDRESS

into a terminal window.
If your address is saved to a file, for example `payment.addr`, you can instead type

    curl -v -XPOST "https://faucet.shelley-testnet.dev.cardano.org/send-money/$(cat payment.addr)"

If all goes well, the last line of the output should declare success and say how much funds where sent to your address.

    *   Trying 3.122.86.4...
    * TCP_NODELAY set
    * Connected to faucet.shelley-testnet.dev.cardano.org (3.122.86.4) port 443 (#0)
    ...
    * Connection #0 to host faucet.shelley-testnet.dev.cardano.org left intact
    {"success":true,"amount":1000000000,"fee":168141,"txid":"8ed4383f7af20e81c9cef88b8aab0ff2b1b284dff0ed6614480f8dbfec7d6fb5"}

Now, check the balance of your address to see if you have got the funds:

    export CARDANO_NODE_SOCKET_PATH=~/cardano-node/relay/db/node.socket


Make sure that your node is running.  Then use

    cardano-cli shelley query utxo \
    --address $(cat payment.addr) \
    --testnet-magic 42

You should see something like this:

                              TxHash                                 TxIx        Lovelace
    ----------------------------------------------------------------------------------------
    65e99578e91dbf400c42989b5b5ae6dde877510900074f4afd8ff472639da6b3     0     1000000000000

    (Please note: although this command usually completes very quickly, under some conditions it could take severl minutes or even hours to complete.)


IMPORTANT: Extra funds can be returned to 00677291d73b71471afa49fe2d20b96f7227b05f863dafe802598964533e0dc3bc0cf7eb8153441db271a2288560378b209014350792f273bdc307f06ca34f0c6f
