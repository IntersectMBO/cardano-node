# Testing cardano-tx-submit-webapi

Setting this up for testing and for actual use on a real network is significantly more difficult
than it should be. This is definitely not ready for end users yet.

The biggest thing missing at the moment is documentation of generation of transactions with the
command line tools. That needs to be *thoroughly* documented and this document is not the place
for that.

The following instructions are for generating a testnet and testing tx-submission on that testnet.

### Start a local shelley testnet


* This process generates a lot of state in the `cardano-node` checkout that can be cleaned up using:
    ```
    cardano-node $ rm -rf configuration/genesis/ db/db-* logs/ transaction.bin
    ```

* Run up a `tmux` session (the nodes of the testnet will end up in the tmux session):
    ```
    cardano-node $ tmux new-session -s Demo
    ```

* Run the testnet using the following script:
    ```
    cardano-node $ ./scripts/shelley-testnet.sh
    ```

   This will also run scripts/genesis.sh if you do not have already a genesis file and create one and all necessairy delegate keys, certs etc.
   They will be located inside cardano-node/configuration/genesis directory. As the result of this command you should see a tmux session with 4 windows - 3 of them should show the log outputs for running nodes and the remaining one the output with information about location of genesis directory and hash value for genesis. If there is an issue and some of the nodes did not start you can stop them (ctrl+C) and close the windows (ctrl+D) and start the testnet script again in the first left-top window.


* Generate a transaction:

    ```
    cardano-node$ ./scripts/issue-genesis-utxo-expenditure.sh genesis_transaction.bin
    ```

    Note: Transaction scripts also generate genesis directory with config files - but only if you don not have them. 


### Set up and run the cardano-tx-submit-webapi

* clone node repo:
    ```
    git clone https://github.com/input-output-hk/cardano-explorer 
    cd cardano-explorer 
    ```


* Generate a config file for the `tx-submit-webapi` (note the change of directory):
    ```
    cardano-explorer $ scripts/generate-tx-submit-config.sh --require-magic \
         --genesis-hash 6f9371...939776 --output config.yaml
    ```
    where the Genesis hash is the one output in an previous step. You can also get the value of genesis hash by running:

    ```
    cardano-node $ cat configuration/genesis/GENHASH 
    ```
    from cardano-node directory.

* Run the `cardano-tx-submit-webapi`:
    ```
    cardano-explorer $ cabal run cardano-tx-submit-webapi -- \
         --config config.yaml \
        --genesis-file ../cardano-node/configuration/genesis/genesis.json \
        --socket-path ../cardano-node/socket/0 \
        --port 8101
    ```

* Submit the transaction to the webapi:
    ```
    cardano-explorer $ curl -X POST \
        --header "Content-Type:application/cbor" \
        --data-binary @transaction.bin http://localhost:8101/api/submit/tx
    ```
  which returns a status as a chunk of JSON.


    ```
    {"status":"success","errorMsg":"No error"} 
    ```
    Example output from running web-api after successful transaction submission:

    ```
    tx-submit-webapi/build/cardano-tx-submit-webapi/cardano-tx-submit-webapi ...
    [cardano-tx-submit:Info:9] [2020-02-09 18:20:04.18 UTC] Running tx-submit node
    [cardano-tx-submit:Info:9] [2020-02-09 18:20:04.18 UTC] NetworkMagic: RequiresMagic 459045235
    [cardano-tx-submit:Info:9] [2020-02-09 18:20:04.18 UTC] localInitiatorNetworkApplication: connecting to node via "../cardano-node/socket/0"
    [cardano-tx-submit:Info:10] [2020-02-09 18:20:04.18 UTC] Running tx-submit web server on http://localhost:8101/
    [cardano-tx-submit:Info:22] [2020-02-09 18:27:29.80 UTC] txSubmitPost: received 249 bytes
    [cardano-tx-submit:Info:22] [2020-02-09 18:27:29.80 UTC] Success
    ```
