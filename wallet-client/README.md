## Running the wallet client

First you will need to start the core node with which the wallet client will
communicate.  You can do that with `./script/start-node.sh` (or
`./script/demo.sh`).  Then run 

```
./scripts/start-wallet.sh --bft -n 0 -m 3
```
