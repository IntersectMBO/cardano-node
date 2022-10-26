## Exchange multi-asset management scenarios

Native assets follow a set of predefined rules known as *minting policies*, which are defined by users who want to create a new asset. These policies set the upper and lower limits of what a token can do, who can do it, and might also define the point in the transaction lifecycle at which it can be done.

A minting policy is bound by the network protocol and ledger-defined rules. One such rule is [the minimum UTXO value](https://cardano-ledger.readthedocs.io/en/latest/explanations/min-utxo-mary.html) (the minimum amount of ada that must be sent in a single transaction.) The value is currently set to 1 ada, so to transact any native asset on the Cardano network, a minimum of 1 ada must be included and accounted for within the transaction. This does not mean that you have to send only 1 ada to transact native assets. You can use any amount of ada, so it is important that exchanges and wallets are aware of this, and know what to do in such cases.  

> Note: It is possible to verify and pinpoint these specific native asset transactions using [cardano-graphql](https://github.com/input-output-hk/cardano-graphql#overview) or [cardano-rosetta](https://docs.cardano.org/cardano-components/cardano-rosetta/about-cardano-rosetta).

## Multi-asset management scenarios

Exchanges listing ada usually might encounter two specific scenarios:

- Scenario 1 - Spending a UTXO with a multi-asset attached
- Scenario 2 - Unexpectedly received multi-asset in cardano-wallet

## Scenario 1: Spending a UTXO with a multi-asset attached

> Note: This scenario applies to exchanges that manage their own UTXOs.

### How to pinpoint UTXOs with a native asset attached

Exchanges and third-party wallets that manage their own UTXOs often use a local block explorer (`cardano-graphql` or `cardano-rosetta`, for example). 

`cardano-graphql` is a Cardano API query language and a runtime component for fulfilling queries with existing data extracted from the [cardano-db-sync](https://github.com/input-output-hk/cardano-db-sync#cardano-db-sync) PostgreSQL database. It provides a complete and understandable description of this data, and gives clients the power to request the necessary information. This, in turn, both simplifies the evolution of client APIs’ overtime and enables powerful developer tools.

Cardano has adapted `cardano-rosetta` (initially developed by Coinbase as blockchain middleware) to help exchanges and third-party wallets extract blockchain data and integrate with Cardano. See this [Readme file](https://github.com/input-output-hk/cardano-rosetta/blob/master/docs/multi-assets-support.md) for more information about extracting multi-asset information from `cardano-rosetta`.

### Getting started with native assets

After understanding what a transaction with native assets might look like, you might want to create or burn some native assets or tokens on the testnet. See prerequisites in the getting-started file in this folder and follow the below steps to start minting some native assets:

- Connect `cardano-node` to testnet
- Build `cardano-node` and connect to testnet
- Download `cardano-cli` prebuilt binary or use build from source
- Follow the steps from the getting-started tutorial

### Spending UTXO with native assets attached

When building a transaction with native assets using `cardano-cli`, it is important to account for two things:

1. The ada value you wish to send
2. The amount of tokens

The above is also true for any exchange or third-party that wants to either spend, return, or store any multi-asset. By definition, receiving a multi-asset is the same as receiving any other Cardano transaction. The only difference is that some other data -like multi-asset- might be attached to the transaction.

We strongly recommend that all exchanges and third-party wallets verify all transactions in the block for multi-assets and handle accordingly. How these types of transactions are managed is an enterprise decision.

### What to do with unwanted tokens?

The purpose of any multi-asset can be arbitrary, but it is important to remember that, much like ada, a multi-asset is part of the transaction and needs to be handled and balanced in the same way. 

Multi-assets will cause no issue whether located in the issuer’s wallet or the exchange’s wallet. It will exist until it is redeemed, used, or burned.

Ultimately, the decision of what to do with a multi-asset lies with the exchange or the third-party wallet. More specifically, with an exchange that manages their own UTXOs.

[cardano-wallet](https://github.com/input-output-hk/cardano-wallet/releases) uses a UTXO algorithm to handle and manage UTXOs. A multi-asset handled by `cardano-wallet` on the blockchain could easily go unnoticed.

Choosing a UTXO means having to account for the asset attached to that UTXO. If this extra input is not handled correctly, an out-of-balance error will occur.

You control what to do with the native asset. You can either return it to the sender only if you know the address or alternatively you can move the native asset to an address in your wallet adhering to the network guidelines and the minimum UTXO value.

Read the getting-started tutorial (example:minting a new native token) for more information on creating and balancing a transaction with a native asset attached.

## Scenario 2 - Unexpectedly received multi-asset in cardano-wallet

> Note: This scenario only applies to exchanges that use `cardano-wallet`.

### Verify that you have received a multi-asset

1. Check the wallet information to confirm
2. Run the following curl command to check and verify the wallet information:

**Byron**
```
curl http://localhost:8090/v2/byron-wallets/ 
```
**Shelley**
```
http://localhost:8090/v2/wallets
```

3. Check the assets section:

```
"assets": {
      "total": [
        {
          "asset_name": "6e7466636f696e",
          "quantity": 10,
          "policy_id": "f625fdd8e936d56d3d9943557380cff64d0db66e545f99a3cc01ab97"
        }
      ],
      "available": [
        {
          "asset_name": "6e7466636f696e",
          "quantity": 10,
          "policy_id": "f625fdd8e936d56d3d9943557380cff64d0db66e545f99a3cc01ab97"
        }
      ]
    }
```

### Token redemption or removing unwanted multi-assets from cardano-wallet

Tokens created using a minting policy follow a predefined set of rules. The minting policy might allow token holders to burn or create new tokens, for example. In most cases, if you receive unwanted tokens, you will need to either return them to the issuer or sender, or put them somewhere else for safekeeping. Read the [minting policies topic](https://cardano-ledger.readthedocs.io/en/latest/explanations/policies.html) for more information on minting policies.

> Note: The minimum cost of sending any amount of native asset is one ada, plus the transaction fee. 

**Option 1**:  Send tokens back to the issuer or sender:

1. Verify the sender’s address or issuer’s address.
2. Create a JSON transaction in cardano-wallet with the minimum UTXO amount of one ada and include the native asset.

**Sample transaction for sending the multi-asset:**
```
curl -XPOST http://localhost:8090/v2/byron-wallets/{wallet_id}/transactions \ 
-H 'Content-Type: application/json \; charset=utf-8' 
-d '{
      "payments": [
      {
            "address":"{destination_address}",
            "amount":{
                   "quantity":3000000,
                   "unit":"lovelace"
              }, 
              "assets": [
                   {
                         "policy_id":"asset_policy_id", 
                         "asset_name": "6e7466636f696e", 
                         "quantity": 5
                  }
              ]
           }
      ], 
      "passphrase":"myfirstpassword"
  }'
```

3. Verify that the multi-asset transaction is complete, and that the assets have been moved from the wallet.

**Byron**

```
curl http://localhost:8090/v2/byron-wallets/ 
```

**Shelley**
```
curl http://localhost:8090/v2/wallets
```

4. You should see the following result:

```
"assets": {
      "total": [],
      "available": [] }
```

**Option 2**: Move tokens to an address inside the existing wallet:

1. Verify that you have native tokens in the wallet.
2. Specify an address within the wallet (or elsewhere) to send the tokens.
3. Follow the steps in Option 1 to send the tokens to an address not one ada min UTXO value.
4. Keep track of the address containing native assets.
