# Using the collateral return output and total collateral features in Babbage transactions

In the Babbage era we now have the possibility of specifying a collateral return output in the transaction body. Why would we want to do this? Prior to Babbage we would need to provide collateral transaction inputs in the event our Plutus script failed validation. We would lose whatever transaction input we specified. There is a problem with this however. For example if we needed 100 lovelace to be put up as collateral but the transaction input we specified has 1000 lovelace, we would unnecessarily lose 900 lovelace. To avoid this we can now specify a collateral return output. In this example our collateral return output would have 900 lovelace being returned to an address of our choosing and if our Plutus script fails validation we will only lose the required 100 lovelace of collateral, with the remaining 900 lovelace being sent to an address of our choosing.

It is optional to specify the total collateral. This simply allows the ledger to provide an additional check that the collateral specified is indeed the total collateral. This also lets users
write transactions whose collateral is evident by just looking at the transaction body instead of
requiring information contained in the UTXO, which hardware wallets for example might not
have access to.

