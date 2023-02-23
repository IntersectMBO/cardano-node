# Multi-asset support

From the Mary ledger upgrade and onwards, Cardano supports [multi-assets](https://github.com/input-output-hk/cardano-ledger/releases/download/cardano-ledger-spec-2023-01-18/mary-ledger.pdf), also referred to as a *native tokens* feature. This feature extends the ledger’s accounting infrastructure (originally designed for processing ada-only transactions) to accommodate transactions using a range of assets. These assets include ada and a variety of user-defined token types, the mixture of which can be transacted in a single tx output.

## What is a multi-asset?

Multi-assets are user-defined, custom tokens. They are supported natively, which means that the ledger handles the accounting and tracking of token-related activities. This offers distinct advantages for developers as there is no need to create smart contracts to mint or burn custom tokens, removing a layer of added complexity and potential for manual errors.

An asset is uniquely identified by an *asset ID*, which is a pair of both the *policy ID* and an *asset name*:

+ *PolicyID* - the unique identifier that is associated with a minting policy (hash of the minting policy).
+ *Asset name* - an (immutable) property of an asset that is used to distinguish different assets within the same policy. Unlike the policyID, the asset name does not refer to any code or set of rules. It is an arbitrary sequence of bytes. In simplest case it is an ASCII-encoded common word, eg. ‘couttscoin’. In this document we use asset name `"636f75747473636f696e" = hex("couttscoin")` as an example.

Tokens that have the same asset ID have the property of being fungible with each other, and are not fungible with tokens that have a different asset ID.

Further reading:

- [Native token explainers](https://cardano-ledger.readthedocs.io/en/latest/)
- [Getting started with native tokens](../../../doc/reference/native-tokens/02-getting-started.md). Includes script examples for minting, transferring, and burning tokens.
