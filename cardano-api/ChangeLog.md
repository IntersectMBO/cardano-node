# Changelog for cardano-api

## 1.19.0 -- August 2020

- Support for scripts and specifically multi-sig scripts (#1623)
- Support for JSON syntax for multi-sig scripts (#1660)
- Support for converting tx metadata to/from JSON (#1606, #1682)
- Support for Bech32-encoded stake pool IDs (#1528, #1638)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

## 1.18.0 -- July 2020

None

## 1.17.0 -- July 2020

- Allow genesis keys as tx witnesses (#1483)
- Allow extended genesis delegate keys to sign operational certs (#1497)
- Add support for extended keys for stake, genesis and delegate keys (#1487)

## 1.16.0 -- July 2020

- Remove the old API (#1444, #1456)
- Added raw serialisation instances for all key types (#1455)
- Added bech32 serialisation following draft CIP 5 (#1455)

## 1.15.1 -- July 2020

- Include tx metadata in transactions in the new api (#1406)
- Add support for extended ed25519 keys for payment keys (#1411)
- Improve tx submission API in the new API (#1430)

## 1.15.0 -- July 2020

- Fix the ledger state dump query (#1333, #1334)
- Support for Byron witnesses in Shelley txs in the typed API (#1339)
- Support for Bech32 serialisation in the typed API (#1382)
- Support for other additional functionality in the typed API (#1337, #1375)
- More tests for the typed API (#1360, #1369, #1378)
- Moving code around to eliminate the cardano-config package (#1289, #1380)

## 1.14.2 -- June 2020

- Fix the query that dumps the ledger state as JSON (#1333)

## 1.14.1 -- June 2020

No changes in the cardano-api. There were changes in the cardano-node.

## 1.14.0 -- June 2020

- Improvements to the strongly-typed API (#1112, #1220, #1227, #1246)

  The API is not yet stable in this release.

## 1.13.0 -- June 2020

- Initial version of an improved strongly-typed API.
  Initially focusing on creating and serialising keys.

  The API is not yet stable in this release.

## 1.11.0 -- April 2020

- Initial version of the API package. The package provides client-side
  functionality for constructing and submitting transactions.

  The API is not yet stable in this release.

- Initial transaction API with Byron support and Shelley stubs (#787)
- Shelley address key pair generation (#799)
