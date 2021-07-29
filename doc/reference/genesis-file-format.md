# Genesis File Entries

The possible genesis entries are described here.

## Byron-specific Entries

TODO: document these!

## Shelley-specific Entries

These entries are also used for the Allegra and Mary eras.
TODO: document these!

## Alonzo-Specific Entries

All these entries are Optional.

|  Field |  Type | Required? | Description |
|---|---|---|---|
 `lovelacePerUTxOWord`  |  `<nat>` | Optional | How many lovelace are charged per word in a UTxO value |
| `executionPrices` | `{ "priceSteps": rational, "priceMemory": rational } `  | Optional | Price (in lovelace) charged for Plutus script per CPU execution step/memory unit. |
|  `maxTxExUnits` |     `{ "memory": <nat>, "steps" : <nat> }`| Optional  |  Maximum number of Plutus memory units/CPU steps allowed for a single transaction. |
|  `maxBlockExUnits` |     `{ "memory": <nat>, "steps" : <nat> }`| Optional  |  Maximum number of Plutus memory units/CPU steps allowed for a complete block. |
| `maxValueSize`  | `<nat>`  | Optional |  Maximum size of a Plutus `Value` in a transaction output. |
| `collateralPercentage`  | `<nat>`  | Optional | The percentage of Plutus script execution fee that must be provided as collateral. |
| `maxCollateralInputs`  |  `<nat>` | Optional |  The maximum number of collateral inputs that can be provided in a transaction. |
| `costModels`  | `<costmodel>`  | Optional |  Plutus cost models (see below). |

### Plutus Cost Models

TODO: Provide this.

### Example Alonzo Genesis File
```
{
    "lovelacePerUTxOWord": 34482,
    "executionPrices": {
        "priceSteps":
		{ "numerator" : 1,
		  "demoninator" : 100
		},
        "priceMemory":
		{ "numerator" : 5,
		  "demoninator" : 1000
		}
    },
    "maxTxExUnits": {
        "memory": 11000000000,
        "steps": 11000000000
    },
    "maxBlockExUnits": {
        "memory": 110000000000,
        "steps": 110000000000
    },
    "maxValueSize": 5000,
    "collateralPercentage": 150,
    "maxCollateralInputs": 6
}
```