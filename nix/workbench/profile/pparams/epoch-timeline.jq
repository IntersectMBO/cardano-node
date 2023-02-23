def epochs:
{
  "208": {
    "epoch": 208,
    "description": "Shelley era",
    "shelley": {
      "protocolVersion": {
        "minor": 0,
        "major": 2
      },
      "decentralisationParam": 1,
      "eMax": 18,
      "extraEntropy": {
        "tag": "NeutralNonce"
      },
      "maxTxSize": 16384,
      "maxBlockBodySize": 65536,
      "maxBlockHeaderSize": 1100,
      "minFeeA": 44,
      "minFeeB": 155381,
      "minUTxOValue": 1000000,
      "poolDeposit": 500000000,
      "minPoolCost": 340000000,
      "keyDeposit": 2000000,
      "nOpt": 150,
      "rho": 0.003,
      "tau": 0.2,
      "a0": 0.3
    }
  },
  "211": {
    "epoch": 211,
    "description": "First step towards decentralisation",
    "shelley": {
      "decentralisationParam": 0.9
    }
  },
  "212": {
    "epoch": 212,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.8
    }
  },
  "217": {
    "epoch": 217,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.7
    }
  },
  "222": {
    "epoch": 222,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.6
    }
  },
  "227": {
    "epoch": 227,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.5
    }
  },
  "232": {
    "epoch": 232,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.4
    }
  },
  "234": {
    "epoch": 234,
    "description": "Incentive adjusted towards more decentralisation",
    "shelley": {
      "nOpt": 500
    }
  },
  "236": {
    "epoch": 236,
    "description": "Allegra era",
    "shelley": {
      "protocolVersion": {
        "major": 3
      }
    }
  },
  "242": {
    "epoch": 242,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.3
    }
  },
  "247": {
    "epoch": 247,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.2
    }
  },
  "251": {
    "epoch": 251,
    "description": "Mary era",
    "shelley": {
      "protocolVersion": {
        "major": 4
      }
    }
  },
  "252": {
    "epoch": 252,
    "description": "Further decentralisation",
    "shelley": {
      "decentralisationParam": 0.1
    }
  },
  "257": {
    "epoch": 257,
    "description": "Fully decentralised",
    "shelley": {
      "decentralisationParam": 0
    }
  },
  "259": {
    "epoch": 259,
    "description": "Extra entropy injection",
    "shelley": {
      # "extraEntropy": "d982e06fd33e7440b43cefad529b7ecafbaa255e38178ad4189a37e4ce9bf1fa"
    }
  },
  "290": {
    "epoch": 290,
    "description": "Alonzo era",
    "shelley": {
      "protocolVersion": {
        "major": 5
      },
      "minUTxOValue": 34482
    },
    "alonzo": {
      "lovelacePerUTxOWord": 34482,
      "executionPrices": {
        "prSteps": {
          "numerator": 721,
          "denominator": 10000000
        },
        "prMem": {
          "numerator": 577,
          "denominator": 10000
        }
      },
      "maxTxExUnits": {
        "exUnitsMem": 10000000,
        "exUnitsSteps": 10000000000
      },
      "maxBlockExUnits": {
        "exUnitsMem": 50000000,
        "exUnitsSteps": 40000000000
      },
      "maxValueSize": 5000,
      "collateralPercentage": 150,
      "maxCollateralInputs": 3
    },
    "costModels": {
      "PlutusV1": {
        "addInteger-cpu-arguments-intercept": 197209,
        "addInteger-cpu-arguments-slope": 0,
        "addInteger-memory-arguments-intercept": 1,
        "addInteger-memory-arguments-slope": 1,
        "appendByteString-cpu-arguments-intercept": 396231,
        "appendByteString-cpu-arguments-slope": 621,
        "appendByteString-memory-arguments-intercept": 0,
        "appendByteString-memory-arguments-slope": 1,
        "appendString-cpu-arguments-intercept": 150000,
        "appendString-cpu-arguments-slope": 1000,
        "appendString-memory-arguments-intercept": 0,
        "appendString-memory-arguments-slope": 1,
        "bData-cpu-arguments": 150000,
        "bData-memory-arguments": 32,
        "blake2b-cpu-arguments-intercept": 2477736,
        "blake2b-cpu-arguments-slope": 29175,
        "blake2b-memory-arguments": 4,
        "cekApplyCost-exBudgetCPU": 29773,
        "cekApplyCost-exBudgetMemory": 100,
        "cekBuiltinCost-exBudgetCPU": 29773,
        "cekBuiltinCost-exBudgetMemory": 100,
        "cekConstCost-exBudgetCPU": 29773,
        "cekConstCost-exBudgetMemory": 100,
        "cekDelayCost-exBudgetCPU": 29773,
        "cekDelayCost-exBudgetMemory": 100,
        "cekForceCost-exBudgetCPU": 29773,
        "cekForceCost-exBudgetMemory": 100,
        "cekLamCost-exBudgetCPU": 29773,
        "cekLamCost-exBudgetMemory": 100,
        "cekStartupCost-exBudgetCPU": 100,
        "cekStartupCost-exBudgetMemory": 100,
        "cekVarCost-exBudgetCPU": 29773,
        "cekVarCost-exBudgetMemory": 100,
        "chooseData-cpu-arguments": 150000,
        "chooseData-memory-arguments": 32,
        "chooseList-cpu-arguments": 150000,
        "chooseList-memory-arguments": 32,
        "chooseUnit-cpu-arguments": 150000,
        "chooseUnit-memory-arguments": 32,
        "consByteString-cpu-arguments-intercept": 150000,
        "consByteString-cpu-arguments-slope": 1000,
        "consByteString-memory-arguments-intercept": 0,
        "consByteString-memory-arguments-slope": 1,
        "constrData-cpu-arguments": 150000,
        "constrData-memory-arguments": 32,
        "decodeUtf8-cpu-arguments-intercept": 150000,
        "decodeUtf8-cpu-arguments-slope": 1000,
        "decodeUtf8-memory-arguments-intercept": 0,
        "decodeUtf8-memory-arguments-slope": 8,
        "divideInteger-cpu-arguments-constant": 148000,
        "divideInteger-cpu-arguments-model-arguments-intercept": 425507,
        "divideInteger-cpu-arguments-model-arguments-slope": 118,
        "divideInteger-memory-arguments-intercept": 0,
        "divideInteger-memory-arguments-minimum": 1,
        "divideInteger-memory-arguments-slope": 1,
        "encodeUtf8-cpu-arguments-intercept": 150000,
        "encodeUtf8-cpu-arguments-slope": 1000,
        "encodeUtf8-memory-arguments-intercept": 0,
        "encodeUtf8-memory-arguments-slope": 8,
        "equalsByteString-cpu-arguments-constant": 150000,
        "equalsByteString-cpu-arguments-intercept": 112536,
        "equalsByteString-cpu-arguments-slope": 247,
        "equalsByteString-memory-arguments": 1,
        "equalsData-cpu-arguments-intercept": 150000,
        "equalsData-cpu-arguments-slope": 10000,
        "equalsData-memory-arguments": 1,
        "equalsInteger-cpu-arguments-intercept": 136542,
        "equalsInteger-cpu-arguments-slope": 1326,
        "equalsInteger-memory-arguments": 1,
        "equalsString-cpu-arguments-constant": 1000,
        "equalsString-cpu-arguments-intercept": 150000,
        "equalsString-cpu-arguments-slope": 1000,
        "equalsString-memory-arguments": 1,
        "fstPair-cpu-arguments": 150000,
        "fstPair-memory-arguments": 32,
        "headList-cpu-arguments": 150000,
        "headList-memory-arguments": 32,
        "iData-cpu-arguments": 150000,
        "iData-memory-arguments": 32,
        "ifThenElse-cpu-arguments": 1,
        "ifThenElse-memory-arguments": 1,
        "indexByteString-cpu-arguments": 150000,
        "indexByteString-memory-arguments": 1,
        "lengthOfByteString-cpu-arguments": 150000,
        "lengthOfByteString-memory-arguments": 4,
        "lessThanByteString-cpu-arguments-intercept": 103599,
        "lessThanByteString-cpu-arguments-slope": 248,
        "lessThanByteString-memory-arguments": 1,
        "lessThanEqualsByteString-cpu-arguments-intercept": 103599,
        "lessThanEqualsByteString-cpu-arguments-slope": 248,
        "lessThanEqualsByteString-memory-arguments": 1,
        "lessThanEqualsInteger-cpu-arguments-intercept": 145276,
        "lessThanEqualsInteger-cpu-arguments-slope": 1366,
        "lessThanEqualsInteger-memory-arguments": 1,
        "lessThanInteger-cpu-arguments-intercept": 179690,
        "lessThanInteger-cpu-arguments-slope": 497,
        "lessThanInteger-memory-arguments": 1,
        "listData-cpu-arguments": 150000,
        "listData-memory-arguments": 32,
        "mapData-cpu-arguments": 150000,
        "mapData-memory-arguments": 32,
        "mkCons-cpu-arguments": 150000,
        "mkCons-memory-arguments": 32,
        "mkNilData-cpu-arguments": 150000,
        "mkNilData-memory-arguments": 32,
        "mkNilPairData-cpu-arguments": 150000,
        "mkNilPairData-memory-arguments": 32,
        "mkPairData-cpu-arguments": 150000,
        "mkPairData-memory-arguments": 32,
        "modInteger-cpu-arguments-constant": 148000,
        "modInteger-cpu-arguments-model-arguments-intercept": 425507,
        "modInteger-cpu-arguments-model-arguments-slope": 118,
        "modInteger-memory-arguments-intercept": 0,
        "modInteger-memory-arguments-minimum": 1,
        "modInteger-memory-arguments-slope": 1,
        "multiplyInteger-cpu-arguments-intercept": 61516,
        "multiplyInteger-cpu-arguments-slope": 11218,
        "multiplyInteger-memory-arguments-intercept": 0,
        "multiplyInteger-memory-arguments-slope": 1,
        "nullList-cpu-arguments": 150000,
        "nullList-memory-arguments": 32,
        "quotientInteger-cpu-arguments-constant": 148000,
        "quotientInteger-cpu-arguments-model-arguments-intercept": 425507,
        "quotientInteger-cpu-arguments-model-arguments-slope": 118,
        "quotientInteger-memory-arguments-intercept": 0,
        "quotientInteger-memory-arguments-minimum": 1,
        "quotientInteger-memory-arguments-slope": 1,
        "remainderInteger-cpu-arguments-constant": 148000,
        "remainderInteger-cpu-arguments-model-arguments-intercept": 425507,
        "remainderInteger-cpu-arguments-model-arguments-slope": 118,
        "remainderInteger-memory-arguments-intercept": 0,
        "remainderInteger-memory-arguments-minimum": 1,
        "remainderInteger-memory-arguments-slope": 1,
        "sha2_256-cpu-arguments-intercept": 2477736,
        "sha2_256-cpu-arguments-slope": 29175,
        "sha2_256-memory-arguments": 4,
        "sha3_256-cpu-arguments-intercept": 0,
        "sha3_256-cpu-arguments-slope": 82363,
        "sha3_256-memory-arguments": 4,
        "sliceByteString-cpu-arguments-intercept": 150000,
        "sliceByteString-cpu-arguments-slope": 5000,
        "sliceByteString-memory-arguments-intercept": 0,
        "sliceByteString-memory-arguments-slope": 1,
        "sndPair-cpu-arguments": 150000,
        "sndPair-memory-arguments": 32,
        "subtractInteger-cpu-arguments-intercept": 197209,
        "subtractInteger-cpu-arguments-slope": 0,
        "subtractInteger-memory-arguments-intercept": 1,
        "subtractInteger-memory-arguments-slope": 1,
        "tailList-cpu-arguments": 150000,
        "tailList-memory-arguments": 32,
        "trace-cpu-arguments": 150000,
        "trace-memory-arguments": 32,
        "unBData-cpu-arguments": 150000,
        "unBData-memory-arguments": 32,
        "unConstrData-cpu-arguments": 150000,
        "unConstrData-memory-arguments": 32,
        "unIData-cpu-arguments": 150000,
        "unIData-memory-arguments": 32,
        "unListData-cpu-arguments": 150000,
        "unListData-memory-arguments": 32,
        "unMapData-cpu-arguments": 150000,
        "unMapData-memory-arguments": 32,
        "verifySignature-cpu-arguments-intercept": 3345831,
        "verifySignature-cpu-arguments-slope": 1,
        "verifySignature-memory-arguments": 1
      }
    }
  },
  "298": {
    "epoch": 298,
    "description": "???",
    "shelley": {
      "protocolVersion": {
        "major": 6
      }
    }
  },
  "306": {
    "epoch": 306,
    "description": "First block size bump + first per-tx exUnitsMem bump",
    "shelley": {
      "maxBlockBodySize": 73728
    },
    "alonzo": {
      "maxTxExUnits": {
        "exUnitsMem": 11250000
      }
    }
  },
  "317": {
    "epoch": 317,
    "description": "Bump per-tx exUnitsMem",
    "alonzo": {
      "maxTxExUnits": {
        "exUnitsMem": 12500000
      }
    }
  },
  "319": {
    "epoch": 319,
    "description": "Block size and per-tx exunit bump",
    "shelley": {
      "maxBlockBodySize": 81920
    },
    "alonzo": {
      "maxTxExUnits": {
        "exUnitsMem": 14000000
      }
    }
  },
  "321": {
    "epoch": 321,
    "description": "Per-block execution unit ceiling bump",
    "alonzo": {
      "maxBlockExUnits": {
        "exUnitsMem": 56000000
      }
    }
  },
  "328": {
    "epoch": 328,
    "description": "Another per-block execution unit ceiling bump",
    "alonzo": {
      "maxBlockExUnits": {
        "exUnitsMem": 62000000
      }
    }
  },
  "335": {
    "epoch": 335,
    "description": "Block size bump",
    "shelley": {
      "maxBlockBodySize": 90112
    }
  },
  "365": {
    "epoch": 365,
    "description": "Babbage era",
    "shelley": {
      "protocolVersion": {
        "major": 7
      },
      "minUTxOValue": 4310
    },
    "alonzo": {
      "lovelacePerUTxOWord": 4310
    }
  },
  "366": {
    "epoch": 366,
    "description": "Plutus V2 Cost Model",
    "costModels": {
      "PlutusV2": {
        "addInteger-cpu-arguments-intercept": 205665,
        "addInteger-cpu-arguments-slope": 812,
        "addInteger-memory-arguments-intercept": 1,
        "addInteger-memory-arguments-slope": 1,
        "appendByteString-cpu-arguments-intercept": 1000,
        "appendByteString-cpu-arguments-slope": 571,
        "appendByteString-memory-arguments-intercept": 0,
        "appendByteString-memory-arguments-slope": 1,
        "appendString-cpu-arguments-intercept": 1000,
        "appendString-cpu-arguments-slope": 24177,
        "appendString-memory-arguments-intercept": 4,
        "appendString-memory-arguments-slope": 1,
        "bData-cpu-arguments": 1000,
        "bData-memory-arguments": 32,
        "blake2b_256-cpu-arguments-intercept": 117366,
        "blake2b_256-cpu-arguments-slope": 10475,
        "blake2b_256-memory-arguments": 4,
        "cekApplyCost-exBudgetCPU": 23000,
        "cekApplyCost-exBudgetMemory": 100,
        "cekBuiltinCost-exBudgetCPU": 23000,
        "cekBuiltinCost-exBudgetMemory": 100,
        "cekConstCost-exBudgetCPU": 23000,
        "cekConstCost-exBudgetMemory": 100,
        "cekDelayCost-exBudgetCPU": 23000,
        "cekDelayCost-exBudgetMemory": 100,
        "cekForceCost-exBudgetCPU": 23000,
        "cekForceCost-exBudgetMemory": 100,
        "cekLamCost-exBudgetCPU": 23000,
        "cekLamCost-exBudgetMemory": 100,
        "cekStartupCost-exBudgetCPU": 100,
        "cekStartupCost-exBudgetMemory": 100,
        "cekVarCost-exBudgetCPU": 23000,
        "cekVarCost-exBudgetMemory": 100,
        "chooseData-cpu-arguments": 19537,
        "chooseData-memory-arguments": 32,
        "chooseList-cpu-arguments": 175354,
        "chooseList-memory-arguments": 32,
        "chooseUnit-cpu-arguments": 46417,
        "chooseUnit-memory-arguments": 4,
        "consByteString-cpu-arguments-intercept": 221973,
        "consByteString-cpu-arguments-slope": 511,
        "consByteString-memory-arguments-intercept": 0,
        "consByteString-memory-arguments-slope": 1,
        "constrData-cpu-arguments": 89141,
        "constrData-memory-arguments": 32,
        "decodeUtf8-cpu-arguments-intercept": 497525,
        "decodeUtf8-cpu-arguments-slope": 14068,
        "decodeUtf8-memory-arguments-intercept": 4,
        "decodeUtf8-memory-arguments-slope": 2,
        "divideInteger-cpu-arguments-constant": 196500,
        "divideInteger-cpu-arguments-model-arguments-intercept": 453240,
        "divideInteger-cpu-arguments-model-arguments-slope": 220,
        "divideInteger-memory-arguments-intercept": 0,
        "divideInteger-memory-arguments-minimum": 1,
        "divideInteger-memory-arguments-slope": 1,
        "encodeUtf8-cpu-arguments-intercept": 1000,
        "encodeUtf8-cpu-arguments-slope": 28662,
        "encodeUtf8-memory-arguments-intercept": 4,
        "encodeUtf8-memory-arguments-slope": 2,
        "equalsByteString-cpu-arguments-constant": 245000,
        "equalsByteString-cpu-arguments-intercept": 216773,
        "equalsByteString-cpu-arguments-slope": 62,
        "equalsByteString-memory-arguments": 1,
        "equalsData-cpu-arguments-intercept": 1060367,
        "equalsData-cpu-arguments-slope": 12586,
        "equalsData-memory-arguments": 1,
        "equalsInteger-cpu-arguments-intercept": 208512,
        "equalsInteger-cpu-arguments-slope": 421,
        "equalsInteger-memory-arguments": 1,
        "equalsString-cpu-arguments-constant": 187000,
        "equalsString-cpu-arguments-intercept": 1000,
        "equalsString-cpu-arguments-slope": 52998,
        "equalsString-memory-arguments": 1,
        "fstPair-cpu-arguments": 80436,
        "fstPair-memory-arguments": 32,
        "headList-cpu-arguments": 43249,
        "headList-memory-arguments": 32,
        "iData-cpu-arguments": 1000,
        "iData-memory-arguments": 32,
        "ifThenElse-cpu-arguments": 80556,
        "ifThenElse-memory-arguments": 1,
        "indexByteString-cpu-arguments": 57667,
        "indexByteString-memory-arguments": 4,
        "lengthOfByteString-cpu-arguments": 1000,
        "lengthOfByteString-memory-arguments": 10,
        "lessThanByteString-cpu-arguments-intercept": 197145,
        "lessThanByteString-cpu-arguments-slope": 156,
        "lessThanByteString-memory-arguments": 1,
        "lessThanEqualsByteString-cpu-arguments-intercept": 197145,
        "lessThanEqualsByteString-cpu-arguments-slope": 156,
        "lessThanEqualsByteString-memory-arguments": 1,
        "lessThanEqualsInteger-cpu-arguments-intercept": 204924,
        "lessThanEqualsInteger-cpu-arguments-slope": 473,
        "lessThanEqualsInteger-memory-arguments": 1,
        "lessThanInteger-cpu-arguments-intercept": 208896,
        "lessThanInteger-cpu-arguments-slope": 511,
        "lessThanInteger-memory-arguments": 1,
        "listData-cpu-arguments": 52467,
        "listData-memory-arguments": 32,
        "mapData-cpu-arguments": 64832,
        "mapData-memory-arguments": 32,
        "mkCons-cpu-arguments": 65493,
        "mkCons-memory-arguments": 32,
        "mkNilData-cpu-arguments": 22558,
        "mkNilData-memory-arguments": 32,
        "mkNilPairData-cpu-arguments": 16563,
        "mkNilPairData-memory-arguments": 32,
        "mkPairData-cpu-arguments": 76511,
        "mkPairData-memory-arguments": 32,
        "modInteger-cpu-arguments-constant": 196500,
        "modInteger-cpu-arguments-model-arguments-intercept": 453240,
        "modInteger-cpu-arguments-model-arguments-slope": 220,
        "modInteger-memory-arguments-intercept": 0,
        "modInteger-memory-arguments-minimum": 1,
        "modInteger-memory-arguments-slope": 1,
        "multiplyInteger-cpu-arguments-intercept": 69522,
        "multiplyInteger-cpu-arguments-slope": 11687,
        "multiplyInteger-memory-arguments-intercept": 0,
        "multiplyInteger-memory-arguments-slope": 1,
        "nullList-cpu-arguments": 60091,
        "nullList-memory-arguments": 32,
        "quotientInteger-cpu-arguments-constant": 196500,
        "quotientInteger-cpu-arguments-model-arguments-intercept": 453240,
        "quotientInteger-cpu-arguments-model-arguments-slope": 220,
        "quotientInteger-memory-arguments-intercept": 0,
        "quotientInteger-memory-arguments-minimum": 1,
        "quotientInteger-memory-arguments-slope": 1,
        "remainderInteger-cpu-arguments-constant": 196500,
        "remainderInteger-cpu-arguments-model-arguments-intercept": 453240,
        "remainderInteger-cpu-arguments-model-arguments-slope": 220,
        "remainderInteger-memory-arguments-intercept": 0,
        "remainderInteger-memory-arguments-minimum": 1,
        "remainderInteger-memory-arguments-slope": 1,
        "serialiseData-cpu-arguments-intercept": 1159724,
        "serialiseData-cpu-arguments-slope": 392670,
        "serialiseData-memory-arguments-intercept": 0,
        "serialiseData-memory-arguments-slope": 2,
        "sha2_256-cpu-arguments-intercept": 806990,
        "sha2_256-cpu-arguments-slope": 30482,
        "sha2_256-memory-arguments": 4,
        "sha3_256-cpu-arguments-intercept": 1927926,
        "sha3_256-cpu-arguments-slope": 82523,
        "sha3_256-memory-arguments": 4,
        "sliceByteString-cpu-arguments-intercept": 265318,
        "sliceByteString-cpu-arguments-slope": 0,
        "sliceByteString-memory-arguments-intercept": 4,
        "sliceByteString-memory-arguments-slope": 0,
        "sndPair-cpu-arguments": 85931,
        "sndPair-memory-arguments": 32,
        "subtractInteger-cpu-arguments-intercept": 205665,
        "subtractInteger-cpu-arguments-slope": 812,
        "subtractInteger-memory-arguments-intercept": 1,
        "subtractInteger-memory-arguments-slope": 1,
        "tailList-cpu-arguments": 41182,
        "tailList-memory-arguments": 32,
        "trace-cpu-arguments": 212342,
        "trace-memory-arguments": 32,
        "unBData-cpu-arguments": 31220,
        "unBData-memory-arguments": 32,
        "unConstrData-cpu-arguments": 32696,
        "unConstrData-memory-arguments": 32,
        "unIData-cpu-arguments": 43357,
        "unIData-memory-arguments": 32,
        "unListData-cpu-arguments": 32247,
        "unListData-memory-arguments": 32,
        "unMapData-cpu-arguments": 38314,
        "unMapData-memory-arguments": 32,
        "verifyEcdsaSecp256k1Signature-cpu-arguments": 35892428,
        "verifyEcdsaSecp256k1Signature-memory-arguments": 10,
        "verifyEd25519Signature-cpu-arguments-intercept": 9462713,
        "verifyEd25519Signature-cpu-arguments-slope": 1021,
        "verifyEd25519Signature-memory-arguments": 10,
        "verifySchnorrSecp256k1Signature-cpu-arguments-intercept": 38887044,
        "verifySchnorrSecp256k1Signature-cpu-arguments-slope": 32947,
        "verifySchnorrSecp256k1Signature-memory-arguments": 10
      }
    }
  }
};

def lastKnownEpoch:
    epochs
  | keys
  | map(fromjson)
  | max;
