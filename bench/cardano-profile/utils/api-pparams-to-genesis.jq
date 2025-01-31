## Sources:
##   - cardano-ledger/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Genesis.hs
##   - cardano-api/src/Cardano/Api/ProtocolParameters.hs:toBabbagePParamsUpdate
def cardano_api_pparams_to_geneses(api; desc):
  { epoch:                   1000000
  , description:             desc
  , alonzo:
    (
    { collateralPercentage:  api.collateralPercentage
    , executionPrices:
      { prSteps:
        { numerator:         (api.executionUnitPrices.priceSteps * 10000000)
        , denominator:       10000000
        }
      , prMem:
        { numerator:         (api.executionUnitPrices.priceMemory * 10000)
        , denominator:       10000
        }
      }
    , maxBlockExUnits:
      { exUnitsMem:          api.maxBlockExecutionUnits.memory
      , exUnitsSteps:        api.maxBlockExecutionUnits.steps
      }
    , maxCollateralInputs:   api.maxCollateralInputs
    , maxTxExUnits:
      { exUnitsMem:          api.maxTxExecutionUnits.memory
      , exUnitsSteps:        api.maxTxExecutionUnits.steps
      }
    , maxValueSize:          api.maxValueSize
    , coinsPerUTxOByte:      api.utxoCostPerByte
    }
    * if api.utxoCostPerWord == null then {} else
    { lovelacePerUTxOWord:   api.utxoCostPerWord
    } end
    * if api.minUTxOValue == null then {} else
    { minUTxOValue:          api.minUTxOValue
    } end
    )
  , shelley:
    (
    { maxBlockBodySize:      api.maxBlockBodySize
    , maxBlockHeaderSize:    api.maxBlockHeaderSize
    , maxTxSize:             api.maxTxSize
    , minPoolCost:           api.minPoolCost
    , minUTxOValue:          api.minUTxOValue
    , rho:                   api.monetaryExpansion
    , a0:                    api.poolPledgeInfluence
    , eMax:                  api.poolRetireMaxEpoch
    , protocolVersion:       api.protocolVersion
    , keyDeposit:            api.stakeAddressDeposit
    , poolDeposit:           api.stakePoolDeposit
    , nOpt:                  api.stakePoolTargetNum
    , tau:                   api.treasuryCut
    , minFeeB:               api.txFeeFixed
    , minFeeA:               api.txFeePerByte
    }
    * if api.decentralisation == null then {} else
    { decentralisationParam: api.decentralisation
    } end
    * if api.extraPraosEntropy == null then {} else
    { extraEntropy:          api.extraPraosEntropy
    } end
    )
  , costModels:
    { PlutusV1:              api.costModels.PlutusScriptV1
    , PlutusV2:              api.costModels.PlutusScriptV2
    }
  };
