### Changed

- Replaced `caseShelleyToBabbageOrConwayEraOnwards` and `conwayEraOnwardsConstraints` patterns with `obtainCommonConstraints` and a new `unsafeEraFromSbe` helper that converts `ShelleyBasedEra` to the experimental `Era` witness, simplifying era-dependent code in governance tests and epoch state processing.
