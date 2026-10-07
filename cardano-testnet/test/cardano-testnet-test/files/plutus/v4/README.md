# CIP-160 per-output Receiving/Spending fixture

This envelope is byte-identical to the genuine ledger preprocessor output for
`receivingEvenDatum`, regenerated against the per-output V4 context source at
Plutus proposal commit `dcbb7e3`. Receiving checks its specific authorized
output's inline integer datum is even; Spending checks the consumed inline
datum. Each protected Plutus output has a separate Receiving index and budget.

Text-envelope SHA256: df7f93cfaa4fc11032aed99d00215d43479eec5922a5765c696e09cc8240ae44.
Serialized script: 208 bytes; SHA256 dbce2a96a7a81771a27ea29635b0c05d4f7c45fee8714e63a2ab83b1cd436fbc.
Plutus V4 script hash: 28d467557081773a051b5f83982574abfccceb079bc8012f192505e2.

The preprocessor completed two byte-identical generation passes; its receipt
compares all 41 legacy V1-V3 fixtures unchanged. The registered real-node test uses distinct indexes 2/3 for the two V4 outputs.
Its creation/rejection, exact Local State Query and API replay, same-database relay
restart and subsequent spending checks passed with the current 369-parameter
V4 genesis model. This validates the local Dijkstra lifecycle; it does not
establish scheduled activation, snapshot restoration, rollback or Leios support.
