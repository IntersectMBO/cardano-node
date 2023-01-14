#!/bin/sh

jq --null-input '
  include "api-pparams-to-genesis";

  cardano_api_pparams_to_geneses($pparams[0]; $desc)
  ' --slurpfile pparams ${1:?USAGE: $0 CARDANO-API-PPARAMS-JSON} \
    --arg       desc    "Imported from $(basename $1)" \
    --sort-keys
