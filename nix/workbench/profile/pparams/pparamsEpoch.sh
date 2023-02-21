#!/bin/sh

jq --null-input '
  include "epoch-timeline";

  filterMapPParams(.value.epoch <= $upto; .)
  ' --argjson                       upto "${1:-10000000}"
