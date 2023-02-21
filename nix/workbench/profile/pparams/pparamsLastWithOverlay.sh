#!/bin/sh

jq --null-input '
  include "epoch-timeline";
  include "../prof1-pparams";

  pParamsWithOverlays(lastKnownEpoch; [$overlay])
  ' --arg overlay ${1:?USAGE: $0 EXTRA-NAME}
