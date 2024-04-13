#!/usr/bin/env bash

( cd cardano-testnet
  stat -c "%y %n" * | grep '\(start\|stop\)' | sort
)
