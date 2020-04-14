#!/usr/bin/env bash

exec stack build cardano-node:exe:cardano-node --copy-bins --local-bin-path .
