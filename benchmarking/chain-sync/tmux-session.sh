#!/bin/sh

tmux new-window -n ByronProxy "./launch_mainnet_proxy.sh; $SHELL"

tmux new-window -n Benchmark "./benchmark-chain-sync-mainnet.sh; $SHELL"

