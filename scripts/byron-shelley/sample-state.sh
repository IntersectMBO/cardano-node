mkdir -p data
n=0
export CARDANO_NODE_SOCKET_PATH=example/node-bft1/node.sock
while true
do
    cardano-cli shelley query tip --testnet-magic 42 > data/state.$n
    cardano-cli shelley query ledger-state --testnet-magic 42 >> data/state.$n
    echo $n
    cardano-cli shelley query ledger-state --testnet-magic 42 | jq .esAccountState
    cardano-cli shelley query ledger-state --testnet-magic 42 | jq .esLState._delegationState._dstate._rewards
    echo "rewardPot: "
    cardano-cli shelley query ledger-state --testnet-magic 42 | jq .esNonMyopic.rewardPotNM
    n=$((${n} + 1))
    sleep 200
done
