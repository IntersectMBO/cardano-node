#!/bin/bash

# Current working dir: cardano-node

CWD=$PWD

# Source common lib

. $CWD/e2e-automation-scripts/common.sh

info_msg "Checking stake disribution ..."

stake_distribution_output=$(cardano-cli shelley query stake-distribution --testnet-magic $testnet_magic)

if [ $?	!= 0 ]; then
    error_msg "Error when checking stake distribution"
    exit 1
fi

# Get first entry - first 2 lines are headers, info about pool is located on 3rd line
# If there is no entry then exit script with status 2 - warning
stake_distribution_stats=$(echo "$stake_distribution_output" | head -3 | tail -1)

if [[ -z $stake_distribution_stats || $stake_distribution_stats == *"----"* ]]; then
    warn_msg "There is no info about stake distibution. You might need to wait for a little bit longer."
    warn_msg "Exiting ..."
    exit 2
fi

# Used for testing/debugging - don't remove it, it might be needed in the future
: <<'END'
stake_distribution_output=$(cat << HEREDOC
                           PoolId                                 Stake frac
------------------------------------------------------------------------------
2dd35efde230703aaea3687d268f5d241e90ac3164bb4957889cd7516697d501   1.000e0
2dd35efde230703aaea3687d268f5d2kikul8c3164bb4957889cd75sdf4hd502   2.000e0
2dd35efde230703aaea3687d268fbnbmbn90ac3164bb4957889cd7516697d503   3.000e0
HEREDOC
)
END

# If there was one entry then get stake distribution info for all pools and list it

stake_pool_entries=$(echo "$stake_distribution_output" | wc -l)
header_lines=2
stake_distribution_stats=$(echo "$stake_distribution_output" | tail -$(( stake_pool_entries - header_lines )))

IFS=$'\n'
for line in $stake_distribution_stats
do
    pool_id=$(echo $line | awk -F'[[:space:]]*' '{print $1}')
    stake_fraction=$(echo $line | awk -F'[[:space:]]*' '{print $2}')

    regex_number='^([+-]?[0-9.]+)[eE]\+?(-?)([0-9]+)?$'
    if ! [[ $stake_fraction =~ $regex_number ]] ; then
        error_msg "Stake fraction $stake_fraction not a number" >&2; exit 1
    fi

    info_msg "Pool Id: $pool_id"
    info_msg "Stake fraction: $stake_fraction"
done

success_msg "Stake distribution check successfull"
exit 0
