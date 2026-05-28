## Remove parts of profile that don't invalidate
## the cryptographic material in genesis.  Note the opportunistic approach.
##
## Note also, that the genesis cache entry itself must still be updated
## to match these parameters, hence the distinction between parameters.
##
def profile_genesis_cache_key($p; $profile_file):

  ($p.genesis * $p.composition * $p.derived)
  |
  { network_magic

  , funds_balance
  , per_pool_balance
  , pool_coin

  , n_pools
  , n_bft_hosts
  , n_dense_hosts
  , dense_pool_density

  , delegators
  , utxo_stuffed
  , dreps

  } as $genesis_crypto_affecting_data

  | $genesis_crypto_affecting_data | to_entries
  | map(if .value == null
        then error("FATAL: undefined key \(.key) in profile \(.profile_file)")
        else null end)

  | $genesis_crypto_affecting_data
;

## Profiles with a preset short-circuit at the genesis.sh top-level dispatcher.
def profile_genesis_cache_entry_name($p; $params_hash):

  [ "k\(.composition.n_pools)" ]
  +
  if .composition.dense_pool_density == 1
  then []
  else [ "d\(.composition.dense_pool_density)" ]
  end
  +
  [ "\(.genesis.delegators / 1000)kD" ]
  +
  if .genesis.dreps != 0
  then ["\(.genesis.dreps)Dr"]
  else []
  end
  +
  [ "\(.derived.utxo_stuffed / 1000)kU"
  , "\($params_hash)"
  ]
  | join("-")
;

