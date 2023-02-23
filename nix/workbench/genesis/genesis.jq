## WARNING:  keep in sync with 'profile-cache-key-input' below this one: vvv
##
def fmt_decimal_10_5($x):
  ($x / 100000 | tostring) + "00000";

def profile_cli_args($p):
{ common:
  { createSpec:
    [ "--supply",                  fmt_decimal_10_5($p.derived.supply_total)
    , "--testnet-magic",           $p.genesis.network_magic
    , "--gen-genesis-keys",        $p.composition.n_bft_hosts
    , "--gen-utxo-keys",           1
    ]
 , createFinalIncremental:
    ([ "--supply",                 $p.genesis.funds_balance
     , "--gen-utxo-keys",          1
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [  ]
     else [] end)
  , createFinalBulk:
    ([ "--supply",                 fmt_decimal_10_5($p.genesis.funds_balance)
     , "--gen-utxo-keys",          1
     , "--gen-genesis-keys",       $p.composition.n_bft_hosts
     , "--supply-delegated",       fmt_decimal_10_5($p.derived.supply_delegated)
     , "--gen-pools",              $p.composition.n_pools
     , "--gen-stake-delegs",       $p.derived.delegators_effective
     , "--testnet-magic",          $p.genesis.network_magic
     , "--num-stuffed-utxo",       fmt_decimal_10_5($p.derived.utxo_stuffed)
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [ "--bulk-pool-cred-files",   $p.composition.n_dense_hosts
     , "--bulk-pools-per-file",    $p.composition.dense_pool_density ]
     else [] end)
  , pools:
    [ "--argjson"
    , "initialPoolCoin",           fmt_decimal_10_5($p.genesis.pool_coin)
    ]
  }
}
| .common * (.[$p.era] // {})
;

## Remove parts of profile that don't invalidate
## the cryptographic material in genesis.  Note the opportunistic approach.
##
## Note also, that the genesis cache entry itself must still be updated
## to match these parameters, hence the distinction between parameters:
##
## WARNING:  keep in sync with 'profile_cli_args' above ^^^
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

  } as $genesis_crypto_affecting_data

  | $genesis_crypto_affecting_data | to_entries
  | map(if .value == null
        then error("FATAL: undefined key \(.key) in profile \(.profile_file)")
        else null end)

  | $genesis_crypto_affecting_data
;

def profile_genesis_cache_entry_name($p; $params_hash):

if $p.preset == null
then [ "k\(.composition.n_pools)" ]
     +
     if .composition.dense_pool_density == 1 then []
     else
     [ "d\(.composition.dense_pool_density)" ] end
     +
     [ "\(.genesis.delegators / 1000)kD"
     , "\(.derived.utxo_stuffed / 1000)kU"
     , "\($params_hash)" ]
else [ "preset"
     , $profile[0].preset ]
end
| join("-")
;
