def profile_cli_args($p):
($p.genesis.per_pool_balance * $p.composition.n_pools) as $pools_balance
|
{ common:
  { createSpec:
    [ "--supply",                  ($pools_balance + $p.genesis.funds_balance)
    , "--testnet-magic",           $p.genesis.network_magic
    , "--gen-genesis-keys",        $p.composition.n_bft_hosts
    , "--gen-utxo-keys",           1
    ]
 , createFinalIncremental:
    ([ "--supply",                 ($p.genesis.funds_balance)
     , "--gen-utxo-keys",          1
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [  ]
     else [] end)
  , createFinalBulk:
    ([ "--supply",                 ($p.genesis.funds_balance)
     , "--gen-utxo-keys",          1
     , "--gen-genesis-keys",       $p.composition.n_bft_hosts
     , "--supply-delegated",       $pools_balance
     , "--gen-pools",              $p.composition.n_pools
     , "--gen-stake-delegs",       ([ $p.composition.n_pools
                                    , $p.genesis.delegators ]
                                     | max)
     , "--testnet-magic",          $p.genesis.network_magic
     , "--num-stuffed-utxo",       $p.genesis.utxo
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [ "--bulk-pool-cred-files",   $p.composition.n_dense_hosts
     , "--bulk-pools-per-file",    $p.composition.dense_pool_density ]
     else [] end)
  , pools:
    [ "--argjson"
    , "initialPoolCoin",           $p.genesis.pool_coin
    ]
  }
}
| .common * (.[$p.era] // {})
;
