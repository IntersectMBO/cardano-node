usage_chaindb() {
     usage "chaindb" "Manage ChainDBs" <<EOF
  Shared args:
    --source IMMUTABLEDB-SRCDIR  Use the specified chainDB as source for immutable chunks
    --geneses JSON               Geneses: { byron: PATH, shelley: PATH, alonzo: PATH }
    --mainnet                    Use the Cardano mainnet as source of chunks
    --cachedir DIR               Where to store mainnet snapshot cache

  Commands
    immutable-until-chunk OUTDIR FINAL-CHUNK-NO
                     Produce a ChainDB derivative trimmed up to specified chunk#
    snapshot-at-slot OUTDIR FINAL-SLOT-NO
                     Produce a ChainDB derivative with snapshot at given slot#
EOF
}

cardano_mainnet_geneses=(
    --arg byron   "$global_basedir"/../../configuration/cardano/mainnet-byron-genesis.json
    --arg shelley "$global_basedir"/../../configuration/cardano/mainnet-shelley-genesis.json
    --arg alonzo  "$global_basedir"/../../configuration/cardano/mainnet-alonzo-genesis.json
    --arg shelley_hash '1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81'
)

membench_params=$(jq '
{ final_chunk:   1800
, snapshot_slot: 37173650
}' --null-input)

chaindb() {
local source= geneses=() mainnet= cachedir= sargs=()
while test $# -gt 0
do case "$1" in
       --source )   source=$2;   sargs+=($1 $2); shift;;
       --cachedir ) cachedir=$2; sargs+=($1 $2); shift;;
       --geneses )  geneses=$2;  sargs+=($1 $2); shift;;
       --mainnet )
           sargs+=($1)
           mainnet='true'
           cachedir=$HOME/.cache/cardano-workbench
           source=$CARDANO_MAINNET_MIRROR
           geneses=$(jq '
{ byron:        $byron
, shelley:      $shelley
, alonzo:       $alonzo
}
' "${cardano_mainnet_geneses[@]}" --null-input);;
       * ) break;; esac; shift; done

op=${1:?$(usage_chaindb)}; shift

case "$op" in

mainnet-membench-chaindb )
    local usage="USAGE: wb chaindb $op OUTDIR"
    local out=${1:?$usage}; shift

    progress "chaindb" "producing a membench-like chaindb"
    chaindb mainnet-chunks-with-snapshot-at-slot "$out" \
            $(jq .snapshot_slot <<<$membench_params) \
            $(jq .final_chunk   <<<$membench_params)
    ;;

mainnet-chunks-with-snapshot-at-slot )
    local usage="USAGE: wb chaindb $op OUTDIR SNAPSHOT-SLOTNO FINAL-CHUNKNO"
    local out=${1:?$usage}
    local snapshot_slotno=${2:?$usage}
    local final_chunkno=${3:?$usage}

    progress "chaindb" "producing a mainnet chaindb until chunk $final_chunkno, with snapshot at slot $snapshot_slotno"
    rm -rf "${out}_tmp"
    chaindb --mainnet                  immutable-until-chunk "${out}_tmp" "$final_chunkno"
    chaindb --mainnet --source "${out}_tmp" snapshot-at-slot "$out"       "$snapshot_slotno"
    rm -rf "${out}_tmp"
    ;;

immutable-until-chunk )
    local usage="USAGE: wb chaindb (--source IMMUTABLEDB-SRCDIR | --mainnet) $op IMMUTABLEDB-OUTDIR FINAL-CHUNKNO"
    local out=${1:?$usage}; shift
    local final_chunkno=${1:?$usage}; shift

    progress "chaindb" "subsetting ChainDB until chunk $final_chunkno"
    (
    mkdir -p $out/immutable
    cd $out
    cp ${source}/protocolMagicId protocolMagicId
    for epoch in $(seq -w 00000 $((final_chunkno - 1))); do
      ln -s ${source}/immutable/${epoch}.{chunk,primary,secondary} immutable
    done
    cp ${source}/immutable/$(printf "%05d" $final_chunkno).{chunk,primary,secondary} immutable
    chmod +w immutable/$(printf "%05d" $final_chunkno).*

    ## Existence of 'clean' is necessary to avoid ImmutableDB revalidation:
    touch clean
    );;

snapshot-at-slot )
    local usage="USAGE: wb chaindb --source IMMUTABLEDB-SRCDIR [--mainnet] $op SNAPSHOT-OUTDIR SLOTNO"
    local out=${1:?$usage}; shift
    local slotno=${1:?$usage}; shift

    local args=(
        --genesis "$(jq .shelley -r <<<$geneses)"
    )
    local shelleyGenesisHash=$(cardano-cli genesis hash "${args[@]}" 2>/dev/null)
         test -n "$shelleyGenesisHash" ||
             fail "Invalid Shelley genesis: $(jq .shelley <<<$geneses)"

    cp -r ${source}  $out
    chmod +w -R      $out

    progress "chaindb" "deriving a ChainDB with ledger snapshot at slot $slotno"

    local cache_entry=$cachedir/ledger/mainnet-ledger.$slotno
    if test -n "$mainnet" -a -d "$cachedir" -a -f $cache_entry

    then progress "chaindb" "using mainnet cache entry for slot $slotno"
         mkdir -p         $out/ledger/
         cp  $cache_entry $out/ledger/$slotno

    else progress "chaindb" "reapplying blocks.."

         ## Actually produce the snapshot:
         args=( --configByron     "$(jq -r .byron   <<<$geneses)"
                --configShelley   "$(jq -r .shelley <<<$geneses)"
                --configAlonzo    "$(jq -r .alonzo  <<<$geneses)"
                --nonce           "$shelleyGenesisHash"
                --store-ledger    $slotno
              )
         db-analyser  --db $out cardano "${args[@]}"

         ls -ltrh          $out/ledger

         mkdir             $out/temp
         mv                $out/ledger/{$slotno,$(cd $out/ledger; ls | tail -n1)} $out/temp 2>/dev/null || true
         rm                $out/ledger/* 2>/dev/null || true
         mv    $out/temp/* $out/ledger/
         rmdir $out/temp
    fi

    if test -n "$mainnet" -a -d "$cachedir" -a -f "$out/ledger/$slotno"
    then cp               $out/ledger/$slotno $cache_entry
    fi

    local last_chunk=$(cd $out/immutable; ls | tail -n1 | cut -d. -f1)

    args=( --argjson snapshotSlot       $slotno
           --arg     finalChunkNo       $last_chunk
           --arg     shelleyGenesisHash $shelleyGenesisHash
         )
    jq '{ snapshotSlot:                 $snapshotSlot
        , finalChunkNo:                 $finalChunkNo
        , shelleyGenesisHash:           $shelleyGenesisHash
      # , snapshottingConsensusNodeRev: $snapshottingConsensusNodeRev
        }
       ' --null-input "${args[@]}" > $out/snapshot-info.json
    ;;

validate )
    local usage="USAGE: wb chaindb $op DIR [LEDGER-SNAPSHOT-SLOTNO]"
    local dir=${1:?$usage}; shift
    local slot=${1:-}

    progress "chaindb" "validating.."
    local args=(
        --db "$dir"
        $(if test -n "$slot"
          then echo --only-immutable-db --analyse-from $slot
          else echo --validate-all-blocks; fi)
        cardano
        --configByron     "$(jq -r .byron   <<<$geneses)"
        --configShelley   "$(jq -r .shelley <<<$geneses)"
        --configAlonzo    "$(jq -r .alonzo  <<<$geneses)"
    )
    db-analyser "${args[@]}"
    ;;

* ) usage_chaindb;; esac
}
