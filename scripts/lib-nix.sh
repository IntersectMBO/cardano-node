#!/usr/bin/env bash
# shellcheck disable=SC2039,SC2154,SC2206,SC2155,SC2086,SC2034
## Citing the bash BUGS manpage section:
## > Array variables may not (yet) be exported
run_nix_executable() {
        local name="$1" extra="$2"
        shift 2
        local cache_var=nix_executable_cache_${name/-/_}
        if test -z "${!cache_var}"
        then dprint "executable cache miss for \"${name}\": ${!nix_executable_cache_@}"
             fill_nix_executable_cache_entry "${name}" "${extra}"
        else dprint "executable cache hit for \"${name}\""
        fi
        dprint "${!cache_var} $*"
        ${!cache_var} "$@"
}

fill_nix_executable_cache_entry() {
        local name="$1" extra="$2"
        vprint "filling the Nix executable cache for \"$1\".."
        NIX_BUILD=(
                nix-build
                "${root}/default.nix"
                --no-out-link
                ${extra}
                ${defaultnix_args}
                -A 'haskellPackages.cardano-node.components.exes.'${name}
        )
        dprint "${NIX_BUILD[*]}"
        local out=$("${NIX_BUILD[@]}")/bin/${name}
        local cache_var=nix_executable_cache_${name/-/_}
        eval export ${cache_var}
        declare -n nix_cache_write_ref=${cache_var}
        nix_cache_write_ref=${out}
        vprint "cached ${name} = ${!cache_var}"
        dprint "new cache:  ${!nix_executable_cache_@}"
}
