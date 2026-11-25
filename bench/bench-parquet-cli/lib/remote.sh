# shellcheck shell=bash
# bench-parquet: remote: commands on another host over ssh.
# Sourced by bench-parquet: uses its helpers (msg, die, sqlq, ...) and
# variables (self_dir, sql_dir, DUCKDB, ...).

# POSIX shell quoting, for command strings run by a remote login shell.
shq() { local s=${1//\'/\'\\\'\'}; printf "'%s'" "$s"; }

# What a remote run needs besides the package itself: its tools (bash, duckdb,
# coreutils, gawk, sed, findutils), as store paths for each system a remote may
# run on, from the nixpkgs revision the enclosing flake locks (lib/remote-tools.nix;
# evaluated locally, nothing is built). The remote picks its system's paths,
# fetches them from its substituters (nix-store -r; no trusted user needed) and
# runs with nothing else on PATH. Cached per content of flake.lock and of the
# expression: one evaluation per flake.lock version.
# Output lines: "SYSTEM PATH..", bash first.
remote_tool_paths() {
    local dir=$self_dir lock key cache
    until test -f "$dir/flake.lock" -o "$dir" = /; do dir=$(dirname "$dir"); done
    lock=$dir/flake.lock
    test -f "$lock" || die "remote: no flake.lock in $self_dir or above (the tools come from its nixpkgs)"
    key=$(cat "$lock" "$self_dir/lib/remote-tools.nix" | sha256sum | cut -c1-32)
    cache=${XDG_CACHE_HOME:-$HOME/.cache}/bench-parquet/tools/$key
    if ! test -s "$cache"
    then mkdir -p "$(dirname "$cache")"
         msg "remote: evaluating the tools' store paths (nixpkgs of $lock)"
         BP_TOOLS_NIX=$self_dir/lib/remote-tools.nix BP_LOCK=$lock \
             nix --extra-experimental-features 'nix-command flakes' eval --impure --raw --expr \
             'import (builtins.getEnv "BP_TOOLS_NIX") { lockFile = builtins.getEnv "BP_LOCK"; }' \
             > "$cache.tmp" || die "remote: could not evaluate lib/remote-tools.nix"
         mv "$cache.tmp" "$cache"
    fi
    cat "$cache"
}

# A nix system name as uname -sm on that system prints it.
remote_uname() {
    case "$1" in
        x86_64-linux )   echo "Linux x86_64";;
        aarch64-linux )  echo "Linux aarch64";;
        aarch64-darwin ) echo "Darwin arm64";;
        x86_64-darwin )  echo "Darwin x86_64";;
        * )              die "remote: unknown system $1";;
    esac
}

# The package as shipped: the script, its modules and its *.sql files (nothing
# else is read at run time), reproducibly (same content, same bytes, same hash).
remote_pkg_tar() {
    ( cd "$self_dir" &&
      { echo bench-parquet; find lib -type f -name '*.sh'; find sql user_sql -type f -name '*.sql'; } |
      LC_ALL=C sort |
      tar -c --mtime=@0 --owner=0 --group=0 --numeric-owner --no-recursion -T - )
}

# On the remote, everything lives below ~/.cache/bench-parquet (relative to the
# home directory, where ssh starts): pkg/HASH (one per package version), req/ID
# (a call's shipped SQL files), spill/ (DuckDB's temp directories).
remote_base=.cache/bench-parquet

remote_cmd() {
    local host=${1:?remote: SSH-HOST missing}; shift
    local memory=4GB threads=4 spill_limit=10GB
    while test $# -gt 0
    do case "$1" in
           --memory )      memory=${2:?--memory needs a size}; shift;;
           --threads )     threads=${2:?--threads needs a number}; shift;;
           --spill-limit ) spill_limit=${2:?--spill-limit needs a size}; shift;;
           -* )            die "remote: unknown option: $1";;
           * )             break;;
       esac; shift; done
    local cmd=${1:?remote: COMMAND missing}; shift
    case "$cmd" in
        query | run | check | verify | head | tail | lines | host-summary ) ;;
        * ) die "remote: $cmd is not available remotely (query, run, check, verify, head, tail, lines, host-summary are)";;
    esac
    local pq=${1:?remote: PQDIR (on $host) missing}; shift

    local hash tools
    hash=$(remote_pkg_tar | sha256sum | cut -c1-32)
    tools=$(remote_tool_paths) || exit 1
    local pkg=$remote_base/pkg/$hash

    # Local SQL files (run FILE.sql, --load PATH, lines --ns-file FILE) go along:
    # files of the package are already there, others are shipped into req/ID.
    local stage= req=
    stage=$(mktemp -d)
    # Removed on any exit (a die in remote_path included); a global, as the
    # trap runs after this function's locals are gone.
    remote_stage=$stage
    trap 'rm -r -- "$remote_stage"' EXIT
    req=$remote_base/req/$(date +%s)-$$-$RANDOM
    remote_path() {
        local rp; rp=$(realpath -e "$1") || die "remote: no such file or directory: $1"
        if [[ $rp == "$self_dir"/* ]]
        then echo "$pkg/${rp#"$self_dir"/}"; return
        fi
        # Called in command substitutions (subshells): number by what is staged.
        local n; n=$(( $(find "$stage" -mindepth 1 -maxdepth 1 | wc -l) + 1 ))
        mkdir "$stage/$n"
        if test -d "$rp"
        then find "$rp" -maxdepth 1 -type f -name '*.sql' -exec cp -t "$stage/$n" {} +
             echo "$req/$n"
        else cp "$rp" "$stage/$n/"; echo "$req/$n/$(basename "$rp")"
        fi
    }
    local args=() a interactive=t
    while test $# -gt 0
    do a=$1
       case "$cmd:$a" in
           query:--ui )      die "remote: --ui is not available remotely";;
           query:-c | query:-cmd ) interactive=; args+=("$a" "${2:?}"); shift;;
           query:--load | run:--load | lines:--ns-file )
                             args+=("$a" "$(remote_path "${2:?$a needs a path}")") || exit 1; shift;;
           run:-* )          args+=("$a");;
           run:* )           if [[ $a == */* || $a == *.sql ]]
                             then args+=("$(remote_path "$a")") || exit 1
                             else args+=("$a"); fi;;
           * )               args+=("$a");;
       esac; shift
    done

    if compgen -G "$stage/*" >/dev/null
    then tar -c -C "$stage" . | ssh -T "$host" "mkdir -p $(shq "$req") && tar -x -C $(shq "$req")" ||
             die "remote: could not ship the SQL files to $host"
    else req=
    fi

    # The remote command (POSIX sh): its system's tool paths (P; bash first),
    # fetched if missing, the package present, then remote-exec with PATH from P.
    local sys paths remote_line="case \"\$(uname -sm)\" in"
    while read -r sys paths
    do remote_line+=" $(shq "$(remote_uname "$sys")")) P=$(shq "$paths");;"
    done <<< "$tools"
    remote_line+=" *) echo \"no tools for \$(uname -sm)\" >&2; exit 195;; esac;"
    # Expanded on the remote: single quotes on purpose.
    # shellcheck disable=SC2016
    remote_line+=' nix-store --check-validity $P >/dev/null 2>&1 || nix-store -r $P >/dev/null || exit 196;'
    remote_line+=" test -f $(shq "$pkg/.complete") || exit 197;"
    # shellcheck disable=SC2016
    remote_line+=' B=; PB=; for p in $P; do B=${B:-$p}; PB=${PB:+$PB:}$p/bin; done;'
    remote_line+=" exec \"\$B/bin/bash\" $(shq "$pkg/bench-parquet") remote-exec --path \"\$PB\""
    local exec_args=(--memory "$memory" --threads "$threads" --spill-limit "$spill_limit")
    test -z "$req" || exec_args+=(--req "$req")
    for a in "${exec_args[@]}" -- "$cmd" "$pq" "${args[@]}"; do remote_line+=" $(shq "$a")"; done

    local tty=(-T)
    # Interactive: a terminal; ssh's INFO line "Connection to HOST closed." suppressed.
    test "$cmd" != query -o -z "$interactive" -o ! -t 0 || tty=(-t -o LogLevel=ERROR)
    local status=0
    # The command line is built (and quoted) locally on purpose.
    # shellcheck disable=SC2029
    ssh "${tty[@]}" "$host" "$remote_line" || status=$?
    if test $status -eq 197
    then msg "remote: shipping bench-parquet $hash to $host"
         remote_pkg_tar | ssh -T "$host" "$(remote_unpack_line "$pkg")" ||
             die "remote: could not ship bench-parquet to $host"
         status=0
         # shellcheck disable=SC2029
         ssh "${tty[@]}" "$host" "$remote_line" || status=$?
    fi
    test $status -ne 195 ||
        die "remote: $host is none of the systems the tools are evaluated for ($(cut -d' ' -f1 <<< "$tools" | paste -sd' '))"
    test $status -ne 196 ||
        die "remote: $host could not fetch its tools (nix-store -r; see lib/remote-tools.nix for the list)"
    return $status
}

# The remote command that unpacks the package from stdin into DIR: into a fresh
# directory beside it, marked complete, then renamed (a concurrent shipment of
# the same version wins, this one is dropped).
remote_unpack_line() {
    local dir=$1 parent
    parent=$(dirname "$dir")
    printf '%s' "mkdir -p $(shq "$parent") && t=\$(mktemp -d $(shq "$parent/.new.XXXXXX")) &&"
    printf '%s' " tar -x -C \"\$t\" && touch \"\$t/.complete\" &&"
    printf '%s' " { mv -T \"\$t\" $(shq "$dir") 2>/dev/null || rm -r -- \"\$t\"; }"
}

# On the remote: the command with PATH set to the shipped tools, resource
# bounds for its sessions, its own spill directory; afterwards the spill
# directory and the call's shipped files are removed (those of calls that never
# got that far, after a day). Package versions: each call marks its own as used
# (the mtime of its .complete); versions unused for 30 days are removed (shipped
# again on next use), as are unpack leftovers (pkg/.new.*) after a day.
remote_exec() {
    local path= req= memory= threads= spill_limit=
    while test $# -gt 0
    do case "$1" in
           --path )        path=$2; shift;;
           --req )         req=$2; shift;;
           --memory )      memory=$2; shift;;
           --threads )     threads=$2; shift;;
           --spill-limit ) spill_limit=$2; shift;;
           -- )            shift; break;;
           * )             die "remote-exec: unknown option: $1";;
       esac; shift; done
    test -n "$path" || die "remote-exec: --path missing"
    export PATH=$path
    local base spill status=0
    mkdir -p "$remote_base/spill" "$remote_base/req"
    base=$(realpath "$remote_base")
    find "$base/spill" "$base/req" -mindepth 1 -maxdepth 1 -mmin +1440 -exec rm -r -- {} + 2>/dev/null || true
    touch "$self_dir/.complete"
    local me d
    me=$(realpath "$self_dir")
    find "$base/pkg" -mindepth 1 -maxdepth 1 -name '.new.*' -mmin +1440 -exec rm -r -- {} + 2>/dev/null || true
    for d in "$base"/pkg/*/
    do d=$(realpath "$d")
       test "$d" != "$me" || continue
       test -z "$(find "$d/.complete" -mtime +30 2>/dev/null)" || rm -r -- "$d"
    done
    spill=$(mktemp -d "$base/spill/XXXXXX")
    BENCH_PARQUET_MEMORY=$memory BENCH_PARQUET_THREADS=$threads \
    BENCH_PARQUET_SPILL=$spill BENCH_PARQUET_SPILL_LIMIT=$spill_limit \
        "$BASH" "$self_dir/bench-parquet" "$@" || status=$?
    rm -r -- "$spill"
    if test -n "$req"; then rm -r -- "$base/req/$(basename "$req")"; fi
    return $status
}
