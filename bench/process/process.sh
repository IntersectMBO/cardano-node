#!/bin/sh
# shellcheck disable=SC1090
set -eu

fail() {
    echo -e "ERROR:  $*" >&2
    exit 1
}

default_op='report'
default_format='csv'

format=$default_format
header_footer='false'
no_progress=

while test $# -ge 1
do case "$1" in
       --github )          format='github';;
       --csv )             format='csv'; header_footer='false';;
       --cls )             echo -en "\ec" >&2;;
       --no-progress )     no_progress='true';;
       --trace )           set -x;;
       * )                 break;; esac; shift; done

op=${1:-$default_op}; shift || true

main() {
    PROCESS_ORIG_PWD=$(pwd)
    pushd "$(dirname $0)" >/dev/null || exit 1
    case "${op}" in
        collect )    op_collect "$@";;
        process )    op_process;;
        render )     op_render;;

        report )     op_collect "$@" | op_process | op_render;;

        call )       eval "$@";;
        * ) echo "ERROR:  operation must be one of:  collect process render report" >&2; exit 1;; esac
}

hardcoded_branch='membench'
hardcoded_commit='a7ee17d1af44b571c6e476916bd24ed65db97e15'

function op_collect() {
    local desc=${1?-USAGE: $0 collect DESCRIPTION [FORMAT] [DIR]}
    local format=${2:-membenches_v1}
    local literal_dir=${3:-.}

    local dir=$(pushd "$PROCESS_ORIG_PWD" >/dev/null; realpath "$literal_dir")
    test -d "${dir}" -a -n "$(ls "${dir}"/*/*.json)" ||
      fail "${literal_dir} (realpath $dir) must be a writable directory with subdirectories containing JSON files with ${format} output schema"

    test -n "$no_progress" || echo -ne "Collecting runs in $dir: " >&2
    local args_global=(
        --arg desc    "$desc"
        --arg format  "$format"
        --arg now     "$(date --utc --iso-8601=seconds)"
    )
case $format in
    membenches_v1 )
        {
        for f in $dir/*/refined.json
        do local fpad="$f        "
           test -n "$no_progress" || echo -n "$fpad" >&2
           local args_run=(
               --arg filename "$f"
               --arg format   "$format"
               --arg sha256   "$(sha256sum $f | cut -d' ' -f1)"
               --arg ctime    "$(stat --format=%w $f)"
               --arg mtime    "$(stat --format=%y $f)"
               --arg config   "$(echo $f | xargs dirname | xargs basename | cut -d- -f1)"
               --arg iter     "$(echo $f | xargs dirname | xargs basename | cut -d- -f2)"
               --arg hardcoded_branch $hardcoded_branch
               --arg hardcoded_commit $hardcoded_commit
               --slurpfile data "$f"
           )
           jq 'include "collect";

               standard_run_desc($filename; $sha256; $format; $ctime; $mtime; $hardcoded_branch; $hardcoded_commit; $config; $iter; $data[0])
              ' "$f" "${args_global[@]}" "${args_run[@]}"
           test -n "$no_progress" || printf "${fpad//?/\\b}" >&2
        done
        test -n "$no_progress" || echo >&2; };;
    * )
        fail "unknown result format: $format"
esac |
    jq 'include "'"$format"'";

        { description: $desc
        , format:      $format
        , ctime:       $now
        , runs:        .
        }
        + format_specs
       ' --slurp "${args_global[@]}"
}

function op_process() {
    jq 'include "process";

        . as $batch
        | $batch.runs
        | group_by(.config_name)
        | map (aggregate_config_runs_variables ($batch.config_specs; $batch.result_specs))
        | $batch
          + { configs: . }
       '
}

function op_render() {
    jq 'include "render";

        render('"$header_footer"')
       ' --raw-output
}

###
### Main
###
main "$@"
