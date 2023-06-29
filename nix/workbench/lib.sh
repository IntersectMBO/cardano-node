to_jsonlist() {
    for x in "$@"
    do echo "\"$x\""
    done | jq --slurp '.'
}

jq_tolist() {
    local exp=$1; shift
    jq "$exp | join (\" \")" --raw-output "$@"
}

jq_fmutate() {
    local f=$1; shift
    test -f "$f" || { echo '{}' > "$f"; }
    jq "$@" "$f" | sponge "$f"
}

jq_check_json() {
    jq '.' "$1" >/dev/null
}

json_compact_prettify()
{
    for f in "$@"
    do if test -n "$(find $f -size +1M)";
          ## Skip large files.
       then continue; fi

       jq_fmutate "$f" --raw-input --raw-output --slurp \
           'gsub("\n      +";"")|gsub("\n    ]";"]")|gsub(",\"";",\n          \"")' &
    done

    wait
}

jscompact()
{
    json_compact_prettify "$@"
}

helptopcmd() {
    set +x
    local topcmd=$1 cmd=$2; shift 2
    white $topcmd
    echo -n " "
    yellow $cmd
    echo -n " "
    green $*
}

helpcmd() {
    set +x
    local cmd=$1; shift
    yellow $cmd
    echo -n " "
    green $*
}

helpopt() {
    set +x
    green $*
}

__usage() {
    local op=$1 desc=$2
    cat >&2 <<EOF
$(red USAGE:)  $(white $(basename "$0")) $(blue WB-OPTS..) $(red $op) $(green ${op^^[a-z]}-OPTS..) $(yellow SUBOP) $(green SUBOP-ARGS..)

  $(blue $desc):

$(cat)

EOF
}

usage() {
    set +x
    __usage "$@"
    exit 1
}

muffle_trace_set_exit='if test -n "$(echo $- | tr -cd x)"; then set +x; local exit="set -x"; else local exit=; fi'
restore_trace='eval $exit'

declare -A colormap
colormap=(
    [black]=30
    [red]=31
    [green]=32
    [yellow]=33
    [blue]=34
    [magenta]=35
    [cyan]=36
    [white]=37
    [reset]=0
)

color() {
    echo -ne "\033[${colormap[$1]}m"
}

with_color() {
    eval $muffle_trace_set_exit

    local color=$1; shift
    color $color
    echo -ne "$@"
    color reset

    eval $restore_trace
}

colorise_colors=(
    red green blue yellow white cyan
)
colorise() {
    eval $muffle_trace_set_exit

    local i
    for ((i=0; $#!=0; i++))
    do echo -n "$(with_color ${colorise_colors[$((i % 6))]} $1) "
       shift
    done

    eval $restore_trace
}

newline() {
    echo >&2
}

msg() {
    echo "workbench:  $*" >&2
}

msg_ne() {
    echo -ne "workbench:  $*" >&2
}

plain() {
    eval $muffle_trace_set_exit
    with_color reset $*
    eval $restore_trace
}

green() {
    eval $muffle_trace_set_exit
    with_color green $*
    eval $restore_trace
}

blue() {
    eval $muffle_trace_set_exit
    with_color blue $*
    eval $restore_trace
}

white() {
    eval $muffle_trace_set_exit
    with_color white $*
    eval $restore_trace
}

blk() {
    eval $muffle_trace_set_exit
    with_color black $*
    eval $restore_trace
}

yellow() {
    eval $muffle_trace_set_exit
    with_color yellow $*
    eval $restore_trace
}

red() {
    eval $muffle_trace_set_exit
    with_color red $*
    eval $restore_trace
}

verbose() {
    eval $muffle_trace_set_exit

    if test -n "${verbose:-}"
    then local subsys=$1; shift
         msg "$(with_color blue $subsys):  $*"
    fi

    eval $restore_trace
}

progress() {
    eval $muffle_trace_set_exit

    local subsys=$1; shift
    msg "$(with_color green $subsys):  $(with_color blue "$@")"

    eval $restore_trace
}

progress_ne() {
    eval $muffle_trace_set_exit

    local subsys=$1; shift
    msg_ne "$(with_color green $subsys):  $(with_color blue "$@")"

    eval $restore_trace
}

warn() {
    eval $muffle_trace_set_exit

    local subsys=$1; shift
    msg "$(with_color green $subsys):  $(with_color yellow "$@")"

    eval $restore_trace
}

fail() {
    eval $muffle_trace_set_exit

    msg "$(with_color red $*)"
    exit 1
}

fatal() {
    fail "FATAL: $*"
}

jqtest() {
    jq --exit-status "$@" > /dev/null
}

git_repo_commit_description() {
    local repo=$1
    local commit=$(git -C "$repo" rev-parse 'HEAD' 2>/dev/null || true)

    test -n "$commit" && {
        git -C "$repo" describe --match '[0-9].*' --tags $commit 2>/dev/null |
            cut -d'-' -f1,2 | tr -d '\n'
        git -C "$repo" diff --exit-code --quiet || echo '-modified'
    } || echo "unknown-not-a-git-repo"
}

# Wait for any job to fail or all to be OK!
wait_fail_any () {
  local processes=("$@")
  # There are any processes left?
  if test -n "${processes[*]:-}"
  then
    local wait_exit_status
    local exited_process
    wait -n -p exited_process "${processes[@]}"
    wait_exit_status=$?
    # New array without the exited process
    local processes_p=()
    for p in "${processes[@]}"
    do
      if test "${p}" != "${exited_process}"
      then
        processes_p+=("${p}")
      fi
    done
    # Something else to wait for?
    if test -n "${processes_p[*]:-}"
    then
      # Keep waiting or kill 'em all ?'
      if test "${wait_exit_status}" -eq 0
      then
        wait_fail_any "${processes_p[@]}"
      else
        kill "${processes_p[@]}" 2>/dev/null || true
        return "${wait_exit_status}"
      fi
    else
      return "${wait_exit_status}"
    fi
  else
    return 0
  fi
}
